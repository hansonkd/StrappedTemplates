{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Text.Strapped.Render 
  ( combineBuckets
  , varBucket
  , bucketLookup
  , bucketFromList
  , emptyBucket
  , render
  , defaultConfig
  ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Monad
import qualified Data.Map as M
import Data.Foldable (foldlM)
import Data.List (intersperse)
import Data.Monoid ((<>), mempty, mconcat)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Text.Strapped.Types
import Text.Parsec.Pos

import Debug.Trace

instance Renderable Builder where
  renderOutput _ = id

instance Renderable Literal where
  renderOutput (RenderConfig _ ef) (LitText s) = fromText $ ef s
  renderOutput _ (LitSafe s)     = fromText s
  renderOutput rc (LitInteger i) = fromShow i
  renderOutput rc (LitDouble i)  = fromShow i
  renderOutput rc (LitBool i)    = fromShow i
  renderOutput _ (LitBuilder b)  = b
  renderOutput rc (LitList l)    = (fromChar '[') <> 
                                   (mconcat $ intersperse (fromChar ',') (map (renderOutput rc) l)) <> 
                                   (fromChar ']')
  renderOutput rc (LitDyn r) = renderOutput rc r
  
-- | Default render configuration. No text escaping.
defaultConfig :: RenderConfig
defaultConfig = RenderConfig (\_ -> return Nothing) id

-- | Basic bucket. Matches on string and return input. Returns Nothing for
--   everything else.
varBucket :: String -> Input m -> InputBucket m
varBucket varName o = bucketFromList [(varName, o)]


-- | If the first bucket fails, try the second.
combineBuckets :: InputBucket m -> InputBucket m -> InputBucket m
combineBuckets (InputBucket a) (InputBucket b) = InputBucket (a ++ b) 

emptyBucket :: InputBucket m
emptyBucket = InputBucket []

bucketLookup :: String -> InputBucket m -> Maybe (Input m)
bucketLookup v (InputBucket []) = Nothing
bucketLookup v (InputBucket (m:ms)) = maybe (bucketLookup v (InputBucket ms)) Just (M.lookup v m)

bucketFromList :: [(String, Input m)] -> InputBucket m
bucketFromList l = InputBucket [M.fromList l]


{-
combineBuckets (InputBucket a) (InputBucket b) = InputBucket $ M.union a b

emptyBucket = InputBucket M.empty

bucketLookup v (InputBucket a) = M.lookup v a

bucketFromList = InputBucket . M.fromList
-}

getOrThrow :: (Monad m) => String -> RenderT m (Input m)
getOrThrow v = do
  !getter <- getBucket
  maybe (getPos >>= (\pos -> throwError $ InputNotFound v pos)) return (bucketLookup v getter)

reduceExpression :: (Monad m) => ParsedExpression -> RenderT m Literal
reduceExpression (ParsedExpression exp pos) = putPos pos >> convert exp
  where convert (LiteralExpression i) = return $ i
        convert (Multipart []) = return $ LitEmpty
        convert (Multipart (f:[])) = reduceExpression f
        convert (Multipart ((ParsedExpression (LookupExpression func) ipos):args)) = do
          val <- getOrThrow func
          case val of
            (Func f) -> convert (Multipart args) >>= f
            _ -> throwParser $ "`" ++ func ++ "` is not a function but has args: " ++ (show args)
        convert (Multipart v) = throwParser $ "`" ++ (show v) ++ "` cannot be reduced."
        convert (ListExpression args) = mapM reduceExpression args >>= (return . LitList) 
        convert (LookupExpression f) = do
            val <- getOrThrow f
            inputToLiteral val
        inputToLiteral inp = case inp of
                    (List args) -> mapM inputToLiteral args >>= (return . LitList)
                    (RenderVal r) -> getConfig >>= (\c -> return $ LitBuilder (renderOutput c r))
                    (Func f) -> f LitEmpty
                    (LitVal v) -> return v
{-
-- | Using a 'TemplateStore' and an 'InputBucket' render the template name.
render :: MonadIO m => RenderConfig -> InputBucket m -> String -> m (Either StrapError Output)
render renderConfig getter' tmplName = do
      tmpl <- liftIO $ tmplStore tmplName
      maybe (return $ Left $ TemplateNotFound tmplName (initialPos tmplName)) 
            (\(Template c) -> runExceptT $ loop mempty mempty getter' c) 
            tmpl
  where tmplStore = templateStore renderConfig
        loop accum _ _ [] = return accum
        loop accum blks getter ((ParsedPiece (StaticPiece s) pos):ps) =
          loop (accum <> s) blks getter ps
        loop accum blks getter ((ParsedPiece (BlockPiece n def) pos):ps) = 
          (maybe (loop accum blks getter def) 
                 (\content -> loop accum blks getter content)
                 (lookup n blks)
          ) >>= (\a -> loop a blks getter ps)
        loop accum blks getter ((ParsedPiece (ForPiece n exp c) pos):ps) = do
          var <- reduceExpression renderConfig exp getter
          case var of 
            LitList l -> (processFor getter n c accum blks l) >>= (\a -> loop a blks getter ps)
            _ -> throwError $ StrapError ("`" ++ show exp ++ "` is not a LitList") pos
        loop accum blks getter ((ParsedPiece (IfPiece exp p n) pos):ps) = do
          var <- reduceExpression renderConfig exp getter
          case (toBool var) of
            True -> (loop accum blks getter p) >>= (\a -> loop a blks getter ps)
            False -> (loop accum blks getter n) >>= (\a -> loop a blks getter ps)
        loop accum blks getter ((ParsedPiece (Inherits n b) pos):ps) =
            liftIO (tmplStore n) >>=
            maybe (throwError (TemplateNotFound n pos))
                  (\(Template c) -> (loop accum (b ++ blks) getter c) >>= 
                                    (\a -> loop a blks getter ps))
        loop accum blks getter ((ParsedPiece (Include n) pos):ps) =
            liftIO (tmplStore n) >>=
            maybe (throwError (TemplateNotFound n pos)) 
                  (\(Template c) -> (loop accum blks getter c) >>=
                                    (\a -> loop a blks getter ps))
        loop accum blks getter ((ParsedPiece (Decl n exp) pos):ps) = 
            (reduceExpression renderConfig exp getter) >>=
            (\v -> loop accum blks (combineBuckets (varBucket n (LitVal v)) getter) ps)

        loop accum blks getter ((ParsedPiece (FuncPiece exp) pos):ps) = 
            (reduceExpression renderConfig exp getter) >>= 
            (\r -> loop (accum <> (renderOutput renderConfig r)) blks getter ps)      
        
        processFor getter varName content accum blks objs = loopFor accum objs
          where loopGetter o = combineBuckets (varBucket varName (LitVal o)) getter
                loopFor accum [] = return accum
                loopFor accum (o:os) = do
                      s <- loop accum blks (loopGetter o) content
                      loopFor s os
                      
-}
                  
putPos :: Monad m => SourcePos -> RenderT m ()
putPos a = RenderT $ lift $ modify (\i -> i { position=a })

putBucket :: Monad m => (InputBucket m) -> RenderT m ()
putBucket a = RenderT $ lift $ modify (\i -> i { bucket=a })

getPos :: Monad m => RenderT m SourcePos
getPos = liftM position getState

getBucket :: Monad m => RenderT m (InputBucket m)
getBucket = liftM bucket getState

getConfig :: Monad m => RenderT m RenderConfig
getConfig = liftM renderConfig getState

getBlocks :: Monad m => RenderT m BlockMap
getBlocks = liftM blocks getState

putBlocks :: Monad m => BlockMap -> RenderT m ()
putBlocks a = RenderT $ lift $  modify (\i -> i { blocks=a })

getState :: Monad m => RenderT m (RenderState m)
getState = RenderT $ lift $ get

throwParser :: (Monad m) => String -> RenderT m b
throwParser s = getPos >>= (\pos -> throwError $ StrapError s pos) 

instance Block Piece where
  process (StaticPiece s) = return s
  process (BlockPiece n default_content) = do
    blks <- getBlocks
    maybe (buildContent default_content) (buildContent) (M.lookup n blks)
  process (ForPiece n exp c) = do
    var <- reduceExpression exp
    curState <- getState
    curBucket <- getBucket
    ret <- case var of 
      LitList l -> forM l (\obj -> do
          r <- lift $ flip evalStateT (curState {bucket=combineBuckets (varBucket n (LitVal obj)) curBucket}) $ runExceptT $ runRenderT $ buildContent c
          either throwError return r) 
          >>= (return . mconcat)
      _ -> throwParser $ "`" ++ show exp ++ "` is not a LitList"
    putBucket curBucket
    return ret
  process (IfPiece exp p n) = do
      var <- reduceExpression exp
      c <- case (toBool var) of
          True -> buildContent p
          False -> buildContent n
      return c
  process (Inherits n b) = do
      tmplStore <- liftM templateStore getConfig
      mtmpl <- liftIO (tmplStore n)
      maybe (getPos >>= (\pos -> throwError (TemplateNotFound n pos)))
                 (\(Template c) -> do 
                      curBlocks <- getBlocks
                      putBlocks $ M.union b curBlocks
                      r <- buildContent c
                      putBlocks curBlocks
                      return r)
                 mtmpl
  process (Include n) = do
    tmplStore <- liftM templateStore getConfig
    mtmpl <- liftIO (tmplStore n)
    maybe (getPos >>= (\pos -> throwError (TemplateNotFound n pos)))
          (\(Template c) -> buildContent c)
          mtmpl
  process (Decl n exp) = do
      val <- (reduceExpression exp)
      bucket <- getBucket
      putBucket $ combineBuckets (varBucket n (LitVal val)) bucket
      return mempty
  process (FuncPiece exp) = do
    config <- getConfig
    val <- reduceExpression exp
    return $ renderOutput config val
    
      
-- buildContent pieces accum = forM_ pieces (\(ParsedPiece piece pos) -> putPos pos >> process piece accum)
buildContent pieces = mapM (\(ParsedPiece piece pos) -> putPos pos >> process piece) pieces >>= (return . mconcat)

render :: (MonadIO m) => RenderConfig -> InputBucket m -> String -> m (Either StrapError Output)
render renderConfig !getter' tmplName = do
      tmpl <- liftIO $ tmplStore tmplName
      maybe (return $ Left $ TemplateNotFound tmplName (initialPos tmplName)) 
            (\(Template c) -> (flip evalStateT startState $ runExceptT $ runRenderT $ buildContent c))
            tmpl
            
  where tmplStore = templateStore renderConfig
        startState = RenderState (initialPos tmplName) renderConfig M.empty getter'
        
        

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
import qualified Data.Map.Strict as M
import Data.Foldable (foldlM)
import Data.List (intersperse)
import Data.Monoid ((<>), mempty, mconcat)
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Writer
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Text.Strapped.Types
import Text.Parsec.Pos

import Debug.Trace
import Data.Conduit
import qualified Data.Conduit.Lift as CLift
import qualified Data.Conduit.List as CL
import Control.DeepSeq

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
combineBuckets (InputBucket !a) (InputBucket !b) = InputBucket $!! (a ++ b) 

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

getOrThrow :: (Monad m) => String -> RenderConduit m (Input m)
getOrThrow v = do
  !getter <- getBucket
  maybe (getPos >>= (\pos -> throwStrap $ InputNotFound v pos)) return (bucketLookup v getter)

reduceExpression :: (Monad m) => ParsedExpression -> RenderConduit m Literal
reduceExpression (ParsedExpression exp pos) = putPos pos >> convert exp >>= (\a -> return $!! a)
  where convert (LiteralExpression i) = return $! i
        convert (Multipart []) = return $ LitEmpty
        convert (Multipart (f:[])) = reduceExpression f
        convert (Multipart ((ParsedExpression (LookupExpression func) ipos):args)) = do
          val <- getOrThrow func
          case val of
            (Func f) -> convert (Multipart args) >>= (\r -> lift $! f r)
            _ -> throwParser $ "`" ++ func ++ "` is not a function but has args: " ++ (show args)
        convert (Multipart v) = throwParser $ "`" ++ (show v) ++ "` cannot be reduced."
        convert (ListExpression args) = mapM reduceExpression args >>= (return . LitList) 
        convert (LookupExpression f) = do
            val <- getOrThrow f
            inputToLiteral val
        inputToLiteral inp = case inp of
                    (List args) -> mapM inputToLiteral args >>= (return . LitList)
                    (RenderVal r) -> getConfig >>= (\c -> return $ LitBuilder (renderOutput c r))
                    (Func f) -> lift $! f LitEmpty
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

modifyRender f = modify' (\i -> let a = f i in a `deepseq` a)
         
putPos :: Monad m => SourcePos -> RenderConduit m ()
putPos a = modifyRender (\i -> i { position=a })

putBucket :: Monad m => (InputBucket m) -> RenderConduit m ()
putBucket b = modifyRender (\i -> i { bucket=b })

getPos :: Monad m => RenderConduit m SourcePos
getPos = gets position

getBucket :: Monad m => RenderConduit m (InputBucket m)
getBucket = gets bucket

getConfig :: Monad m => RenderConduit m RenderConfig
getConfig = gets renderConfig

getBlocks :: Monad m => RenderConduit m BlockMap
getBlocks = gets blocks

putBlocks :: Monad m => BlockMap -> RenderConduit m ()
putBlocks a = modifyRender (\i -> i { blocks=a })

getState :: Monad m => RenderConduit m (RenderState m)
getState = get

throwStrap :: (Monad m) => StrapError -> RenderConduit m b
throwStrap e = lift $ throwError e

throwParser :: (Monad m) => String -> RenderConduit m b
throwParser s = getPos >>= (\pos -> throwStrap (StrapError s pos))

instance Block Piece where
  process (StaticPiece s) = yield s
  process (BlockPiece n default_content) = do
    blks <- getBlocks
    maybe (buildContent default_content) (buildContent) (M.lookup n blks)
  process (ForPiece n exp c) = do
    var <- reduceExpression exp
    curState <- getState
    curBucket <- getBucket
    case var of 
      LitList l -> forM_ l (\(!obj) -> do
          putBucket $ combineBuckets (varBucket n (LitVal obj)) curBucket
          buildContent c)
      _ -> throwParser $ "`" ++ show exp ++ "` is not a LitList"
    putBucket curBucket
  process (IfPiece exp p n) = do
      var <- reduceExpression exp
      case (toBool var) of
          True -> buildContent p
          False -> buildContent n
  process (Inherits n b) = do
      tmplStore <- liftM templateStore getConfig
      mtmpl <- liftIO (tmplStore n)
      maybe (getPos >>= (\pos -> throwError (TemplateNotFound n pos)))
                 (\(Template c) -> do 
                      curBlocks <- getBlocks
                      putBlocks $ M.union b curBlocks
                      buildContent c
                      putBlocks curBlocks)
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
      yield mempty
  process (FuncPiece !exp) = do
    config <- getConfig
    val <- reduceExpression exp
    yield $! renderOutput config $!! val
    
      
-- buildContent pieces accum = forM_ pieces (\ -> putPos pos >> process piece accum)
buildContent :: MonadIO m => [ParsedPiece] -> RenderSource m
buildContent pieces = do
    let source = CL.sourceList pieces
        conduit = awaitForever $ \(ParsedPiece piece pos) -> do
              modifyRender  (\i -> i { position=pos })
              toProducer $ process piece           
    source =$= conduit
    -- mapM (\(ParsedPiece piece pos) -> putPos pos >> process piece) pieces >>= (return . mconcat)

render :: (MonadIO m) => RenderConfig -> InputBucket m -> String -> m (Either StrapError Output)
render renderConfig !getter' tmplName = do
      tmpl <- liftIO $ tmplStore tmplName
      maybe (return $ Left $ TemplateNotFound tmplName (initialPos tmplName)) 
            (\(Template c) -> processTemplate c)
            tmpl
            
  where processTemplate blocks = runExceptT (CLift.evalStateC startState (buildContent blocks) $$ CL.foldMapM return)

        tmplStore = templateStore renderConfig
        startState = RenderState (initialPos tmplName) renderConfig M.empty getter'        
        
        

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
{-# INLINE varBucket #-}   

-- | If the first bucket fails, try the second.
combineBuckets :: InputBucket m -> InputBucket m -> InputBucket m
combineBuckets (InputBucket a) (InputBucket b) = InputBucket (a ++ b) 
{-# INLINE combineBuckets #-}   

emptyBucket :: InputBucket m
emptyBucket = InputBucket []

bucketLookup :: String -> InputBucket m -> Maybe (Input m)
bucketLookup v (InputBucket []) = Nothing
bucketLookup v (InputBucket (m:ms)) = maybe (bucketLookup v (InputBucket ms)) Just (M.lookup v m)
{-# INLINE bucketLookup #-}   

bucketFromList :: [(String, Input m)] -> InputBucket m
bucketFromList l = InputBucket [M.fromList l]
{-# INLINE bucketFromList #-}   

{-
combineBuckets (InputBucket a) (InputBucket b) = InputBucket $ M.union a b

emptyBucket = InputBucket M.empty

bucketLookup v (InputBucket a) = M.lookup v a

bucketFromList = InputBucket . M.fromList
-}

getOrThrow :: (Monad m) => String -> RenderT m (Input m)
getOrThrow v = do
  getter <- getBucket
  maybe (getPos >>= (\pos -> throwError $ InputNotFound v pos)) return (bucketLookup v getter)

{-# INLINE getOrThrow #-}

reduceExpression :: (Monad m) => ParsedExpression -> RenderT m Literal
reduceExpression (ParsedExpression exp pos) = convert exp
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

{-# INLINE reduceExpression #-}

                 
putPos :: Monad m => SourcePos -> RenderT m ()
putPos a = RenderT $ modify (\i -> i { position=a })

putBucket :: Monad m => (InputBucket m) -> RenderT m ()
putBucket a = RenderT $ modify (\i -> i { bucket=a })

getPos :: Monad m => RenderT m SourcePos
getPos = liftM position getState

getBucket :: Monad m => RenderT m (InputBucket m)
getBucket = liftM bucket getState
{-# INLINE getBucket #-}

getConfig :: Monad m => RenderT m RenderConfig
getConfig = liftM renderConfig getState
{-# INLINE getConfig #-}   

getBlocks :: Monad m => RenderT m BlockMap
getBlocks = liftM blocks getState

putBlocks :: Monad m => BlockMap -> RenderT m ()
putBlocks a = RenderT $ lift $  modify (\i -> i { blocks=a })

getState :: Monad m => RenderT m (RenderState m)
getState = RenderT $ get

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
      LitList l -> foldlM (\acc obj -> buildContent c >>= return . (<>) acc ) mempty l
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
buildContent pieces = foldlM (\acc (ParsedPiece piece pos) -> (process piece) >>= return . (<>) acc ) mempty pieces
{-# INLINE buildContent #-}   

render :: (MonadIO m) => RenderConfig -> InputBucket m -> String -> m (Either StrapError Output)
render renderConfig !getter' tmplName = do
      tmpl <- liftIO $ tmplStore tmplName
      maybe (return $ Left $ TemplateNotFound tmplName (initialPos tmplName)) 
            (\(Template c) -> (flip evalStateT startState $ runExceptT $ runRenderT $ buildContent c))
            tmpl
            
  where tmplStore = templateStore renderConfig
        startState = RenderState (initialPos tmplName) renderConfig M.empty getter'
        
        

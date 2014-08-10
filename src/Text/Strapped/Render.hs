{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE  RecordWildCards #-}
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
import qualified Data.Text.IO as T
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Writer
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Text.Strapped.Types
import Text.Parsec.Pos
import Text.Strapped.Expressions
import Text.Strapped.Utils
import Data.IORef
import Debug.Trace
import Data.Conduit
import           Data.Conduit.Internal            (ConduitM (..), Pipe (..))

import qualified Data.Conduit.Lift as CLift
import qualified Data.Conduit.List as CL
import Control.DeepSeq
import Text.Strapped.State


modifyRender f = {-# SCC modify_render #-} modify' (\i -> let a = f i in a `deepseq` a)
         
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
  process (StaticPiece s) = {-# SCC static_yield #-} yield s
  process (BlockPiece n default_content) = {-# SCC block_piece #-} do
    blks <- getBlocks
    maybe (buildContent default_content) (\a -> buildContent $!! a) (M.lookup n blks)
  process (ForPiece n exp c) = {-# SCC for_piece #-} do
    var <- reduceExpression exp
    curBucket <- getBucket
    curState <- getState
    putBucket $ combineBuckets (varBucket n (LitVal $ LitInteger 1)) curBucket
    case var of 
      LitList l -> {-# SCC forLoopForM #-} forM_ l (\(!obj) -> do
          buildContent $!! c)
      _ -> throwParser $ "`" ++ show exp ++ "` is not a LitList"
    putBucket curBucket
  process (IfPiece exp p n) = do
      var <- reduceExpression exp
      case (toBool var) of
          True -> buildContent p
          False -> buildContent n
  process (Inherits n b) = {-# SCC inherits_piece #-}do
      tmplStore <- liftM templateStore getConfig
      mtmpl <- liftIO (tmplStore n)
      maybe (getPos >>= (\pos -> throwError (TemplateNotFound n pos)))
                 (\(Template c) -> do 
                      curBlocks <- getBlocks
                      putBlocks $ M.union b curBlocks
                      buildContent c
                      putBlocks curBlocks)
                 mtmpl
  process (Include n) = {-# SCC include_piece #-} do
    tmplStore <- liftM templateStore getConfig
    mtmpl <- liftIO (tmplStore n)
    maybe (getPos >>= (\pos -> throwError (TemplateNotFound n pos)))
          (\(Template c) -> buildContent c)
          mtmpl
  process (Decl n exp) = {-# SCC decl_piece #-} do
      val <- (reduceExpression exp)
      bucket <- getBucket
      putBucket $ combineBuckets (varBucket n (LitVal val)) bucket
      yield mempty
  process (FuncPiece !exp) = {-# SCC func_piece #-} do
    config <- getConfig
    val <- reduceExpression exp
    yield $! {-# SCC renderDirect #-} renderOutput config $!! val
    
  
reduceExpression :: (Monad m) => ParsedExpression -> RenderConduit m Literal
reduceExpression exp = get >>= (\s -> lift $ lift $ convertExp s exp)

convertExp :: Monad m => RenderState m -> ParsedExpression -> ExceptT StrapError m Literal
convertExp state@RenderState{..} (ParsedExpression exp pos) = {-# SCC convertExp #-} convert exp        
  where convertMore = convertExp state
        convert (LiteralExpression i) = return $! i
        convert (Multipart []) = return $ LitEmpty
        convert (Multipart (f:[])) = convertMore f
        convert (Multipart ((ParsedExpression (LookupExpression func) ipos):args)) = do
          val <- getOrThrow func
          case val of
            (Func f) -> convert (Multipart args) >>= f
            _ -> throwError $ StrapError ("`" ++ func ++ "` is not a function but has args: " ++ (show args)) pos
        convert (Multipart v) = throwError $ StrapError ("`" ++ (show v) ++ "` cannot be reduced.") pos
        convert (ListExpression args) = mapM convertMore args >>= (return . LitList) 
        convert (LookupExpression f) = do
            !val <- getOrThrow f
            inputToLiteral $! val
        inputToLiteral (!inp) = case inp of
                    (List args) -> mapM inputToLiteral args >>= (\a -> return $! LitList a)
                    (RenderVal r) -> return $ LitText (renderOutput renderConfig r)
                    (Func f) -> f LitEmpty
                    (LitVal v) -> return $! v
        getOrThrow v = maybe (throwError $ InputNotFound v pos) return (bucketLookup v bucket)


-- | Default render configuration. No text escaping.
defaultConfig :: RenderConfig
defaultConfig = RenderConfig (\_ -> return Nothing) id


-- buildContent pieces accum = forM_ pieces (\ -> putPos pos >> process piece accum)
buildContent :: MonadIO m => [ParsedPiece] -> RenderSource m
buildContent pieces = {-# SCC buildContentConduit_9 #-}  do
    let source = {-# SCC buildContentConduitSourceList #-} CL.sourceList $!! pieces
        conduit = {-# SCC buildContentConduitAwait #-} awaitForever $ \(ParsedPiece piece pos) -> do
              modifyRender  (\i -> let a = i { position=pos } in a `deepseq` a)
              {-# SCC process_piece #-} yield piece           
    {-# SCC buildContentConduit #-} source =$= conduit =$= awaitForever (\a -> {-# SCC process_piece #-} toProducer $ process a)
    -- mapM (\(ParsedPiece piece pos) -> putPos pos >> process piece) pieces >>= (return . mconcat)

render ::  RenderConfig -> InputBucket IO -> String -> IO (Either StrapError Output)
render renderConfig !getter' tmplName = do
      tmpl <- liftIO $ tmplStore tmplName
      maybe (return $ Left $ TemplateNotFound tmplName (initialPos tmplName)) 
            (\(Template c) -> (processTemplate c))
            tmpl
            
  where processTemplate blocks = do
          iRef <- newIORef mempty
          me <- runExceptT (CLift.evalStateC startState (buildContent blocks) $$ awaitForever $ (\(i) -> liftIO $ {-# SCC m_ioref #-} modifyIORef' iRef (\a -> a <> i)))
          either (return . Left) (\_ -> liftIO $ readIORef iRef >>= (return . Right)) me
        tmplStore = templateStore renderConfig
        startState = RenderState (initialPos tmplName) renderConfig M.empty getter'
        
        
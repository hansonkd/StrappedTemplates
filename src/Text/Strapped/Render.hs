module Text.Strapped.Render 
  ( combineBuckets
  , varBucket
  , bucketLookup
  , bucketFromList
  , render
  , defaultConfig
  ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Monad
import qualified Data.Map as M
import Data.List (intersperse)
import Data.Monoid ((<>), mempty, mconcat)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as T
import Text.Strapped.Types
import Text.Parsec.Pos


instance Renderable Builder where
  renderOutput _ = id

instance Renderable Literal where
  renderOutput (RenderConfig _ ef) (LitText s) = fromLazyText $ ef s
  renderOutput _ (LitSafe s)     = fromLazyText s
  renderOutput rc (LitInteger i) = fromShow i
  renderOutput rc (LitDouble i)  = fromShow i
  renderOutput _ (LitBuilder b)  = b
  renderOutput rc (LitList l)    = (fromChar '[') <> 
                                   (mconcat $ intersperse (fromChar ',') (map (renderOutput rc) l)) <> 
                                   (fromChar ']')
  renderOutput rc (LitDyn r) = renderOutput rc r
  
-- | Default render configuration. No text escaping.
defaultConfig :: RenderConfig
defaultConfig = RenderConfig (\_ -> return Nothing) id

-- | If the first bucket fails, try the second.
combineBuckets :: InputBucket m -> InputBucket m -> InputBucket m
combineBuckets = (++) 

-- | Basic bucket. Matches on string and return input. Returns Nothing for
--   everything else.
varBucket :: String -> Input m -> InputBucket m
varBucket varName o = [M.fromList [(varName, o)]]

bucketLookup :: String -> InputBucket m -> Maybe (Input m)
bucketLookup v [] = Nothing
bucketLookup v (m:ms) = maybe (bucketLookup v ms) Just (M.lookup v m)

bucketFromList :: [(String, Input m)] -> InputBucket m
bucketFromList l = [M.fromList l]

getOrThrow v getter pos = maybe (throwError $ InputNotFound v pos) return (bucketLookup v getter)

reduceExpression :: Monad m => RenderConfig -> ParsedExpression -> InputBucket m -> ExceptT StrapError m Literal
reduceExpression c (ParsedExpression exp pos) getter = convert exp
  where convertMore exp = reduceExpression c exp getter
        convert (IntegerExpression i) = return $ LitInteger i
        convert (FloatExpression i) = return $ LitDouble i
        convert (StringExpression s) = return $ LitText (T.pack s)
        convert (Multipart []) = return $ LitEmpty
        convert (Multipart (f:[])) = convertMore f
        convert (Multipart ((ParsedExpression (LookupExpression func) ipos):args)) = do
          val <- getOrThrow func getter pos
          case val of
            (Func f) -> convert (Multipart args) >>= f
            _ -> throwError $ StrapError ("`" ++ func ++ "` is not a function but has args: " ++ (show args)) ipos
        convert (Multipart v) = throwError $ StrapError ("`" ++ (show v) ++ "` cannot be reduced.") pos
        convert (ListExpression args) = mapM convertMore args >>= (return . LitList) 
        convert (LookupExpression f) = do
            val <- getOrThrow f getter pos
            inputToLiteral val
        inputToLiteral inp = case inp of
                    (List args) -> mapM inputToLiteral args >>= (return . LitList)
                    (RenderVal r) -> return $ LitBuilder (renderOutput c r)
                    (Func f) -> f LitEmpty
                    (LitVal v) -> return v

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

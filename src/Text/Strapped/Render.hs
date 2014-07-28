module Text.Strapped.Render 
  ( combineBuckets
  , varBucket
  , render
  , defaultConfig
  ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Control.Monad
import Data.Monoid ((<>), mempty, mconcat)
import Control.Monad.Error
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as T
import Text.Strapped.Types

instance Renderable Builder where
  renderOutput _ = id

instance Renderable Literal where
  renderOutput (RenderConfig _ ef) (LitText s) = fromText $ T.toStrict $ ef s
  renderOutput _ (LitSafe s)     = fromText $ T.toStrict s
  renderOutput rc (LitInteger i)     = renderOutput rc (LitText $ T.pack $ show i)
  renderOutput rc (LitDouble i)   = renderOutput rc (LitText $ T.pack $ show i)
  renderOutput _ (LitBuilder b)  = b
  renderOutput rc (LitList l)    = mconcat $ map (renderOutput rc) l 
  renderOutput rc (LitDyn r) = renderOutput rc r
  
-- | Default render configuration. No text escaping.
defaultConfig :: RenderConfig
defaultConfig = RenderConfig (\_ -> return Nothing) id

-- | If the first bucket fails, try the second.
combineBuckets :: InputBucket m -> InputBucket m -> InputBucket m
combineBuckets g1 g2 i = maybe (g2 i) Just (g1 i) 

-- | Basic bucket. Matches on string and return input. Returns Nothing for
--   everything else.
varBucket :: String -> Input m -> InputBucket m
varBucket varName o v | v == varName = Just o
                      | otherwise    = Nothing

getOrThrow v getter = maybe (throwError $ InputNotFound v) return (getter v)

reduceExpression :: Monad m => RenderConfig -> Expression -> InputBucket m -> ErrorT StrapError m Literal
reduceExpression c exp getter = convert exp
  where convert (IntegerExpression i) = return $ LitInteger i
        convert (FloatExpression i) = return $ LitDouble i
        convert (StringExpression s) = return $ LitText (T.pack s)
        convert (Multipart []) = return $ LitEmpty
        convert (Multipart (f:[])) = convert f
        convert (Multipart ((LookupExpression func):args)) = do
          val <- getOrThrow func getter
          case val of
            (Func f) -> convert (Multipart args) >>= f
            _ -> throwError $ StrapError $ func ++ " is not a function but has args: " ++ (show args)
        convert (Multipart v) = throwError $ StrapError $ (show v) ++ " cannot be reduced."
        convert (ListExpression args) = mapM convert args >>= (return . LitList) 
        convert (LookupExpression f) = do
            val <- getOrThrow f getter
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
      maybe (return $ Left $ TemplateNotFound tmplName) 
            (\(Template c) -> runErrorT $ loop mempty mempty getter' c) 
            tmpl
  where tmplStore = templateStore renderConfig
        loop accum _ _ [] = return accum
        loop accum blks getter ((StaticPiece s):ps) =
          loop (accum <> s) blks getter ps
        loop accum blks getter ((BlockPiece n def):ps) = 
          (maybe (loop accum blks getter def) 
                 (\content -> loop accum blks getter content)
                 (lookup n blks)
          ) >>= (\a -> loop a blks getter ps)
        loop accum blks getter ((ForPiece n l c):ps) = 
          maybe (throwError $ InputNotFound n) 
          (\l -> (processFor getter n c accum blks l) >>= 
                 (\a -> loop a blks getter ps))
          (getter l)
        loop accum blks getter ((Inherits n b):ps) =
            liftIO (tmplStore n) >>=
            maybe (throwError $ TemplateNotFound n) 
                  (\(Template c) -> (loop accum (b ++ blks) getter c) >>= 
                                    (\a -> loop a blks getter ps))
        loop accum blks getter ((Include n):ps) =
            liftIO (tmplStore n) >>=
            maybe (throwError $ TemplateNotFound n) 
                  (\(Template c) -> (loop accum blks getter c) >>=
                                    (\a -> loop a blks getter ps))
        loop accum blks getter ((Decl n exp):ps) = (reduceExpression renderConfig exp getter) >>=
                                                   (\v -> loop accum blks (combineBuckets (varBucket n (LitVal v)) getter) ps)

        loop accum blks getter ((FuncPiece exp):ps) = (reduceExpression renderConfig exp getter) >>= 
                                                         (\r -> loop (accum <> (renderOutput renderConfig r)) blks getter ps)

        parseArgs args getter = (forM args (\arg -> maybe (throwError $ InputNotFound arg) return (getter arg)))
        
        
        processFor getter varName content accum blks (List objs) = loopFor accum objs
          where loopGetter o = combineBuckets (varBucket varName o) getter
                loopFor accum [] = return accum
                loopFor accum (o:os) = do
                      s <- loop accum blks (loopGetter o) content
                      loopFor s os
        processFor _ varName _ _ _ _ = throwError $ StrapError varName

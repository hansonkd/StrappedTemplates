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
  renderOutput rc (LitInt i)     = renderOutput rc (LitText $ T.pack $ show i)
  renderOutput _ (LitBuilder b)  = b
  renderOutput rc (LitList l)    = mconcat $ map (renderOutput rc) l 
  
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

-- | Using a 'TemplateStore' and an 'InputBucket' render the template name.
render :: MonadIO m => RenderConfig -> InputBucket m -> String -> m (Either StrapError Output)
render renderConfig getter' tmplName = do
      tmpl <- liftIO $ tmplStore tmplName
      maybe (return $ Left $ TemplateNotFound tmplName) 
            (\(Template c blks) -> runErrorT $ loop mempty blks getter' c) 
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
        loop accum blks getter ((Extends n):ps) =
            liftIO (tmplStore n) >>=
            maybe (throwError $ TemplateNotFound n) 
                  (\(Template c b) -> (loop accum (blks ++ b) getter c) >>= 
                                      (\a -> loop a blks getter ps))
        loop accum blks getter ((Include n):ps) =
            liftIO (tmplStore n) >>=
            maybe (throwError $ TemplateNotFound n) 
                  (\(Template c _) -> (loop accum blks getter  c) >>= 
                                      (\a -> loop a blks getter ps))
        loop accum blks getter ((Decl n fn args):ps) = 
          case (getter fn) of
            Just (Func f) -> parseArgs args getter >>= f >>= (\r -> loop accum blks (combineBuckets (varBucket n (LitVal r)) getter) ps)
            Just v        -> loop accum blks (combineBuckets (varBucket n v) getter) ps
            Nothing       -> throwError $ InputNotFound fn
        -- 0 arity functions could be a value or a monadic function.
        loop accum blks getter ((FuncPiece n []):ps) = 
          case (getter n) of
              Just (Func f) -> (f []) >>= 
                               (\s -> loop (accum <> (renderOutput renderConfig s)) blks getter ps)
              Just (RenderVal v) -> loop (accum <> (renderOutput renderConfig v)) blks getter ps
              Just (LitVal v) -> loop (accum <> (renderOutput renderConfig v)) blks getter ps
              Just _ -> throwError $ StrapError $ n ++ " is not a function."
              Nothing -> throwError $ InputNotFound n
        loop accum blks getter ((FuncPiece n args):ps) = 
          case (getter n) of
              Just (Func f) ->  (parseArgs args getter) >>= 
                                (\inputs -> (f inputs) >>= 
                                            (\s -> loop (accum <> (renderOutput renderConfig s)) blks getter ps)
                                )
              Just _ -> throwError $ StrapError $ n ++ " is not a function."
              Nothing -> throwError $ InputNotFound n
        getBlock _ = Nothing
        
        parseArgs args getter = (forM args (\arg -> maybe (throwError $ InputNotFound arg) return (getter arg)))
        
        processFor getter varName content accum blks (List objs) = loopFor accum objs
          where loopGetter o = combineBuckets (varBucket varName o) getter
                loopFor accum [] = return accum
                loopFor accum (o:os) = do
                      s <- loop accum blks (loopGetter o) content
                      loopFor s os
        processFor _ varName _ _ _ _ = throwError $ StrapError varName

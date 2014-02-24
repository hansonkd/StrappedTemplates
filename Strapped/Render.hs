module Strapped.Render where
  
import Control.Monad
import Data.Monoid ((<>), mempty)
import Control.Monad.Error
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import Strapped.Types

-- | If the first getter fails, try the second.
combineGetter :: InputGetter m -> InputGetter m -> InputGetter m
combineGetter g1 g2 i = maybe (g2 i) Just (g1 i) 

varGetter :: String -> Input m -> InputGetter m
varGetter varName o v | v == varName = Just o
                      | otherwise    = Nothing

render :: MonadIO m => TemplateStore -> InputGetter m -> String -> m (Either StrapError Output)
render tmplStore getter' tmplName = do
      tmpl <- liftIO $ tmplStore tmplName
      maybe (return $ Left $ TemplateNotFound tmplName) 
            (\(Template c blks) -> runErrorT $ loop mempty blks getter' c) 
            tmpl
  where loop accum _ _ [] = return accum
        loop accum blks getter ((StaticPiece s):ps) = loop (accum <> s) blks getter ps
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
        loop accum blks getter ((Decl n fn arg):ps) = 
          (loop mempty blks getter [FuncPiece fn arg]) >>= 
          (\r -> loop accum blks (combineGetter (varGetter n (Value r)) getter) ps)
        -- 0 arity functions could be a value or a monadic function.
        loop accum blks getter ((FuncPiece n []):ps) = 
          case (getter n) of
              Just (Func f) -> (f []) >>= 
                               (\s -> loop (accum <> s) blks getter ps)
              Just (Value v) -> loop (accum <> v) blks getter ps
              Nothing -> throwError $ InputNotFound n
        loop accum blks getter ((FuncPiece n args):ps) = 
          case (getter n) of
              Just (Func f) ->  (parseArgs args getter) >>= 
                                (\inputs -> (f inputs) >>= 
                                            (\s -> loop (accum <> s) blks getter ps)
                                )
              Just _ -> throwError $ StrapError $ n ++ " is not a function."
              Nothing -> throwError $ InputNotFound n
        getBlock _ = Nothing
        
        parseArgs args getter = (forM args (\arg -> maybe (throwError $ InputNotFound arg) return (getter arg)))
        
        processFor getter varName content accum blks (List objs) = loopFor accum objs
          where loopGetter o = combineGetter (varGetter varName o) getter
                loopFor accum [] = return accum
                loopFor accum (o:os) = do
                      s <- loop accum blks (loopGetter o) content
                      loopFor s os
        processFor _ varName _ _ _ _ = throwError $ StrapError varName

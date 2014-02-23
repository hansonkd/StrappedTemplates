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

render :: MonadIO m => InputGetter m -> Template -> m (Either StrapError Output)
render getter' (Template c blocks) = runErrorT $ loop mempty getter' c
  where loop accum getter [] = return accum
        loop accum getter ((StaticPiece s):ps) = loop (accum <> s) getter ps
        loop accum getter ((VarPiece varName):ps) = maybe (throwError $ InputNotFound varName) 
                                                    (\var -> do { v <- renderVar varName var; loop (accum <> v) getter ps}) (getter varName)
        loop accum getter ((BlockPiece n def):ps) = (maybe (loop accum getter def) (\content -> loop accum getter content) (getBlock n)) >>= (\a -> loop a getter ps)
        loop accum getter ((ForPiece n l c):ps) = maybe (throwError $ InputNotFound n) ((\l -> (processFor getter n c accum l) >>= (\a -> loop a getter ps))) (getter l)
        loop accum getter ((FuncPiece n args):ps) = case (getter n) of
                                                Just (Func f) -> (forM args (\arg -> maybe (throwError $ InputNotFound arg) return (getter arg))) >>= 
                                                                      (\inputs -> (f inputs) >>= (\s -> loop (accum <> s) getter ps))
                                                                      
                                                Just _ -> throwError $ StrapError $ n ++ " is not a function."
                                                Nothing -> throwError $ InputNotFound n
        getBlock _ = Nothing
        
        renderVar _ (Value v) = return $ v
        renderVar varName _ = throwError $ StrapError $ varName ++ " is not renderable."
        
        processFor getter varName content accum (List objs) = loopFor accum objs
          where getVarName o v | v == varName = Just o
                               | otherwise    = Nothing
                loopGetter o = combineGetter (getVarName o) getter
                
                loopFor accum [] = return accum
                loopFor accum (o:os) = do
                      s <- loop accum (loopGetter o) content
                      loopFor s os
        processFor _ varName _ _ _ = throwError $ StrapError varName

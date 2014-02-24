module Text.Strapped.Utils where

import           Control.Monad
import           Data.List hiding (find)
import           Text.Strapped.Types
import           Text.Strapped.Parser

import           System.FilePath.Find
import           System.FilePath.Posix (addTrailingPathSeparator)

import           Text.ParserCombinators.Parsec

templateStoreFromList :: [(String, String)] -> Either ParseError TemplateStore
templateStoreFromList tmpls = do
  templateTups <- forM tmpls (\(tn, t) -> fmap ((,) tn) $ parseTemplate t tn)
  return (\n -> return $ lookup n templateTups)

-- | Given a file path and extension, load all templates in a directory, recursively.
templateStoreFromDirectory :: FilePath -> String -> IO (Either ParseError TemplateStore)
templateStoreFromDirectory dir ext = do
  files <- find always (extension ==? ext) dirPath
  tmpls <- forM files (\fn -> let fname = maybe [] id $ stripPrefix dirPath fn 
                              in print fname >> readFile fn >>= (return . (,) fname))
  return $ templateStoreFromList tmpls
  where dirPath = addTrailingPathSeparator dir

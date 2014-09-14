module Text.Strapped.Utils where

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char.Utf8
import           Control.Monad
import           Data.List hiding (find)
import           Text.Strapped.Types
import           Text.Strapped.Parser
import           Data.Typeable

import           System.FilePath.Find
import           System.FilePath.Posix (addTrailingPathSeparator)

import           Text.ParserCombinators.Parsec

templateStoreFromList :: StrappedConfig -> [(String, String)] -> Either ParseError TemplateStore
templateStoreFromList config tmpls = do
  templateTups <- forM tmpls (\(tn, t) -> fmap ((,) tn) $ parseTemplate config t tn)
  return (\n -> return $ lookup n templateTups)

-- | Given a file path and extension, load all templates in a directory, recursively.
templateStoreFromDirectory :: StrappedConfig -> FilePath -> String -> IO (Either ParseError TemplateStore)
templateStoreFromDirectory config dir ext = do
  files <- find always (extension ==? ext) dirPath
  tmpls <- forM files (\fn -> let fname = maybe [] id $ stripPrefix dirPath fn 
                              in print fname >> readFile fn >>= (return . (,) fname))
  return $! templateStoreFromList config tmpls
  where dirPath = addTrailingPathSeparator dir

putStore :: TemplateStore -> StrappedConfig -> StrappedConfig
putStore ts rc = rc { templateStore = ts } 

showToBuilder :: Show a => a -> Builder
showToBuilder = fromShow

lit :: ToLiteral a => a -> Input m
lit = LitVal . toLiteral

dyn :: (Renderable a, Typeable a) => a -> Input m
dyn = LitVal . LitDyn
module Text.Strapped.Utils where

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char.Utf8
import           Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.List hiding (find)
import           Text.Strapped.Types
import           Text.Strapped.Parser
import Data.Monoid ((<>), mempty, mconcat)

import           System.FilePath.Find
import           System.FilePath.Posix (addTrailingPathSeparator)

import           Text.ParserCombinators.Parsec
import           Control.DeepSeq

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
  return $! templateStoreFromList tmpls
  where dirPath = addTrailingPathSeparator dir

putStore :: TemplateStore -> RenderConfig -> RenderConfig
putStore ts rc = rc { templateStore = ts } 

showToOutput :: Show a => a -> Output
showToOutput = T.pack . show


-- | Basic bucket. Matches on string and return input. Returns Nothing for
--   everything else.
varBucket :: String -> Input m -> InputBucket m
varBucket varName o = bucketFromList [(varName, o)]
{-
-- | If the first bucket fails, try the second.
combineBuckets :: InputBucket m -> InputBucket m -> InputBucket m
combineBuckets (InputBucket a) (InputBucket b) = InputBucket $! (a ++ b) 

emptyBucket :: InputBucket m
emptyBucket = InputBucket []

bucketLookup :: String -> InputBucket m -> Maybe (Input m)
bucketLookup v (InputBucket []) = Nothing
bucketLookup v (InputBucket (m:ms)) = maybe (bucketLookup v (InputBucket ms)) Just (M.lookup v m)

bucketFromList :: [(String, Input m)] -> InputBucket m
bucketFromList l = InputBucket [M.fromList l]
-}

instance Renderable Builder where
  renderOutput _ a = T.decodeUtf8 $ toByteString a

instance Renderable Literal where
  renderOutput (RenderConfig _ ef) (LitText s) = ef s
  renderOutput _ (LitSafe s)     = s
  renderOutput rc (LitInteger i) = showToOutput $! i
  renderOutput rc (LitDouble i)  = showToOutput $! i
  renderOutput rc (LitBool i)    = showToOutput $! i
  renderOutput rc (LitBuilder b)  = renderOutput rc b
  renderOutput rc (LitList l)    = (T.singleton '[') <> 
                                   (mconcat $ intersperse (T.singleton ',') (map (renderOutput rc) l)) <> 
                                   (T.singleton ']')
  renderOutput rc (LitDyn r) = renderOutput rc $!! r


combineBuckets (InputBucket a) (InputBucket b) = {-# SCC combine_bucket #-} InputBucket $ M.union a b

emptyBucket = InputBucket M.empty

bucketLookup v (InputBucket a) = {-# SCC bucket_lookup #-} M.lookup v a

bucketFromList = InputBucket . M.fromList

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time

import Text.Strapped

makeBucket :: Integer -> InputBucket IO
makeBucket i = bucketFromList 
      [ ("fromFile", Func readFileFunc)
      , ("linesFromFile", Func readLines)
      ]

readFileFunc (LitText t) = (liftIO $ T.readFile (T.unpack t)) >>= (return . LitText)
readFileFunc _ = return LitEmpty

readLines (LitText t) = (liftIO $ T.readFile (T.unpack t)) >>= (\t -> return $ LitList $ map (LitText . (T.replace "{" "&#x7b;") . (T.replace "}" "&#x7d;"))  (T.lines t))
readLines _ = return LitEmpty

main :: IO ()
main = do
  tmpls <- templateStoreFromDirectory defaultConfig "examples/templates" ".strp"
  case tmpls of
    Left err -> print err
    Right store -> do
      rendered <- render (defaultConfig {templateStore = store}) (makeBucket 2) "README.strp"
      either (print) (BS.writeFile "README.md" . B.toByteString) rendered
      cabalrendered <- render (defaultConfig {templateStore = store}) (makeBucket 2) "StrappedTemplates.cabal.strp"
      either (print) (BS.writeFile "StrappedTemplates.cabal" . B.toByteString) cabalrendered
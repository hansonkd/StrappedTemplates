import Control.Monad
import Control.Monad.IO.Class
import Text.Strapped
import Criterion.Main
import qualified Blaze.ByteString.Builder as B

import Text.ParserCombinators.Parsec

import Data.Time

makeBucket :: Int -> InputBucket IO
makeBucket i = bucket
  where bucket "render_size" = Just $ LitVal $ LitInt i
        bucket "is" = Just $ List $ map (LitVal . LitInt) [1..i]
        bucket "ioTime" = Just $ Func (\_ -> (liftIO $ getCurrentTime) >>= (\c -> return $ LitString $ show c) )
        bucket _ = Nothing

benchmarks st = map (\i -> bench (show i) $ whnfIO $ (liftM (fmap (B.toByteString)) $ render st (makeBucket i) "big.strp")) [100,200..1000]

main :: IO ()
main = do
  tmpls <- templateStoreFromDirectory "benchmarks/strapped_templates" ".strp"
  case tmpls of
    Left err -> print err
    Right store -> do
      rendered <- render store (makeBucket 2) "big.strp"
      either (print) (print . B.toByteString) rendered
      defaultMain [ bgroup "builder" (benchmarks store) ]

  
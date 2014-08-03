import Control.Monad.IO.Class
import qualified Blaze.ByteString.Builder as B
import qualified Data.Text.Lazy as T
import Data.Time
import Data.Typeable
import qualified Data.ByteString as BS
import Data.Monoid
import Text.Strapped


data Custom = Custom
  deriving (Show)

instance Renderable Custom where
  renderOutput _ c = showToBuilder c

instance Renderable UTCTime where
  renderOutput _ c = showToBuilder c

instance Renderable NominalDiffTime where
  renderOutput _ c = showToBuilder c

makeBucket :: Integer -> UTCTime -> InputBucket IO
makeBucket i t = bucketFromList [
          ("custom", RenderVal Custom),
          ("whenLoaded", LitVal $ LitDyn t),
          ("render_size", LitVal $ LitInteger i),
          ("is", List $ map (LitVal . LitInteger) [1..i]),
          ("ioTime", Func (\_ -> (liftIO $ getCurrentTime) >>= (\c -> return $ LitDyn $ c) )),
          ("diffTime", Func diffTime),
          ("addNumbers", Func add)
        ]
  where add (LitList ((LitInteger a):(LitInteger b):[])) = return $ LitInteger $ a + b
        add (LitList ((LitInteger a):(LitInteger b):bs)) = add $ LitList ((LitInteger (a + b)):bs)
        add _ = return $ LitText $ T.pack $ "Only list of ints."

        diffTime (LitList ((LitDyn a):(LitDyn b):_)) = do
          case do {t1 <- cast a; t2 <- cast b; return (t1, t2)} of
            Just (time1, time2) -> return $ LitDyn (diffUTCTime time1 time2)
            Nothing -> return $ LitText $ T.pack "Only UTCTimes Please..."

main :: IO ()
main = do 
  (return "blah") >>= (\r -> return ["what", r]) >>= print
  tmpls <- templateStoreFromDirectory "examples/templates" ".strp"
  time <-getCurrentTime
  case tmpls of
    Left err -> print err
    Right store -> do
      rendered <- render (defaultConfig {templateStore = store}) (makeBucket 100 time) "big-complex.strp"
      either (print) (BS.putStr . B.toByteString) rendered

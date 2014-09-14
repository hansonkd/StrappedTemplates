
import Blaze.ByteString.Builder.Char8
import Data.Monoid ((<>))
import Data.Text as T
import Text.Strapped
import Text.Strapped.Render
import Text.Strapped.Parser
import Text.Parsec
import Text.Parsec.String

data MyData = MyData ParsedExpression
    deriving (Show)

-- | Process MyData in the RenderT monad when it is rendered.
instance Block MyData where
    process (MyData ex) = do
        config <- getConfig
        lit <- reduceExpression ex
        return $ fromString "MyData ->" <> (renderOutput config lit) <> fromString "<- MyData"

-- | Parse out MyData using Parsec
parseMyData :: ParserM MyData
parseMyData = do
    tagStart >> spaces >> (string "MyData") >> spaces >> parseExpression (spaces >> tagEnd) >>= (return . MyData)

main :: IO ()
main = do
    
    case templateStoreFromList config [("mydata", "testing... {$ MyData lookup $}")] of
        Left e -> print e
        Right ts -> do
            r <- render (config {templateStore = ts}) (varBucket "lookup" $ LitVal $ LitText $ T.pack "Cooolio") "mydata"
            print r
    where config = defaultConfig {customParsers = [BlockParser parseMyData]}
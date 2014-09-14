{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Test.Hspec

import qualified Blaze.ByteString.Builder as B
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Typeable
import Text.Strapped
import System.Exit

data Custom = Custom
  deriving (Show, Typeable)

instance Renderable Custom where
  renderOutput _ c = showToBuilder c

mainBucket :: InputBucket IO
mainBucket = bucketFromList [
          ("custom", LitVal $ LitDyn Custom),
          ("is", lit $ map (LitInteger) [1..3]),
          ("addNumbers", Func add)
        ]
  where add (LitList ((LitInteger a):(LitInteger b):[])) = return $ LitInteger $ a + b
        add (LitList ((LitInteger a):(LitInteger b):bs)) = add $ LitList ((LitInteger (a + b)):bs)
        add _ = return $ LitText $ T.pack $ "Only list of ints."

renderShouldBe :: IO (Either StrapError Output) -> Either StrapError BS.ByteString -> Expectation
renderShouldBe f c = f >>= (\e -> (fmap B.toByteString e) `shouldBe` c)

spec :: StrappedConfig -> Spec
spec ts = do
    describe "for loop" $
      it "Should loop through stuff" $ do
        (render ts mainBucket "for_loop") `renderShouldBe` (Right "1, 2, 3, ")

    describe "Comment" $
      it "Check comments" $ do
        (render ts mainBucket "comment") `renderShouldBe` (Right "")

    describe "Raw" $
      it "Check raw" $ do
        (render ts mainBucket "raw") `renderShouldBe` (Right "${ custom }")

    describe "Let" $
      it "Check let" $ do
        (render ts mainBucket "let") `renderShouldBe` (Right "Custom")

    describe "Include" $
      it "Check include" $ do
        (render ts mainBucket "include") `renderShouldBe` (Right "Custom")

    describe "Inherit" $
      it "Check inherits" $ do
        (render ts mainBucket "inherits") `renderShouldBe` (Right "Some base override")

    describe "Function" $
      it "Checks Function" $ 
        (render ts mainBucket "func") `renderShouldBe` (Right "4")

    describe "If" $
      it "Checks if" $ 
        (render ts mainBucket "if") `renderShouldBe` (Right "show me")

main = do
    let ets = templateStoreFromList defaultConfig
            [ ("for_loop", "{$ for i in is $}${ i }, {$ endfor $}")
            , ("custom", "${ custom }")
            , ("comment", "{$ comment $}${ custom }{$ endcomment $}")
            , ("raw", "{$ raw $}${ custom }{$ endraw $}")
            , ("let", "{$ let i = custom $}\n${ i }")
            , ("include", "{$ include custom $}")
            , ("base", "Some base {$ block some_block $}blah blah {$ endblock $}")
            , ("inherits", "{$ inherits base $}{$ isblock some_block $}override{$ endisblock $}")
            , ("func", "${ addNumbers [(addNumbers [1, 1]), 2] }")
            , ("if", "{$ if True $}show me{$ else $}dont show{$ endif $}{$ if False $}shouldn't see{$ endif $}")
            ]
    either (\_ -> exitFailure) (\ts -> hspec $ spec (defaultConfig {templateStore = ts})) ets
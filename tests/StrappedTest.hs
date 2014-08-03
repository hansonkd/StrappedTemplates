{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified Blaze.ByteString.Builder as B
import qualified Data.Text.Lazy as T
import qualified Data.ByteString as BS
import Text.Strapped
import System.Exit

data Custom = Custom
  deriving (Show)

instance Renderable Custom where
  renderOutput _ c = showToBuilder c

mainBucket :: InputBucket IO
mainBucket = bucketFromList [
          ("custom", RenderVal Custom),
          ("is", List $ map (LitVal . LitInteger) [1..3]),
          ("addNumbers", Func add)
        ]
  where add (LitList ((LitInteger a):(LitInteger b):[])) = return $ LitInteger $ a + b
        add (LitList ((LitInteger a):(LitInteger b):bs)) = add $ LitList ((LitInteger (a + b)):bs)
        add _ = return $ LitText $ T.pack $ "Only list of ints."

renderShouldBe :: IO (Either StrapError Output) -> Either StrapError BS.ByteString -> Expectation
renderShouldBe f c = f >>= (\e -> (fmap B.toByteString e) `shouldBe` c)

spec :: RenderConfig -> Spec
spec ts = do
    describe "for loop" $
      it "Should loop through stuff" $ do
        (render ts mainBucket "for_loop") `renderShouldBe` (Right "1, 2, 3, ")

    describe "RenderVal" $
      it "class RenderVal" $ do
        (render ts mainBucket "custom") `renderShouldBe` (Right "Custom")

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


main = do
    let ets = templateStoreFromList 
            [ ("for_loop", "{$ for i in is $}${ i }, {$ endfor $}")
            , ("custom", "${ custom }")
            , ("comment", "{$ comment $}${ custom }{$ endcomment $}")
            , ("raw", "{$ raw $}${ custom }{$ endraw $}")
            , ("let", "{$ let i = custom $}\n${ i }")
            , ("include", "{$ include custom $}")
            , ("base", "Some base {$ block some_block $}blah blah {$ endblock $}")
            , ("inherits", "{$ inherits base $}{$ isblock some_block $}override{$ endisblock $}")
            , ("func", "${ addNumbers [(addNumbers [1, 1]), 2] }")
            ]
    either (\_ -> exitFailure) (\ts -> hspec $ spec (defaultConfig {templateStore = ts})) ets
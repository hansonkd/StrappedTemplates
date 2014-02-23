{-# LANGUAGE OverloadedStrings #-}
import Data.Char (toUpper)
import Control.Monad
import Control.Monad.Error
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Strapped.Parser
import Strapped.Render 
import Strapped.Types
import Criterion.Main
import qualified Blaze.ByteString.Builder as B
import Blaze.ByteString.Builder.Char.Utf8 as B

import Text.ParserCombinators.Parsec


templates = 
  [ "Blah blah blah"
  , "@{ some_var } string @{ other_var } s"
  , "{@ block body @}{@ endblock @}"
  , "{@ block body @}@{ hello }{@ endblock @}"
  , "{@ block body @}{@ {@ endblock @}"
  , "{@ block body @}{@ endblock @} "
  , "{@ block body @} {} { @{ }{ @{{} cool } {@ block inner_body @} Hello: @{ hello }{@ endblock @} body string {@ endblock @}"
  , "{@ block body @} {} @{ cool } {@ block inner_body @} Hello: @{ hello }{@ endblock @}{@ endblock @} ldsakjf"
  , "{@ block body @} {@ for b in bs @} @{ b } {@ endfor @}{@ endblock @} "
  , "{@ for i in is @} @{ i } {@ endfor @}"
  , "@{ raiseLetters some_var }"
  ]

bigTemplate = either (error "invalid template") (id) $ parseTemplate templateText

templateText = concat
  [ "Blah blah blah"
  , "{@ for i in is @} @{ i }  @{ other_var } {@ endfor @}"
  , "{@ for i in is @} @{ i } {@ endfor @}"
  , "{@ for i in is @} @{ i } @{ some_var } string @{ other_var } {@ endfor @}"
  , "{@ for i in is @} @{ i } {@ endfor @}"
  , "{@ for i in is @} @{ i } {@ endfor @}"
  , "{@ block body @} {} { @{ }{ @{{} cool } {@ block inner_body @} Hello: @{ hello }{@ endblock @} body string {@ endblock @}"
  , "{@ for i in is @} @{ i } {@ endfor @}"
  , "{@ for i in is @} @{ i } @{ some_var } string @{ other_var } {@ endfor @}"
  , "{@ for i in is @} {@ for i in is @} @{ i } {@ endfor @} {@ endfor @}"
  , "@{ raiseLetters some_var }"
  ]

parsedTemplates = map parseTemplate templates

renderedTemplates = mapM (render (makeBucket 1)) (rights parsedTemplates)

makeBucket :: Int -> InputGetter IO
makeBucket i = bucket
  where bucket "some_var" = Just $ Value $ packInput "This rendered!"
        bucket "hello" = Just $ Value $ packInput "Hi!"
        bucket "some_var" = Just $ Value (packInput $ show i)
        bucket "other_var" = Just $ Value $ packInput "Other rendered!"
        bucket "is" = Just $ List $ map (Value . packInput . show) [1..i]
        bucket "raiseLetters" = Just $ Func raiseLetters
          where raiseLetters ((Value v):[]) = return $ v
                raiseLetters _ = throwError $ StrapError "Needs Value"
        bucket _ = Nothing

benchmarks = map (\i -> bench (show i) $ whnfIO $ render (makeBucket i) bigTemplate) [100,200..1000]

main :: IO ()
main = do
  -- forM_ (zip [1..] $ parsedTemplates) (\(i, r) -> print i >> print r)
  results <- renderedTemplates
  --forM_ (zip [1..] (rights results)) (\(i, r) -> print i >> print $ B.toByteString r)
  
  rendered <- render (makeBucket 2) bigTemplate
  
  print $ templateText
  let (Template c _) = bigTemplate
  
  print $ length c
  
  either (print) (print . B.toByteString) rendered
  
  defaultMain [ bgroup "builder" benchmarks ] 
  
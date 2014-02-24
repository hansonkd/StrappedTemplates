module Strapped.Types where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Control.Monad.Error
import Data.Text as T

packInput = fromString

type Output = Builder

data Piece = StaticPiece Output
           | BlockPiece String [Piece]
           | ForPiece String String [Piece]
           | FuncPiece String [String]
           | Decl String String [String]
           | Include String
           | Extends String
           
instance Show Piece where
  show (StaticPiece a) = "StaticPiece " ++ (show $ toByteString a)
  show (Decl n f args) = "Decl " ++ n ++ " " ++ f ++ " " ++ (show args)
  show (Extends e) = "Extends " ++ e
  show _ = "eh"
  
data Input m = 
             Value Output 
           | List [Input m]
           | Func  ([Input m] -> ErrorT StrapError m Output)  

data StrapError = StrapError String | InputNotFound String | TemplateNotFound String
  deriving (Show)
  
instance Error StrapError where
  noMsg    = StrapError "A Strap Error"
  strMsg s = StrapError s

type InputGetter m = String -> Maybe (Input m)

type TemplateStore = String -> IO (Maybe Template)

data Template = Template [Piece] [(String, [Piece])]
  deriving (Show)
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
           | VarPiece String
           | FuncPiece String [String]

data Input m = 
             Value Output 
           | List [Input m]
           | Func  ([Input m] -> ErrorT StrapError m Output)  

data StrapError = StrapError String | InputNotFound String
  deriving (Show)
  
instance Error StrapError where
  noMsg    = StrapError "A Strap Error"
  strMsg s = StrapError s

type InputGetter m = String -> Maybe (Input m)

data Template = Template [Piece] [Piece]

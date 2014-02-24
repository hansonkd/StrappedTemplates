{-# LANGUAGE ExistentialQuantification #-}
module Text.Strapped.Types where

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
  
class Renderable a where
  renderOutput :: a -> Output
  
data Input m = forall a . Renderable a => RenderVal a
           | List [Input m]
           | Func  ([Input m] -> ErrorT StrapError m Literal)
           | LitVal Literal

data Literal = LitString String
             | LitInt Int
             | LitBuilder Builder
             | LitList [Literal]

data StrapError = StrapError String | InputNotFound String | TemplateNotFound String
  deriving (Show)
  
instance Error StrapError where
  noMsg    = StrapError "A Strap Error"
  strMsg s = StrapError s

type InputBucket m = String -> Maybe (Input m)

type TemplateStore = String -> IO (Maybe Template)

data Template = Template [Piece] [(String, [Piece])]
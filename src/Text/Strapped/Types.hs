{-# LANGUAGE ExistentialQuantification #-}
module Text.Strapped.Types where

import Blaze.ByteString.Builder
import Control.Monad.Error
import Data.Text.Lazy
import Data.Typeable
import Text.Parsec.Pos

type Output = Builder

instance Show Builder where
  show _ = "blah"

data Expression = 
  LookupExpression String |
  IntegerExpression Integer |
  FloatExpression Double |
  StringExpression String |
  ListExpression [Expression] |
  Multipart [Expression]
  deriving (Show)

data Piece = StaticPiece Output
           | BlockPiece String [ParsedPiece]
           | ForPiece String Expression [ParsedPiece]
           | FuncPiece Expression
           | Decl String Expression
           | Include String
           | Inherits String [(String, [ParsedPiece])]
           deriving (Show)

data ParsedPiece = ParsedPiece Piece SourcePos
  deriving (Show)
  
class Renderable a where
  renderOutput :: RenderConfig -> a -> Output
  
data Input m = forall a . (Renderable a) => RenderVal a
             | List [Input m]
             | Func  (Literal -> ErrorT StrapError m Literal)
             | LitVal Literal

data Literal = forall a . (Typeable a, Renderable a) => LitDyn a
             | LitText Text
             | LitSafe Text
             | LitInteger Integer
             | LitDouble Double
             | LitBuilder Builder
             | LitList [Literal]
             | LitEmpty

data StrapError = StrapError String  SourcePos | InputNotFound String  SourcePos | TemplateNotFound String  SourcePos
  deriving (Show)
  
instance Error StrapError where
  noMsg    = StrapError "A Strap Error" (initialPos "empty")
  strMsg s = StrapError s (initialPos "empty")

type InputBucket m = String -> Maybe (Input m)

type TemplateStore = String -> IO (Maybe Template)

data Template = Template [ParsedPiece]
  deriving Show

data RenderConfig = RenderConfig 
  { templateStore :: TemplateStore
  , escapeFunc :: Text -> Text
  }
{-# LANGUAGE ExistentialQuantification #-}
module Text.Strapped.Types where

import Blaze.ByteString.Builder
import Control.Monad.Except
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Monoid (mconcat)
import Data.Text.Lazy (Text)
import Data.Typeable
import Text.Parsec.Pos

type Output = Builder

instance Show Builder where
  show = show . toByteString

data Expression = 
  LookupExpression String |
  IntegerExpression Integer |
  FloatExpression Double |
  StringExpression String |
  ListExpression [ParsedExpression] |
  Multipart [ParsedExpression]

instance Show Expression where
  show (LookupExpression s) = s
  show (IntegerExpression s) = show s
  show (FloatExpression s) = show s
  show (StringExpression s) = "\"" ++ s ++ "\""
  show (ListExpression l) = "[" ++ (mconcat $ intersperse "," (map show l)) ++ "]"
  show (Multipart l) = mconcat $ map show l

data ParsedExpression = ParsedExpression Expression SourcePos

instance Show ParsedExpression where
  show (ParsedExpression exp _) = show exp

data Piece = StaticPiece Output
           | BlockPiece String [ParsedPiece]
           | ForPiece String ParsedExpression [ParsedPiece]
           | FuncPiece ParsedExpression
           | Decl String ParsedExpression
           | Include String
           | Inherits String [(String, [ParsedPiece])]
           deriving (Show)

data ParsedPiece = ParsedPiece Piece SourcePos
  deriving (Show)
  
class Renderable a where
  renderOutput :: RenderConfig -> a -> Output
  
data Input m = forall a . (Renderable a) => RenderVal a
             | List [Input m]
             | Func  (Literal -> ExceptT StrapError m Literal)
             | LitVal Literal

data Literal = forall a . (Typeable a, Renderable a) => LitDyn a
             | LitText Text
             | LitSafe Text
             | LitInteger Integer
             | LitDouble Double
             | LitBuilder Builder
             | LitList ![Literal]
             | LitEmpty

data StrapError = StrapError String  SourcePos 
                | InputNotFound String  SourcePos 
                | TemplateNotFound String  SourcePos
  deriving (Show)

type InputBucket m = [M.Map String (Input m)]

type TemplateStore = String -> IO (Maybe Template)

data Template = Template [ParsedPiece]
  deriving Show

data RenderConfig = RenderConfig 
  { templateStore :: TemplateStore
  , escapeFunc :: Text -> Text
  }
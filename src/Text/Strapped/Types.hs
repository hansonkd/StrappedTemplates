{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Strapped.Types where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8 hiding (fromString)
import           Control.Applicative
import Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List  as L (intersperse, null)
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Monoid (mconcat)
import Data.Text as T (Text, pack, null, unpack)
import Data.Typeable
import Data.String
import Text.Parsec.Pos

import Text.Parsec
import Text.Parsec.String

-- | RenderT m
--
--   * m -> Monad we are transforming
newtype RenderT m a = RenderT 
  { runRenderT :: ExceptT StrapError (StateT (RenderState m) m) a 
  } deriving ( Functor, Applicative, Monad, MonadIO )

instance MonadTrans RenderT where
  lift = RenderT . lift . lift
  
data RenderState m = RenderState
  { position     :: SourcePos
  , renderConfig :: StrappedConfig
  , blocks       :: BlockMap
  , bucket       :: InputBucket m
  }

instance (Monad m) => MonadError StrapError (RenderT m) where
    throwError = RenderT . throwError
    catchError (RenderT m) f = RenderT (catchError m (runRenderT . f))
    
type Output = Builder
type BlockMap = M.Map String [ParsedPiece]

instance Show Builder where
  show = show . toByteString

data Expression = 
  LookupExpression String |
  LiteralExpression Literal |
  ListExpression [ParsedExpression] |
  Multipart [ParsedExpression]

instance Show Expression where
  show (LookupExpression s) = s
  show (LiteralExpression s) = show s
  show (ListExpression l) = "[" ++ (mconcat $ intersperse "," (map show l)) ++ "]"
  show (Multipart l) = mconcat $ map show l

data ParsedExpression = ParsedExpression Expression SourcePos

instance Show ParsedExpression where
  show (ParsedExpression exp _) = show exp

data Piece = StaticPiece Output
           | BlockPiece String [ParsedPiece]
           | ForPiece String ParsedExpression [ParsedPiece]
           | IfPiece ParsedExpression [ParsedPiece] [ParsedPiece]
           | FuncPiece ParsedExpression
           | Decl String ParsedExpression
           | Include String
           | Inherits String BlockMap
           deriving (Show)

data ParsedPiece = forall a . (Block a) => ParsedPiece a SourcePos

instance Show ParsedPiece where
  show (ParsedPiece a b) = show b
  
class Renderable a where
  renderOutput :: StrappedConfig -> a -> Output

  
data Input m = Func  (Literal -> RenderT m Literal)
             | LitVal Literal

data Literal = forall a . (Typeable a, Renderable a) => LitDyn !a
             | LitText !Text
             | LitSafe !Text
             | LitInteger !Integer
             | LitDouble !Double
             | LitBuilder !Builder
             | LitList ![Literal]
             | LitBool !Bool
             | LitEmpty

class ToLiteral a where
  toLiteral :: a -> Literal

instance ToLiteral Text where
  toLiteral = LitText

instance ToLiteral Integer where
  toLiteral = LitInteger

instance ToLiteral Double where
  toLiteral = LitDouble

instance ToLiteral Builder where
  toLiteral = LitBuilder

instance ToLiteral [Literal] where
  toLiteral = LitList

instance ToLiteral Bool where
  toLiteral = LitBool

instance ToLiteral String where
  toLiteral = LitText . T.pack

instance IsString Literal where
   fromString = toLiteral

class Block a where
  process :: (MonadIO m) => a -> RenderT m Output

class Booly a where
  toBool :: a -> Bool

instance Booly Literal where
  toBool (LitDyn a) = maybe False id (cast a)
  toBool (LitText a) = not $ T.null a
  toBool (LitSafe a) = not $ T.null a
  toBool (LitInteger a) = (0 /= a)
  toBool (LitDouble a) = (0.0 /= a)
  toBool (LitBuilder a) = not $ BS.null $ (toLazyByteString a)
  toBool (LitList a) = not $ L.null a
  toBool (LitBool a) = a
  toBool LitEmpty = False

instance Show Literal where
  show (LitDyn a) = "LitDyn"
  show (LitText a) = T.unpack a
  show (LitSafe a) = T.unpack a
  show (LitInteger a) = show a
  show (LitDouble a) = show a
  show (LitBuilder a) = BS.unpack $ (toLazyByteString a)
  show (LitList a) = show a
  show (LitBool a) = show a
  show LitEmpty = ""


data StrapError = StrapError String
                | InputNotFound String
                | TemplateNotFound String
                | PositionedError StrapError SourcePos
  deriving (Show, Eq)

data InputBucket m = InputBucket [(M.Map String (Input m))]

type TemplateStore = String -> IO (Maybe Template)

data Template = Template [ParsedPiece]
  deriving Show

type ParserM = GenParser Char StrappedConfig

data BlockParser = forall a . (Block a) => BlockParser (ParserM a)

data StrappedConfig = StrappedConfig
  { customParsers :: [BlockParser]
  , templateStore :: TemplateStore
  , escapeFunc :: Text -> Text
  }
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Strapped.Types where

import Blaze.ByteString.Builder
import           Control.Applicative
import Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List  as L (intersperse, null)
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid (mconcat)
import Data.Text.Lazy as T (Text, null, unpack)
import Data.Typeable
import Text.Parsec.Pos


-- | RenderT m
--
--   * m -> Monad we are transforming
newtype RenderT m a = RenderT 
  { runRenderT :: WriterT Output (ExceptT StrapError (StateT (RenderState m) m)) a 
  } deriving ( Functor, Applicative, Monad, MonadIO )

instance MonadTrans RenderT where
  lift = RenderT . lift . lift . lift
  
data RenderState m = RenderState
  { position :: SourcePos
  , renderConfig :: RenderConfig
  , blocks :: BlockMap
  , bucket :: InputBucket m
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

data ParsedPiece = ParsedPiece Piece SourcePos
  deriving (Show)
  
class Renderable a where
  renderOutput :: RenderConfig -> a -> Output
  
data Input m = forall a . (Renderable a) => RenderVal a
             | List [Input m]
             | Func  (Literal -> RenderT m Literal)
             | LitVal Literal

data Literal = forall a . (Typeable a, Renderable a) => LitDyn a
             | LitText Text
             | LitSafe Text
             | LitInteger Integer
             | LitDouble Double
             | LitBuilder Builder
             | LitList ![Literal]
             | LitBool Bool
             | LitEmpty

class Block a where
  process :: (MonadIO m) => a -> RenderT m ()

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


data StrapError = StrapError String  SourcePos 
                | InputNotFound String  SourcePos 
                | TemplateNotFound String  SourcePos
  deriving (Show, Eq)

type InputBucket m = [M.Map String (Input m)]

type TemplateStore = String -> IO (Maybe Template)

data Template = Template [ParsedPiece]
  deriving Show

data RenderConfig = RenderConfig 
  { templateStore :: TemplateStore
  , escapeFunc :: Text -> Text
  }
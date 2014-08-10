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
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Monoid (mconcat)
import Data.Text as T (Text, null, unpack)
import Data.Typeable
import Text.Parsec.Pos
import Data.Conduit
import Control.DeepSeq

-- | RenderT m
--
--   * m -> Monad we are transforming
newtype RenderT m a = RenderT 
  { runRenderT :: ExceptT StrapError (StateT (RenderState m) m) a 
  } deriving ( Functor, Applicative, Monad, MonadIO )

instance MonadTrans RenderT where
  lift = RenderT . lift . lift
  
data RenderState m = RenderState
  { position     :: !(SourcePos)
  , renderConfig :: !(RenderConfig)
  , blocks       :: !(BlockMap)
  , bucket       :: !(InputBucket m)
  }

instance NFData (RenderState m) where
  rnf (RenderState a b c d) = a `seq` b `seq` c `seq` d `deepseq` ()

type RenderM m = StateT (RenderState m) (ExceptT StrapError m)
type RenderConduit m = ConduitM () Output (RenderM m)
type RenderSource m = RenderConduit m ()

instance (Monad m) => MonadError StrapError (RenderT m) where
    throwError = RenderT . throwError
    catchError (RenderT m) f = RenderT (catchError m (runRenderT . f))
    
type Output = T.Text
type BlockMap = M.Map String [ParsedPiece]

instance Show Builder where
  show = show . toByteString

data Expression = 
  LookupExpression String |
  LiteralExpression Literal |
  ListExpression [ParsedExpression] |
  Multipart [ParsedExpression]

instance NFData Expression where
  rnf (LookupExpression s) = rnf s
  rnf (LiteralExpression l) = rnf l
  rnf (ListExpression l) = rnf l
  rnf (Multipart l) = rnf l

instance Show Expression where
  show (LookupExpression s) = s
  show (LiteralExpression s) = show s
  show (ListExpression l) = "[" ++ (mconcat $ intersperse "," (map show l)) ++ "]"
  show (Multipart l) = mconcat $ map show l

data ParsedExpression = ParsedExpression Expression SourcePos

instance NFData ParsedExpression where
  rnf (ParsedExpression a b) = a `deepseq` b `seq` ()

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

instance NFData Piece where
  rnf (StaticPiece a) = a `seq` ()
  rnf (BlockPiece a b) = a `deepseq` b `deepseq` ()
  rnf (ForPiece a b c) = a `deepseq` b `deepseq` c `deepseq`()
  rnf (IfPiece a b c) = a `deepseq` b `deepseq` c `deepseq`()
  rnf (FuncPiece a) = a `deepseq` ()
  rnf (Decl s a) = s `deepseq` a `deepseq` ()
  rnf (Include s) = s `deepseq` ()
  rnf (Inherits s b) = s `deepseq` b `deepseq` ()

data ParsedPiece = ParsedPiece Piece SourcePos
  deriving (Show)

instance NFData ParsedPiece where
  rnf (ParsedPiece p s) = p `deepseq` s `seq` ()
  
class Renderable a where
  renderOutput :: RenderConfig -> a -> Output

data InputBucket m = InputBucket (M.Map String (Input m))

instance NFData (InputBucket m) where
  rnf (InputBucket l) = rnf l
  rnf a = a `seq` ()

data Input m = forall a . (Renderable a) => RenderVal a
             | List [Input m]
             | Func  (Literal -> (ExceptT StrapError m) Literal)
             | LitVal Literal

instance NFData (Input m) where
  rnf (List l) = rnf l
  rnf (LitVal v) = rnf v
  rnf a = a `seq` ()

data Literal = forall a . (Typeable a, NFData a, Renderable a) => LitDyn !a
             | LitText Text
             | LitSafe Text
             | LitInteger Integer
             | LitDouble Double
             | LitBuilder Builder
             | LitList [Literal]
             | LitBool Bool
             | LitEmpty

instance NFData Literal where
  rnf (LitList l) = rnf l
  rnf (LitInteger i) = rnf i
  rnf (LitDyn a) = rnf a
  rnf a = a `seq` ()

class Block a where
  process :: (MonadIO m) => a -> RenderSource m 

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


type TemplateStore = String -> IO (Maybe Template)

data Template = Template [ParsedPiece]
  deriving Show

data RenderConfig = RenderConfig 
  { templateStore :: TemplateStore
  , escapeFunc :: Text -> Text
  }
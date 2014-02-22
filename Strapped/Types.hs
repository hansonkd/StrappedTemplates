module Strapped.Types where

data Piece = StaticPiece String 
           | BlockPiece String [Piece]
           | VarPiece String
           | ForPiece String String [Piece]
  deriving (Show)

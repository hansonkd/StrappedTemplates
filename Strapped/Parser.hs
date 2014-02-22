module Strapped.Parser where

import Control.Applicative ((<*>))
import Control.Monad
import Text.ParserCombinators.Parsec

import Strapped.Types

wordString = many1 $ oneOf "_" <|> alphaNum

peekChar = void . try . lookAhead . char

tryTag = void . try . tag

tag p = between (string "{$" >> spaces) (spaces >> string "$}") p <?> "Tag"

parseContent end = do
  ps <- manyTill parsePiece end
  return $ combineStatics ps
  
parseBlock = do
  blockName <- tag (string "block" >> spaces >> wordString) <?> "Block tag"
  blockContent <- parseContent (tryTag $ string "endblock") 
  return $ BlockPiece blockName blockContent

parseFor = do
  (newVarName, listName) <- tag (string "for" >> argParser) <?> "For tag"
  blockContent <- parseContent (tryTag $ string "endfor") 
  return $ ForPiece newVarName listName blockContent
  where argParser = do 
        spaces
        v <- wordString
        spaces >> (string "in") >> spaces
        l <- wordString
        return (v, l)

parseVar = do
  var <- between (string "${" >> spaces) (spaces >> string "}") wordString <?> "Variable tag"
  return $ VarPiece var

parseStatic = do
  c <- anyChar
  s <- manyTill anyChar (peekChar '{' <|> peekChar '$' <|> eof)
  return $ StaticPiece (c:s)

parseNonStatic = try parseBlock <|> try parseFor <|> parseVar
parsePiece = try parseNonStatic <|> parseStatic

parsePieces = parseContent eof

{-
  Refactor this section.
-}
isStatic :: Piece -> Bool
isStatic (StaticPiece _) = True
isStatic _ = False

unStatic :: Piece -> String
unStatic (StaticPiece s) = s
unStatic _ = error "unStatic: applied to something other than a StaticPiece"

combineStatics :: [Piece] -> [Piece] 
combineStatics pieces = let (nonstatics,therest) = span (not.isStatic) pieces in
    nonstatics ++ combine therest where
      combine [] = []
      combine ps = let (statics,more) = span isStatic ps in
        (StaticPiece . concat . map unStatic) statics : combineStatics more

parseTemplate :: String -> Either ParseError [Piece]
parseTemplate = parse parsePieces ""
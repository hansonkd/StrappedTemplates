module Strapped.Parser where

import Control.Applicative ((<*>))
import Control.Monad
import Data.Monoid
import qualified Data.Text as T
import Blaze.ByteString.Builder as B
import Blaze.ByteString.Builder.Char.Utf8 as B
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (getState, modifyState)
import Strapped.Types

wordString = many1 $ oneOf "_" <|> alphaNum
pathString = many1 $ oneOf "_./" <|> alphaNum

peekChar = void . try . lookAhead . char

tryTag = void . try . tag

tag p = between (string "{@" >> spaces) (spaces >> string "@}") p <?> "Tag"

parseContent end = do
  decls <- many (try parseDecl)
  extends <- many (try parseExtends)
  ps <- manyTill parsePiece end
  
  -- Throw away all other content if we have an extends (we already parsed out blocks)
  case (extends) of
    (e:_) -> return $ (decls) ++ [e]
    _     -> return $ (decls) ++ (combineStatics ps)
  
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

parseDecl = do {decl <- tag parserDecl; spaces; return decl}
  where parserDecl = do
          string "let" >> spaces
          varName <- wordString
          spaces >> string "=" >> spaces
          func <- wordString
          args <- many wordString
          return $ Decl varName func args
          
parseIsBlock = do
      blockName <- tag (string "isblock" >> spaces >> wordString)
      blockContent <- parseContent (tryTag $ string "endisblock") 
      modifyState (\blks -> (blockName, blockContent):blks)
      return $ StaticPiece (packInput [])

parseInclude = tag parserInclude
  where parserInclude = do
                string "include" >> spaces
                includeName <- pathString
                return $ Include includeName

parseExtends = tag parserInclude
  where parserInclude = do
                string "extends" >> spaces
                includeName <- pathString
                return $ Extends includeName 

parseFunc = do
  (string "@{" >> spaces)
  var <- wordString
  spaces
  args <- manyTill (spaces >> wordString) (try $ spaces >> string "}")
  return $ FuncPiece var args

parseStatic = do
  c <- anyChar
  s <- manyTill anyChar (peekChar '{' <|> peekChar '@' <|> eof)
  return $ StaticPiece (packInput $ c:s)

parseNonStatic =  try parseBlock 
              <|> try parseFor
              <|> try parseIsBlock
              <|> try parseInclude 
              <|> parseFunc
parsePiece = (try parseNonStatic <|> parseStatic)

parsePieces = parseContent eof

parseToTemplate = do
    c <- parseContent eof
    blks <- getState
    return $ Template c blks
{-
  Refactor this section.
-}
isStatic :: Piece -> Bool
isStatic (StaticPiece _) = True
isStatic _ = False

unStatic :: Piece -> Output
unStatic (StaticPiece s) = s
unStatic _ = error "unStatic: applied to something other than a StaticPiece"

combineStatics :: [Piece] -> [Piece] 
combineStatics pieces = let (nonstatics,therest) = span (not.isStatic) pieces in
    nonstatics ++ combine therest where
      combine [] = []
      combine ps = let (statics,more) = span isStatic ps in
        (StaticPiece . mconcat . map unStatic) statics : combineStatics more

parseTemplate :: String -> String -> Either ParseError Template
parseTemplate s tmplN = runParser parseToTemplate mempty tmplN s

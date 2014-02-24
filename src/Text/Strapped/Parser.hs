module Text.Strapped.Parser
  ( parseTemplate
  ) where

import Control.Applicative ((<*>))
import Control.Monad
import Data.Monoid
import qualified Data.Text as T
import Blaze.ByteString.Builder as B
import Blaze.ByteString.Builder.Char.Utf8 as B
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (getState, modifyState)

import Text.Strapped.Types

wordString = many1 $ oneOf "_" <|> alphaNum
pathString = many1 $ oneOf "_./" <|> alphaNum

peekChar = void . try . lookAhead . char

tryTag = void . try . tag

tag p = between (string "{@" >> spaces) (spaces >> string "@}") p <?> "Tag"

parseContent end = do
  decls <- many (try parseDecl)
  extends <- many (try parseExtends)
  ps <- manyTill parsePiece end
  
  -- Throw away all other content if we have an extends (we already parsed out
  -- blocks at this point.)
  case (extends) of
    (e:_) -> return $ (decls) ++ [e]
    _     -> return $ (decls) ++ (compress ps)
  
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

parseDecl = do {spaces; decl <- tag parserDecl; spaces; return decl}
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
      return $ StaticPiece (fromString [])

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
  return $ StaticPiece (B.fromString $ c:s)

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

-- | Put statics together.
compress :: [Piece] -> [Piece] 
compress pieces = loop mempty pieces
  where loop accum [] = accum
        loop ((StaticPiece s):accum) ((StaticPiece s2):ps) = loop (accum <> [StaticPiece (s <> s2)]) ps
        loop accum (p:ps) = loop (accum <> [p]) ps

-- | Take a template body and a template name and return either an error or a
--   renderable template.
parseTemplate :: String -> String -> Either ParseError Template
parseTemplate s tmplN = runParser parseToTemplate mempty tmplN s

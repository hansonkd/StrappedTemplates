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
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Text.Strapped.Types

tagStart = string "{@"
tagEnd = string "@}"

wordString = many1 $ oneOf "_" <|> alphaNum
pathString = many1 $ oneOf "_./" <|> alphaNum

peekChar = void . try . lookAhead . char

tryTag = void . try . tag

tag p = between (tagStart >> spaces) (spaces >> tagEnd) p <?> "Tag"


parseFloat :: GenParser Char st Double
parseFloat = do sign <- option 1 (do s <- oneOf "+-"
                                     return $ if s == '-' then-1.0 else 1.0)
                x    <- P.float $ P.makeTokenParser emptyDef

                return $ sign * x

parseInt :: GenParser Char st Integer
parseInt = do sign <- option 1 (do s <- oneOf "+-"
                                   return $ if s == '-' then-1 else 1)
              x    <- P.integer $ P.makeTokenParser emptyDef

              return $ sign * x

parseContent end = do
  decls <- many (try $ spaces >> parseDecl)
  spaces
  extends <- optionMaybe (try $ spaces >> parseInherits)
  case (extends) of
    Just e -> do
      includes <- manyTill parseIsIgnoreSpace end
      return $ (decls) ++ [Inherits e includes]
    _     -> do
      ps <- manyTill parsePiece end
      return $ decls ++ ps
  where parseIsIgnoreSpace = do {spaces; b <- parseIsBlock; spaces; return b}
  
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

parseDecl = do {spaces; decl <- parserDecl; spaces; return decl} <?> "Let tag"
  where parserDecl = do
          tagStart >> spaces
          string "let" >> spaces
          varName <- wordString
          spaces >> string "=" >> spaces
          func <- parseExpression (try $ spaces >> tagEnd)
          return $ Decl varName func
          
parseIsBlock = do
      blockName <- tag (string "isblock" >> spaces >> wordString) <?> "Isblock tag"
      blockContent <- parseContent (tryTag $ string "endisblock") 
      return (blockName, blockContent)

parseInclude = tag parserInclude <?> "Include tag"
  where parserInclude = do
                string "include" >> spaces
                includeName <- pathString
                return $ Include includeName

parseInherits = tag (string "inherits" >> spaces >> pathString) <?> "Inherits tag"

parseFunc = parserFunc <?> "Call tag"
  where parserFunc = do
          string "@{" >> spaces
          exp <- parseExpression (try $ spaces >> string "}")
          return $ FuncPiece exp

parseExpression end = manyPart <?> "Expression"
  where parseAtomic  =  try parens
                    <|> try parseList
                    <|> try (parseString '\"')
                    <|> try (parseString '\'')
                    <|> try (parseFloat >>= (return . FloatExpression))
                    <|> try (parseInt >>= (return . IntegerExpression))
                    <|> lit

        parens = (string "(" >> spaces) >> parseExpression (try $ spaces >> string ")")
        parseList = between (string "[" >> spaces) 
                            (spaces >> string "]") 
                            (sepBy (spaces >> parseAtomic) (string ",")) 
                    >>= (return . ListExpression)
        manyPart = (manyTill (spaces >> parseAtomic) end) >>= (return . Multipart)
        parseString esc = parseStringContents esc >>= (return . StringExpression)
        lit = wordString >>= (return . LookupExpression)

parseStringContents esc = between (char esc) (char esc) (many chars)
    where chars = (try escaped) <|> noneOf [esc]
          escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
          escapedChar code replacement = char code >> return replacement
          codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '\'', '/']
          replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '\'', '/']

parseStatic = do
  c <- anyChar
  s <- manyTill anyChar (peekChar '{' <|> peekChar '@' <|> eof)
  return $ StaticPiece (B.fromString $ c:s)

parseNonStatic =  try parseBlock 
              <|> try parseFor
              <|> try parseInclude 
              <|> parseFunc
parsePiece = (try parseNonStatic <|> parseStatic)

parsePieces = parseContent eof

parseToTemplate = (parseContent eof) >>= (return . Template)

-- | Take a template body and a template name and return either an error or a
--   renderable template.
parseTemplate :: String -> String -> Either ParseError Template
parseTemplate s tmplN = parse parseToTemplate tmplN s

module Text.Strapped.Parser
  ( parseTemplate
  ) where

import Control.Applicative ((<*>))
import Control.Monad
import Data.Monoid
import qualified Data.Text.Lazy as T
import Blaze.ByteString.Builder as B
import Blaze.ByteString.Builder.Char.Utf8 as B
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Text.Strapped.Types


tagStart :: GenParser Char st String
tagStart = string "{@"

tagEnd :: GenParser Char st String
tagEnd = string "@}"

wordString :: GenParser Char st String
wordString = many1 $ oneOf "_" <|> alphaNum

pathString :: GenParser Char st String
pathString = many1 $ oneOf "_./" <|> alphaNum

peekChar :: Char -> GenParser Char st ()
peekChar = void . try . lookAhead . char

tryTag :: GenParser Char st a -> GenParser Char st ()
tryTag = void . try . tag

tag :: GenParser Char st a -> GenParser Char st a
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

parseContent :: GenParser Char st a -> GenParser Char st [ParsedPiece]
parseContent end = do
  decls <- many (try $ spaces >> parseDecl)
  spaces
  extends <- optionMaybe (try $ spaces >> parseInherits)
  case (extends) of
    Just (e, epos) -> do
      includes <- manyTill parseIsIgnoreSpace end
      return $ (decls) ++ [ParsedPiece (Inherits e includes) epos]
    _     -> do
      ps <- manyTill parsePiece end
      return $ decls ++ ps
  where parseIsIgnoreSpace = do {spaces; b <- parseIsBlock; spaces; return b}

parseBlock :: GenParser Char st ParsedPiece
parseBlock = do
  pos <- getPosition
  blockName <- tag (string "block" >> spaces >> wordString) <?> "Block tag"
  blockContent <- parseContent (tryTag $ string "endblock") 
  return $ ParsedPiece (BlockPiece blockName blockContent) pos

parseFor :: GenParser Char st ParsedPiece
parseFor = do
  pos <- getPosition
  (newVarName, exp) <- (tagStart >> spaces >> string "for" >> argParser) <?> "For tag"
  blockContent <- parseContent (tryTag $ string "endfor") 
  return $ ParsedPiece (ForPiece newVarName exp blockContent) pos
  where argParser = do 
        spaces
        v <- wordString
        spaces >> (string "in") >> spaces
        func <- parseExpression (try $ spaces >> tagEnd)
        return (v, func)

parseDecl :: GenParser Char st ParsedPiece
parseDecl = do {spaces; decl <- parserDecl; spaces; return decl} <?> "Let tag"
  where parserDecl = do
          pos <- getPosition
          tagStart >> spaces
          string "let" >> spaces
          varName <- wordString
          spaces >> string "=" >> spaces
          func <- parseExpression (try $ spaces >> tagEnd)
          return $ ParsedPiece (Decl varName func) pos
          
parseIsBlock = do
      blockName <- tag (string "isblock" >> spaces >> wordString) <?> "Isblock tag"
      blockContent <- parseContent (tryTag $ string "endisblock") 
      return (blockName, blockContent)

parseInclude :: GenParser Char st ParsedPiece
parseInclude = do
  pos <- getPosition
  tag (parserInclude pos) <?> "Include tag"
  where parserInclude pos = do
                string "include" >> spaces
                includeName <- pathString
                return $ ParsedPiece (Include includeName) pos

parseInherits = do {pos <- getPosition; mtag <- tag (string "inherits" >> spaces >> pathString); return (mtag, pos)} <?> "Inherits tag"

parseFunc ::  GenParser Char st ParsedPiece
parseFunc = parserFunc <?> "Call tag"
  where parserFunc = do
          pos <- getPosition
          string "@{" >> spaces
          exp <- parseExpression (try $ spaces >> string "}")
          return $ ParsedPiece (FuncPiece exp) pos

parseExpression ::  GenParser Char st a -> GenParser Char st ParsedExpression
parseExpression end = manyPart <?> "Expression"
  where parseGroup = try parens <|> parseAtomic
        parseAtomic  = do
            pos <- getPosition
            exp <- try parseList <|> 
                   try (parseString '\"') <|> 
                   try (parseString '\'') <|> 
                   try (parseFloat >>= (return . FloatExpression)) <|> 
                   try (parseInt >>= (return . IntegerExpression)) <|>
                   literal
            return $ ParsedExpression exp pos
        parens = (string "(" >> spaces) >> parseExpression (try $ spaces >> string ")")
        parseList = between (string "[" >> spaces) 
                            (spaces >> string "]") 
                            (sepBy (spaces >> parseGroup) (string ",")) 
                    >>= (return . ListExpression)
        manyPart = do
          pos <- getPosition
          pieces <- manyTill (spaces >> parseGroup) end
          return $ ParsedExpression (Multipart pieces) pos
        parseString esc = parseStringContents esc >>= (return . StringExpression)
        literal = wordString >>= (return . LookupExpression)

parseStringContents ::  Char -> GenParser Char st String
parseStringContents esc = between (char esc) (char esc) (many chars)
    where chars = (try escaped) <|> noneOf [esc]
          escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
          escapedChar code replacement = char code >> return replacement
          codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '\'', '/']
          replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '\'', '/']

parseStatic :: GenParser Char st ParsedPiece
parseStatic = do
  pos <- getPosition
  c <- anyChar
  s <- manyTill anyChar (peekChar '{' <|> peekChar '@' <|> eof)
  return $ ParsedPiece (StaticPiece (B.fromString $ c:s)) pos

parseNonStatic :: GenParser Char st ParsedPiece
parseNonStatic =  try parseBlock
              <|> try parseFor
              <|> try parseInclude
              <|> parseFunc
              
parsePiece :: GenParser Char st ParsedPiece
parsePiece = (try parseNonStatic <|> parseStatic)

parsePieces :: GenParser Char st [ParsedPiece]
parsePieces = parseContent eof

parseToTemplate :: GenParser Char st Template
parseToTemplate = (parseContent eof) >>= (return . Template)

-- | Take a template body and a template name and return either an error or a
--   renderable template.

parseTemplate :: String -> String -> Either ParseError Template
parseTemplate s tmplN = parse parseToTemplate tmplN s

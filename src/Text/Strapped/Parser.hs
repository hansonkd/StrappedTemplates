{-# LANGUAGE FlexibleContexts #-}

module Text.Strapped.Parser
  ( parseTemplate
  -- * Building custom template parsers
  , parseExpression
  , parseContent
  , tagStart
  , tagEnd
  , peekTag
  , tryTag
  , tag
  , wordString
  , pathString
  , peekChar
  ) where

import Control.Applicative ((<*>))
import Control.Monad
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Blaze.ByteString.Builder as B
import Blaze.ByteString.Builder.Char.Utf8 as B
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Text.Strapped.Types
import Text.Strapped.Render hiding (getState)

-- | Parse the beginning of a tag
tagStart :: ParserM String
tagStart = string "{$"

-- | Parse the end of a tag.
tagEnd :: ParserM String
tagEnd = string "$}"

-- | Parse alpha-numeric characters and '_'
wordString :: ParserM String
wordString = many1 $ oneOf "_" <|> alphaNum

-- | Parse alpha-numeric characters and '_./'
pathString :: ParserM String
pathString = many1 $ oneOf "_./" <|> alphaNum

-- | Look at a character but don't consume
peekChar :: Char -> ParserM ()
peekChar = void . try . lookAhead . char

-- | Look at a tag but don't consume
peekTag :: ParserM a -> ParserM ()
peekTag = void . try . lookAhead . tag

-- | Try a tag and consume if it matches
tryTag :: ParserM  a -> ParserM ()
tryTag = void . try . tag

-- | Parse content between `tagStart` and `tagEnd`
tag :: ParserM a -> ParserM a
tag p = between (tagStart >> spaces) (spaces >> tagEnd) p <?> "Tag"

parseFloat :: ParserM Double
parseFloat = do sign <- option 1 (do s <- oneOf "+-"
                                     return $ if s == '-' then-1.0 else 1.0)
                x    <- P.float $ P.makeTokenParser emptyDef
                return $ sign * x

parseInt :: ParserM Integer
parseInt = do sign <- option 1 (do s <- oneOf "+-"
                                   return $ if s == '-' then-1 else 1)
              x    <- P.integer $ P.makeTokenParser emptyDef
              return $ sign * x

parseContent :: ParserM a -> ParserM [ParsedPiece]
parseContent end = do
  decls <- many (try $ spaces >> parseWithPos parseDecl)
  spaces
  extends <- optionMaybe (try $ spaces >> parseInherits)
  case (extends) of
    Just (e, epos) -> do
      includes <- manyTill parseIsIgnoreSpace end
      return $ (decls) ++ [ParsedPiece (Inherits e (M.fromList includes)) epos]
    _     -> do
      ps <- manyTill parsePiece end
      return $ decls ++ ps
  where parseIsIgnoreSpace = do {spaces; b <- parseIsBlock; spaces; return b}

parseBlock :: ParserM Piece
parseBlock = do
  blockName <- tag (string "block" >> spaces >> wordString) <?> "Block tag"
  blockContent <- parseContent (tryTag $ string "endblock") 
  return $ (BlockPiece blockName blockContent)

parseRaw :: ParserM Piece
parseRaw = do
  tag (string "raw") <?> "Raw tag"
  c <- anyChar
  s <- manyTill anyChar (tryTag (string "endraw"))
  return $ StaticPiece (B.fromString $ c:s)

parseComment :: ParserM Piece
parseComment = do
  tag (string "comment") <?> "Comment tag"
  c <- anyChar
  s <- manyTill anyChar (tryTag (string "endcomment"))
  return $ StaticPiece mempty

parseIf :: ParserM Piece
parseIf = do
  exp <- (tagStart >> spaces >> string "if" >> spaces >> parseExpression (try $ spaces >> tagEnd)) <?> "If tag"
  positive <- parseContent ((peekTag $ string "endif") <|> (tryTag $ string "else"))
  negative <- parseContent (tryTag $ string "endif")
  return $ IfPiece exp positive negative

parseFor :: ParserM Piece
parseFor = do
  (newVarName, exp) <- (tagStart >> spaces >> string "for" >> argParser) <?> "For tag"
  blockContent <- parseContent (tryTag $ string "endfor") 
  return $ ForPiece newVarName exp blockContent
  where argParser = do 
        spaces
        v <- wordString
        spaces >> (string "in") >> spaces
        func <- parseExpression (try $ spaces >> tagEnd)
        return (v, func)

parseDecl :: ParserM Piece
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

parseInclude :: ParserM Piece
parseInclude = do
  tag parserInclude <?> "Include tag"
  where parserInclude = do
                string "include" >> spaces
                includeName <- pathString
                return $ Include includeName

parseInherits = do {pos <- getPosition; mtag <- tag (string "inherits" >> spaces >> pathString); return (mtag, pos)} <?> "Inherits tag"

parseFunc ::  ParserM Piece
parseFunc = parserFunc <?> "Call tag"
  where parserFunc = do
          pos <- getPosition
          string "${" >> spaces
          exp <- parseExpression (try $ spaces >> string "}")
          return $ FuncPiece exp

-- | Parse an expression that produces a `Literal`
parseExpression ::  ParserM a -> ParserM ParsedExpression
parseExpression end = manyPart <?> "Expression"
  where parseGroup = try parens <|> parseAtomic
        parseAtomic  = do
            pos <- getPosition
            exp <- try parseList <|> 
                   try (parseString '\"') <|> 
                   try (parseString '\'') <|> 
                   try (parseFloat >>= (return . LiteralExpression . LitDouble)) <|> 
                   try (parseInt >>= (return . LiteralExpression . LitInteger)) <|>
                   try (parseBool >>= (return . LiteralExpression . LitBool)) <|>
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
        parseString esc = parseStringContents esc >>= (return . LiteralExpression . LitText . T.pack)
        parseBool = (try $ string "True" >> return True) <|> (try $ string "False" >> return False)
        literal = wordString >>= (return . LookupExpression)

parseStringContents ::  Char -> ParserM String
parseStringContents esc = between (char esc) (char esc) (many chars)
    where chars = (try escaped) <|> noneOf [esc]
          escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
          codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '\'', '/']
          replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '\'', '/']
          escapedChar code replacement = char code >> return replacement

parseStatic :: ParserM Piece
parseStatic = do
  c <- anyChar
  s <- manyTill anyChar (peekChar '{' <|> peekChar '$' <|> eof)
  return $ StaticPiece (B.fromString $ c:s)

parseNonStatic :: ParserM Piece
parseNonStatic =  try parseComment
              <|> try parseRaw
              <|> try parseBlock
              <|> try parseIf
              <|> try parseFor
              <|> try parseInclude
              <|> parseFunc

parsePiece :: ParserM ParsedPiece
parsePiece = do
    parsers <- liftM customParsers getState 
    foldr (\(BlockParser p) acc -> try (parseWithPos p) <|> acc) base_parser parsers
    where base_parser = parseWithPos (try parseNonStatic <|> parseStatic)

parseWithPos :: (Block a) => ParserM a -> ParserM ParsedPiece
parseWithPos p = do
  pos <- getPosition
  v <- p
  return $ ParsedPiece v pos

parsePieces :: ParserM [ParsedPiece]
parsePieces = parseContent eof

parseToTemplate :: ParserM Template
parseToTemplate = (parseContent eof) >>= (return . Template)

-- | Take config, a template body and a template name and return either an error or a
--   renderable template.
parseTemplate :: StrappedConfig -> String -> String -> Either ParseError Template
parseTemplate config s tmplN = runParser parseToTemplate config tmplN s

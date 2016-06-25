module Slip.SExpression (
  SExpression(..),
  readSExpression,
  readSExpressionSequence
) where

import Text.ParserCombinators.Parsec

data SExpression
  = SSymbol String
  | SString String
  | SNumber Integer
  | SList [SExpression]
  | SDottedList [SExpression] SExpression
  | SQuotedExpression SExpression
  deriving (Show, Eq)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseSymbol :: Parser SExpression
parseSymbol = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ SSymbol (first : rest)

parseString :: Parser SExpression
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ SString x

parseNumber :: Parser SExpression
parseNumber = (SNumber . read) <$> many1 digit

parseList :: Parser SExpression
parseList = SList <$> sepBy parseExpression spaces1

parseDottedList :: Parser SExpression
parseDottedList = do
  hd <- endBy parseExpression spaces1
  tl <- char '.' >> spaces1 >> parseExpression
  return $ SDottedList hd tl

parseEitherList :: Parser SExpression
parseEitherList = do
  _ <- char '(' >> spaces
  x <- try parseList <|> parseDottedList
  _ <- spaces >> char ')'
  return x

parseQuoted :: Parser SExpression
parseQuoted = do
  _ <- char '\''
  x <- parseExpression
  return $ SQuotedExpression x

parseExpression :: Parser SExpression
parseExpression
  = parseSymbol
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> parseEitherList

readSExpression :: String -> Either String SExpression
readSExpression str =
  case parse topExpr "s-expression" str of
    Left err -> Left (show err)
    Right expr -> Right expr
  where
    topExpr = do
      _ <- spaces
      e <- parseExpression
      _ <- spaces >> eof
      return e

readSExpressionSequence :: String -> Either String [SExpression]
readSExpressionSequence str =
  case parse exprList "s-expression-list" str of
    Left err -> Left (show err)
    Right exprs -> Right exprs
  where
    exprList = do
      _ <- spaces
      l <- sepEndBy parseExpression spaces1
      _ <- eof
      return l

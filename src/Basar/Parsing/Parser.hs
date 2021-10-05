{-# LANGUAGE OverloadedStrings #-}

module Basar.Parsing.Parser (parseBasar) where

import Basar.Parsing.Ast (Expr (ECall, EDefun, EFloat, EGroup, EInt, ELambda, ELet, ERef, EStr), Ident (MkIdent), Range (MkRange), Type (MkType))
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), ParseErrorBundle, Parsec, choice, getSourcePos, many, manyTill, runParser, takeWhileP, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void String

parseBasar :: String -> Either (ParseErrorBundle String Void) [Expr]
parseBasar = runParser codeblock ""

codeblock :: Parser [Expr]
codeblock = many $ lexeme decl

type' :: Parser Type
type' = lexeme $ do
  name <-
    (:)
      <$> letterChar
      <*> many alphaNumChar
      <?> "identifier"

  return $ MkType name

ident :: Parser Ident
ident = lexeme $ do
  name <-
    (:)
      <$> identChar
      <*> many (identChar <|> alphaNumChar)
      <?> "identifier"

  return $ MkIdent name MkRange
  where
    identChar :: Parser Char
    identChar =
      choice
        [ char '-',
          char '+',
          char '/',
          char '*',
          char '^',
          char '&',
          char '|',
          char '=',
          char '>',
          char '<',
          char '!',
          char '/',
          char '\'',
          char '_',
          letterChar
        ]

expr :: Parser Expr
expr = lexeme $ eLet <|> eLambda <|> eDefun <|> eCall
  where
    eLambda :: Parser Expr
    eLambda = do
      keyword "lambda"
      parameter <- parameter
      code <- codeblock

      return $ ELambda parameter code MkRange

    eDefun :: Parser Expr
    eDefun = do
      keyword "defun"
      name <- ident
      parameters <- keyword "(" *> many parameter <* keyword ")"

      EDefun name parameters <$> codeblock

    eLet :: Parser Expr
    eLet = do
      keyword "let"
      variables <- keyword "(" *> many variable <* keyword ")"
      code <- codeblock

      return $ ELet variables code MkRange

    eCall :: Parser Expr
    eCall = do
      callee <- primary
      arguments <- many expr

      return $ foldl (\acc argument -> ECall acc argument MkRange) callee arguments

    parameter :: Parser (Ident, Type)
    parameter = lexeme $ keyword "(" *> ((,) <$> ident <*> type') <* keyword ")"

    variable :: Parser (Ident, Expr)
    variable = lexeme $ keyword "(" *> ((,) <$> ident <*> expr) <* keyword ")"

primary :: Parser Expr
primary = lexeme $ eStr <|> eInt <|> eFloat <|> eGroup <|> eRef
  where
    eStr :: Parser Expr
    eStr = do
      text <- char '"' *> manyTill L.charLiteral (char '"')

      return $ EStr text MkRange

    eInt :: Parser Expr
    eInt = do
      n <- L.decimal

      return $ EInt n MkRange

    eFloat :: Parser Expr
    eFloat = do
      n <- L.float

      return $ EFloat n MkRange

    eRef :: Parser Expr
    eRef = do
      name <- ident

      return $ ERef name MkRange

    eGroup :: Parser Expr
    eGroup = do
      expr <- parenthesis expr

      return $ EGroup expr MkRange

parenthesis :: Parser a -> Parser a
parenthesis parser = symbol "(" *> parser <* symbol ")"

decl :: Parser Expr
decl = parenthesis expr

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

keyword :: String -> Parser String
keyword = lexeme . string

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment ";;")
    (L.skipBlockComment "(*" "*)")

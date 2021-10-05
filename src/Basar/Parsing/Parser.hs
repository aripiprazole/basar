{-# LANGUAGE OverloadedStrings #-}

module Basar.Parsing.Parser (parseBasar) where

import Basar.Parsing.Ast (Decl (DDefun), Expr (ECall, EFloat, EGroup, EInt, ELambda, ELet, ERef, EStr), Ident (MkIdent), Range (MkRange), Stmt (SDecl, SDefun, SExpr), Type (MkType))
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), ParseErrorBundle, Parsec, choice, getSourcePos, many, manyTill, runParser, takeWhileP, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void String

parseBasar :: String -> Either (ParseErrorBundle String Void) [Decl]
parseBasar = runParser program ""

program :: Parser [Decl]
program = many decl

codeblock :: Parser [Stmt]
codeblock = many stmt

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

  return $ MkIdent MkRange name
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

decl :: Parser Decl
decl = lexeme . parenthesis $ dDefun
  where
    dDefun :: Parser Decl
    dDefun = do
      keyword "defun"
      name <- ident
      parameters <- keyword "(" *> many parameter <* keyword ")"
      DDefun MkRange name parameters <$> codeblock

stmt :: Parser Stmt
stmt = lexeme . parenthesis $ sDecl <|> sExpr
  where
    sDecl :: Parser Stmt
    sDecl = SDecl MkRange <$> decl

    sExpr :: Parser Stmt
    sExpr = SExpr MkRange <$> expr

expr :: Parser Expr
expr = lexeme $ eLet <|> eLambda <|> eCall
  where
    eLambda :: Parser Expr
    eLambda = do
      keyword "lambda"
      parameter <- parameter
      ELambda MkRange parameter <$> codeblock

    eLet :: Parser Expr
    eLet = do
      keyword "let"
      variables <- keyword "(" *> many variable <* keyword ")"
      ELet MkRange variables <$> codeblock

    eCall :: Parser Expr
    eCall = do
      callee <- primary
      arguments <- many expr

      return $ foldl (ECall MkRange) callee arguments

primary :: Parser Expr
primary = lexeme $ eStr <|> eInt <|> eFloat <|> eGroup <|> eRef
  where
    eStr :: Parser Expr
    eStr = EStr MkRange <$> (char '"' *> manyTill L.charLiteral (char '"'))

    eInt :: Parser Expr
    eInt = EInt MkRange <$> L.decimal

    eFloat :: Parser Expr
    eFloat = EFloat MkRange <$> L.float

    eRef :: Parser Expr
    eRef = ERef MkRange <$> ident

    eGroup :: Parser Expr
    eGroup = EGroup MkRange <$> parenthesis expr

parenthesis :: Parser a -> Parser a
parenthesis parser = symbol "(" *> parser <* symbol ")"

parameter :: Parser (Ident, Type)
parameter = lexeme $ keyword "(" *> ((,) <$> ident <*> type') <* keyword ")"

variable :: Parser (Ident, Expr)
variable = lexeme $ keyword "(" *> ((,) <$> ident <*> expr) <* keyword ")"

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

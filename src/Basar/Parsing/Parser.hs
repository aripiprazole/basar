{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Basar.Parsing.Parser (parseBasar) where

import Basar.Parsing.Ast (Decl (..), Expr (..), Ident (..), Loc (..), Stmt (..), Type (..))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (..), ParseErrorBundle, Parsec, SourcePos (..), choice, getSourcePos, many, manyTill, runParser, takeWhileP, unPos, (<?>), (<|>))
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
type' = lexeme $ Type <$> name
  where
    name :: Parser String
    name =
      (:)
        <$> letterChar
        <*> many (letterChar <|> alphaNumChar)
        <?> "identifier"

ident :: Parser Ident
ident = lexeme $ Ident <$> name <*> currentLoc
  where
    name :: Parser String
    name =
      (:)
        <$> identChar
        <*> many (identChar <|> alphaNumChar)
        <?> "identifier"

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
decl = lexeme . parenthesis $ defunDecl
  where
    defunDecl :: Parser Decl
    defunDecl = keyword "defun" *> (DefunDecl <$> ident <*> parameters <*> codeblock <*> currentLoc)
      where
        parameters :: Parser [(Ident, Type)]
        parameters = parenthesis $ many parameter

stmt :: Parser Stmt
stmt = lexeme . parenthesis $ declStmt <|> exprStmt
  where
    declStmt :: Parser Stmt
    declStmt = DeclStmt <$> decl <*> currentLoc

    exprStmt :: Parser Stmt
    exprStmt = ExprStmt <$> expr <*> currentLoc

expr :: Parser Expr
expr = lexeme $ letExpr <|> lambdaExpr <|> callExpr
  where
    lambdaExpr :: Parser Expr
    lambdaExpr = keyword "lambda" *> (LambdaExpr <$> parameter <*> codeblock <*> currentLoc)

    letExpr :: Parser Expr
    letExpr = keyword "let" *> (LetExpr <$> parenthesis (many variable) <*> codeblock <*> currentLoc)

    callExpr :: Parser Expr
    callExpr = do
      callee <- primary
      arguments <- many expr

      foldl (\acc arg -> (`CallExpr` arg) <$> acc <*> currentLoc) (pure callee) arguments

primary :: Parser Expr
primary = lexeme $ strExpr <|> intExpr <|> floatExpr <|> groupExpr <|> refExpr
  where
    strExpr :: Parser Expr
    strExpr = StrExpr <$> (char '"' *> manyTill L.charLiteral (char '"')) <*> currentLoc

    intExpr :: Parser Expr
    intExpr = IntExpr <$> L.decimal <*> currentLoc

    floatExpr :: Parser Expr
    floatExpr = FloatExpr <$> L.float <*> currentLoc

    refExpr :: Parser Expr
    refExpr = RefExpr <$> ident <*> currentLoc

    groupExpr :: Parser Expr
    groupExpr = GroupExpr <$> parenthesis expr <*> currentLoc

currentLoc :: Parser Loc
currentLoc = do
  SourcePos _ x y <- getSourcePos

  return $ Loc (unPos x) (unPos y)

parenthesis :: Parser arguments -> Parser arguments
parenthesis parser = symbol "(" *> parser <* symbol ")"

parameter :: Parser (Ident, Type)
parameter = lexeme . parenthesis $ (,) <$> ident <*> type'

variable :: Parser (Ident, Expr)
variable = lexeme . parenthesis $ (,) <$> ident <*> expr

lexeme :: Parser arguments -> Parser arguments
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

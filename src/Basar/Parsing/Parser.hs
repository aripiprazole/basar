{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Basar.Parsing.Parser (parseBasar) where

import Basar.Parsing.Ast (Decl (DDefun), Expr (ECall, EFloat, EGroup, EInt, ELambda, ELet, ERef, EStr), Ident (MkIdent), Loc (MkLoc), Stmt (SDecl, SDefun, SExpr), Type (MkType))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), ParseErrorBundle, Parsec, SourcePos (SourcePos), choice, getSourcePos, many, manyTill, runParser, takeWhileP, unPos, (<?>), (<|>))
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
ident = lexeme $ MkIdent <$> currentLoc <*> name
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
decl = lexeme . parenthesis $ dDefun
  where
    dDefun :: Parser Decl
    dDefun = keyword "defun" *> (DDefun <$> currentLoc <*> ident <*> parameters <*> codeblock)
      where
        parameters :: Parser [(Ident, Type)]
        parameters = keyword "(" *> many parameter <* keyword ")"

stmt :: Parser Stmt
stmt = lexeme . parenthesis $ sDecl <|> sExpr
  where
    sDecl :: Parser Stmt
    sDecl = SDecl <$> currentLoc <*> decl

    sExpr :: Parser Stmt
    sExpr = SExpr <$> currentLoc <*> expr

expr :: Parser Expr
expr = lexeme $ eLet <|> eLambda <|> eCall
  where
    eLambda :: Parser Expr
    eLambda = keyword "lambda" *> (ELambda <$> currentLoc <*> parameter <*> codeblock)

    eLet :: Parser Expr
    eLet = keyword "let" *> (ELet <$> currentLoc <*> variables <*> codeblock)
      where
        variables :: Parser [(Ident, Expr)]
        variables = keyword "(" *> many variable <* keyword ")"

    eCall :: Parser Expr
    eCall = do
      callee <- primary
      arguments <- map pure <$> many expr

      foldl (\acc arg -> ECall <$> currentLoc <*> acc <*> arg) (pure callee) arguments

primary :: Parser Expr
primary = lexeme $ eStr <|> eInt <|> eFloat <|> eGroup <|> eRef
  where
    eStr :: Parser Expr
    eStr = EStr <$> currentLoc <*> (char '"' *> manyTill L.charLiteral (char '"'))

    eInt :: Parser Expr
    eInt = EInt <$> currentLoc <*> L.decimal

    eFloat :: Parser Expr
    eFloat = EFloat <$> currentLoc <*> L.float

    eRef :: Parser Expr
    eRef = ERef <$> currentLoc <*> ident

    eGroup :: Parser Expr
    eGroup = EGroup <$> currentLoc <*> parenthesis expr

currentLoc :: Parser Loc
currentLoc = do
  SourcePos _ x y <- getSourcePos

  return $ MkLoc (unPos x) (unPos y)

parenthesis :: Parser arguments -> Parser arguments
parenthesis parser = symbol "(" *> parser <* symbol ")"

parameter :: Parser (Ident, Type)
parameter = lexeme $ keyword "(" *> ((,) <$> ident <*> type') <* keyword ")"

variable :: Parser (Ident, Expr)
variable = lexeme $ keyword "(" *> ((,) <$> ident <*> expr) <* keyword ")"

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

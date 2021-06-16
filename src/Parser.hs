module Parser where

import Control.Monad ()
import Data.Char ()
import Debug.Trace
import Text.Parsec (eof, many, many1, parse, sepBy, upper, (<|>))
import Text.Parsec.Expr
  ( Assoc (AssocNone),
    Operator (Infix, Postfix),
    buildExpressionParser,
  )
import Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Token as Token
import Type
  ( Expr (App, Case, DataConstructor, If, Lam, Let, Lit, Var),
    Literal (LBool, LInt),
    Path (PCon, PLit, PVar),
  )

wordTokens :: [String]
wordTokens =
  [ "if",
    "then",
    "else",
    "case",
    "of",
    "true",
    "false",
    "->",
    "let",
    "in",
    "data"
  ]

operatorTokens :: [String]
operatorTokens =
  ["+", "-", "*", "/", "==", ">=", "<=", "<", ">", "/=", ",", "="]

languageStyle =
  haskellStyle
    { Token.reservedOpNames = operatorTokens,
      Token.reservedNames = wordTokens
    }

lexer = Token.makeTokenParser style
  where
    operators = operatorTokens
    words = wordTokens
    style = languageStyle

reservedWords = Token.reserved lexer

reservedOperators = Token.reservedOp lexer

identifiers = Token.identifier lexer

parens = Token.parens lexer

idNumbers = Token.natural lexer

whiteSpace x = do
  Token.whiteSpace lexer
  r <- x
  eof
  return r

expLambda = do
  reservedOperators "\\"
  lambda <- many1 identifiers
  reservedOperators "->"
  expression <- build
  return $ foldr Lam expression lambda

variable = do
  Var <$> identifiers

string = do
  identifiers

number = do
  Lit . LInt . fromIntegral <$> idNumbers

boolean =
  do
    reservedWords "true"
    return (Lit (LBool True))
    <|> do
      reservedWords "false"
      return (Lit (LBool False))

ifStmt = do
  reservedWords "if"
  condition <- build
  reservedWords "then"
  expression <- build
  reservedWords "else"
  If condition expression <$> build

caOf = do
  reservedWords "case"
  condition <- build
  reservedWords "of"
  paths <- sepBy parsePaths $ reservedOperators ";"
  return (Case condition paths)

parsePaths =
  do
    firstPath <- patterns
    reservedWords "->"
    expression <- build
    return (firstPath, expression)
    <|> do
      secondPath <- patterns
      reservedWords "->"
      expression <- build
      return (secondPath, expression)

patterns =
  do
    parens generatePaths
    <|> do
      PVar <$> string
    <|> do
      PLit . LInt . fromIntegral <$> idNumbers
    <|> do
      reservedWords "true"
      return (PLit (LBool True))
    <|> do
      reservedWords "false"
      return (PLit (LBool False))

generatePaths = do
  constructor <- capitalize
  paths <- many patterns
  return (PCon constructor paths)

capitalize = do
  firstLetter <- upper
  tail <- string
  return (firstLetter:tail)

letIn = do
  reservedWords "let"
  expression <- identifiers
  reservedOperators "="
  expressionIn <- build
  reservedWords "in"
  Let (expression, expressionIn) <$> build

constructor = do
  reservedWords "data"
  id <- identifiers
  reservedOperators "="
  expression <- identifiers
  reservedWords "in"
  DataConstructor (id, expression) <$> build

operator =
  [ [Postfix (reservedOperators "+" >> return (App (Var "+")))],
    [Postfix (reservedOperators "-" >> return (App (Var "-")))],
    [Postfix (reservedOperators "*" >> return (App (Var "*")))],
    [Postfix (reservedOperators "/" >> return (App (Var "/")))],
    [Postfix (reservedOperators "<" >> return (App (Var "<")))],
    [Postfix (reservedOperators ">" >> return (App (Var ">")))],
    [Postfix (reservedOperators "==" >> return (App (Var "==")))],
    [Postfix (reservedOperators "=<" >> return (App (Var "=<")))],
    [Postfix (reservedOperators "=>" >> return (App (Var "=>")))],
    [Postfix (reservedOperators "/=" >> return (App (Var "/=")))],
    [Infix (return App) AssocNone]
  ]

build = buildExpressionParser operator build2

build2 =
  parens build
    <|> constructor
    <|> letIn
    <|> boolean
    <|> ifStmt
    <|> variable
    <|> number
    <|> expLambda
    <|> caOf

parsing = parse (whiteSpace build) "error"

handleError :: Either a p -> p
handleError (Right o) = o
handleError (Left o) = error "verify the syntax of the expression"

parseExpr :: String -> Expr
parseExpr = handleError . parsing
module Template.Fuddle.BParser where

import Control.Applicative (asum, optional, many, (<|>), some)

import Data.Functor (($>))
import Data.Text (Text, pack, cons)
import Data.Void (Void)
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq, fromList)

import qualified Control.Monad.Combinators.Expr as CE
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as ML
import qualified Text.Megaparsec.Debug as MD
import GHC.Real (reduce)


import Template.Fuddle.BAst

type Parser = M.Parsec Void Text

reservedWords :: [Text]
reservedWords =
  [ "if", "then", "else", "import", "True", "False" ]


run = M.parse fuddleStmt "test"
runTest = M.parseTest fuddleStmt


fuddleParser :: Parser StatementFd
fuddleParser = M.between spaceF M.eof fuddleStmt

{- Top-level parsers: -}
fuddleStmt :: Parser StatementFd
fuddleStmt = curlies stmtSequence <|> stmtSequence

stmtSequence :: Parser StatementFd
stmtSequence = stmtFilter <$> M.sepBy1 fuddleStmt' endOfStmt
  where
    stmtFilter aList = if length aList == 1 then head aList else SeqST aList


{- Statement parsers: -}
fuddleStmt' :: Parser StatementFd
fuddleStmt' =
  MD.dbg "stmt" $ asum [
      MD.dbg "bind-stmt" $ M.try bindStmt      -- <qual-ident> = <expr>
      , MD.dbg "val-stmt" valueStmt     -- <expr>
      , MD.dbg "short-stmt" shortStmt     -- @? <expr> @[
      , MD.dbg "blck-stmt" blockEnd      -- @]
      , MD.dbg "elif-stmt" elseIfStmt    -- else if <expr> @[
      , MD.dbg "imp-stmt" importStmt    -- import [qualified] <qual-ident> [ as <alias> ]
    ]

blockEnd :: Parser StatementFd
blockEnd = do
  symbol "@]"
  pure BlockEndST


elseIfStmt :: Parser StatementFd
elseIfStmt = do
  a <- optional $ pReservedWord "else"
  pReservedWord "if"
  cond <- expressionFd
  symbol "@["
  pure $ ElseIfShortST (isJust a) cond

shortStmt :: Parser StatementFd
shortStmt =
  ifElseNilSS

ifElseNilSS :: Parser StatementFd
ifElseNilSS = do
  pReservedWord "@?"
  cond <- expressionFd
  symbol "@["
  pure $ IfElseNilSS cond

importStmt :: Parser StatementFd
importStmt = do
  pReservedWord "import"
  a <- optional $ pReservedWord "qualified"
  b <- qualifiedIdent
  c <- optional $ do
    pReservedWord "as"
    qualifiedIdent
  pure $ ImportST (isJust a) b c


bindStmt :: Parser StatementFd
bindStmt = do
  a <- MD.dbg "bind-ident" qualifiedIdent
  b <- MD.dbg "bind-args" $ many qualifiedIdent
  symbol "="
  BindOneST (a, b) <$> expressionFd


valueStmt :: Parser StatementFd
valueStmt = ExpressionST <$> expressionFd


expressionFd =
  MD.dbg "expr" $ asum [
      MD.dbg "pars-expr" parensExpr
      , MD.dbg "oper-expr" $ M.try operatorExpr
      , MD.dbg "redu-expr" reductionExpr
      , MD.dbg "lit-expr" literalExpr
      , MD.dbg "array-expr" arrayExpr
    ]

nonOperExpr :: Parser Expression
nonOperExpr =
  MD.dbg "no-expr" $ asum [
    MD.dbg "pars-no-expr" parensExpr
    , MD.dbg "redu-no-expr" reductionExpr
    , MD.dbg "lit-no-expr" literalExpr
    , MD.dbg "array-no-expr" arrayExpr
  ]


parensExpr :: Parser Expression
parensExpr = do
    symbol "("
    a <- MD.dbg "expr-pars" expressionFd
    symbol ")"
    pure $ ParenExpr a



{-
  a <- literalExpr
  symbol "+"
  BinOpExpr AddOP a <$> literalExpr
-}


literalExpr :: Parser Expression
literalExpr =
  LiteralExpr <$> asum [
      MD.dbg "arit-lit" $ ArithValue <$> integer
      , MD.dbg "bool-lit" boolValue
      , MD.dbg "char-lit" $ CharValue <$> M.between (M.char '\'') (M.char '\'') ML.charLiteral <* M.space
      , MD.dbg "str-lit" $ StringValue <$> stringLiteral
    ]


boolValue :: Parser LiteralValue
boolValue =
  (pReservedWord "True" $> BoolValue True)
  <|> (pReservedWord "False" $> BoolValue True)


arrayExpr :: Parser Expression
arrayExpr = do
  symbol "["
  a <- M.sepBy expressionFd (symbol ",")
  symbol "]"
  pure $ ArrayExpr a


{- Terms for operators: -}

operatorExpr :: Parser Expression
operatorExpr = MD.dbg "operatorExpr" $ CE.makeExprParser nonOperExpr precOperators  -- expressionFd

{- Operators: -}

precOperators :: [[CE.Operator Parser Expression]]
precOperators = [
    [ 
      CE.Prefix (UnaryExpr NegateOP <$ symbol "-")
    , CE.Prefix (UnaryExpr NotOP <$ symbol "!")
    , CE.Prefix (UnaryExpr BitNotOP <$ symbol "~")
    ]
  , [
      CE.InfixL (BinOpExpr MultiplyOP <$ symbol "*")
    , CE.InfixL (BinOpExpr DivideOP <$ symbol "/")
    , CE.InfixL (BinOpExpr ModuloOP <$ symbol "%")
    , CE.InfixL (BinOpExpr CarAddOP <$ symbol ":")
    ]
  , [
      CE.InfixL (BinOpExpr ConcatOP <$ symbol "++")
    , CE.InfixL (BinOpExpr ConcatOP <$ symbol "<>")
    , CE.InfixL (BinOpExpr AddOP <$ symbol "+")
    , CE.InfixL (BinOpExpr SubstractOP <$ symbol "-")
    ]
  , [
      CE.InfixL (BinOpExpr BitShiftLeftOP <$ symbol "<<")
      , CE.InfixL (BinOpExpr BitShiftRightOP <$ symbol ">>")
    ]
  , [
      CE.InfixL (BinOpExpr LtOP <$ symbol "<")
      , CE.InfixL (BinOpExpr LeOP <$ symbol "<=")
      , CE.InfixL (BinOpExpr GtOP <$ symbol ">")
      , CE.InfixL (BinOpExpr GeOP <$ symbol ">=")
    ]
  , [
      CE.InfixL (BinOpExpr EqOP <$ symbol "==")
      , CE.InfixL (BinOpExpr NeOP <$ symbol "/=")
    ]
  , [
      CE.InfixL (BinOpExpr BitXorOP <$ symbol "^")
    ]
  , [
      CE.InfixL (BinOpExpr BitOrOP <$ symbol "|")
    ]
  , [
    CE.InfixL (BinOpExpr AndOP <$ symbol "&&")
    ]
  , [
      CE.InfixL (BinOpExpr OrOP <$ symbol "||")
    ]
  ]


reductionExpr :: Parser Expression
reductionExpr = do
  a <- MD.dbg "redu-ident" qualifiedIdent
  b <- MD.dbg "redu-args" $ M.optional $ M.try (many expressionFd)
  pure $ ReductionExpr a (fromMaybe [] b)


{- Utilities for parsing: -}
spaceF :: Parser ()
spaceF = ML.space M.space1 lineCmnt blockCmnt
  where
    lineCmnt  = ML.skipLineComment "//"
    blockCmnt = ML.skipBlockComment "/*" "*/"


lexeme :: Parser a -> Parser a
lexeme = ML.lexeme spaceF


symbol :: Text -> Parser Text
symbol = ML.symbol spaceF


endOfStmt :: Parser Text
endOfStmt = symbol "\n" <|> symbol ";"

stringLiteral :: Parser Text
stringLiteral = pack <$> M.between (symbol "\"") (symbol "\"") (many stringChar)
  where
    stringChar = M.char '\\' *> M.anySingle <|> M.noneOf ['"']


-- | 'parens' parses something between parenthesis.

parens :: Parser a -> Parser a
parens = M.between (symbol "(") (symbol ")")

curlies :: Parser a -> Parser a
curlies = M.between (symbol "{") (symbol "}")

-- | 'integer' parses an integer.

integer :: Parser Int
integer = lexeme ML.decimal


pReservedWord :: Text -> Parser ()
pReservedWord w = M.string w *> M.notFollowedBy M.alphaNumChar *> spaceF


identifier :: Parser Text
identifier = (lexeme . M.try) (pChars >>= check)
  where
    pChars :: Parser Text
    pChars = cons <$> M.lowerChar <*> (pack <$> many M.alphaNumChar)
    check :: Text -> Parser Text
    check w = if w `elem` reservedWords then
                fail $ "keyword " ++ show w ++ " cannot be an identifier"
              else
                pure w

qualifiedIdent :: Parser QualifiedIdent
qualifiedIdent = do
  a <- identifier
  b <- many (symbol "." *> identifier)
  pure $ fromList (a : b)
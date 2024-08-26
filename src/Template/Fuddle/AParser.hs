module Template.Fuddle.AParser where

import Control.Applicative (asum, optional, many, (<|>), some)

import Data.Text (Text, pack, cons)
import Data.Void (Void)
import Data.Maybe (fromMaybe)

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as ML
import qualified Text.Megaparsec.Debug as MD

import Template.Fuddle.AAst
import Control.Lens (op)
import GHC.Read (paren)
import Text.Megaparsec (MonadParsec(eof))

type Parser = M.Parsec Void Text

reservedWords =
  [ "if", "then", "else", "import" ]


run = M.parse fuddleStmt "test"
runTest = M.parseTest fuddleStmt


fuddleStmt :: Parser StatementFd
fuddleStmt = do
  M.space
  a <- asum [
      ifStmt
      , blockEnd
      , elseIfStmt
      , elseStmt
      , ifElseNil
      , shortStmt
      , importStmt
      , valueStmt
    ]
  M.space
  eof
  pure a


ifStmt :: Parser StatementFd
ifStmt = do
  M.string "if"
  M.space1
  a <- IfShortST <$> expressionFd
  M.space1
  blockStart
  pure a


elseIfStmt = do
  M.string "else if"
  M.space1
  a <- ElseIfShortST <$> expressionFd
  M.space1
  blockStart
  pure a


elseStmt = do
  M.string "else"
  M.space1
  blockStart
  pure ElseShortST


importStmt = do
  M.string "import"
  M.space1
  ImportST <$> symbol

blockStart = do
  M.space
  M.string "@["
  pure BlockStartST


blockEnd = do
  M.space
  M.string "@]"
  pure BlockEndST


shortStmt :: Parser StatementFd
shortStmt =
    ifElseNil

ifElseNil :: Parser StatementFd
ifElseNil = do
  M.space
  M.string "@?"
  M.space
  a <- IfElseNilSS <$> expressionFd
  M.space
  blockStart
  pure $ SpecialST a


valueStmt =
  ValueST <$> expressionFd


expressionFd :: Parser ExprFd
expressionFd = do
  M.space
  asum [
      parenthesisExpr
      , literalExpr
      -- , operationExprV2
      , variableExpr
      , dereferenceExpr
      -- TODO: put other constructs
    ]

parenthesisExpr :: Parser ExprFd
parenthesisExpr = do
  M.char '('
  M.space
  a <- expressionFd
  M.space
  M.char ')'
  pure $ ParenthesisEX a


operationExpr :: Parser ExprFd
operationExpr = do
  mbA <- optional . M.try $ do
    M.space
    expressionFd
  M.space
  case mbA of
    Just a -> do
      op <- binaryOperator
      M.space
      BinaryOperEX op a <$> expressionFd
    Nothing -> do
      op <- unaryOperator
      UnaryOperEX op <$> expressionFd


operationExprV2 :: Parser ExprFd
operationExprV2 = do
  M.space
  a <- expressionFd
  M.space
  op <- binaryOperator
  M.space
  BinaryOperEX op a <$> expressionFd


unaryOperator :: Parser UnaryOperatorFd
unaryOperator =
  asum [
      NotOP <$ M.char '!'
      , NegOP <$ M.char '-'
    ]

binaryOperator :: Parser BinaryOperatorFd
binaryOperator =
  asum [
      arrayOp
      , logicalBinOp
      , mathBinOp
      , compareBinOp
    ]


mathBinOp :: Parser BinaryOperatorFd
mathBinOp =
  asum [
      PlusOP <$ M.char '+'
      , MinusOP <$ M.char '-'
      , MultiplyOP <$ M.char '*'
      , DivideOP <$ M.char '/'
      , ModuloOP <$ M.char '%'
      , ExponentialOP <$ M.char '^'
    ]

arrayOp :: Parser BinaryOperatorFd
arrayOp =
  asum [
      ConcatOP <$ M.string "++"
      -- Synonym:
      , ConcatOP <$ M.string "<>"
      , CarAddOP <$ M.char ':'
  ]

logicalBinOp :: Parser BinaryOperatorFd
logicalBinOp =
  asum [
      OrOP <$ M.string "||"
      , AndOP <$ M.string "&&"
    ]

compareBinOp :: Parser BinaryOperatorFd
compareBinOp =
  asum [
      EqOP <$ M.string "=="
      , NeOP <$ M.string "!="
      , LtOP <$ M.char '<'
      , LeOP <$ M.string "<="
      , GtOP <$ M.char '>'
      , GeOP <$ M.string ">="
    ]

variableExpr :: Parser ExprFd
variableExpr = VariableEX <$> symbol


symbol :: Parser VariableFd
symbol = do
  a <- MD.dbg "symb-ident" legalIdent
  identList <- optional ( do
      M.char '.'
      MD.dbg "symb-qual" $ legalIdent `M.sepBy1` M.char '.'
     )
  case identList of
    Just aList -> pure . QualVar $ QualifiedVar True $ a : aList
    Nothing -> pure . LocVR $ LocalVar $ Just a


localVar :: Parser LocalVarFd
localVar = do
  firstLetter <- M.lowerChar
  b <- pack <$> M.many M.alphaNumChar
  pure . LocalVar $ Just (cons firstLetter b)


qualifiedVar :: Parser QualifiedVarFd
qualifiedVar = do
  rootName <- legalIdent
  M.char '.'
  identList <- optional ( do
      legalIdent `M.sepBy1` M.char '.'
     )
  pure . QualifiedVar True $ rootName : (fromMaybe [] identList)


dereferenceExpr :: Parser ExprFd
dereferenceExpr = do
  f <- MD.dbg "deref-ident" legalIdent
  args <- optional . M.try $ do
      M.space1
      MD.dbg "deref-arg" $ expressionFd `M.sepBy1` M.char ' '
  pure $ ApplicationEX f (fromMaybe [] args)


literalExpr :: Parser ExprFd
literalExpr = do
  a <- asum [
      intValue
      , stringValue
      , charValue
      , boolValue
    ]
  pure $ LiteralEX a


legalIdent :: Parser Text
legalIdent = do
  hChar <- M.lowerChar
  tString <- pack <$> M.many M.alphaNumChar
  pure $ cons hChar tString


intValue :: Parser LiteralFd
intValue = do
  a <- ML.signed M.space ML.decimal
  pure $ IntLV a


stringValue :: Parser LiteralFd
stringValue = do
  StringLV <$> stringLiteral


stringLiteral :: Parser Text
stringLiteral = do
  pack <$> (M.char '\"' *> M.manyTill ML.charLiteral (M.char '\"'))

charValue :: Parser LiteralFd
charValue = do
  M.char '\''
  a <- M.alphaNumChar
  M.char '\''
  pure $ CharLV a


boolValue :: Parser LiteralFd
boolValue = do
  a <- asum [
      True <$ M.string "true"
      , False <$ M.string "false"
    ]
  pure $ BoolLV a


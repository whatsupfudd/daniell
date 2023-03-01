module Template.TemplateText.Parser where

import Control.Applicative (asum, optional, many, (<|>), some)

import Data.Text (Text, pack, cons)
import Data.Void (Void)

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as ML

import Template.TemplateText.Ast

{-
    {{ if <cond> }} TrueBlock [ {{ else }} FalseBlock ] {{ end }}
    {{ with <value> }} JustBlock [ {{ else }} NothingBlock ] {{ end }}
    {{ range <array | slice | map | channel> }} LoopBlock [ {{ else }} OtherwiseBlock ] {{ end }}
    {{ break }}
    {{ continue }}
    {{ partial <label> [ <context> ] }}
    {{ template <label> [ <context> ] }}

 *var-def* and *var-assign* are also statements.


A <value> is called a *pipeline* in golang *template/text* library.  It is defined as:
  
    <pipelines> ::= <pipeline> | "(" <pipeline> ")"
    <pipeline> ::= <args> | <call>
    <call> ::= <function> [ <args> ] [ "|" <call> ]
    <args>::= <var> | <constant> | "."
    <var> ::= [ <var> ] "." <label> | <local-var>
    <local-var> ::= "$" <label>
    <var-def> ::= <local-var> ":=" <pipelines>
    <var-assign> ::= <local-var> "=" <pipelines>
-}


type Parser = M.Parsec Void Text


reservedWords =
  [ "if", "with", "range", "else", "break", "continue", "template" ]


run = M.parseTest goStmt

goStmt :: Parser GoStmt
goStmt =
  asum [
      ifStmt
      , withStmt
      , rangeStmt
      , elseIfStmt
      , elseStmt
      , endStmt
      , breakStmt
      , continueSttmt
      , partialStmt
      , templateStmt
      -- , varDefStmt
      -- , varAssignStmt
      , valueStmt
    ]


ifStmt :: Parser GoStmt
ifStmt = do
  M.string "if"
  M.space1
  a <- pipelineExpr
  pure $ IfST a


withStmt = do
  M.string "with"
  M.space1
  a <- qualifiedVar -- valueExpr
  pure $ WithST a


rangeStmt = do
  M.string "range"
  M.space1
  a <- asum [
      -- TODO: make it right.
      LocVR <$> localVar
      -- , sliceExpr
      -- , mapExpr
      -- , channelExpr
    ]
  pure $ RangeST (RangeEX a)


elseIfStmt = do
  M.string "else if"
  M.space1
  a <- pipelineExpr
  pure $ ElseIfST a


elseStmt = do
  M.string "else"
  pure ElseST

endStmt = do
  M.string "end"
  pure EndST

breakStmt = do
  M.string "break"
  pure BreakST

continueSttmt = do
  M.string "continue"
  pure ContinueST


partialStmt = do
  M.string "partial"
  M.space1
  a <- stringLiteral
  b <- M.optional (M.space1 *> qualifiedVar)
  pure $ PartialST a b


templateStmt = do
  M.string "template"
  M.space1
  a <- stringLiteral
  b <- M.optional (M.space1 *> qualifiedVar)
  pure $ TemplateST a b

valueStmt = do
  a <- pipelineExpr
  pure $ ValueST a


pipelineExpr :: Parser PipelineExpr
pipelineExpr = do
  a <- asum [
      do
        M.char '('
        M.space
        a <- pipelineExpr
        M.space
        M.char ')'
        pure $ ParenthesisExpr a
      , callExpr
      , variableExpr
      , literalExpr
      -- TODO: put other constructs
    ]
  binConn <- optional ( do
      M.space
      op <- binaryOperator
      M.space
      b <- pipelineExpr
      pure $ BinaryOperExpr op a b
    )
  case binConn of
    Just aRez -> pure aRez
    Nothing -> pure a


binaryOperator =
  asum [
      logicalBinOp
      , mathBinOp
      , compareBinOp
      , funcBinOp
    ]


mathBinOp =
  asum [
      PlusOP <$ M.char '+'
      , MinusOP <$ M.char '-'
      , MultiplyOP <$ M.char '*'
      , DivideOP <$ M.char '/'
      , ModuloOP <$ M.char '%'
      , ExponentialOP <$ M.char '^'
    ]


logicalBinOp =
  asum [
      OrOP <$ M.string "||"
      , AndOP <$ M.string "&&"
    ]

compareBinOp =
  asum [
      EqOP <$ M.string "=="
      , NeOP <$ M.string "!="
      , LtOP <$ M.char '<'
      , LeOP <$ M.string "<="
      , GtOP <$ M.char '>'
      , GeOP <$ M.string ">="
    ]

funcBinOp =
  PipeOP <$ M.char '|'


literalExpr :: Parser PipelineExpr
literalExpr = do
  a <- asum [
      intValue
      , stringValue
      , charValue
      , boolValue
    ]
  pure $ LiteralEX a


intValue :: Parser LiteralValue
intValue = do
  a <- ML.signed M.space ML.decimal
  pure $ IntLV a


stringValue :: Parser LiteralValue
stringValue = do
  StringLV <$> stringLiteral


stringLiteral :: Parser Text
stringLiteral = do
  M.char '"'
  a <- pack <$> many M.alphaNumChar
  M.char '"'
  pure $ a

charValue :: Parser LiteralValue
charValue = do
  M.char '\''
  a <- M.alphaNumChar
  M.char '\''
  pure $ CharLV a


boolValue :: Parser LiteralValue
boolValue = do
  a <- asum [
      True <$ M.string "true"
      , False <$ M.string "false"
    ]
  pure $ BoolLV a


variableExpr :: Parser PipelineExpr
variableExpr = do
  a <- LocVR <$> localVar <|>  QualVar <$> qualifiedVar
  pure $ VariableEX a


localVar :: Parser LocalVar
localVar = do
  M.char '$'
  a <- optional ( pack <$> M.some M.alphaNumChar )
  pure $ LocalVar a


qualifiedVar :: Parser QualifiedVar
qualifiedVar = do
  M.char '.'
  identList <- optional ( legalIdent `M.sepBy1` (M.char '.') )
  pure $ QualifiedVar True (case identList of Nothing -> []; Just aList -> aList)


callExpr = do
  f <- legalIdent
  args <- optional ( do
      M.space1
      argumentExpr `M.sepBy1` (M.char ' ')
    )
  pure $ CallEX f (case args of Nothing -> []; Just aList -> aList)


argumentExpr = do
  a <- asum [
      do
        M.char '('
        M.space
        a <- pipelineExpr
        M.space
        M.char ')'
        pure $ ParenthesisExpr a
      , variableExpr
      , literalExpr
      -- TODO: put other constructs
    ]
  pure a


legalIdent :: Parser Text
legalIdent = do
  hChar <- M.letterChar
  tString <- pack <$> M.many M.alphaNumChar
  pure $ cons hChar tString

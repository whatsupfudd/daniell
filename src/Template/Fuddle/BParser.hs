module Template.Fuddle.BParser where

import Control.Applicative (asum, optional, many, (<|>), some)

import qualified Data.ByteString as BS
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq, fromList)
import Data.Text (Text, pack, cons)
import Data.Void (Void)
{- TODO: transition parser from Text to ByteString -}
import qualified Data.Text.Encoding as TE

import qualified Control.Monad.Combinators.Expr as CE
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as ML
import qualified Text.Megaparsec.Debug as MD

import Conclusion (GenError (..))
import Template.Fuddle.BAst
import RunTime.Interpreter.Context (VerbatimBlock(blocks))


type Parser = M.Parsec Void Text

reservedWords :: [Text]
reservedWords =
  [ "if", "then", "else", "import", "True", "False" ]

oDbg str p =
  if False then MD.dbg str p else p


{- TODO: when megaparsec logic is stable, change this from IO to pure (IO is for debugging with runTest...). -}
parseLogicBlock :: (Int, Int) -> String -> BS.ByteString -> IO (Either GenError BlockAst)
parseLogicBlock startOffset codeName blockText = do
  let
    logicText = TE.decodeUtf8
          . BS.dropWhileEnd (\c -> c == 32 || c == 10) . BS.dropWhile (\c -> c == 32 || c == 10)
          . BS.dropWhileEnd (== 125) . BS.dropWhile (== 123) $ blockText
    eiParseRez = run codeName logicText
  -- runTest logicText
  case eiParseRez of
    Left err ->
      -- putStrLn $ "@[parseLogicBlock] run err: " ++ show err
      -- "@[parseLogicBlock] parse err: " <>
      pure . Left . SimpleMsg $ displayError startOffset err
    Right parseRez ->
      -- putStrLn $ "@[parseLogicBlock] parseRez: " ++ show parseRez
      pure . Right $ LogicBlock parseRez

{-
  - create a root node that is the global context, push on stack, then for each pBlock:
    - if it is logic:
      - create an AST node from the block's content,
        - if it is ending with a block-start, push the node on the stack
        - if it is ending with a block-end, pop the node from the stack
        - otherwise, add as child of current top node of stack. 
    - if it is a Verbatim:
      - create an AST node that is a 'call spit function' with the pBlock address in the constant region,
-}

astBlocksToTree :: [BlockAst] -> Either GenError NodeAst
astBlocksToTree aBlocks =
  let
    rootNode = AstLogic $ StmtAst (SeqST []) []
    tree = foldl treeMaker (rootNode, [], Right ()) aBlocks
  in
    case tree of
      (top, _, Right _) -> Right top
      (_, _, Left err) -> Left err
  where
    treeMaker :: (NodeAst, [NodeAst], Either GenError ()) -> BlockAst -> (NodeAst, [NodeAst], Either GenError ())
    treeMaker (topStack, stack, result) aBlock =
      case aBlock of
        VerbatimBlock vText ->
          let
            (AstLogic topStmt) = topStack
            updTop = AstLogic $ topStmt { children = topStmt.children <> [ CloneText vText ] }
          in
          (updTop, stack, result)
        LogicBlock stmtFd ->
          case stmtFd of
            IfElseNilSS cond args ->
              let
                newRoot = AstLogic $ StmtAst stmtFd []
              in
              (newRoot, topStack : stack, result)

            ElseIfShortST isElse cond args ->
              let
                newRoot = AstLogic $ StmtAst stmtFd []
                (ncTop, ncStack, ncResult) =
                  if isElse then
                    -- make sure the top of stack is a else-if stmt, ie we close the previous and start a new one.
                    -- Any other block should have been closed with a matching end-block.
                    case topStack of
                      AstLogic {}  ->
                        case stack of
                          [] ->
                            (topStack, stack, Left $ SimpleMsg "@[treeMaker] ran out of stack for else-if stmt!")
                          parent : rest ->
                            let
                              (AstLogic curParent) = topStack
                              updParent = AstLogic $ curParent { children = curParent.children <> [ topStack ] }
                            in
                            (updParent, rest, result)
                      _ -> (topStack, stack, Left $ SimpleMsg "@[treeMaker] else-if stmt not matching a previous if/else-if block!")
                  else
                    (topStack, stack, result)
              in
              case ncResult of
                Left err -> (ncTop, ncStack, Left err)
                Right _ -> (newRoot, ncTop : ncStack, ncResult)

            SeqST subStmts ->
              let
                subTree = astBlocksToTree (map LogicBlock subStmts)
              in
                case subTree of
                  Left err ->
                    (topStack, stack, Left err)
                  Right subTreeRoot ->
                    let
                      (AstLogic curTop) = topStack
                      updTop = AstLogic $ curTop { children = curTop.children <> [ subTreeRoot ] }
                    in
                    (updTop, stack, result)

            BlockEndST -> case stack of
              [] ->
                (topStack, stack, Left . SimpleMsg $ "@[treeMaker] ran out of stack for end-block!" <> pack (show topStack))
              parent : rest ->
                let
                  (AstLogic curParent) = parent
                  updParent = AstLogic $ curParent { children = curParent.children <> [ topStack ] }
                in
                (updParent, rest, result)

            _ ->
              let
                (AstLogic curTop) = topStack
                updTop = AstLogic $ curTop { children = curTop.children <> [ AstLogic $ StmtAst stmtFd [] ] }
              in
              (updTop, stack, result)


displayError :: (Int, Int) -> M.ParseErrorBundle Text Void -> Text
displayError lOffset err =
  let
    nErr = adjustLineOffset lOffset err
  in
  pack $ M.errorBundlePretty nErr


adjustLineOffset :: (Int, Int) -> M.ParseErrorBundle s e -> M.ParseErrorBundle s e
adjustLineOffset (rowOffset, colOffset) peBundle =
  let
    nSourceLine = M.unPos peBundle.bundlePosState.pstateSourcePos.sourceLine + rowOffset
    nSourceColumn = M.unPos peBundle.bundlePosState.pstateSourcePos.sourceColumn + colOffset
    nSourcePos = peBundle.bundlePosState.pstateSourcePos { M.sourceLine = M.mkPos nSourceLine, M.sourceColumn = M.mkPos nSourceColumn }
    nPosState = peBundle.bundlePosState { M.pstateSourcePos = nSourcePos }
  in
  peBundle { M.bundlePosState = nPosState }


parseVerbatimBlock :: BS.ByteString -> Either GenError BlockAst
parseVerbatimBlock vBlock = Right $ VerbatimBlock vBlock


run :: String -> Text -> Either (M.ParseErrorBundle Text Void) StatementFd
run = M.parse fuddleStmt

runTest :: Text -> IO ()
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
  oDbg "stmt" $ asum [
      oDbg "bind-stmt" $ M.try bindStmt      -- <qual-ident> = <expr>
      , oDbg "val-stmt" valueStmt     -- <expr>
      , oDbg "short-stmt" shortStmt     -- @? <expr> @[
      , oDbg "blck-stmt" blockEnd      -- @]
      , oDbg "elif-stmt" elseIfStmt    -- else if <expr> @[
      , oDbg "imp-stmt" importStmt    -- import [qualified] <qual-ident> [ as <alias> ]
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
  args <- optional (symbol "\\" *> some identifier)
  pure $ ElseIfShortST (isJust a) cond args

shortStmt :: Parser StatementFd
shortStmt =
  ifElseNilSS

ifElseNilSS :: Parser StatementFd
ifElseNilSS = do
  pReservedWord "@?"
  cond <- expressionFd
  symbol "@["
  args <- optional (symbol "\\" *> some identifier)
  pure $ IfElseNilSS cond args

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
  a <- oDbg "bind-ident" qualifiedIdent
  b <- oDbg "bind-args" $ many qualifiedIdent
  symbol "="
  BindOneST (a, b) <$> expressionFd


valueStmt :: Parser StatementFd
valueStmt = ExpressionST <$> expressionFd


expressionFd =
  oDbg "expr" $ asum [
      oDbg "pars-expr" parensExpr
      , oDbg "oper-expr" $ M.try operatorExpr
      , oDbg "redu-expr" reductionExpr
      , oDbg "lit-expr" literalExpr
      , oDbg "array-expr" arrayExpr
    ]

nonOperExpr :: Parser Expression
nonOperExpr =
  oDbg "no-expr" $ asum [
    oDbg "pars-no-expr" parensExpr
    , oDbg "redu-no-expr" reductionExpr
    , oDbg "lit-no-expr" literalExpr
    , oDbg "array-no-expr" arrayExpr
  ]


parensExpr :: Parser Expression
parensExpr = do
    symbol "("
    a <- oDbg "expr-pars" expressionFd
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
      oDbg "arit-lit" $ ArithValue <$> integer
      , oDbg "bool-lit" boolValue
      , oDbg "char-lit" $ CharValue <$> M.between (M.char '\'') (M.char '\'') ML.charLiteral <* M.space
      , oDbg "str-lit" $ StringValue <$> stringLiteral
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
operatorExpr = oDbg "operatorExpr" $ CE.makeExprParser nonOperExpr precOperators  -- expressionFd

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
  a <- oDbg "redu-ident" qualifiedIdent
  b <- oDbg "redu-args" $ M.optional $ M.try (many expressionFd)
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
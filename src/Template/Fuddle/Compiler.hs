{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module Template.Fuddle.Compiler where

import Control.Monad (foldM)
import Data.Text (Text)
import qualified Data.Vector as Vc
import qualified Data.Map as Mp

import Conclusion (GenError (..))
import Template.Fuddle.BAst


type VMCode = Vc.Vector Int

data FunctionDef = FunctionDef {
    funcName :: Text
    , funcArgs :: [ Text ]
    , funcBody :: VMCode
  }

data CompContext = CompContext {
    idents :: Mp.Map Text Int
    , imports :: [ String ]
    , constants :: Mp.Map Int Text
    , localVars :: [ Mp.Map Int Text ]
    , functions :: Mp.Map Text FunctionDef
  }

defaultContext = CompContext Mp.empty [] Mp.empty [] Mp.empty


type CompileResult = Either GenError CompContext

{-
    - Once the AST node tree is created:
      - compile the AST node tree into a VM code,
      - add the VM code to the global context.
      - consolidate constants, local vars, function definitions.
-}

compileAstTree :: NodeAst -> Either GenError VMCode
compileAstTree nTree =
  -- putStrLn $ "@[compileAst] ast: " ++ show ast
  let
    rez = compileNode defaultContext nTree
  in
  Right $ Vc.fromList [ 1, 2, 3 ]
  where
    compileNode :: CompContext -> NodeAst -> CompileResult
    compileNode context node =
      case node of
        AstLogic (StmtAst stmt children) ->
          let
            rez = compileStmt context stmt
          in
          case rez of
            Left err -> Left err
            Right newCtxt ->
              if null children then
                Right newCtxt
              else
                case foldM compileNode newCtxt children of
                  Left err -> Left err
                  Right childrenCtxt -> Right childrenCtxt
        CloneText someText -> Right context


compileStmt :: CompContext -> StatementFd -> CompileResult
compileStmt ctx ast =
  case ast of
    SeqST stmts -> foldM compileStmt ctx stmts
    ElseIfShortST _ _ _ -> Right ctx
    BlockEndST -> Right ctx
    IfElseNilSS _ _ -> Right ctx
    ImportST _ _ _ -> Right ctx
    BindOneST _ _ -> Right ctx
    LetST _ _ -> Right ctx
    ExpressionST expr -> compileExpr ctx expr


{- Expressions: -}
compileExpr :: CompContext -> Expression -> CompileResult
compileExpr ctx expr =
  case expr of
    LiteralExpr literal -> compileLiteral ctx literal
    ParenExpr expr -> compileExpr ctx expr
    ArrayExpr exprArray -> compileExprArray ctx exprArray
    UnaryExpr oper expr -> compileUniOper ctx oper expr
    BinOpExpr oper exprA exprB -> compileBinOper ctx oper exprA exprB
    ReductionExpr qualIdent exprArray -> compileReduction ctx qualIdent exprArray


compileLiteral :: CompContext -> LiteralValue -> CompileResult
compileLiteral ctx lit =
  case lit of
    ArithValue lit -> compileArith ctx lit
    BoolValue lit -> compileBool ctx lit
    CharValue lit -> compileChar ctx lit
    StringValue lit -> compileString ctx lit
    ArrayValue litArray -> compileLitArray ctx litArray

{- Literals: -}
compileArith :: CompContext -> Int -> CompileResult
compileArith ctx lit = Right ctx

compileBool :: CompContext -> Bool -> CompileResult
compileBool ctx lit = Right ctx

compileChar :: CompContext -> Char -> CompileResult
compileChar ctx lit = Right ctx

compileString :: CompContext -> Text -> CompileResult
compileString ctx lit = Right ctx

compileLitArray :: CompContext -> [ LiteralValue ] -> CompileResult
compileLitArray ctx lit = Right ctx

{- Array: -}
compileExprArray :: CompContext -> [ Expression ] -> CompileResult
compileExprArray ctx exprArray = Right ctx

{- Operations: -}
compileUniOper :: CompContext -> UnaryOp -> Expression -> CompileResult
compileUniOper ctx oper exprA = Right ctx

compileBinOper :: CompContext -> BinaryOp -> Expression -> Expression -> CompileResult
compileBinOper ctx oper exprA exprB = Right ctx

{- Reduction: -}
compileReduction :: CompContext -> QualifiedIdent -> [ Expression ] -> CompileResult
compileReduction ctx qualIdent exprArray = Right ctx


module RunTime.Interpreter.Engine
where

import RunTime.Interpreter.Context (VmContext (..), StatusVM (..), defaultVM)
import RunTime.Interpreter.OpCodes (OpCode (..))

data VmError =
  UnimplementedOpCode OpCode


data SourceText = SourceText
data Program = Program {
    opcodes :: [ OpCode ]
  }
data ExecResult = ExecResult VmContext
data Heap = Heap
data Stack = Stack

fakeExec =
  execCode defaultVM SourceText (Program [])


execCode :: VmContext -> SourceText -> Program -> IO (Either String ExecResult)
execCode ctxt srcTxt program =
  let
    stack = newStack
    heap = newHeap
  in do
  putStrLn "@[execCode] starting..."
  newCtxt <- execCodeWithStack heap stack ctxt srcTxt program
  case newCtxt of
    Left errMsg -> pure $ Left errMsg
    Right aCtxt -> pure . Right $ ExecResult aCtxt
  where
    newStack = Stack
    newHeap = Heap


execCodeWithStack :: Heap -> Stack -> VmContext -> SourceText -> Program -> IO (Either String VmContext)
execCodeWithStack heap stack ctxt srcTxt program =
  case ctxt.status of
    Running -> do
      eiRez <- doVM heap stack (ctxt, srcTxt) program.opcodes
      case eiRez of
        Right (nHeap, nStack, nCtxt) ->
          -- TODO: create a new context based on the result of running the VM...
          pure $ Right ctxt
        Left errMsg -> pure $ Left errMsg


doVM :: Heap -> Stack -> (VmContext, SourceText) -> [ OpCode ] -> IO (Either String (Heap, Stack, VmContext))
doVM heap stack (ctxt, srcTxt) opcodes =
  case opcodes of
    [] -> pure $ Right (heap, stack, ctxt)
    anOp : rest -> do
      eiRez <- doOpcode heap stack anOp
      case eiRez of
        Right (nHeap, nStack) ->
          let
            -- TODO: modify the context based on new heap / stack.
            nCtxt = ctxt
          in doVM nHeap nStack (nCtxt, srcTxt) rest
        Left err -> pure . Left $ analyzeVmError err heap stack


analyzeVmError err heap stack =
  case err of
    UnimplementedOpCode op -> "Unimplemented opcode " <> show op <> "."


doOpcode :: Heap -> Stack -> OpCode -> IO (Either VmError (Heap, Stack))
doOpcode heap stack opcode =
  case opcode of
    NOP -> pure . Left $ UnimplementedOpCode opcode
    -- Variable access:
    SET_VAR varID -> pure . Left $ UnimplementedOpCode opcode
    GET_VAR varID -> pure . Left $ UnimplementedOpCode opcode
    GET_FIELD fieldID -> pure . Left $ UnimplementedOpCode opcode
    SET_FIELD fieldID -> pure . Left $ UnimplementedOpCode opcode
    -- Register access:
    SET_REG_BOOL regID boolVal -> pure . Left $ UnimplementedOpCode opcode
    SET_REG_CHAR regID charVal -> pure . Left $ UnimplementedOpCode opcode
    SET_REG_INT regID intVal -> pure . Left $ UnimplementedOpCode opcode
    SET_REG_FLOAT regID floatVal -> pure . Left $ UnimplementedOpCode opcode
    SET_REG_DOUBLE regID doubleVal -> pure . Left $ UnimplementedOpCode opcode
    -- Comparisons:
    CMP_BOOL regA regB -> pure . Left $ UnimplementedOpCode opcode
    CMP_CHAR regA regB -> pure . Left $ UnimplementedOpCode opcode
    CMP_INT regA regB -> pure . Left $ UnimplementedOpCode opcode
    CMP_FLOAT regA regB -> pure . Left $ UnimplementedOpCode opcode
    CMP_DOUBLE regA regB -> pure . Left $ UnimplementedOpCode opcode
    -- PC ops:
    JUMP_ABS pcPtr -> pure . Left $ UnimplementedOpCode opcode
    JUMP_REL intVal -> pure . Left $ UnimplementedOpCode opcode
    JUMP_INDEX regID -> pure . Left $ UnimplementedOpCode opcode
    -- Stack Access:
    PUSH_BOOL regID -> pure . Left $ UnimplementedOpCode opcode
    PUSH_BOOL_IMM boolVal -> pure . Left $ UnimplementedOpCode opcode
    PUSH_CHAR regID -> pure . Left $ UnimplementedOpCode opcode
    PUSH_CHAR_IMM charVal -> pure . Left $ UnimplementedOpCode opcode
    PUSH_INT regID -> pure . Left $ UnimplementedOpCode opcode
    PUSH_INT_IMM intVal -> pure . Left $ UnimplementedOpCode opcode
    PUSH_FLOAT regID -> pure . Left $ UnimplementedOpCode opcode
    PUSH_FLOAT_IMM floatVal -> pure . Left $ UnimplementedOpCode opcode
    PUSH_DOUBLE regID -> pure . Left $ UnimplementedOpCode opcode
    PUSH_DOUBLE_IMM doubleVal -> pure . Left $ UnimplementedOpCode opcode
    POP_BOOL regID -> pure . Left $ UnimplementedOpCode opcode
    POP_CHAR regID -> pure . Left $ UnimplementedOpCode opcode
    POP_INT regID -> pure . Left $ UnimplementedOpCode opcode
    POP_FLOAT regID -> pure . Left $ UnimplementedOpCode opcode
    POP_DOUBLE regID -> pure . Left $ UnimplementedOpCode opcode
    POP_BOOL_VOID -> pure . Left $ UnimplementedOpCode opcode
    POP_CHAR_VOID -> pure . Left $ UnimplementedOpCode opcode
    POP_INT_VOID -> pure . Left $ UnimplementedOpCode opcode
    POP_FLOAT_VOID -> pure . Left $ UnimplementedOpCode opcode
    POP_DOUBLE_VOID -> pure . Left $ UnimplementedOpCode opcode
    -- Invoke:
    EVAL -> pure . Left $ UnimplementedOpCode opcode
    -- Heap management (save new heap ID to register):
    ALLOC_ABS intVal regID -> pure . Left $ UnimplementedOpCode opcode
    ALLOC_REL regA regB -> pure . Left $ UnimplementedOpCode opcode

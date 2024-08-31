module RunTime.Interpreter.Engine
where

import Data.Text (Text, unpack)
import Data.Int (Int32)
import qualified Data.Vector as Vc

import RunTime.Interpreter.Context
import RunTime.Interpreter.OpCodes
import Data.List (find)
import Data.Maybe (isNothing, fromJust)

data VmError =
  UnimplementedOpCode OpCode
  | UnknownOpcode OpCode
  | MissingArgForOpcode OpCode
  | UnknownFunction Int32
  deriving Show


newtype ExecResult = ExecResult VmContext
data Heap = Heap
data Stack = Stack


execModule :: VMModule -> IO (Either String ExecResult)
execModule vmModule =
  let
    ctxt = defaultVM
    stack = newStack
    heap = newHeap
    entryPoint = "$main"
  in do
  putStrLn "@[execModule] starting..."
  newCtxt <- execCodeWithStack heap stack ctxt vmModule entryPoint
  putStrLn $ "@[execModule] done, status: " <> show newCtxt
  case newCtxt of
    Left errMsg -> pure $ Left errMsg
    Right aCtxt -> pure . Right $ ExecResult aCtxt
  where
    newStack = Stack
    newHeap = Heap


execCodeWithStack :: Heap -> Stack -> VmContext -> VMModule -> Text -> IO (Either String VmContext)
execCodeWithStack heap stack ctxt vmModule entryPoint =
  case ctxt.status of
    Running ->
      let
        mbFunction = find (\f -> f.name ==  entryPoint) vmModule.functions
      in
      case mbFunction of
        Nothing -> pure . Left $ "@[execCodeWithStack] function " <> unpack entryPoint <> " not found in module."
        Just fctDef -> do
          eiRez <- 
            case fctDef.body of
              NativeCode ->
                pure . Left $ "@[execCodeWithStack] native functions are not yet supported."
              ByteCode _ ->
                doVM heap stack (ctxt, vmModule) fctDef 0
          case eiRez of
            Right (nHeap, nStack, nCtxt) ->
              -- TODO: create a new context based on the result of running the VM...
              pure $ Right ctxt
            Left errMsg -> pure $ Left errMsg
    Init ->
      execCodeWithStack heap stack (ctxt { status = Running }) vmModule entryPoint
    Halted -> pure $ Right ctxt


doVM :: Heap -> Stack -> (VmContext, VMModule) -> FunctionDef -> Int -> IO (Either String (Heap, Stack, VmContext))
doVM heap stack (ctxt, vmModule) funDef pcCounter =
  case funDef.body of
    NativeCode -> pure . Left $ "@[doVM] trying to run a native function."
    ByteCode opcodes ->
      case opcodes Vc.!? pcCounter of
        Nothing -> pure $ Right (heap, stack, ctxt)
        Just anOp ->
          let
            opLength = 1 + opParCount (toEnum $ fromIntegral anOp)           
            opWithArgs = if opLength == 1 then Vc.singleton anOp else Vc.slice pcCounter opLength opcodes
          in do
          eiRez <- doOpcode vmModule.constants heap stack opWithArgs
          case eiRez of
            Right (nHeap, nStack, isRunning) ->
              let
                -- TODO: modify the context based on new heap / stack.
                nCtxt = ctxt
              in
              if isRunning then
                doVM nHeap nStack (nCtxt, vmModule) funDef (pcCounter + opLength)
              else
                pure $ Right (nHeap, nStack, nCtxt)
            Left err -> pure . Left $ analyzeVmError err heap stack


analyzeVmError err heap stack =
  case err of
    UnimplementedOpCode op -> "Unimplemented opcode " <> show op <> "."
    UnknownOpcode op -> "Unknown opcode " <> show op <> "."
    MissingArgForOpcode op -> "Missing argument for opcode " <> show op <> "."
    UnknownFunction fctID -> "Unknown function " <> show fctID <> "."


doOpcode :: Vc.Vector ConstantValue -> Heap -> Stack -> Vc.Vector Int32 -> IO (Either VmError (Heap, Stack, Bool))
doOpcode constants heap stack opWithArgs =
  let
    opcode = toEnum . fromIntegral $ Vc.head opWithArgs
  in
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
    PUSH_INT_IMM _ ->
      case opWithArgs Vc.!? 1 of
        Nothing ->
          pure . Left $ MissingArgForOpcode opcode
        Just anInt ->
          -- push anInt to stack.
          pure $ Right (heap, stack, True)
    PUSH_FLOAT regID -> pure . Left $ UnimplementedOpCode opcode
    PUSH_FLOAT_IMM floatVal -> pure . Left $ UnimplementedOpCode opcode
    PUSH_DOUBLE regID -> pure . Left $ UnimplementedOpCode opcode
    PUSH_DOUBLE_IMM doubleVal -> pure . Left $ UnimplementedOpCode opcode
    PUSH_CONST_IMM _ -> pure . Left $ UnimplementedOpCode opcode
    PUSH_CONST _ ->
      case opWithArgs Vc.!? 1 of
        Nothing ->
          pure . Left $ MissingArgForOpcode opcode
        Just constID ->
          let
            constant = constants Vc.!? fromIntegral constID
          in do
          putStrLn $ "@[doOpcode] push constant " <> show constant
          -- push constant from heap to stack.
          pure $ Right (heap, stack, True)
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
    REDUCE _ _->
      -- reduce fctID with arity from stack.
      let
        (fctID, arity) = (opWithArgs Vc.!? 1, opWithArgs Vc.!? 2)
      in
      if isNothing fctID || isNothing arity then
        pure . Left $ MissingArgForOpcode opcode
      else do
        case fromJust fctID of
          0 -> do
            putStrLn $ "@[doOpcode] spit."
            -- spit: pop last element from stack, sent it to output stream.
            pure $ Right (heap, stack, True)
          n -> do
            putStrLn $ "@[doOpcode] unknown fct:" <> show n <> ", arity: " <> show arity <> "."
            pure $ Right (heap, stack, True)
    -- Heap management (save new heap ID to register):
    ALLOC_ABS intVal regID -> pure . Left $ UnimplementedOpCode opcode
    ALLOC_REL regA regB -> pure . Left $ UnimplementedOpCode opcode
    HALT -> pure $ Right (heap, stack, False)
    _ -> pure . Left $ UnimplementedOpCode opcode

module RunTime.Interpreter.Engine
where

import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import RunTime.Interpreter.Context
import RunTime.Interpreter.OpCodes
import Data.List (find)
import Data.Maybe (isNothing, fromJust)


data VmError =
  UnimplementedOpCode OpCode
  | UnknownOpcode OpCode
  | MissingArgForOpcode OpCode
  | UnknownFunction Int32
  | StackError String
  deriving Show


newtype ExecResult = ExecResult VmContext


execModule :: VMModule -> IO (Either String ExecResult)
execModule vmModule =
  let
    fakeFrame = Frame { stack = [], heap = V.empty
        , pc = 0, flags = NoFlag
        , function = FunctionDef { name = "$fake", args = [], heapDef = 0, body = NativeCode }
        , returnValue = (Nothing, Nothing)
      }
    ctxt = VmContext { status = Init, frame = fakeFrame, frameStack = [], outStream = BS.empty, modules = V.singleton vmModule, constants = vmModule.constants }
    entryPoint = "$main"
    moduleID = 0
    -- TODO: have a proper selection for modules.
    mbFctID = V.findIndex (\f -> f.name ==  entryPoint) vmModule.functions
  in
  case mbFctID of
    Nothing -> pure . Left $ "@[execModule] function " <> unpack entryPoint <> " not found in module."
    Just fctID -> do
      putStrLn "@[execModule] starting..."
      eiCtxt <- execCodeOnFunctionID ctxt (moduleID, fctID)
      putStrLn $ "@[execModule] done, status: " <> either (const "error") (\ctxt -> show ctxt.status) eiCtxt
      case eiCtxt of
        Left errMsg -> pure $ Left errMsg
        Right aCtxt -> pure . Right $ ExecResult aCtxt


execCodeOnFunctionID :: VmContext -> (Int, Int) -> IO (Either String VmContext)
execCodeOnFunctionID ctxt (moduleID, fctID) =
  let
    -- TODO: have a proper selection for modules.
    mbModule = ctxt.modules V.!? moduleID
    eiFunction = case mbModule of
      Nothing -> Left $ "@[execCodeOnFunctionID] module id " <> show moduleID <> " not found."
      Just aModule ->
        case aModule.functions V.!? fctID of
          Nothing -> Left $ "@[execCodeOnFunctionID] function id " <> show fctID <> " not found in module."
          Just fctDef -> Right fctDef
  in
  case eiFunction of
    Left err -> pure $ Left err
    Right fctDef ->
      case ctxt.status of
        Running -> do
          eiRez <-
            case fctDef.body of
              NativeCode ->
                pure . Left $ "@[execCodeWithStack] native functions are not yet supported."
              ByteCode _ ->
                doVM ctxt
          case eiRez of
            Right nCtxt ->
              -- TODO: create a new context based on the result of running the VM...
              pure $ Right nCtxt
            Left errMsg -> pure $ Left errMsg
        Init ->
          let
            -- TODO: pass the global variables as heap values.
            newFrame = Frame { stack = [], heap = V.empty
                , pc = 0, flags = NoFlag
                , function = fctDef, returnValue = (Nothing, Nothing)
              }
            newContext = ctxt { status = Running, frame = newFrame, frameStack = ctxt.frame : ctxt.frameStack }
          in
          doVM newContext
        Halted -> pure $ Right ctxt


doVM :: VmContext -> IO (Either String VmContext)
doVM context =
  case context.frame.function.body of
    NativeCode -> pure . Left $ "@[doVM] trying to run a native function."
    ByteCode opcodes ->
      doByteCodeVM context context.frame opcodes
  where
  doByteCodeVM :: VmContext -> Frame -> V.Vector Int32 -> IO (Either String VmContext)
  doByteCodeVM inCtxt frame opcodes =
    case opcodes V.!? frame.pc of
      Nothing -> pure $ Right inCtxt { status = Halted }
      Just anOp ->
        let
          opLength = 1 + opParCount (toEnum $ fromIntegral anOp)           
          opWithArgs = if opLength == 1 then V.singleton anOp else V.slice frame.pc opLength opcodes
        in do
        eiRez <- doOpcode inCtxt frame opWithArgs
        case eiRez of
          Right (retCtxt, retFrame, isRunning) -> do
            let
              -- TODO: find a better way to manage the frame/context updates.
              newFrame = retFrame { pc = frame.pc + opLength }
              nCtxt = retCtxt { frame = frame }
            -- in
            if isRunning then
              doByteCodeVM nCtxt newFrame opcodes
            else
              pure $ Right nCtxt { status = Halted }
          Left err -> pure . Left $ analyzeVmError err heap stack


analyzeVmError :: VmError -> p1 -> p2 -> String
analyzeVmError err heap stack =
  case err of
    UnimplementedOpCode op -> "Unimplemented opcode " <> show op <> "."
    UnknownOpcode op -> "Unknown opcode " <> show op <> "."
    MissingArgForOpcode op -> "Missing argument for opcode " <> show op <> "."
    UnknownFunction fctID -> "Unknown function " <> show fctID <> "."
    StackError msg -> "Stack error: " <> msg


doOpcode :: VmContext -> Frame -> V.Vector Int32 -> IO (Either VmError (VmContext, Frame, Bool))
doOpcode context frame opWithArgs =
  let
    opcode = toEnum . fromIntegral $ V.head opWithArgs
  in
  case opcode of
    NOP -> pure . Left $ UnimplementedOpCode opcode

    -- Comparisons:
    CMP_BOOL_IMM ->
      -- pop value on stack, should be boolean, then compare with immediate value.
      let
        eiBool = popBool context frame
      in
      case eiBool of
        Left (StackError aMsg) -> pure . Left $ StackError ("In CMP_BOOL_IMM, " <> aMsg)
        Right (newFrame, aBool) ->
          pure $ Right (context, newFrame { flags = if aBool then TrueFlag else FalseFlag}, True)
    -- PC ops:
    JUMP_TRUE _ ->
      -- jump to immediate value if true flag is set.
      if frame.flags == TrueFlag then
        case opWithArgs V.!? 1 of
          Nothing ->
            pure . Left $ MissingArgForOpcode opcode
          Just newPC ->
            pure $ Right (context, frame { pc = frame.pc + fromIntegral newPC }, True)
      else
        pure $ Right (context, frame, True)
    JUMP_ABS _ ->
      -- jump to immediate value.
      case opWithArgs V.!? 1 of
        Nothing ->
          pure . Left $ MissingArgForOpcode opcode
        Just newPC ->
          pure $ Right (context, frame { pc = frame.pc + fromIntegral newPC }, True)
  
    -- Stack access:
    PUSH_INT_IMM _ ->
      case opWithArgs V.!? 1 of
        Nothing ->
          pure . Left $ MissingArgForOpcode opcode
        Just anInt ->
          -- push anInt to stack.
          pure $ Right (context, frame { stack = (IntSV, anInt) : frame.stack }, True)

    PUSH_CONST _ ->
      case opWithArgs V.!? 1 of
        Nothing ->
          pure . Left $ MissingArgForOpcode opcode
        Just constID ->
          let
            constant = context.constants V.!? fromIntegral constID
            mbStackValue = case constant of
              Nothing -> Nothing
              Just aConst -> Just (ConstantRefSV, constID)
          in
          case mbStackValue of
            Nothing -> pure . Left . StackError $ "Constant ID " <> show constID <> " not found."
            Just aValue -> do
              -- putStrLn $ "@[doOpcode] push constant " <> show constant
              -- push constant from constant area to stack.
              pure $ Right (context, frame { stack = aValue : frame.stack }, True)

    -- Invoke:
    REDUCE _ _->
      -- reduce fctID with arity from stack.
      let
        (fctID, arity) = (opWithArgs V.!? 1, opWithArgs V.!? 2)
      in
      if isNothing fctID || isNothing arity then
        pure . Left $ MissingArgForOpcode opcode
      else do
        case fromJust fctID of
          0 ->
            -- spit: pop last element from stack, sent it to output stream.
            let
              eiDerefStr = popString context frame
            in
            case eiDerefStr of
                Left (StackError aMsg) -> pure . Left $ StackError ("In REDUCE fct " <> show fctID <> ", " <> aMsg)
                Right (newFrame, aStr, isQuoted) ->
                  -- putStrLn $ "@[doOpcode] spit: " <> unpack (TE.decodeUtf8 aStr)
                  -- TODO: quote a string value vs a verbatim-block.
                  let
                    newStr = if isQuoted then "\"" <> aStr <> "\"" else aStr
                    newStream = context.outStream <> newStr
                  in
                  pure $ Right (context { outStream = newStream }, newFrame, True)
          n ->
            let
              curModule = context.modules V.! 0
            in
            if V.length curModule.functions > fromIntegral n then
              let
                newFct = curModule.functions V.! fromIntegral n
                newStack = drop (fromIntegral $ fromJust arity) frame.stack
                newFrame = frame { function = newFct, stack = newStack, pc = 0 }
              in
              pure $ Right (context, newFrame, True)
            else
              let
                fakeType = case n of
                  2 -> StringSV   -- jwkDefaultLocation
                  3 -> IntSV      -- serverPortDefault
                  4 -> BoolSV     -- hasWebServer
                  5 -> StringSV   -- appName
                  6 -> StringSV   -- appConfEnvVar
                  _ -> IntSV
                heapPos = fromIntegral $ V.length frame.heap
                (newStack, newHeap) = case fakeType of
                  BoolSV ->
                    ((BoolSV, 1) : frame.stack, frame.heap)
                  IntSV ->
                    ((IntSV, 1) : frame.stack, frame.heap)
                  StringSV ->
                    ((HeapRefSV, heapPos) : frame.stack, frame.heap V.++ V.singleton (StringCte "test-string"))
              in do
              putStrLn $ "@[doOpcode] unknown fct:" <> show n <> ", arity: " <> show arity <> "."
              pure $ Right (context, frame { stack = newStack, heap = newHeap }, True)
    -- Heap management (save new heap ID to register):
    ARR_CONCAT ->
      let
        eiFstStr = popString context frame
        eiSndStr = case eiFstStr of
          Left (StackError aMsg) -> Left (StackError ("In ARR_CONCAT, 2nd value: " <> aMsg))
          Right (fstFrame, fstStr, _) -> case popString context fstFrame of
            Left (StackError aMsg) -> Left (StackError ("In ARR_CONCAT, 2nd value: " <> aMsg))
            Right (sndFrame, sndStr, _) -> Right (sndFrame, (fstStr, sndStr))
      in
      case eiSndStr of
        Left err -> pure $ Left err
        Right (postFrame, (fstStr, sndStr)) ->
          let
            concatStr = sndStr <> fstStr -- operand positions are reversed in the stack.
            heapPos = fromIntegral $ V.length frame.heap
            newHeap = frame.heap V.++ V.singleton (StringCte concatStr)
            newStack = (HeapRefSV, heapPos) : postFrame.stack
          in
          pure $ Right (context, postFrame { stack = newStack, heap = newHeap }, True)

    HALT -> pure $ Right (context { status = Halted }, frame, False)
    GET_FIELD ->
      -- TODO: implement GET_FIELD. For, fake a value.
      pure $ Right (context, frame { stack = (IntSV, 1) : frame.stack }, True)
 
    _ -> pure . Left $ UnimplementedOpCode opcode


popString :: VmContext -> Frame -> Either VmError (Frame, BS.ByteString, Bool)
popString context frame =
  let
    (mbTopValue, newStack) = case frame.stack of
      [] -> (Nothing, [])
      (topValue : rest) -> (Just topValue, rest)
  in
  case mbTopValue of
    Nothing -> Left $ StackError "@[popString] Empty stack."
    Just topValue ->
      case topValue of
        (ConstantRefSV, constID) ->
          let
            constant = context.constants V.!? fromIntegral constID
          in
          case constant of
            Nothing ->
              Left . StackError $ "@[popString] constant ID " <> show constID <> " not found."
            Just aConst ->
              case aConst of
                StringCte aStr -> Right (frame { stack = newStack }, aStr, True)
                VerbatimCte aStr -> Right (frame { stack = newStack }, aStr, False)
                _ ->
                  Left . StackError $ "@[popString] constant ID " <> show constID <> " is not a string."
        (HeapRefSV, heapID) ->
          case frame.heap V.!? fromIntegral heapID of
            Nothing ->
              Left . StackError $ "@[popString] heap ID " <> show heapID <> " not found."
            Just aHeapValue ->
              case aHeapValue of
                StringCte aStr -> Right (frame { stack = newStack }, aStr, True)
                _ ->
                  Left . StackError $ "@[popString] , heap ID " <> show heapID <> " is not a string."
        (IntSV, anInt) ->
          Right (frame { stack = newStack }, TE.encodeUtf8 . pack $ show anInt, False)
        (aType, _) ->
          Left . StackError $ "@[popString] invalid popped value of type " <> show aType <> " for string dereference."


popBool :: VmContext -> Frame -> Either VmError (Frame, Bool)
popBool context frame =
  let
    (mbTopValue, newStack) = case frame.stack of
      [] -> (Nothing, [])
      (topValue : rest) -> (Just topValue, rest)
  in
  case mbTopValue of
    Nothing -> Left $ StackError "@[popBool] Empty stack."
    Just topValue ->
      case topValue of
        (BoolSV, aBool) -> Right (frame { stack = newStack }, aBool /= 0)
        (IntSV, anInt) -> Right (frame { stack = newStack }, anInt /= 0)
        (aType, _) -> Left . StackError $ "@[popBool] invalid popped value of type " <> show aType <> " for boolean dereference."
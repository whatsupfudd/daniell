module RunTime.Compiler.Assembler where

import Data.Int (Int32)
import qualified Data.Map as Mp
import qualified Data.Vector as V

import RunTime.Interpreter.OpCodes
import RunTime.Compiler.Types
import RunTime.Interpreter.Context (ConstantValue (..))
import Control.Monad (foldM)


assemble :: CompFunction -> Either String (V.Vector Int32)
assemble fct =
  case derefLabels fct.opcodes fct.labels of
    Left err -> Left err
    Right i32Labels ->
      V.foldM (\accum opCode ->
            if isLabeledCode opCode then
              -- Haskell syntax: why does this work? accum is a 0-arg function (a var), why is <> working on that and the fromList as
              -- if they are 2 separate args?
              (<>) accum . V.fromList <$> solveLabel opCode i32Labels
            else
            Right $ V.snoc accum (fromIntegral (fromEnum opCode))
          ) V.empty fct.opcodes
  where
  isLabeledCode :: OpCode -> Bool
  isLabeledCode opCode = case opCode of
    JUMP_ABS _ -> True
    JUMP_REL _ -> True
    JUMP_TRUE _ -> True
    JUMP_FALSE _ -> True
    _ -> False
  solveLabel :: OpCode -> Mp.Map Int32 Int32 -> Either String [ Int32 ]
  solveLabel opCode i32Labels =
    let
      jumpI32 = fromIntegral . fromEnum $ opCode
      label = case opCode of
        JUMP_ABS label -> label
        JUMP_REL label -> label
        JUMP_TRUE label -> label
        JUMP_FALSE label -> label
    in
    case label of
      LabelRef anID -> case Mp.lookup anID i32Labels of
        Just pos -> Right [ jumpI32, pos ]
        Nothing -> Left $ "Label " <> show label <> " not found"
      I32Pc pos -> Right [ jumpI32, pos ]


derefLabels :: V.Vector OpCode -> Mp.Map Int32 (Maybe Int32) -> Either String (Mp.Map Int32 Int32)
derefLabels opCodes symbLabels =
  let
    derefedLabels = foldM adjustPositions ([], opCodes, 0) $ Mp.assocs symbLabels
  in
  case derefedLabels of
    Left err -> Left err
    Right (dList, _, _) -> Right . Mp.fromList . reverse $ dList
  where
  adjustPositions :: ([(Int32, Int32)], V.Vector OpCode, Int32) -> (Int32, Maybe Int32) -> Either String ([(Int32, Int32)], V.Vector OpCode, Int32)
  adjustPositions (accum, opCodes, curPos) (label, mbPos) =
    case mbPos of
      Nothing -> Left $ "Label " <> show label <> " not found"
      Just pos ->
        let
          sOrigin = fromIntegral curPos
          sLength = fromIntegral (pos - curPos)
          eiNewPos
            | curPos < pos = Right $ foldl (\aSum opCode -> fromIntegral (opParCount opCode) + aSum) curPos (V.slice sOrigin sLength opCodes)
            | curPos == pos = Right curPos
            | otherwise = Left $ "@[derefLabels] label points before a previously generated label (" <> show label <> ")."
        in
        case eiNewPos of
          Left err -> Left err
          Right newPos -> Right ((label, newPos) : accum, V.drop sLength opCodes, newPos)


convertCompCteToTempl :: CompConstant -> ConstantValue
convertCompCteToTempl (IntC a) = IntCte (fromIntegral a)
convertCompCteToTempl (FloatC a) = FloatCte (realToFrac a)
convertCompCteToTempl (BoolC a) = IntCte (if a then 1 else 0)
convertCompCteToTempl (StringC a) = StringCte a
convertCompCteToTempl (VerbatimC a) = VerbatimCte False a

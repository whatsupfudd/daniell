{-# LANGUAGE OverloadedStrings #-}


import Data.List (intercalate, dropWhileEnd)
type ConvAccum = (Int, [String], [String], [String])


-- Extension of the 'lines' function of Data.List to provide an argument rather than a hard-coded line-break.
split :: Char -> String -> [String]
split _ "" =  []
split blade s =
  cons (case break (== blade) s of
    (l, s') -> (l, case s' of
        []      -> []
        _:rest   -> split blade rest))
  where
    cons ~(h, t) =  h : t


opsToEnum :: String -> String
opsToEnum srcText =
  let
    ops = map (split ' ' . dropWhile (' ' ==) . dropWhileEnd (\c -> c == ' ' || c == '\n' )) (split '|' srcText)
    (opID, fromEnums, toEnums, parCounts) = foldl convertOp (0, [], [], []) ops
  in
  "instance Enum OpCode where\n  fromEnum :: OpCode -> Int\n"
    <> (concat . reverse $ fromEnums)
    <> "  fromEnum a = error $ \"fromEnum: bad argument\" <> show a\n\n  toEnum :: Int -> OpCode\n"
    <> (concat . reverse $ toEnums)
    <> "  toEnum _ = error \"toEnum: bad argument\"\n\n"
    <> "opParCount :: OpCode -> Int\n"
    <> (concat . reverse $ parCounts)
  where
  convertOp :: ConvAccum -> [String] -> ConvAccum
  convertOp (opID, fromEnums, toEnums, parCnt) anOp =
    let
      opSize = length anOp - 1
      opName = head anOp
      opFormat = if opSize == 0 then
          opName
        else
          "(" <> opName <> " " <> intercalate " " (replicate opSize "_") <> ")"
      newFrom = "  fromEnum " <> opFormat <> " = " <> show opID <> "\n"
      newTo = "  toEnum " <> show opID <> " = " <> opName <> concat (replicate opSize " undefined") <> "\n"
      newParCnt = "opParCount " <> opFormat <> " = " <> show opSize <> "\n"

    in
      (opID + 1
        , newFrom : fromEnums
        , newTo : toEnums
        , newParCnt : parCnt
        )


opsToInstr :: String -> String
opsToInstr srcText =
  let
    ops = map (split ' ' . dropWhile (' ' ==) . dropWhileEnd (\c -> c == ' ' || c == '\n' )) (split '|' srcText)
    (opID, toInstr, _, _) = foldl convertOp (0, [], [], []) ops
  in
  "toInstr :: OpCode -> [Int32]\n"
    <> (concat . reverse $ toInstr)
    <> "  toInstr a = error $ \"fromEnum: bad argument\" <> show a\n\n"
  where
  convertOp :: ConvAccum -> [String] -> ConvAccum
  convertOp (opID, fromList, toList, parList) anOp =
    let
      opSize = length anOp - 1
      opName = head anOp
      buildValList :: String -> Int -> String
      buildValList sep 0 = ""
      buildValList sep n = foldl (\accum i -> if accum == "" then "a" <> show i else accum <> sep <> "a" <> show i) "" [1..opSize]
      (argList, instrList) = if opSize == 0 then
          (opName, "")
        else
          ( "(" <> opName <> " " <> (buildValList " " opSize) <> ")"
            , ", " <> (buildValList ", " opSize) <> ""
          )
      newInstr = "toInstr " <> argList <> " = [" <> show opID <> instrList <> "]\n"
    in
      (opID + 1
        , newInstr : fromList
        , toList
        , parList
        )

main = do
  inFile <- readFile "opcodes.txt"
  let
    result = opsToEnum inFile <> "\n\n" <> opsToInstr inFile
  writeFile "enums.txt" result


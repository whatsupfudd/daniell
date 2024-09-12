module ProjectDefinition.Paraml where

import Control.Monad (foldM)

import qualified Data.HashMap.Strict as Hm
import qualified Data.List.NonEmpty as Nem
import qualified Data.Map as Mp
import Data.Text (Text, unpack, pack)

import qualified Toml as Tm
import qualified Toml.Type.PrefixTree as Tpt

import ProjectDefinition.Types (DictEntry (..))

tomlToDict :: Tm.TOML -> Either String (Mp.Map Text DictEntry)
tomlToDict tomlBlock =
  let
    eiPairs = foldM (\accum (fKey Tm.:|| rKey, value)-> case tomlToValue value of
          Left err -> Left $ "@[tomlToDict] pair error: " <> show err
          Right aVal -> Right $ Mp.insert fKey.unPiece aVal accum
        ) Mp.empty $ Hm.toList tomlBlock.tomlPairs
    eiTables =
      case eiPairs of
        Left _ -> eiPairs
        Right pairs -> foldM (\accum (fKey Tm.:|| rKey, value) -> case tomlToDict value of
              Left err -> Left $ "@[tomlToDict] table error: " <> err
              Right aVal -> Right $ Mp.insert fKey.unPiece (DictDV aVal) accum
          ) pairs (Tpt.toList tomlBlock.tomlTables)
  in
  case eiTables of
    Left err -> eiTables
    Right tables -> foldM (\accum (fKey Tm.:|| rKey, arrayVal) ->
        let
          mbExistingArray = Mp.lookup fKey.unPiece accum
          newValues = mapM tomlToDict (Nem.toList arrayVal)
        in
        case newValues of
          Left err -> Left $ "@[tomlToDict] table array conversion error: " <> err
          Right anArray ->
            case mbExistingArray of
              Nothing -> Right $ Mp.insert fKey.unPiece (ListDV (map DictDV anArray)) accum
              Just (ListDV existingArray) -> Right $ Mp.insert fKey.unPiece (ListDV $ existingArray <> map DictDV anArray) accum
              _ -> Left $ "@[tomlToDict] array error, key " <> unpack fKey.unPiece <> " already exists for a non-array value."
      ) tables (Hm.toList tomlBlock.tomlTableArrays)
  where
  tomlToValue :: Tm.AnyValue -> Either Tm.MatchError DictEntry
  tomlToValue (Tm.AnyValue v) =
    case Tm.matchBool v of
      Right b -> Right $ BoolDV b
      Left _ -> case Tm.matchText v of
        Right s -> Right $ StringDV s
        Left _ -> case Tm.matchInteger v of
          Right i -> Right $ IntDV i
          Left _ -> case Tm.matchDouble v of
            Right d -> Right $ DoubleDV d
            Left _ -> case Tm.matchArray tomlToValue v of
                Right aArray -> Right $ ListDV aArray
                Left _ -> Tm.mkMatchError Tm.TText (Tm.Text $ "@[tomlToValue] unknown value type: " <> pack (show v))


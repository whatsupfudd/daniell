module Scaffold.Archive where

import qualified Data.ByteString.Lazy as Lbs
import Data.Either (lefts, rights)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.BZip as BZip

-- Example: List contents of a .tar.bz2 archive
listTarBz2Contents :: FilePath -> IO ()
listTarBz2Contents archivePath = do
    compressedData <- Lbs.readFile archivePath
    let
      decompressedData = BZip.decompress compressedData
      entries = Tar.read decompressedData
      eiFilePaths = getEntries (Right []) entries
    case eiFilePaths of
      Right filePaths -> mapM_ (putStrLn . showEntry) filePaths
      someErrs -> putStrLn $ "Errors: " <> show someErrs

getEntries :: Either [String] [Tar.Entry] -> Tar.Entries Tar.FormatError -> Either [String] [Tar.Entry]
getEntries accum entries =
  case accum of
    Left errs ->
      case entries of
        Tar.Fail err -> Left (errs <> [show err])
        _ -> accum
    Right filePaths ->
      case entries of
        Tar.Done -> Right filePaths
        Tar.Fail err -> Left [show err]
        Tar.Next entry rest ->
          getEntries (Right (filePaths <> [entry])) rest

showEntry :: Tar.Entry -> String
showEntry entry =
  case Tar.entryContent entry of
    Tar.NormalFile content size ->
      "F: " <> Tar.entryPath entry <> " (" <> show size <> ")"
    Tar.Directory ->
      "D: " <> Tar.entryPath entry
    _ -> "other"

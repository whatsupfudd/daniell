module Template.PHP.NeParser where

import Control.Monad.Cont (foldM)
import Control.Applicative (asum, many, some, (<|>))

import Control.Lens (Identity)
import Control.Monad.Identity (Identity(..))

import qualified Data.Vector as V

import TreeSitter.Node ( TSPoint(..) )

import qualified Template.PHP.Scanner as B
import qualified Template.PHP.Class as B
import qualified Template.PHP.State as B
import qualified Template.PHP.Error as E

import Template.PHP.Debug (debug)
import Template.PHP.Types (NodeEntry)
import Template.PHP.AST (PhpAction, PhpContext (..))
import Template.PHP.Parser.Types (ScannerB, TError (..))
import Template.PHP.Parser.Statements (statementS)
import Template.PHP.Parser.Support (debugOpt)
-- **** Combinatorial Monadic approach to parsing, derived from Megaparsec. **** --


testScannerB :: [NodeEntry] -> Either TError PhpContext
testScannerB nodes =
  let
    mainScanner = debugOpt "main" phpS <* B.pEof
    result = B.doScan mainScanner nodes
  in do
  -- putStrLn $ "@[testScannerB] endState: " <> show endState
  case result of
    Left err -> Left $ TError $ E.showScanErrorBundle err
    Right (logic, demands) -> Right $ PhpContext (V.fromList logic) demands


phpS :: ScannerB [PhpAction]
phpS = do
  many $ debugOpt "phpS" statementS

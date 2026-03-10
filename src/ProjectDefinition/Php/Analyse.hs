module ProjectDefinition.Php.Analyse where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bsl
import Data.Binary.Put (runPut, putInt32be)
import Data.Either (lefts, rights, partitionEithers)
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Sequence as Seq
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Vector as V

import qualified Crypto.Hash.MD5 as Cr

import System.FilePath.Posix ((</>))

import Hasql.Pool (Pool)

import TreeSitter.Node (TSPoint(..))
import Cannelle.TreeSitter.Types (SegmentPos)
import Cannelle.PHP.Parse (tsParsePhp)
import Cannelle.PHP.AST (PhpContext (..), PhpAction (..)
        , PhpStatement (..), DanglingClosure (..), PhpExpression (..)
        , CallerSpec (..), MemberAccessMode (..), ScopeMode (..), IncludeMode (..)
        , VariableSpec (..), UnaryOps (..), BinaryOps (..), UpdateOp (..)
        , LiteralValue (..), StringDetails (..), EncapsedMode (..)
        , MemberModifier (..), Attribute (..), AttributeGroup (..), AttributeList (..)
        , ClassMemberDecl (..), UseList (..), MethodImplementation (..)
        , TypeDecl (..), QualifiedName (..))
import Cannelle.PHP.Print (printPhpContext)
import qualified Cannelle.PHP.Serialize as PhS

import qualified FileSystem.Types as Fs
import qualified FileSystem.Explore as Fs

import qualified DB.FileOps as Do
import Utils (seqPartitionEithers)


processDir :: Text -> FilePath -> Pool -> IO ()
processDir projectName rootPath dbPool = do
  eiPathFiles <- Fs.loadFolderTree rootPath
  case eiPathFiles of
    Left err -> putStrLn $ "@[php.processDir] error loading folder tree: " <> rootPath <> " - " <> err
    Right pathFiles -> do
      eiProject <- Do.getProject dbPool projectName
      case eiProject of
        Left err -> putStrLn $ "@[php.processDir] getProject err: " <> err
        Right projectID -> do
          eiRezA <- mapM (\aPathFile ->
              let
                dirPath = fst aPathFile
              in do
              eiRezB <- Do.getFolder dbPool projectID dirPath
              case eiRezB of
                Left err -> pure $ Left err
                Right folderID -> pure $ Right (pack dirPath, folderID)
              ) pathFiles
          case seqPartitionEithers eiRezA of
            ([], folderIDs) ->
              let
                folderIDMap = Mp.fromList folderIDs
              in
              mapM_ (processFilesInDir dbPool folderIDMap rootPath) pathFiles
            (errs, _) -> do
              putStrLn $ "@[processDir] processFilesInDir errs: " <> show errs
              pure ()


processFilesInDir :: Pool -> Mp.Map Text Int32 -> FilePath -> Fs.PathNode -> IO ()
processFilesInDir dbPool folderIDMap rootPath (dirPath, files) =
  let
    folderID = fromMaybe 0 $ Mp.lookup (pack dirPath) folderIDMap
    fullPath = rootPath </> dirPath
  in do
  putStrLn $ "@[processFilesInDir] folderID: " <> show folderID <> " fullPath: " <> fullPath
  fRegistries <- mapM (registerFile dbPool folderID fullPath) (filter Fs.isPhpExtItem files)
  case partitionEithers fRegistries of
    (errs, _) -> do
      putStrLn $ "@[processFilesInDir] registerFile errs: " <> show errs
      pure ()
  pure ()


registerFile :: Pool -> Int32 -> FilePath -> Fs.ExtFileItem -> IO (Either String ())
registerFile dbPool folderID fullDirPath fileItem =
  let
    fileItemPath = Fs.getExtFileItemPath fileItem
  in do
  putStrLn $ "@[registerFile] folderID: " <> show folderID <> " file: " <> fileItemPath
  rezA <- Do.getFile dbPool folderID fileItemPath
  case rezA of
    Left err -> do
      putStrLn $ "@[registerFile] getFile err: " <> err
      pure $ Left err
    Right fileID -> do
      putStrLn $ "@[registerFile] getFile fileID: " <> show fileID
      rezA <- Do.getAst dbPool fileID
      case rezA of
        Left err -> do
          putStrLn $ "@[registerFile] getAst err: " <> err
          pure $ Left err
        Right mbAst ->
          case mbAst of
            Just (format, ast) -> do
              putStrLn $ "@[registerFile] getAst format: " <> show format <> " ast: " <> show (Bs.length ast)
              pure $ Right ()
            Nothing -> do
              let
                fullFilePath = fullDirPath </> fileItemPath
              startTime <- getCurrentTime
              parseRez <- tsParsePhp False fullFilePath
              endTime <- getCurrentTime
              let
                duration = diffUTCTime endTime startTime
              putStrLn $ "@[registerFile] parse time: " <> show duration <> ", rez: " <> show parseRez
              case parseRez of
                Left err -> do
                  rezB <- Do.addError dbPool fileID (pack $ show err) (Just duration)
                  case rezB of
                    Left dbErr -> pure . Left $ show dbErr
                    Right _ -> pure . Left $ show err
                Right phpContext -> do
                  compactConstants <- PhS.compactText fullFilePath phpContext.contentDemands
                  let
                    bsAst = PhS.convertAST phpContext.logic compactConstants
                    bsConstants = PhS.convertConstants compactConstants
                  rezC <- Do.addAST dbPool fileID "html" bsAst
                  rezD <- Do.addConstants dbPool fileID bsConstants
                  case (rezC, rezD) of
                    (Left dbErr, _) -> pure . Left $ "addAST err: " <> show dbErr
                    (_, Left dbErr) -> pure . Left $ "addConstants err: " <> show dbErr
                    (Right _, Right _) -> pure $ Right ()

{-
compactText :: FilePath -> V.Vector SegmentPos -> IO (Mp.Map Int32 (Bs.ByteString, [Int32]))
compactText sourceFile contentDemands = do
  sourceText <- Bs.readFile sourceFile
  let
    cLines = V.fromList $ Bs.split 10 sourceText
    demandLines = V.map (fetchContent cLines) $ V.zip contentDemands (V.fromList [0..])
    firstHash =
        Mp.fromListWith mergeHashUsers $ V.toList $ V.map (\(pos, lineText) -> (Cr.hash lineText, (lineText, [pos]))) demandLines
    posFromHash = zipWith (\rid (k, (lt, users)) -> (rid, (lt, users))) [0..] (Mp.toList firstHash)
  pure $ Mp.fromList posFromHash
  where
  mergeHashUsers :: (Bs.ByteString, [Int32]) -> (Bs.ByteString, [Int32]) -> (Bs.ByteString, [Int32])
  mergeHashUsers (lineText, accum) (_, e2) = (lineText, accum <> e2)

  fetchContent :: V.Vector Bs.ByteString -> (SegmentPos, Int) -> (Int32, Bs.ByteString)
  fetchContent cLines ((start, end), lineNum) =
    let
      startLine = fromIntegral start.pointRow
      startCol = fromIntegral start.pointColumn
      endLine = fromIntegral end.pointRow
      endCol = fromIntegral end.pointColumn
      mainText
        | startLine == endLine = Bs.take (endCol - startCol) $ Bs.drop startCol (cLines V.! startLine)
        | endCol == 0 = let
                          prefix = Bs.drop startCol (cLines V.! startLine)
                          middle = if endLine == succ startLine then
                              ""
                            else
                              V.foldl (\acc x -> acc <> "\n" <> x) "" (V.slice (succ startLine) (endLine - startLine - 1) cLines)
                        in
                        prefix <> middle
        | succ startLine == endLine = let
                        prefix = Bs.drop startCol (cLines V.! startLine)
                        postfix = Bs.take endCol (cLines V.! endLine)
                      in
                      prefix <> "\n" <> postfix
        | otherwise = let
                        prefix = Bs.drop startCol (cLines V.! startLine)
                        middle = V.foldl (\acc x -> acc <> "\n" <> x) "" (V.slice (succ startLine) (endLine - startLine - 1) cLines)
                        postfix = Bs.take endCol (cLines V.! endLine)
                      in
                      prefix <> middle <> "\n" <> postfix
    in
    (fromIntegral lineNum, mainText)


convertAST :: V.Vector PhpAction -> Mp.Map Int32 (Bs.ByteString, [Int32]) -> Bs.ByteString
convertAST logic constants =
  let
    byUser = Mp.fromList $ concatMap (\(k, (lineText, users)) -> [(u, k) | u <- users]) (Mp.toList constants)
    intRep = V.map (actionToIntRep byUser) logic
    makeList = V.fromList . map (runPut . putInt32be)
  in
  Bsl.toStrict . Bsl.concat . V.toList $ V.concatMap makeList intRep
  where
  actionToIntRep :: Mp.Map Int32 Int32 -> PhpAction -> [Int32]
  actionToIntRep cteMap actions =
    case actions of
      Verbatim anID -> [1, fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
      Statement aStmt -> statementToIntRep cteMap aStmt
      CommentA anID -> [2, fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
      MiscST aStr (startPos, endPos) -> [3, fromIntegral startPos.pointRow, fromIntegral startPos.pointColumn, fromIntegral endPos.pointRow, fromIntegral endPos.pointColumn]
      Interpolation subActions ->
        let
          -- Only count the non-NoOpAct sub-actions
          nbrSubActions = foldl (\acc sa -> acc + case sa of NoOpAct -> 0; _ -> 1) 0 subActions
        in
        [87, fromIntegral nbrSubActions] <> concatMap (actionToIntRep cteMap) subActions
      NoOpAct -> []


  -- maxID: 86


  statementToIntRep :: Mp.Map Int32 Int32 -> PhpStatement -> [Int32]
  statementToIntRep cteMap stmt =
    case stmt of
      BlockST actions ->
        [86, fromIntegral . length $ actions] <> concatMap (actionToIntRep cteMap) actions
      -- named label:
      NamedLabelST -> [4]
      -- Expression:
      ExpressionST expr -> [5] <> exprToIntRep cteMap expr
      -- If:
      IfST cond thenBranch mbElseBranch -> [if isNothing mbElseBranch then 6 else 7]
        <> exprToIntRep cteMap cond <> actionToIntRep cteMap thenBranch
        <> case mbElseBranch of
            Nothing -> []
            Just anAction -> actionToIntRep cteMap anAction
      -- Switch:
      SwitchST -> [8]
      -- While:
      WhileST -> [9]
      -- Do:
      DoST -> [10]
      -- For:
      ForST -> [11]
      -- Foreach:
      ForEachST expr (isRef, varSpec) mbRefVarSpec body ->
        let
          -- TODO:
          refVarSpec = []
          mbIndex = case mbRefVarSpec of
            Nothing -> []
            Just (idxRef, idxSpec) -> []
        in
        [12] <> exprToIntRep cteMap expr <> refVarSpec <> mbIndex <> actionToIntRep cteMap body
      -- Goto:
      GotoST -> [13]
      -- Continue:
      ContinueST -> [14]
      -- Break:
      BreakST -> [15]
      -- Return:
      ReturnST mbExpr -> [if isNothing mbExpr then 16 else 85]
          <> case mbExpr of
              Nothing -> []
              Just expr -> exprToIntRep cteMap expr
      -- Try:
      TryST -> [17]
      -- Declare:
      DeclareST -> [18]
      -- Echo:
      EchoST exprs ->
        [19, fromIntegral . length $ exprs] <> concatMap (exprToIntRep cteMap) exprs
      -- Exit:
      ExitST mbExpr -> [if isNothing mbExpr then 20 else 21]
          <> case mbExpr of
              Nothing -> []
              Just expr -> exprToIntRep cteMap expr
      -- Unset:
      UnsetST -> [22]
      -- Const:
      ConstDeclST -> [23]
      -- Function definition:
      FunctionDefST qualifiedName action ->
        let
          qualInd = qualNameToIntRep cteMap qualifiedName
        in
        [24] <> qualInd <> actionToIntRep cteMap action
      -- Class: attributes, modifiers, name, extends, implements, members
      ClassDefST mbAttr mbrModifiers anID mbExtend mbIDs clMembers ->
        let
          attrList = [0]   -- bits for attributes
          modList = [0]    -- bits for modifiers
          nameID = fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap
          extendID = case mbExtend of
            Nothing -> 0
            Just aQualName -> 0
          implIDs = [0]    -- length, IDs
        in
        [25, fromIntegral . length $ clMembers] <> [nameID, extendID] <> implIDs <> concatMap (memberToIntRep cteMap) clMembers
      -- Interface:
      InterfaceDefST -> [26]
      -- Trait:
      TraitDefST -> [27]
      -- Enum:
      EnumDefST -> [28]
      -- Namespace:
      NamespaceDefST -> [29]
      -- Namespace use:
      NamespaceUseST -> [30]
      -- Global:
      GlobalDeclST -> [31]
      -- Static:
      FunctionStaticST -> [32]
      -- A holder for dangling statements (else, else-if, end-if, ...)
      DanglingST dClosure -> danglingToIntRep cteMap dClosure

  danglingToIntRep :: Mp.Map Int32 Int32 -> DanglingClosure -> [Int32]
  danglingToIntRep cteMap dClosure =
    case dClosure of
      StatementDC action ->
        [33] <> actionToIntRep cteMap action
      EndDeclareDC -> [34]
      EndForDC -> [35]
      EndForEachDC -> [36]
      EndIfDC -> [37]
      EndSwitchDC -> [38]
      EndWhileDC -> [39]

  exprToIntRep :: Mp.Map Int32 Int32 -> PhpExpression -> [Int32]
  exprToIntRep cteMap expr =
    case expr of
      Variable varSpec -> [40] <> varSpecToIntRep cteMap varSpec
      Symbol anID -> [41, fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
      BinaryOp op left right -> [42, binaryOpToInt op] <> exprToIntRep cteMap left <> exprToIntRep cteMap right
      UnaryOp op operand -> [43, unaryOpToInt op] <> exprToIntRep cteMap operand
      TernaryOp cond thenExpr elseExpr -> [44] <> exprToIntRep cteMap cond <> exprToIntRep cteMap thenExpr <> exprToIntRep cteMap elseExpr
      FunctionCall callerSpec args -> [45, fromIntegral . length $ args]
          <> callerSpecToIntRep cteMap callerSpec
          <> concatMap (exprToIntRep cteMap) args
      ArrayAccess array expr -> [46] <> exprToIntRep cteMap array <> exprToIntRep cteMap expr
      ArrayLiteral exprs -> [47, fromIntegral . length $ exprs] <> concatMap (exprToIntRep cteMap) exprs
      Parenthesized exprs -> [48, fromIntegral . length $ exprs] <> concatMap (exprToIntRep cteMap) exprs
      AssignVar posFlag left right -> [49] <> [if posFlag then 1 else 0] <> exprToIntRep cteMap left <> exprToIntRep cteMap right
      CommentX anID -> [50, fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
      MiscExpr aStr (startPos, endPos) -> [51, fromIntegral startPos.pointRow, fromIntegral startPos.pointColumn, fromIntegral endPos.pointRow, fromIntegral endPos.pointColumn]
      Subscript expr mbExpr -> [if isNothing mbExpr then 52 else 84] <> exprToIntRep cteMap expr
          <> case mbExpr of
              Nothing -> []
              Just subExpr -> exprToIntRep cteMap subExpr
      MemberAccess expr accessMode -> [53] <> exprToIntRep cteMap expr
          <> accessModeToIntRep cteMap accessMode
      MemberCall expr accessMode exprs -> [54, fromIntegral . length $ exprs] <> exprToIntRep cteMap expr
          <> accessModeToIntRep cteMap accessMode <> concatMap (exprToIntRep cteMap) exprs
      Conditional cond thenExpr elseExpr -> [55] <> exprToIntRep cteMap cond <> exprToIntRep cteMap thenExpr <> exprToIntRep cteMap elseExpr
      Casting anID expr -> [56, fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap] <> exprToIntRep cteMap expr
      ObjectCreation accessMode exprs -> [57, fromIntegral . length $ exprs] <> accessModeToIntRep cteMap accessMode <> concatMap (exprToIntRep cteMap) exprs
      Include inclMode expr -> [58, includeModeToIntRep inclMode] <> exprToIntRep cteMap expr
      AugmentedAssign left op right -> [59] <> exprToIntRep cteMap left <> [ binaryOpToInt op ] <> exprToIntRep cteMap right
      ScopeCall scopeModes exprs -> [60, fromIntegral . length $ scopeModes, fromIntegral . length $ exprs]
          <> concatMap (scopeModeToIntRep cteMap) scopeModes
          <> concatMap (exprToIntRep cteMap) exprs
      ScopedPropertyAccess scopeMode expr -> [61] <> scopeModeToIntRep cteMap scopeMode <> exprToIntRep cteMap expr
      ErrorSuppression expr -> [62] <> exprToIntRep cteMap expr
      ListLiteral exprs -> [63, fromIntegral . length $ exprs] <> concatMap (exprToIntRep cteMap) exprs
      HereDoc startLine startCol endLine -> [64, fromIntegral startLine, fromIntegral startCol, fromIntegral endLine]
      ClassConstantAccess scopeMode anID -> [65] <> scopeModeToIntRep cteMap scopeMode
          <> [fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
      ShellCommand expr -> [66] <> exprToIntRep cteMap expr
      ThrowExpr expr -> [67] <> exprToIntRep cteMap expr
      UpdateExpr posFlag op var -> [68, if posFlag then 1 else 0, updateOpToInt op] <> exprToIntRep cteMap var
      CloneExpr expr -> [69] <> exprToIntRep cteMap expr
      Literal aLit -> [70] <> literalToIntRep cteMap aLit


  memberToIntRep :: Mp.Map Int32 Int32 -> ClassMemberDecl -> [Int32]
  memberToIntRep cteMap mbrDecl =
    case mbrDecl of
      CommentCDecl anID -> [66, fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
      ConstantCDecl mods exprs -> [67, fromIntegral . length $ exprs]
          <> concatMap (\(anID, anExpr) ->
                [fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
                <> exprToIntRep cteMap anExpr
             ) exprs
      PropertyCDecl mbAttr mods varSpec mbExpr -> [68]
          <> case mbAttr of
              Nothing -> [0]
              Just attrList -> [0]  -- bits for attributes
      MethodCDecl mbAttr mods access varSpecs impl -> [69]
          <> case mbAttr of
              Nothing -> [0]
              Just attrList -> [0]  -- bits for attributes
      ConstructorCDecl -> [70]
      DestructorCDecl -> [71]
      TraitUseCDecl impls mbUseList -> [72, fromIntegral . length $ impls]
          <> map (\anID -> fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap) impls
          <> case mbUseList of
              Nothing -> [0]
              Just useList -> [0]  -- bits for use list

  varSpecToIntRep :: Mp.Map Int32 Int32 -> VariableSpec -> [Int32]
  varSpecToIntRep cteMap varSpec =
    case varSpec of
      SimpleVS anID -> [72, fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
      Dynamic aID bID -> [73, fromMaybe 0 $ Mp.lookup (fromIntegral aID) cteMap, fromMaybe 0 $ Mp.lookup (fromIntegral bID) cteMap]
      ComplexVS anExpr -> [74] <> exprToIntRep cteMap anExpr

  callerSpecToIntRep :: Mp.Map Int32 Int32 -> CallerSpec -> [Int32]
  callerSpecToIntRep cteMap callerSpec =
    case callerSpec of
      QualNameCS qName -> [75] <> qualNameToIntRep cteMap qName
      VariableCS varSpec -> [76] <> varSpecToIntRep cteMap varSpec
      SubscriptCS expr -> [77] <> exprToIntRep cteMap expr

  accessModeToIntRep :: Mp.Map Int32 Int32 ->MemberAccessMode -> [Int32]
  accessModeToIntRep cteMap accessMode =
    case accessMode of
      NameMT anID -> [0, fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
      ParentMT -> [1]
      SelfMT -> [2]
      VarExprMT expr -> [3] <> exprToIntRep cteMap expr
      StringMT expr -> [4] <> exprToIntRep cteMap expr

  includeModeToIntRep :: IncludeMode -> Int32
  includeModeToIntRep inclMode =
    case inclMode of
      IncludeOnceIM -> 0
      RequireOnceIM -> 1
      RequireIM -> 2
      IncludeIM -> 3

  qualNameToIntRep :: Mp.Map Int32 Int32 -> QualifiedName -> [Int32]
  qualNameToIntRep cteMap qName =
    case qName of
      SimpleNameQN anID -> [78, fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
      QualifiedNameQN qualIDs -> [79, fromIntegral $ length qualIDs]
            <> map (\anID -> fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap) qualIDs

  binaryOpToInt :: BinaryOps -> Int32
  binaryOpToInt op = case op of
    DotOp -> 0
    AddOp -> 1
    SubOp -> 2
    MulOp -> 3
    DivOp -> 4
    ModOp -> 5
    PowOp -> 6
    BitAndOp -> 7
    BitOrOp -> 8
    BitXorOp -> 9
    EqOp -> 10
    EqlOp -> 11
    NeqOp -> 12
    NeqlOp -> 13
    GtOp -> 14
    LtOp -> 15
    GteOp -> 16
    LteOp -> 17
    AndOp -> 18
    OrOp -> 19
    InstanceOfOp -> 20
    BitShiftLeftOp -> 21
    BitShiftRightOp -> 22

  unaryOpToInt :: UnaryOps -> Int32
  unaryOpToInt op = case op of
    NegOp -> 0
    NotOp -> 1

  updateOpToInt :: UpdateOp -> Int32
  updateOpToInt op = case op of
    IncOp -> 0
    DecOp -> 1

  scopeModeToIntRep :: Mp.Map Int32 Int32 -> ScopeMode -> [Int32]
  scopeModeToIntRep cteMap scopeMode = case scopeMode of
    RelativeSelfSM -> [0]
    RelativeStaticSM -> [1]
    RelativeParentSM -> [2]
    NamedSM anID -> [3, fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
    VariableSM varSpec -> [4] <> varSpecToIntRep cteMap varSpec


  literalToIntRep :: Mp.Map Int32 Int32 -> LiteralValue -> [Int32]
  literalToIntRep cteMap aLit =
    case aLit of
      BoolLiteral anID -> [80, fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
      IntLiteral anID -> [81, fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
      FloatLiteral anID -> [82, fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
      -- string [bB] "<content>" | [bB] '<content>'
      StringLiteral flag strDetails -> [83, if flag then 1 else 0] <> stringDetailsToIntRep cteMap strDetails
      NullLiteral -> [84]


  stringDetailsToIntRep :: Mp.Map Int32 Int32 -> StringDetails -> [Int32]
  stringDetailsToIntRep cteMap strDetails =
    let
      (intDetails, encModes) =
        case strDetails of
          SimpleString modes -> (1, modes)
          EncapsedString modes -> (2, modes)
    in
    [ intDetails, fromIntegral . length $ encModes] <> concatMap (encapsedModeToIntRep cteMap) encModes


  encapsedModeToIntRep :: Mp.Map Int32 Int32 -> EncapsedMode -> [Int32]
  encapsedModeToIntRep cteMap encMode =
    case encMode of
      ContentEM anID -> [1, fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
      EscapeEM anID -> [2, fromMaybe 0 $ Mp.lookup (fromIntegral anID) cteMap]
      -- the PhpExpression is a (Variable varSpec)
      VariableEM expr -> [3] <> exprToIntRep cteMap expr
      CurlyEM flag -> [4, if flag then 1 else 0]


convertConstants :: Mp.Map Int32 (Bs.ByteString, [Int32]) -> Bs.ByteString
convertConstants constants =
  let
    bsNbrCtes = runPut . putInt32be . fromIntegral . Mp.size $ constants
    elems = Mp.elems constants
    indices = map (\(lineText, users) -> Bs.length lineText) elems
    bsTotalLength = runPut . putInt32be . fromIntegral . sum $ indices
    bsIndices = Bsl.concat $ map (runPut . putInt32be . fromIntegral) indices
    bsConstants = Mp.foldl (\acc (lineText, users) -> acc <> lineText) "" constants
  in
  Bs.append (Bsl.toStrict $ Bsl.concat [bsNbrCtes, bsTotalLength, bsIndices]) bsConstants
-}
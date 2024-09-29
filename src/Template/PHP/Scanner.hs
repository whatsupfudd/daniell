{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}


module Template.PHP.Scanner where

import Control.Applicative
import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Writer.Class
import Control.Monad.Identity (IdentityT, Identity (runIdentity))


import Data.Monoid
import Data.Semigroup
import Control.DeepSeq (NFData (rnf))


import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as E

import GHC.Generics

import qualified Data.Vector as V

import TreeSitter.Node ( Node(..), TSPoint(TSPoint, pointRow, pointColumn) )

import Template.PHP.Types
import Template.PHP.Error
import Template.PHP.Class
import Template.PHP.State
import Template.PHP.Interface
import Options (ServerOpts(cache))

import Template.PHP.AST (SegmentPos)


newtype ScannerT errT m a = ScannerT {
  -- scannerT state consOK consErr emptyOK emptyErr => m b
  scannerT :: forall b.
    ScanState errT
    -> (a -> ScanState errT -> Hints -> m b)          -- consumed-OK
    -> (ScanError errT -> ScanState errT -> m b)        -- consumed-ERROR
    -> (a -> ScanState errT -> Hints -> m b)          -- empty-OK
    -> (ScanError errT -> ScanState errT -> m b)        -- empty-ERROR
    -> m b
  }

instance (Semigroup a, Functor m, Monad m, Ord errT) => Semigroup (ScannerT errT m a) where
  (<>) = liftA2 (<>)
  sconcat = fmap sconcat . sequence


instance (Monoid a, Functor m, Monad m, Ord errT) => Monoid (ScannerT errT m a) where
  mempty = pure mempty
  mappend = (<>)
  mconcat = fmap mconcat . sequence

instance Functor m => Functor (ScannerT errT m) where
  fmap = sMap

sMap :: (a -> b) -> ScannerT errT m a -> ScannerT errT m b
sMap f scanner = ScannerT $ \state consOK consErr emptyOK emptyErr ->
  scannerT scanner state
    (consOK . f) consErr (emptyOK . f) emptyErr

instance (Functor m, Monad m, Ord errT) => Applicative (ScannerT errT m) where
  pure = sPure
  (<*>) = sAp
  s1 *> s2 = s1 `sBind` const s2
  s1 <* s2 = do x1 <- s1; void s2; return x1

sPure :: a -> ScannerT errT m a
sPure x = ScannerT $ \state _ _ errOK _ -> errOK x state mempty

sAp :: ScannerT errT m (a -> b) -> ScannerT errT m a -> ScannerT errT m b
sAp m k = ScannerT $ \state consOK consErr emptyOK emptyErr ->
  let
    mConsOk x updState hints =
      scannerT k updState (consOK . x) consErr (accHints hints (consOK . x)) (withHints hints consErr)
    mErrOK x updState hints =
      scannerT k updState (consOK . x) consErr (accHints hints (emptyOK . x)) (withHints hints emptyErr)
  in
  scannerT m state mConsOk consErr mErrOK emptyErr


instance (Ord errT, Functor m, Monad m) => Alternative (ScannerT errT m) where
  empty = mzero
  (<|>) = mplus

instance (Ord errT, Monad m) => Monad (ScannerT errT m) where
  return = pure
  (>>=) :: ScannerT errT m a -> (a -> ScannerT errT m b) -> ScannerT errT m b
  (>>=) = sBind


sBind :: ScannerT errT m a -> (a -> ScannerT errT m b) -> ScannerT errT m b
sBind m k = ScannerT $ \state consOK consErr emptyOK emptyErr ->
  let
    mConsOk x updState hints =
      scannerT (k x) updState consOK consErr (accHints hints emptyOK) (withHints hints emptyErr)
    mErrOK x updState hints =
      scannerT (k x) updState consOK consErr (accHints hints emptyOK) (withHints hints emptyErr)
  in
  scannerT m state mConsOk consErr mErrOK emptyErr

instance (Ord errT, Fail.MonadFail m) => Fail.MonadFail (ScannerT errT m) where
  fail = sFail

sFail :: (Ord errT) => String -> ScannerT errT m a
sFail msg = ScannerT $ \st@(ScanState entries parsed errs demands) _ _ _ emptyErr ->
  let
    d = E.singleton (ErrorFail msg)
  in
  emptyErr (FancyError parsed d) st

instance (Ord errT, MonadIO m) => MonadIO (ScannerT errT m) where
  liftIO = lift . liftIO

instance (Ord errT, MonadReader r m) => MonadReader r (ScannerT errT m) where
  ask = lift ask
  local f = hoistS (local f)

instance (Ord errT, MonadState st m) => MonadState st (ScannerT errT m) where
  get = lift get
  put = lift . put

hoistS :: (Monad m) => (m (Reply errT a) -> m (Reply errT b))
    -> ScannerT errT m a -> ScannerT errT m b
hoistS h s = mkScannerT (h . runScannerT s)


instance (Ord errT, MonadWriter w m) => MonadWriter w (ScannerT errT m) where
  tell w = lift (tell w)
  listen = hoistS (fmap (\(repl, w) -> fmap (,w) repl) . listen)
  pass = hoistS $ \m -> pass $ do
    Reply st consumption r <- m
    let (r', ww') = case r of
          OK hs (x, ww) -> (OK hs x, ww)
          Error e -> (Error e, id)
    return (Reply st consumption r', ww')


instance (Ord errT, MonadCont m) => MonadCont (ScannerT errT m) where
  callCC f = mkScannerT $ \s ->
    callCC $ \c ->
      runScannerT (f (\a -> mkScannerT $ \s' -> c (pack s' a))) s
    where
      pack s a = Reply s NotConsumed (OK mempty a)


instance (MonadError e' m, Ord errT) => MonadError e' (ScannerT errT m) where
  throwError = lift . throwError
  p `catchError` h = mkScannerT $ \s ->
    runScannerT p s `catchError` \e ->
      runScannerT (h e) s


mkScannerT :: (Monad m) => (ScanState errT -> m (Reply errT a)) -> ScannerT errT m a
mkScannerT k = ScannerT $ \state consOK consErr emptyOK emptyErr -> do
  (Reply updState consumption result) <- k state
  case consumption of
    Consumed ->
      case result of
        OK hs x -> consOK x updState hs
        Error e -> consErr e updState
    NotConsumed ->
      case result of
        OK hs x -> emptyOK x updState hs
        Error e -> emptyErr e updState


pmkScanner :: (ScanState errT -> Reply errT a) -> ScannerT errT m a
pmkScanner k = ScannerT $ \state consOK consErr emptyOK emptyErr ->
  let
    (Reply updState consumption result) = k state
  in
  case consumption of
    Consumed ->
      case result of
        OK hints x -> consOK x updState hints
        Error err -> consErr err updState
    NotConsumed ->
      case result of
        OK hints x -> emptyOK x updState hints
        Error err -> emptyErr err updState

instance (Ord errT, Functor m, Monad m) => MonadPlus (ScannerT errT m) where
  mzero = pZero
  mplus = pPlus

pZero :: (Ord errT) => ScannerT errT m a
pZero = ScannerT $ \st@(ScanState _ parsed _ _) _ _ _ empErr ->
  empErr (TrivialError parsed "No more input (pZero)." Nothing mempty) st

pPlus :: (Ord errT) => ScannerT errT m a -> ScannerT errT m a -> ScannerT errT m a
pPlus m n = ScannerT $ \state consOK consErr emptyOK emptyErr ->
  let
    mEmptyErr err ms =
      let
        nConsErr err' updState = consErr (err' <> err) (longuestMatch ms updState)
        nEmpOk x updState hints = emptyOK x updState (toHints updState.parsed err <> hints)
        nEmpErr err' updState = emptyErr (err' <> err) (longuestMatch ms updState)
      in
      scannerT n state consOK nConsErr nEmpOk nEmpErr
  in
  scannerT m state consOK consErr emptyOK mEmptyErr

longuestMatch :: Ord errT => ScanState errT -> ScanState errT -> ScanState errT
longuestMatch s1@(ScanState _ parsed1 _ _) s2@(ScanState _ parsed2 _ _) =
  case parsed1 `compare` parsed2 of
    LT -> s2
    EQ -> s2
    GT -> s1


instance (MonadFix m, Ord errT) => MonadFix (ScannerT errT m) where
  mfix f = mkScannerT $ \state -> mfix $ \(~(Reply _ _ result)) -> do
    let
      a = case result of
        OK _ updA -> updA
        Error _ -> error "mfix ScannerT"
    runScannerT (f a) state

instance MonadTrans (ScannerT e) where
  lift amb = ScannerT $ \state _ _ emptyOK _ ->
    amb >>= \a -> emptyOK a state mempty


runScannerT :: (Monad m) => ScannerT errT m a -> ScanState errT -> m (Reply errT a)
runScannerT scanner state = scannerT scanner state consOK consErr emptyOK emptyErr
  where
    consOK a updState hints = return $ Reply updState Consumed (OK hints a)
    consErr err updState = return $ Reply updState Consumed (Error err)
    emptyOK x updState hints = return (Reply updState NotConsumed (OK hints x))
    emptyErr err updState = return (Reply updState NotConsumed (Error err))



--- *** The main monad: MonadScanner *** ---

instance (Ord errT, Monad m) => MonadScanner errT (ScannerT errT m) where
  scanError = pScanError
  token = pToken
  eof = pEof
  getScannerState = pGetScannerState
  updateScannerState = pUpdateScannerState
  tokenPush = pTokenPush
  tokenDemand = pTokenDemand


pScanError :: ScanError errT -> ScannerT errT m a
pScanError e = ScannerT $ \state _ _ _ emptyErr -> emptyErr e state


pToken :: forall errT m a. (NodeEntry -> Maybe a) -> Set (ErrorItem NodeEntry) -> ScannerT errT m a
pToken test errSet =
  ScannerT $ \state consOK consErr emptyOK emptyErr ->
    case state.inputs of
      [] ->
        let
          us = pure EndOfInput
        in emptyErr (TrivialError state.parsed "no entry left." us errSet) state
      hNode : rest ->
        case test hNode of
          Just x ->
            consOK x (ScanState rest (state.parsed + 1) state.errors state.contextDemands) mempty
          Nothing ->
            let
              us = (Just . Tokens . newEmpty) hNode
            in
            emptyErr (TrivialError state.parsed ("nothing found, node: " <> hNode.name) us errSet)
                    (ScanState state.inputs state.parsed state.errors state.contextDemands)

pTokenPush :: forall errT m a. (NodeEntry -> Maybe a) -> Set (ErrorItem NodeEntry) -> ScannerT errT m a
pTokenPush test errSet =
  ScannerT $ \state consOK consErr emptyOK emptyErr ->
    case state.inputs of
      [] ->
        let
          us = pure EndOfInput
        in emptyErr (TrivialError state.parsed "no entry left." us errSet) state
      hNode : rest ->
        case test hNode of
          Just x ->
            consOK x (ScanState (hNode.children <> rest) (state.parsed + 1) state.errors state.contextDemands) mempty
          Nothing ->
            let
              us = (Just . Tokens . newEmpty) hNode
            in
            emptyErr (TrivialError state.parsed ("nothing found, node: " <> hNode.name) us errSet)
                    (ScanState state.inputs state.parsed state.errors state.contextDemands)

pTokenDemand :: forall errT m a. (NodeEntry -> Maybe a) -> Set (ErrorItem NodeEntry) -> ScannerT errT m Int
pTokenDemand test errSet =
  ScannerT $ \state consOK consErr emptyOK emptyErr ->
    case state.inputs of
      [] ->
        let
          us = pure EndOfInput
        in emptyErr (TrivialError state.parsed "no entry left." us errSet) state
      hNode : rest ->
        case test hNode of
          Just x ->
            let
              demandLength = length state.contextDemands
              newDemands = V.snoc state.contextDemands (hNode.start, hNode.end)
            in
            consOK demandLength (ScanState (hNode.children <> rest) (state.parsed + 1) state.errors newDemands) mempty
          Nothing ->
            let
              us = (Just . Tokens . newEmpty) hNode
            in
            emptyErr (TrivialError state.parsed ("nothing found, node: " <> hNode.name) us errSet)
                    (ScanState state.inputs state.parsed state.errors state.contextDemands)



newEmpty x = x :| []

pEof :: ScannerT errT m ()
pEof = ScannerT $ \st _ _ emptyOK emptyErr ->
  case st.inputs of
    [] -> emptyOK () st mempty
    hNode : rest ->
      let us = (pure . Tokens . newEmpty) hNode
          ps = E.singleton EndOfInput
       in emptyErr
            (TrivialError st.parsed ("peof while still " <> show (length st.inputs) <> " nodes in input.") us ps)
            (ScanState st.inputs st.parsed st.errors st.contextDemands)


pGetScannerState :: ScannerT errT m (ScanState errT)
pGetScannerState = ScannerT $ \st _ _ emptyOK _ -> emptyOK st st mempty

pUpdateScannerState :: (ScanState errT -> ScanState errT) -> ScannerT errT m ()
pUpdateScannerState f = ScannerT $ \st _ _ emptyOK _ -> emptyOK () (f st) mempty


--- *** Usable functions *** ---

doScan :: (Ord errT) => ScannerT errT Identity a -> [NodeEntry] -> Either (ScanErrBundle errT) (a, V.Vector SegmentPos)
doScan parser nodes =
  let
    (endState, result) = runIdentity $ doScanT parser (initState nodes)
  in
  case result of
    Left err -> Left err
    Right aVal -> Right (aVal, endState.contextDemands)

doScanT :: (Monad m) => ScannerT errT m a -> ScanState errT -> m (ScanState errT, Either (ScanErrBundle errT) a)
doScanT scanner state = do
  (Reply updState consumption result) <- runScannerT scanner state
  let
    toBundle es =
      ScanErrBundle { errors = NE.sortWith (.position) es, endPos = PosState updState.inputs updState.parsed }
  return $ case result of
    OK _ x -> case NE.nonEmpty updState.errors of
      Nothing -> (updState, Right x)
      Just errs -> (updState, Left (toBundle errs))
    Error err -> (updState, Left (toBundle (err :| updState.errors)))


testScan :: (ShowErrorComponent errT, Show errT, Show a) => ScannerT errT Identity a -> [NodeEntry] -> IO ()
testScan scanner nodes =
  case doScan scanner nodes of
    Left err -> putStrLn $ "doScan err: " <> show err
    Right x -> do
      putStrLn $ "@[testScanT] result: " <> show x


satisfy :: (MonadScanner errT  m) => (String -> Bool) -> m NodeEntry
satisfy f = token testChar E.empty
  where
  testChar :: NodeEntry -> Maybe NodeEntry
  testChar x = if f x.name then Just x else Nothing


single :: (MonadScanner e m) => String -> m NodeEntry
single t = token testToken expected
  where
  testToken x = if x.name == t then Just x else Nothing
  expected = E.singleton (Tokens ((NodeEntry t (TSPoint 0 0) (TSPoint 0 0) []) :| []))


singleP :: (MonadScanner e m) => String -> m NodeEntry
singleP t = tokenPush testToken expected
  where
  testToken x = if x.name == t then Just x else Nothing
  expected = E.singleton (Tokens ((NodeEntry t (TSPoint 0 0) (TSPoint 0 0) []) :| []))

symbol :: (MonadScanner e m) => String -> m Int
symbol t = tokenDemand testToken expected
  where
  testToken x = if x.name == t then Just x else Nothing
  expected = E.singleton (Tokens ((NodeEntry t (TSPoint 0 0) (TSPoint 0 0) []) :| []))


failure :: (MonadScanner e m) =>  String -> Maybe (ErrorItem NodeEntry) -> Set (ErrorItem NodeEntry) -> m a
  -- | us: Expected items
  -- | ps: Unexpected item (if any)
failure msg us ps = do
  curPos <- getOffset
  scanError (TrivialError curPos ("failure: " <> msg) us ps)


getOffset :: (MonadScanner e m) => m Int
getOffset = (.parsed) <$> getScannerState
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}

module Template.PHP.Class where


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
import Control.Monad.Identity (IdentityT (..))

import qualified Control.Monad.RWS.Lazy as L
import qualified Control.Monad.RWS.Strict as S
import qualified Control.Monad.Trans.Reader as L
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S

import Data.Set (Set)
import qualified Data.Set as E

import Template.PHP.Types
import Template.PHP.Error
import Template.PHP.State
import Template.PHP.Interface


class MonadPlus m => MonadScanner errT m | m -> errT where
  scanError :: ScanError errT -> m a

  label :: String -> m a -> m a

  hidden :: m a -> m a
  hidden = label ""

  try :: m a -> m a

  lookAhead :: m a -> m a

  notFollowedBy :: m a -> m ()

  withRecovery ::  (ScanError e -> m a) -> m a -> m a

  observing :: m a -> m (Either (ScanError e) a)

  eof :: m ()

  token :: (NodeEntry -> Maybe a) -> Set (ErrorItem NodeEntry) -> m a

  tokens :: (NodeEntry -> NodeEntry -> Bool) ->  NodeEntry ->  m NodeEntry

  takeWhileP :: Maybe String -> (NodeEntry -> Bool) ->m NodeEntry

  takeWhile1P :: Maybe String ->  (NodeEntry -> Bool) -> m NodeEntry

  takeP :: Maybe String -> Int -> m NodeEntry

  getScannerState :: m (ScanState errT)

  updateScannerState :: (ScanState errT -> ScanState errT) -> m ()

  mkScanner :: (ScanState e -> Reply e a) -> m a

  tokenPush :: (NodeEntry -> Maybe a) -> Set (ErrorItem NodeEntry) -> m a

  tokenDemand :: (NodeEntry -> Maybe a) -> Set (ErrorItem NodeEntry) -> m Int

  tokenTryPush :: (NodeEntry -> Maybe a) -> Set (ErrorItem NodeEntry) -> m (Either Int a)



instance (MonadScanner errT m) => MonadScanner errT (IdentityT m) where
  scanError e = lift (scanError e)
  label n (IdentityT m) = IdentityT $ label n m
  try = IdentityT . try . runIdentityT
  lookAhead (IdentityT m) = IdentityT $ lookAhead m
  notFollowedBy (IdentityT m) = IdentityT $ notFollowedBy m
  withRecovery r (IdentityT m) =
    IdentityT $ withRecovery (runIdentityT . r) m
  observing (IdentityT m) = IdentityT $ observing m
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift $ tokens e ts
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getScannerState = lift getScannerState
  updateScannerState f = lift $ updateScannerState f
  mkScanner f = lift (mkScanner f)
  tokenPush test mt = lift (tokenPush test mt)
  tokenDemand test mt = lift (tokenDemand test mt)


instance (MonadScanner errT m) => MonadScanner errT (L.StateT st m) where
  scanError e = lift (scanError e)
  label n (L.StateT m) = L.StateT $ label n . m
  try (L.StateT m) = L.StateT $ try . m
  lookAhead (L.StateT m) = L.StateT $ \s ->
    (,s) . fst <$> lookAhead (m s)
  notFollowedBy (L.StateT m) = L.StateT $ \s ->
    notFollowedBy (fst <$> m s) >> return ((), s)
  withRecovery r (L.StateT m) = L.StateT $ \s ->
    withRecovery (\e -> L.runStateT (r e) s) (m s)
  observing (L.StateT m) = L.StateT $ \s ->
    fixs s <$> observing (m s)
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift (tokens e ts)
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getScannerState = lift getScannerState
  updateScannerState f = lift (updateScannerState f)
  mkScanner f = lift (mkScanner f)


instance (MonadScanner e m) => MonadScanner e (S.StateT st m) where
  scanError e = lift (scanError e)
  label n (S.StateT m) = S.StateT $ label n . m
  try (S.StateT m) = S.StateT $ try . m
  lookAhead (S.StateT m) = S.StateT $ \s ->
    (,s) . fst <$> lookAhead (m s)
  notFollowedBy (S.StateT m) = S.StateT $ \s ->
    notFollowedBy (fst <$> m s) >> return ((), s)
  withRecovery r (S.StateT m) = S.StateT $ \s ->
    withRecovery (\e -> S.runStateT (r e) s) (m s)
  observing (S.StateT m) = S.StateT $ \s ->
    fixs s <$> observing (m s)
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift (tokens e ts)
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getScannerState = lift getScannerState
  updateScannerState f = lift (updateScannerState f)
  mkScanner f = lift (mkScanner f)


instance (MonadScanner e m) => MonadScanner e (L.ReaderT r m) where
  scanError e = lift (scanError e)
  label n (L.ReaderT m) = L.ReaderT $ label n . m
  try (L.ReaderT m) = L.ReaderT $ try . m
  lookAhead (L.ReaderT m) = L.ReaderT $ lookAhead . m
  notFollowedBy (L.ReaderT m) = L.ReaderT $ notFollowedBy . m
  withRecovery r (L.ReaderT m) = L.ReaderT $ \s ->
    withRecovery (\e -> L.runReaderT (r e) s) (m s)
  observing (L.ReaderT m) = L.ReaderT $ observing . m
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift (tokens e ts)
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getScannerState = lift getScannerState
  updateScannerState f = lift (updateScannerState f)
  mkScanner f = lift (mkScanner f)


instance (Monoid w, MonadScanner e m) => MonadScanner e (L.WriterT w m) where
  scanError e = lift (scanError e)
  label n (L.WriterT m) = L.WriterT $ label n m
  try (L.WriterT m) = L.WriterT $ try m
  lookAhead (L.WriterT m) =
    L.WriterT $
      (,mempty) . fst <$> lookAhead m
  notFollowedBy (L.WriterT m) =
    L.WriterT $
      (,mempty) <$> notFollowedBy (fst <$> m)
  withRecovery r (L.WriterT m) =
    L.WriterT $
      withRecovery (L.runWriterT . r) m
  observing (L.WriterT m) =
    L.WriterT $
      fixs mempty <$> observing m
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift (tokens e ts)
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getScannerState = lift getScannerState
  updateScannerState f = lift (updateScannerState f)
  mkScanner f = lift (mkScanner f)


instance (Monoid w, MonadScanner e m) => MonadScanner e (S.WriterT w m) where
  scanError e = lift (scanError e)
  label n (S.WriterT m) = S.WriterT $ label n m
  try (S.WriterT m) = S.WriterT $ try m
  lookAhead (S.WriterT m) =
    S.WriterT $
      (,mempty) . fst <$> lookAhead m
  notFollowedBy (S.WriterT m) =
    S.WriterT $
      (,mempty) <$> notFollowedBy (fst <$> m)
  withRecovery r (S.WriterT m) =
    S.WriterT $
      withRecovery (S.runWriterT . r) m
  observing (S.WriterT m) =
    S.WriterT $
      fixs mempty <$> observing m
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift (tokens e ts)
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getScannerState = lift getScannerState
  updateScannerState f = lift (updateScannerState f)
  mkScanner f = lift (mkScanner f)


instance (Monoid w, MonadScanner e m) => MonadScanner e (L.RWST r w st m) where
  scanError e = lift (scanError e)
  label n (L.RWST m) = L.RWST $ \r s -> label n (m r s)
  try (L.RWST m) = L.RWST $ \r s -> try (m r s)
  lookAhead (L.RWST m) = L.RWST $ \r s -> do
    (x, _, _) <- lookAhead (m r s)
    return (x, s, mempty)
  notFollowedBy (L.RWST m) = L.RWST $ \r s -> do
    notFollowedBy (void $ m r s)
    return ((), s, mempty)
  withRecovery n (L.RWST m) = L.RWST $ \r s ->
    withRecovery (\e -> L.runRWST (n e) r s) (m r s)
  observing (L.RWST m) = L.RWST $ \r s ->
    fixs' s <$> observing (m r s)
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift (tokens e ts)
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getScannerState = lift getScannerState
  updateScannerState f = lift (updateScannerState f)
  mkScanner f = lift (mkScanner f)


instance (Monoid w, MonadScanner e m) => MonadScanner e (S.RWST r w st m) where
  scanError e = lift (scanError e)
  label n (S.RWST m) = S.RWST $ \r s -> label n (m r s)
  try (S.RWST m) = S.RWST $ \r s -> try (m r s)
  lookAhead (S.RWST m) = S.RWST $ \r s -> do
    (x, _, _) <- lookAhead (m r s)
    return (x, s, mempty)
  notFollowedBy (S.RWST m) = S.RWST $ \r s -> do
    notFollowedBy (void $ m r s)
    return ((), s, mempty)
  withRecovery n (S.RWST m) = S.RWST $ \r s ->
    withRecovery (\e -> S.runRWST (n e) r s) (m r s)
  observing (S.RWST m) = S.RWST $ \r s ->
    fixs' s <$> observing (m r s)
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift (tokens e ts)
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getScannerState = lift getScannerState
  updateScannerState f = lift (updateScannerState f)
  mkScanner f = lift (mkScanner f)


fixs :: s -> Either a (b, s) -> (Either a b, s)
fixs s (Left a) = (Left a, s)
fixs _ (Right (b, s)) = (Right b, s)

fixs' :: (Monoid w) => s -> Either a (b, s, w) -> (Either a b, s, w)
fixs' s (Left a) = (Left a, s, mempty)
fixs' _ (Right (b, s, w)) = (Right b, s, w)

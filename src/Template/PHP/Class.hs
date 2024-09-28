{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

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

import Data.Set (Set)
import qualified Data.Set as E

import Template.PHP.Types
import Template.PHP.Error
import Template.PHP.State
import Template.PHP.Interface


class MonadPlus m => MonadScanner errT m | m -> errT where
  parseError :: ScanError errT -> m a

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

  getParserState :: m (ScanState e)

  updateParserState :: (ScanState e -> ScanState e) -> m ()

  mkScanner :: (ScanState e -> Reply e a) -> m a



instance (MonadScanner errT m) => MonadScanner errT (IdentityT m) where
  parseError e = lift (parseError e)
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
  getParserState = lift getParserState
  updateParserState f = lift $ updateParserState f
  mkScanner f = lift (mkScanner f)



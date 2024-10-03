{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Template.PHP.Debug where

import Control.Monad.Identity (IdentityT, mapIdentityT)
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import qualified Control.Monad.Trans.Reader as L
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import Data.Bifunctor (Bifunctor (first))
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Set as E
import Debug.Trace

import Template.PHP.Class
import Template.PHP.Error
import Template.PHP.Scanner (ScannerT (..))
import Template.PHP.Interface (Hints(..))
import Template.PHP.State (ScanState(..))
import Control.Lens (concatMapOf)
import Template.PHP.Print (showNode, showNodeCap)
import Template.PHP.Types (NodeEntry (..), showRange)


class (MonadScanner errT m) => ScannerDebug errT m where
  debug :: (Show a) => String -> m a -> m a

instance (Show st, ScannerDebug errT m) => ScannerDebug errT (L.StateT st m) where
  debug msg sma = L.StateT $ \st ->
    debugWithComment "STATE" msg $ L.runStateT sma st

instance (ScannerDebug errT m) => ScannerDebug errT (L.ReaderT r m) where
  debug = L.mapReaderT . debug

instance (Monoid w, Show w, ScannerDebug errT m) => ScannerDebug errT (L.WriterT w m) where
  debug msg wma = L.WriterT $ debugWithComment "LOG" msg $ L.runWriterT wma

instance (Monoid w, Show w, ScannerDebug errT m) => ScannerDebug errT (S.WriterT w m) where
  debug msg wma = S.WriterT $ debugWithComment "LOG" msg $ S.runWriterT wma

instance (Monoid w, Show w, Show st, ScannerDebug errT m) => ScannerDebug errT (L.RWST r w st m) where
  debug msg wma = L.RWST $ \r state -> do
    let
      smth = (\(a, st, w) -> ShowComment "LOG" (ShowComment "STATE" (a, st), w))
              <$> L.runRWST wma r state
    ((a, st), w) <- first unComment . unComment <$> debug msg smth
    pure (a, st, w)


instance (Monoid w, Show w, Show st, ScannerDebug errT m) => ScannerDebug errT (S.RWST r w st m) where
  debug msg sma = S.RWST $ \r state -> do
    let
      smth = (\(a, st, w) -> ShowComment "LOG" (ShowComment "STATE" (a, st), w))
              <$> S.runRWST sma r state
    ((a, st), w) <- first unComment . unComment <$> debug msg smth
    pure (a, st, w)

instance (ScannerDebug errT m) => ScannerDebug errT (IdentityT m) where
  debug = mapIdentityT . debug


debugWithComment :: (Show a, Show c, ScannerDebug errT m) => String -> String -> m (a, c) -> m (a, c)
debugWithComment label message ma =
  unComment <$> debug message (ShowComment label <$> ma)

data ShowComment msgT valT = ShowComment String (valT, msgT)

unComment :: ShowComment msgT valT -> (valT, msgT)
unComment (ShowComment _ val) = val

instance (Show msgT, Show valT) => Show (ShowComment msgT valT) where
  show (ShowComment label (val, msg)) = show val <> " (" <> show label <> ": " <> show msg <> ")"


instance (Monad m, ShowErrorComponent e) => ScannerDebug e (ScannerT e m)
  where
  debug lbl p = ScannerT $ \state cok cerr eok eerr ->
    let l = debugLog lbl
        unfold = take 40
        cok' x updSt hs =
          flip trace (cok x updSt hs) $
            l (DbgIn (unfold state.inputs))
              ++ l (DbgCOK (take (updSt.parsed - state.parsed) state.inputs) x hs)
        cerr' err updSt =
          flip trace (cerr err updSt) $
            l (DbgIn (unfold state.inputs))
              ++ l (DbgCERR (take (updSt.parsed - state.parsed) state.inputs) err)
        eok' x updSt hs =
          flip trace (eok x updSt hs) $
            l (DbgIn (unfold state.inputs))
              ++ l (DbgEOK (take (updSt.parsed - state.parsed) state.inputs) x hs)
        eerr' err updSt =
          flip trace (eerr err updSt) $
            l (DbgIn (unfold state.inputs))
              ++ l (DbgEERR (take (updSt.parsed - state.parsed) state.inputs) err)
    in scannerT p state cok' cerr' eok' eerr'


data DbgItem e a
  = DbgIn [NodeEntry]
  | DbgCOK [NodeEntry] a Hints
  | DbgCERR [NodeEntry] (ScanError e)
  | DbgEOK [NodeEntry] a Hints
  | DbgEERR [NodeEntry] (ScanError e)


-- | Render a single piece of debugging info.
debugLog :: forall e a. (ShowErrorComponent e, Show a) =>
  -- | Debugging label
  String ->
  -- | Information to render
  DbgItem e a ->
  -- | Rendered result
  String
debugLog lbl item = prefix msg
  where
    prefix = unlines . fmap ((lbl <> "> ") <>) . lines
    showHints :: E.Set (ErrorItem NodeEntry) -> String
    showHints hints = "[" <> intercalate "," (showErrorItem `map` E.toAscList (E.take 5 hints))
            <> if length hints > 5 then " ...]" else "]"
    msg = case item of
      DbgIn nodes ->
        case nodes of
          [] -> "IN: <empty>"
          [ hNode ] -> "IN: " <> hNode.name <> " " <> showRange hNode.start hNode.end <> showChildren hNode.children
          hNode : rest -> "IN: " <> hNode.name <> " " <> showRange hNode.start hNode.end <> showChildren hNode.children
            <> case rest of
              [ rNode ] -> ", " <> rNode.name <> "."
              hr : rr -> ", " <> hr.name <> " ..."
          -- fst (showNodeCap 0 0 nodes)
      DbgCOK nodes a (Hints hints) ->
        "MATCH (COK): "
          ++ fst (showNodeCap 0 0 nodes)
          ++ "\nVALUE: "
          ++ case show a of
            'n' : 'e' : rest -> "ne: " <> take 80 rest
            _ -> show a
          ++ "\nHINTS: "
          ++ showHints hints
      DbgCERR nodes e ->
        "MATCH (CERR): " ++ fst (showNodeCap 0 0 nodes) ++ "\nERROR:\n" ++ parseErrorPretty e
      DbgEOK nodes a (Hints hints) ->
        "MATCH (EOK): "
          ++ fst (showNodeCap 0 0 nodes)
          ++ "\nVALUE: "
          ++ case show a of
            'n' : 'e' : rest -> "ne: " <> take 80 rest
            o -> show a -- take 160 o <> "..."
          ++ "\nHINTS: "
          ++ showHints hints
      DbgEERR nodes e ->
        "MATCH (EERR): " ++ fst (showNodeCap 0 0 nodes) ++ "\nERROR:\n" ++ parseErrorPretty e
    showChildren :: [NodeEntry] -> String
    showChildren = \case
      [] -> ""
      [ cNode ] -> " > [" <> cNode.name
      hc : rc -> " > [" <> hc.name <> " ...]"

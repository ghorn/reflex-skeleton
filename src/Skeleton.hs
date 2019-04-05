{-# OPTIONS_GHC -Wall -Wextra -Werror #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language PackageImports #-}
{-# Language RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- If you want to use `_` in constraint lists (as mentioned below), you need
-- both these pragmas
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-simplifiable-class-constraints #-}

-- | Skeleton to convert to reflex.
module Skeleton
       ( GuiInputEvent(..)
       , GuiState(..)
       , runSkeleton
       ) where

import Control.Concurrent.Chan ( Chan, readChan, newChan )
import Control.Concurrent ( forkIO, threadDelay, yield )
import Control.Monad ( forever, void, forM, forM_ )
import Control.Monad.IO.Class ( liftIO )
import Data.Dependent.Sum ( DSum (..) )
import Data.Functor.Identity ( Identity (..) )
import Data.IORef ( readIORef )
import Data.Maybe ( catMaybes )
import "gtk3" Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )

-- General Reflex primitives
import Reflex.Class

-- The widget-side interface for calling performEvent
import Reflex.PerformEvent.Class

-- The host-side interface for actually running actions gathered via
-- `performEvent`
import Reflex.PerformEvent.Base

-- Functions for dealing with wallclock time
import Reflex.Time ( throttle )

-- The widget-side interface for creating triggerable events
import Reflex.TriggerEvent.Class

-- The host-side interface for implementing triggerable events
import Reflex.TriggerEvent.Base

-- The `Spider` Reflex engine
import Reflex.Spider ( runSpiderHost, SpiderTimeline, SpiderHost, Spider )

-- Some internals from `Spider`.  I should expose these differently, so that
-- it's not necessary to import any internal modules.
import Reflex.Spider.Internal ( HasSpiderTimeline, SpiderHostFrame, Global )

newtype GuiInputEvent = GuiInputEvent Int deriving Show
data GuiState = GuiState (Maybe GuiInputEvent) (Maybe String) deriving Show

-- | Monads that support building GTK widgets
class Monad m => MonadGtk m where
  -- | Lift to a context where running GTK commands is OK, as with Gtk.postGUI*
  --TODO: Could this be more constrained than IO?
  liftGtk :: IO a -> m a

makeStatusLabel
  :: ( -- `t` is a Reflex "timeline" - an instance of some specific Reflex engine
       Reflex t
       -- `m` is capable of reading temporal data in the timeline `t`; this
       -- means that an action in `m` exists at a specific moment in time
     , MonadSample t m
       -- `m` can manipulate GTK objects
     , MonadGtk m
       -- `m` can request that actions of type `Performable m` be performed
       -- based on `Event`s in timeline `t`.  Note that all
       -- simultaneously-requested actions are gathered in a predictable order
       -- and executed simultaneously; the propagation order within the Reflex
       -- engine has no effect on the outcome
     , PerformEvent t m
       -- Actions requested using `PerformEvent m` can manipulate GTK objects
     , MonadGtk (Performable m)
     ) => Dynamic t GuiState -> m Gtk.Label
makeStatusLabel state = do
  statusLabel <- liftGtk $ Gtk.labelNew (Nothing :: Maybe String)
  let statusMessage = ffor state $ \currentState ->
        let msg = "The current state is: " <> show currentState
        in "Welcome to the skeleton!\n" ++ msg

  -- Set the initial label
  initialStatusMessage <- sample $ current statusMessage --NOTE: This style is actually discouraged, because it has a tendency to cause cycles in large programs; I'll get into the reasons and explain how we've mitigated it (really effectively) in reflex-dom
  liftGtk $ Gtk.labelSetText statusLabel initialStatusMessage

  -- Change the label whenever the state has changed
  performEvent_ $ liftGtk . Gtk.labelSetText statusLabel <$> updated statusMessage

  pure statusLabel



makeTextEntry
  :: ( -- `m` can create `Event`s in `t` that can be triggered from IO
       TriggerEvent t m
       -- If we get tired of writing constraints, we can elide them using
       -- PartialTypeSignatures.  This is a style I'm considering using more
       -- heavily.  The more traditional option is to define one or more
       -- pre-packaged sets of constraints, such as `MonadMyApp`, which includes
       -- everything your widgets typically need
     , _
     )
  => m (Gtk.Entry, Event t (Maybe String))
makeTextEntry = do
  -- text entry
  entry <- liftGtk Gtk.entryNew
  liftGtk $ Gtk.set entry
    [ Gtk.entryEditable := True
    , Gtk.widgetSensitive := True
    ]

  -- This part simulates a worker thread doing a slow action then updating the state
  -- in this implementation order is not guaranteed, which is a problem.
  -- This is one of the key things we need in a reflex implementation.

  -- Note: this pattern of using `newTriggerEvent` and then `on` to bind the
  -- trigger will be extremely common, so we'll definitely want it to be
  -- packaged up nicely for re-use
  (requestStartComputation, triggerStartComputation) <- newTriggerEvent
  -- text update event
  void $ liftGtk $ on entry Gtk.entryActivate $ do
    putStrLn "entryActivate"
    -- get text
    txt <- Gtk.get entry Gtk.entryText :: IO String
    triggerStartComputation txt

  -- Only run the computation at most once per second
  startComputation <- throttle (1 {- second -}) requestStartComputation

  computationFinished <- performEventAsync $ ffor startComputation $ \txt done -> do
    -- run "slow" work in thread
    void $ liftIO $ forkIO $ do
      putStrLn $ "got text entry event (" <> txt <> "), simulating a worker thread..."
      threadDelay 2000000 -- 2 second delay
      putStrLn "worker thread finished doing work, updating state"

      done txt

  let stateUpdate = leftmost
        [ Nothing <$ startComputation
        , Just <$> computationFinished
        ]

  pure (entry, stateUpdate)


makeCommitButton :: _ => m (Gtk.Button, Event t ())
makeCommitButton = do
  commitButton <- liftGtk $ Gtk.buttonNewWithLabel "commit"
  liftGtk $ Gtk.widgetSetTooltipText commitButton
    (Just "SET ME SET ME GO HEAD DO IT COME ON SET ME")

  (commit, triggerCommit) <- newTriggerEvent
  _ <- liftGtk $ on commitButton Gtk.buttonActivated $ do
    putStrLn "commit pressed"
    triggerCommit ()

  pure (commitButton, commit)

mainWindow :: _ => Event t GuiInputEvent -> (GuiState -> IO ()) -> m Gtk.Window
mainWindow inputEvent outputAction = do
  rec stateString <- holdDyn Nothing setStateString

      -- Does this still need to be stateful at all?  I don't think we actually use
      -- it statefully.  Note that we've extracted the `Just` to here, so now it's
      -- obvious from examining this one line of code that `stateEvent` will
      -- transition from `Nothing` to `Just` and then never return to `Nothing`.  We
      -- could even create a newtype of `Dynamic` that enforces that law if we
      -- wanted.
      stateEvent <- holdDyn Nothing $ Just <$> inputEvent

      -- Equivalent to the old `stateRef`
      let state = GuiState <$> stateEvent <*> stateString

      -- commit button
      (commitButton, commit) <- makeCommitButton
      -- I've separated out the logic of what action the commit button triggers
      -- from its own internal logic here; however, if it makes more sense that
      -- the commit button do this part itself, this code can easily be pushed
      -- inside it.  Typically, I find that constraining the effects of each
      -- function as tightly as possible is a good default approach.
      --
      -- Note that if the commit event is simultaneous with a change to the
      -- state, this will pick up the *pre*-change value.  It's also possible to
      -- choose the *post*-change value, but it's not possible to avoid
      -- expressing a preference.
      performEvent_ $ liftIO . outputAction <$> current state <@ commit

      -- current status label
      statusLabel <- makeStatusLabel state

      -- text entry label
      (textEntry, setStateString) <- makeTextEntry

  -- vbox to hold the widgets
  liftGtk $ do
    vbox <- Gtk.vBoxNew False 4
    Gtk.set vbox
      [ Gtk.containerChild := statusLabel
      , Gtk.boxChildPacking statusLabel := Gtk.PackNatural

      , Gtk.containerChild := commitButton
      , Gtk.boxChildPacking commitButton := Gtk.PackNatural

      , Gtk.containerChild := textEntry
      , Gtk.boxChildPacking textEntry := Gtk.PackNatural
      ]

    -- main window
    win <- Gtk.windowNew
    _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                     , Gtk.windowTitle := "skeleton"
                     ]

    -- add widget to window and show
    void $ Gtk.set win [ Gtk.windowTitle := "skeleton 2000"
                       , Gtk.containerChild := vbox
                       ]

    return win

-- | fire up the the GUI
runSkeleton :: Chan GuiInputEvent -> (GuiState -> IO ()) -> IO ()
runSkeleton inputEventStream outputAction = do
  _ <- Gtk.initGUI
  _ <- Gtk.timeoutAddFull (yield >> return True) Gtk.priorityDefault 50

  -- This isn't just inputEventStream, because it also includes events triggered
  -- internally to the program, e.g. by newTriggerEvent
  asyncEvents <- newChan

  -- Start the main Reflex engine, which is called `Spider`.  `fireCommand`
  -- comes from `hostPerformEventT`.
  ((win, triggerInputEvent), fireCommand) <- runSpiderHost $ do

    -- Add a layer to the monad stack for processing synchronous actions from
    -- `Event`s, e.g. from performEvent_
    hostPerformEventT $ do

      -- Add a layer to the monad stack for creating `Event`s to be triggered
      -- from IO later
      flip runTriggerEventT asyncEvents $ do

        -- Create an `Event` for our external input events
        (inputEvent, triggerInputEvent) <- newTriggerEvent

        -- Instantiate the main widget
        win <- mainWindow inputEvent outputAction

        return (win, triggerInputEvent)

  -- Forks a thread to process events through the Reflex engine as they come in
  processAsyncEvents asyncEvents fireCommand

  -- worker thread reading from input channel and trigger the corresponding `Event`
  _ <- forkIO $ forever $ do
    eventValue <- readChan inputEventStream
    -- putStrLn $ "Got input event: " <> show eventValue
    triggerInputEvent eventValue

  Gtk.widgetShowAll win
  Gtk.mainGUI

-- | These instances only work in this program because we know we only invoke
-- runSpiderHost from the GTK thread; to be more safe, we'd want to wrap
-- SpiderHost with a newtype that witnesses this fact
instance HasSpiderTimeline x => MonadGtk (TriggerEventT (SpiderTimeline x) (PerformEventT (SpiderTimeline x) (SpiderHost x))) where
  liftGtk = liftIO

-- | See the note on the other MonadGtk instance
instance HasSpiderTimeline x => MonadGtk (SpiderHostFrame x) where
  liftGtk = liftIO

-- | Lifted from https://github.com/reflex-frp/reflex-dom/blob/c94228143318cad36e145293f7aa3e2d802785f9/reflex-dom-core/src/Reflex/Dom/Main.hs#L291-L300
-- We can expose this from a more sensible place.

type EventChannel t = Chan [DSum (EventTriggerRef t) TriggerInvocation]

processAsyncEvents :: EventChannel Spider -> FireCommand Spider (SpiderHost Global) -> IO ()
processAsyncEvents events (FireCommand fire) = void $ forkIO $ forever $ do
  ers <- readChan events
  _ <- Gtk.postGUISync $ runSpiderHost $ do
    mes <- liftIO $ forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
      me <- readIORef er
      return $ fmap (\e -> e :=> Identity a) me
    _ <- fire (catMaybes mes) $ return ()
    liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
  return ()

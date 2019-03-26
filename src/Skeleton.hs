{-# OPTIONS_GHC -Wall -Wextra -Werror #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# Language PackageImports #-}

-- | Skeleton to convert to reflex.
module Skeleton
       ( GuiInputEvent(..)
       , GuiState(..)
       , runSkeleton
       ) where

import Control.Concurrent.Chan ( Chan, readChan )
import Control.Concurrent ( forkIO, threadDelay, yield )
import Control.Monad ( forever, void )
import Data.IORef ( IORef, modifyIORef, newIORef, readIORef )
import "gtk3" Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )


newtype GuiInputEvent = GuiInputEvent Int deriving Show
data GuiState = GuiState (Maybe GuiInputEvent) (Maybe String) deriving Show

makeStatusLabel :: IORef GuiState -> IO Gtk.Label
makeStatusLabel stateRef = do
  statusLabel <- Gtk.labelNew (Nothing :: Maybe String)
  let makeStatusMessage = do
        currentState <- readIORef stateRef
        let msg = "The current state is: " <> show currentState
        return $ "Welcome to the skeleton!\n" ++ msg

      -- Currently we just poll every 300ms, but ideally this would be responding to
      -- an event and updating only when necessary.
      statusLabelWorker = forever $ do
        threadDelay 300000
        msg <- makeStatusMessage
        Gtk.postGUISync $ Gtk.labelSetText statusLabel msg

  _ <- forkIO statusLabelWorker

  pure statusLabel



makeTextEntry :: IORef GuiState -> IO Gtk.Entry
makeTextEntry stateRef = do
  -- text entry
  entry <- Gtk.entryNew
  Gtk.set entry
    [ Gtk.entryEditable := True
    , Gtk.widgetSensitive := True
    ]

  -- text update event
  void $ on entry Gtk.entryActivate $ do
    -- This part simulates a worker thread doing a slow action then updating the state
    -- in this implementation order is not guaranteed, which is a problem.
    -- This is one of the key things we need in a reflex implementation.

    -- clear current state since it is no longer valid
    modifyIORef stateRef (\(GuiState k _) -> GuiState k Nothing)

    -- get text
    txt <- Gtk.get entry Gtk.entryText :: IO String

    -- run "slow" work in thread
    void $ forkIO $ do
      putStrLn $ "got text entry event (" <> txt <> "), simulating a worker thread..."
      threadDelay 2000000 -- 2 second delay
      putStrLn "worker thread finished doing work, updating state"

      -- update state after work is done
      Gtk.postGUISync $
        modifyIORef stateRef (\(GuiState k _) -> GuiState k (Just txt))

  pure entry


makeCommitButton :: IORef GuiState -> (GuiState -> IO ()) -> IO Gtk.Button
makeCommitButton stateRef outputAction = do
  commitButton <- Gtk.buttonNewWithLabel "commit"
  Gtk.widgetSetTooltipText commitButton
    (Just "SET ME SET ME GO HEAD DO IT COME ON SET ME")

  _ <- on commitButton Gtk.buttonActivated $ do
    putStrLn "commit pressed"
    readIORef stateRef >>= outputAction

  pure commitButton

-- | fire up the the GUI
runSkeleton :: Chan GuiInputEvent -> (GuiState -> IO ()) -> IO ()
runSkeleton inputEventStream outputAction = do
  _ <- Gtk.initGUI
  _ <- Gtk.timeoutAddFull (yield >> return True) Gtk.priorityDefault 50

  -- state IORef
  stateRef <- newIORef (GuiState Nothing Nothing)

  -- commit button
  commitButton <- makeCommitButton stateRef outputAction

  -- current status label
  statusLabel <- makeStatusLabel stateRef

  -- text entry label
  textEntry <- makeTextEntry stateRef

  -- vbox to hold the widgets
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox
    [ Gtk.containerChild := statusLabel
    , Gtk.boxChildPacking statusLabel := Gtk.PackNatural

    , Gtk.containerChild := commitButton
    , Gtk.boxChildPacking commitButton := Gtk.PackNatural

    , Gtk.containerChild := textEntry
    , Gtk.boxChildPacking textEntry := Gtk.PackNatural
    ]

  -- worker thread reading from input channel and update state IOVar
  _ <- forkIO $ forever $ do
    eventValue <- readChan inputEventStream
    -- putStrLn $ "Got input event: " <> show eventValue
    Gtk.postGUIAsync $ modifyIORef stateRef (\(GuiState _ txt) -> GuiState (Just eventValue) txt)


  -- main window
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "skeleton"
                   ]

  -- add widget to window and show
  _ <- Gtk.set win [ Gtk.windowTitle := "skeleton 2000"
                   , Gtk.containerChild := vbox
                   ]
  Gtk.widgetShowAll win
  Gtk.mainGUI

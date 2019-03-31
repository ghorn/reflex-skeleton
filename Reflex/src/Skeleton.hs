{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS -Wall -Wextra #-}
module Skeleton where

import Reflex.Dom
import Data.Functor (($>))
import Control.Monad ( forever, void )
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent as CC
import Control.Concurrent.Chan ( Chan, readChan )

import qualified Data.Text as Text

newtype GuiInputEvent = GuiInputEvent Int deriving Show
data GuiState = GuiState (Maybe GuiInputEvent) (Maybe String) deriving Show

-- Done
makeStatusLabel :: MonadWidget t m => Dynamic t GuiState -> m ()
makeStatusLabel currentStatus = do
  dynText $ do
    currentState <- currentStatus
    let msg = "The current state is: " <> Text.pack (show currentState)
    pure $ "Welcome to the skeleton!\n" <> msg

-- Done
makeTextEntry :: MonadWidget t m => ((GuiState -> GuiState) -> IO ()) -> m ()
makeTextEntry postEvent = do
  entry <- textInput def

  let
    activate = keypress Enter entry

    activateContent = current (value (entry)) <@ activate
    action (Text.unpack -> txt) = void . liftIO . CC.forkIO $ do
      postEvent $ (\(GuiState k _) -> GuiState k Nothing) -- on text entry click, we just clear the status which is not yet valid
      putStrLn $ "got text entry event (" <> txt <> "), simulating a worker thread..."
      CC.threadDelay 2000000 -- 2 second delay
      putStrLn "worker thread finished doing work, updating state"

      postEvent $ \(GuiState k _) -> GuiState k (Just txt)
  
  performEvent_ $ (action <$> activateContent)

-- Done
makeCommitButton :: MonadWidget t m => Behavior t GuiState -> (GuiState -> IO ()) -> m ()
makeCommitButton currentGuiState outputAction = do
  -- TODO: Alt text
  commitButton <- button "commit"

  let
    clickStatus = currentGuiState <@ commitButton

    -- NOTE: all this crap with performEvent is just here because we want to do some
    -- debug using putStrLn and outputAction (which are by definition not "FRP" ready ;)
    action status = liftIO $ do
     putStrLn "commit pressed"
     outputAction status

  performEvent_ (action <$> clickStatus)

runSkeleton :: Chan GuiInputEvent -> (GuiState -> IO ()) -> IO ()
runSkeleton inputChannel outputAction = mainWidget $ mdo
  -- Low level reflex primitives to create the Event
  -- updateEVent will be fired by postEvent
  (updateEvent, postEvent) <- newTriggerEvent

  -- worker thread reading from input channel and pushing updates to state var
  -- updates are pushed as modifying functions

  -- This is the worker thread which pushs events
  void . liftIO . CC.forkIO . forever $ do
    eventValue <- readChan inputChannel
    postEvent $ (\(GuiState _ txt) -> GuiState (Just eventValue) txt)

  -- currentStatus is a fold of all the event modification functions
  currentStatus <- foldDyn ($) (GuiState Nothing Nothing) updateEvent

  el "div" $ makeCommitButton (current currentStatus) outputAction
  el "div" $ makeStatusLabel currentStatus
  el "div" $ makeTextEntry postEvent

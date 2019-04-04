module RunSkeleton ( main ) where

import Control.Concurrent.Chan ( Chan, newChan, writeChan )
import qualified Control.Concurrent as CC

import Skeleton ( GuiInputEvent(..), GuiState, runSkeleton )


inputChanDummyWorker :: Chan GuiInputEvent -> IO ()
inputChanDummyWorker chan =
  let go :: Int -> IO ()
      go k = do
        CC.threadDelay 500000
        writeChan chan (GuiInputEvent k)
        go (k + 1)
  in go 0


main :: IO ()
main = do
  inputStreamChannel <- newChan :: IO (Chan GuiInputEvent)
  _ <- CC.forkIO (inputChanDummyWorker inputStreamChannel)

  let outputAction :: GuiState -> IO ()
      outputAction guiState = putStrLn $ "Output event! " <> show guiState
      
  runSkeleton inputStreamChannel outputAction

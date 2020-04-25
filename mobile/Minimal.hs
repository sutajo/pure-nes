module Main where


import Control.Concurrent.STM
import Control.Concurrent.Chan
import System.Environment
import Emulator.Window
import Communication


main :: IO ()
main = do
    dummy          <- newTChanIO
    sdlEvents      <- newChan
    let comms = CommResources { 
                toEmulatorWindow   = dummy, 
                fromEmulatorWindow = sdlEvents 
              }
    (path : _) <- getArgs

    runEmulatorWindow path comms
    
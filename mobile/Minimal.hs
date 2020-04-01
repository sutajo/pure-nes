module Main where


import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.Chan
import System.Environment
import Emulator.Window
import Communication


main :: IO ()
main = do
    gtkMessages    <- newTChanIO
    sdlEvents      <- newChan
    let comms = CommResources { 
                toSDLWindow   = gtkMessages, 
                fromSDLWindow = sdlEvents 
              }
    (path : _) <- getArgs

    runEmulatorWindow path comms
    
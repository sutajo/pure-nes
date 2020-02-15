{-# LANGUAGE OverloadedLabels, OverloadedLists, OverloadedStrings, FlexibleContexts, NamedFieldPuns, ScopedTypeVariables, RecordWildCards #-}

module Main where

import           Control.Monad                  
import           Control.Monad.IO.Class
import           Control.Concurrent       hiding (yield)
import           Control.Concurrent.STM
import qualified Data.Text                     as Text
import           GI.Gtk                         ( Box(..)
                                                , Button(..)
                                                , FileChooserButton(..)
                                                , Label(..)
                                                , Orientation(..)
                                                , Window(..)
                                                , fileChooserGetFilename
                                                )
import           GI.Gtk.Enums
import           GI.Gtk.Declarative
import           GI.Gtk.Objects.Image(Image(..))
import           GI.Gtk.Declarative.App.Simple as DAS
import           Communication
import           Pipes
import           SDLWindow

data State = Started (Maybe FilePath)
           | Message Text.Text
           | Emulating

view' :: State -> AppView Window Event
view' s = do
  let 
    title = case s of
      Message _ -> "Message"
      _         -> "Pure-Nes Menu"
  bin
      Window
      [ #title := title
      , on #deleteEvent (const (True, Closed))
      ]
    $ windowContent s
    
windowContent s = case s of
    Message text -> 
      container Box
      [#orientation := OrientationVertical, #valign := AlignCenter, #margin := 10 ]
      [
        BoxChild defaultBoxChildProperties { padding = 15 } $ 
          widget Label [#label := text]
      , BoxChild defaultBoxChildProperties { padding = 15 } $ 
          widget Button [#label := "Ok", on #clicked MessageAck]
      ]
    Emulating ->
      container Box
      [#orientation := OrientationVertical, #valign := AlignCenter, #margin := 10 ]
      [
        BoxChild defaultBoxChildProperties { padding = 15 } $ 
          widget Image [ #file := "resources/GUI/back.png"]
      , BoxChild defaultBoxChildProperties { padding = 15 } $ 
          widget Button
          [ 
            #label := "Return to ROM selection"
          , on #clicked CloseEmulator
          ]
      ]
    Started currentFile -> 
      container Box
      [#orientation := OrientationVertical]
      [ 
        BoxChild defaultBoxChildProperties { padding = 10 } $ 
          widget Image [#file := "resources/GUI/logo2.png"]
      , BoxChild defaultBoxChildProperties { padding = 15 } $ 
          widget Label
          [#label := maybe "Please select the ROM you wish to run." ((Text.append "...") . Text.takeEnd 30 . Text.pack) currentFile]
      , BoxChild defaultBoxChildProperties { padding = 10 } $ 
          widget FileChooserButton
          [ onM #selectionChanged (fmap FileSelectionChanged . fileChooserGetFilename)]
      , container Box
        [#orientation := OrientationHorizontal, #halign := AlignCenter, #margin := 10 ]
        [
          BoxChild defaultBoxChildProperties { padding = 15 } $ 
            widget Button [#label := "Start Emulator", on #clicked StartEmulator]
        ]
      ]

launchEmulator :: FilePath -> CommResources -> IO ()
launchEmulator path comms = do
 toSDLWindow' <- atomically $ dupTChan (toSDLWindow comms)
 void . forkOS $ runEmulatorWindow path comms { toSDLWindow = toSDLWindow' }


update :: CommResources -> State -> Event -> Transition State Event
update _ (Started _) (FileSelectionChanged p) 
  = Transition (Started p) (return Nothing)
update _  _ Closed = Exit
update comms s@(Started (Just path)) StartEmulator 
  = Transition Emulating (just $ launchEmulator path comms)
update _ s@(Started Nothing) StartEmulator
  = Transition (Message "No ROM selected.") noop
update _ (Message _) MessageAck = Transition (Started Nothing) noop
update comms Emulating CloseEmulator 
  = Transition (Started Nothing) (just . atomically $ writeTChan (toSDLWindow comms) Stop)
update _ Emulating SDLWindowClosed = Transition (Started Nothing) noop
update _ _ (ErrorReport msg) = Transition (Message $ Text.pack msg) noop
update _ s _ = Transition s noop

noop = pure Nothing
just x = do x; noop



main :: IO ()
main = do
    gtkMessages    <- newBroadcastTChanIO
    sdlEvents      <- newChan
    let comms = CommResources { 
                toSDLWindow   = gtkMessages, 
                fromSDLWindow = sdlEvents 
              }
    let sdlWindowEventProxy = forever $ liftIO (readChan sdlEvents) >>= yield 
    void $ run App {    view         = view'
                      , DAS.update   = Main.update comms
                      , inputs       = [sdlWindowEventProxy]
                      , initialState = Started Nothing
                    }
                      
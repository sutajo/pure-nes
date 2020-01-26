{-# LANGUAGE OverloadedLabels, OverloadedLists, OverloadedStrings, FlexibleContexts, NamedFieldPuns, ScopedTypeVariables #-}

module Main where

import           Control.Monad                  ( void )
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import qualified Data.Text                     as Text
import           Data.Int
import           Data.Vector                    ( Vector )
import           GI.Gtk                         ( Box(..)
                                                , Button(..)
                                                , FileChooserButton(..)
                                                , Label(..)
                                                , Orientation(..)
                                                , Window(..)
                                                , fileChooserGetFilename
                                                , constructFileChooserFilter
                                                , fileFilterNew
                                                , MenuBar(..)
                                                , MenuItem(..)
                                                )
import qualified GI.Gtk                        as Gtk
import qualified GI.GObject                    as GI
import           GI.Gtk.Enums
import           GI.Gtk.Declarative
import           GI.Gtk.Objects.Image(Image(..))
import           GI.Gtk.Declarative.App.Simple
import           SDLWindow

data State = Started (Maybe FilePath) 
           | Done FilePath
           | Message Text.Text
           | Emulating

data Event = FileSelectionChanged (Maybe FilePath) 
          |  Closed 
          |  Help 
          |  StartEmulator 
          |  ControllerConfig
          |  MessageAck
          | CloseEmulator


menuBar :: Widget Event
menuBar = 
  container
  MenuBar
  []
  [ subMenu
    "File"
    [
      menuItem MenuItem [on #activate Closed]
      $ widget Label [#label := "Kilépés"]
    ]
  ]

--view' :: State -> AppView Window Event
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
    Done path ->
      widget Label [#label := (Text.pack path <> " was selected.")]
    Message text -> 
      container
      Box
      [#orientation := OrientationVertical, #valign := AlignCenter, #margin := 10 ]
      [
        BoxChild defaultBoxChildProperties { padding = 15 }
        $ widget Label [#label := text]
      , BoxChild defaultBoxChildProperties { padding = 15 } $ widget
        Button
        [ #label := "I'll try again."
        , on #clicked MessageAck
        ]
      ]
    Emulating ->
      container
      Box
      [#orientation := OrientationVertical, #valign := AlignCenter, #margin := 10 ]
      [
        BoxChild defaultBoxChildProperties { padding = 15 } $ widget
        Image [ #file := "resources/back.png"]
      , BoxChild defaultBoxChildProperties { padding = 15 } $ widget
        Button
        [ #label := "Return to ROM selection"
        , on #clicked CloseEmulator
        ]
      ]
    Started currentFile -> container
      Box
      [#orientation := OrientationVertical]
      [ 
        BoxChild defaultBoxChildProperties { padding = 10, expand = False, fill = False } $ widget 
          Image 
          [ #file := "resources/logo2.png"
          ]
      , BoxChild defaultBoxChildProperties { padding = 15, expand = True, fill = True }
        $ widget
            Label
            [#label := maybe "Please select the ROM you wish to run." ((Text.append "...") . Text.takeEnd 30 . Text.pack) currentFile]
      , BoxChild defaultBoxChildProperties { padding = 10 } $ widget
        FileChooserButton
        [ onM #selectionChanged
              (fmap FileSelectionChanged . fileChooserGetFilename)
        ]
      , container
        Box
        [#orientation := OrientationHorizontal, #halign := AlignCenter, #margin := 10 ]
        [
          BoxChild defaultBoxChildProperties { padding = 15 } $ widget
          Button
          [ #label := "Start Emulator"
          , on #clicked StartEmulator
          ]
        ]
      ]

just x = x *> pure Nothing
noop = just (return ())

launchEmulator :: FilePath -> TChan ParentMessage -> IO ()
launchEmulator path existingChan = do
 chan <- atomically $ dupTChan existingChan
 runEmulator path chan

update' :: TChan ParentMessage -> State -> Event -> Transition State Event
update' child (Started _) (FileSelectionChanged p) =
  Transition (Started p) (return Nothing)
update' child _ Closed = Exit
update' child s@(Started (Just path)) StartEmulator = Transition Emulating (just $ forkIO (launchEmulator path child))
update' child s@(Started Nothing) StartEmulator = Transition (Message "No ROM selected.") noop
update' child (Message _) MessageAck = Transition (Started Nothing) noop
update' child Emulating CloseEmulator = Transition (Started Nothing) (just $ atomically $ writeTChan child Stop)
update' child s _      = Transition s noop

main :: IO ()
main = do
    chan <- newTChanIO
    void $ run App {    view         = view'
                      , update       = update' chan
                      , inputs       = []
                      , initialState = Started Nothing
                    }
                      
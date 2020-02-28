{-# LANGUAGE OverloadedLabels, OverloadedLists, OverloadedStrings, FlexibleContexts, NamedFieldPuns, TemplateHaskell, QuasiQuotes #-}

module Main where

import           Control.Monad                  
import           Control.Monad.IO.Class
import           Control.Concurrent       hiding (yield)
import           Control.Concurrent.STM
import           Data.Char (toUpper)
import           Data.Time
import           Data.List (intercalate)
import           Data.Version
import           Language.Haskell.TH
import           Data.Text (Text)
import qualified Data.Text                     as Text
import           GI.Gtk                         ( Box(..)
                                                , Button(..)
                                                , FileChooserButton(..)
                                                , Label(..)
                                                , Window(..)
                                                , fileChooserGetFilename
                                                )
import           GI.Gtk.Enums
import           GI.Gtk.Declarative
import           GI.Gtk.Objects.Image(Image(..))
import           GI.Gtk.Declarative.App.Simple as DAS
import           System.Info
import           Text.RawString.QQ
import           Communication
import           Pipes
import           System.FilePath.Posix
import           SDLWindow

data State = Started (Maybe FilePath)
           | Message Text
           | Emulating { romName :: Text, running :: Bool }

view' :: Int -> State -> AppView Window Event
view' threadCount s = do
  let 
    title = case s of
      Message _ -> "Message"
      _         -> "Pure-Nes Menu"
  bin
      Window
      [ #title := title
      , on #deleteEvent (const (True, Closed))
      , #heightRequest := 700
      , #widthRequest := 400
      ]
    $ windowContent s
  where
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
        Emulating romName running ->
          container Box
          [#orientation := OrientationVertical, #valign := AlignCenter, #margin := 10 ]
          [
            BoxChild defaultBoxChildProperties { padding = 15 } $ 
              widget Image [ #file := Text.append "resources/GUI/" (if running then "pause.png" else "play.png")]
          , BoxChild defaultBoxChildProperties { padding = 15 } $ 
              widget Button
              [ 
                #label := Text.append (if running then "Pause " else "Resume ") romName,
                on #clicked SwitchMode
              ]
          , BoxChild defaultBoxChildProperties { padding = 15 } $ 
              widget Image [ #file := "resources/GUI/save.png"]
          , BoxChild defaultBoxChildProperties { fill = True } $
              container Box [#orientation := OrientationHorizontal, #valign := AlignCenter, #marginTop := 15, #marginBottom := 15] $
              [
                BoxChild defaultBoxChildProperties $
                  widget Button
                  [ 
                    #label := "Save progress",
                    #marginLeft  := 5,
                    #marginRight := 10
                  ]
              , BoxChild defaultBoxChildProperties { fill = True } $ 
                widget FileChooserButton
                [#action := FileChooserActionSelectFolder, #expand := True, #createFolders := True]
              ]
          , BoxChild defaultBoxChildProperties { padding = 15 } $ 
              widget Image [ #file := "resources/GUI/reload.png"]
          , BoxChild defaultBoxChildProperties {fill = True} $
              container Box [#orientation := OrientationVertical, #valign := AlignCenter, #marginTop := 15, #marginBottom := 15] $
              [
                BoxChild defaultBoxChildProperties $
                  widget Label
                  [ 
                    #label := [r|<span size="larger" font_family="cursive">Load progress</span>|],
                    #useMarkup := True,
                    #marginBottom := 5
                  ]
              , BoxChild defaultBoxChildProperties {fill = True} $ 
                widget FileChooserButton [#action := FileChooserActionOpen, #expand := True]
              ]
          , BoxChild defaultBoxChildProperties { padding = 15 } $ 
              widget Image [ #file := "resources/GUI/back2.png"]
          , BoxChild defaultBoxChildProperties { padding = 15 } $ 
              widget Button
              [ 
                #label := "Return to ROM selection"
              , on #clicked CloseEmulator
              ]
          ]
        Started currentFile -> 
          container Box
          [#orientation := OrientationVertical, #valign := AlignCenter]
          [ 
            BoxChild defaultBoxChildProperties { padding = 10 } $ 
              widget Image [#file := "resources/GUI/logo2.png", #marginTop := 30, #marginBottom := 25, #halign := AlignCenter]
          , BoxChild defaultBoxChildProperties { padding = 15 } $ 
              widget Label
              [#label := maybe "Please select the ROM you wish to run." ((Text.append "...") . Text.takeEnd 30 . Text.pack) currentFile]
          , BoxChild defaultBoxChildProperties { padding = 10 } $ 
              widget FileChooserButton
              [ 
                onM #selectionChanged (fmap FileSelectionChanged . fileChooserGetFilename),
                #marginLeft  := 20,
                #marginRight := 20,
                #title := "Please select the ROM you wish to run."
              ]
          , container Box
            [#orientation := OrientationHorizontal, #halign := AlignCenter, #margin := 10 ]
            [
              BoxChild defaultBoxChildProperties { padding = 15 } $ 
                widget Button [#label := "Start Emulator", on #clicked StartEmulator]
            ]
          , BoxChild defaultBoxChildProperties $ 
              widget Label
              [
                #marginTop := 30,
                #label := "Supported mappers:",
                #halign := AlignCenter
              ]
          , BoxChild defaultBoxChildProperties $ 
              widget Label
              [
                #label := "Mapper 0"
              ]
          , BoxChild defaultBoxChildProperties $ 
              widget Label
              [
                #marginTop := 40,
                #label := ("Available OS threads: " `Text.append` (Text.pack (show threadCount)))
              ]
          , container Box
            [#orientation := OrientationVertical, #halign := AlignEnd, #marginRight := 10, #marginTop := 70, #marginBottom := 10 ]
            [
              BoxChild defaultBoxChildProperties $ 
                widget Label
                [
                  #label := Text.append "Compiler: " 
                    (Text.pack $ map toUpper compilerName ++ " " ++ intercalate "." (map show . versionBranch $ compilerVersion))
                ]
            , BoxChild defaultBoxChildProperties $ 
                widget Label
                [
                  #label := Text.append "Compilation date: " $(stringE =<< runIO ((show . utctDay) `fmap` getCurrentTime))
                ]
            ]
          ]

launchEmulator :: FilePath -> CommResources -> IO ()
launchEmulator path comms = do
 toSDLWindow' <- atomically $ dupTChan (toSDLWindow comms)
 void . forkOS $ runEmulatorWindow path comms { toSDLWindow = toSDLWindow' }


update :: CommResources -> State -> Event -> Transition State Event
update _ (Started _) (FileSelectionChanged p) 
  = Transition (Started p) (return Nothing)

update _  _ Closed 
  = Exit

update comms s@(Started (Just path)) StartEmulator 
  = Transition (Emulating (Text.pack . dropExtension . takeFileName $ path) True) (just $ launchEmulator path comms)

update _ s@(Started Nothing) StartEmulator
  = Transition (Message "No ROM selected.") noop

update _ (Message _) MessageAck 
  = Transition (Started Nothing) noop

update comms (Emulating _ _) CloseEmulator 
  = Transition (Started Nothing) (just . atomically $ writeTChan (toSDLWindow comms) Stop)

update CommResources{..} (Emulating rom running) SwitchMode
  = Transition (Emulating rom (not running)) (just . atomically $ writeTChan toSDLWindow Communication.Switch)

update _ (Emulating _ _) SDLWindowClosed 
  = Transition (Started Nothing) noop

update _ _ (ErrorReport msg) 
  = Transition (Message $ Text.pack msg) noop

update _ s _ = Transition s noop



noop = pure Nothing
just x = do x; noop


main :: IO ()
main = do
    threadCount    <- getNumCapabilities
    gtkMessages    <- newBroadcastTChanIO
    sdlEvents      <- newChan
    let comms = CommResources { 
                toSDLWindow   = gtkMessages, 
                fromSDLWindow = sdlEvents 
              }
    let sdlWindowEventProxy = forever $ liftIO (readChan sdlEvents) >>= yield 
    void $ run App {    view         = view' threadCount
                      , DAS.update   = Main.update comms
                      , inputs       = [sdlWindowEventProxy]
                      , initialState = Started Nothing
                    }
                      
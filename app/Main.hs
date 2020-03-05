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
import           GI.Gtk.Objects.Entry
import           GI.Gtk.Objects.Image(Image(..))
import           GI.Gtk.Interfaces.Editable
import           GI.Gtk.Declarative.App.Simple as DAS
import           System.Info
import           Text.RawString.QQ
import           Communication
import           Pipes
import           System.FilePath.Posix
import           SDLWindow

data State = Started (Maybe FilePath)
           | Message { text :: Text, stateAfterOk :: State }
           | Emulating { 
             romName :: Text, 
             running :: Bool,
             savePath :: Maybe FilePath, 
             loadPath :: FilePath, 
             saveRomName :: String,
             saveResultSuccess :: Maybe (Maybe String),
             loadResultSuccess :: Maybe (Maybe String)
            }

saveLabel = [r|  First, select a folder for your saves.
  Quicksaving creates an anonymous save 
  file that you can access with Quickload.
  You can create unique saves by giving it 
  a name and then hitting the save button. 
  |]

loadLabel = [r|  Quickload loads the quicksave file 
  located in the selected save folder (if one exists).
  You can load ordinary save files using the filechooser.
  |]

saveAsQuick = [r|
You can't name your save as 'quick' 
as it would overwrite the previous quicksave.
  |]

view' :: Int -> State -> AppView Window Event
view' threadCount s = do
  let 
    title = case s of
      Message _ _ -> "Message"
      _           -> "Pure-Nes Menu"
  bin
      Window
      [ #title := title
      , on #deleteEvent (const (True, Closed))
      , #heightRequest := 700
      , #widthRequest := 400
      , #windowPosition := WindowPositionCenter
      ]
    $ windowContent s
  where
    windowContent s = case s of
        Message text _ -> 
          container Box
          [#orientation := OrientationVertical, #valign := AlignCenter, #margin := 10 ]
          [
            BoxChild defaultBoxChildProperties { padding = 15 } $ 
              widget Label [#label := text]
          , BoxChild defaultBoxChildProperties { padding = 15 } $ 
              widget Button [#label := "Ok", on #clicked MessageAck]
          ]
        Emulating{..} ->
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
          , BoxChild defaultBoxChildProperties { padding = 15, expand = True, fill = True } $
              container Box [#orientation := OrientationHorizontal, #halign := AlignCenter] $
                case saveResultSuccess of
                  Nothing -> [BoxChild defaultBoxChildProperties $ widget Image [ #file := "resources/GUI/save.png"]]
                  Just result -> 
                    let 
                      img = case result of
                        Nothing  -> "tick.png"
                        Just err -> "cross.png"
                    in
                      [
                        BoxChild defaultBoxChildProperties $ 
                          widget Image [ #file := Text.append "resources/GUI/" img, #marginRight := 40]
                      , BoxChild defaultBoxChildProperties $ 
                          widget Image [ #file := "resources/GUI/save.png"]
                      ]
          , BoxChild defaultBoxChildProperties $
              widget Label
              [ 
                #label := saveLabel,
                #marginBottom := 5
              ]
          , BoxChild defaultBoxChildProperties { fill = True } $
              container Box [#orientation := OrientationHorizontal, #valign := AlignCenter, #marginTop := 15, #marginBottom := 15] $
              [
                BoxChild defaultBoxChildProperties $
                  widget Button
                  [ 
                    #label := "Quick save",
                    #marginLeft  := 30,
                    #marginRight := 10,
                    on #clicked QuickSavePressed
                  ]
              , BoxChild defaultBoxChildProperties { fill = True } $ 
                widget FileChooserButton
                [ onM #selectionChanged (fmap SavePathChanged . fileChooserGetFilename),
                  #action := FileChooserActionSelectFolder, #expand := True, #createFolders := True]
              ]
          , BoxChild defaultBoxChildProperties { fill = True } $
              container Box [#orientation := OrientationHorizontal, #valign := AlignCenter, #marginTop := 15, #marginBottom := 15] $
              [
                BoxChild defaultBoxChildProperties $
                  widget Button
                  [ 
                    #label := "Save progress",
                    #marginLeft  := 5,
                    #marginRight := 10,
                    on #clicked SaveButtonPressed
                  ]
              , BoxChild defaultBoxChildProperties { fill = True } $ 
                widget Entry
                  [ #expand := True, #placeholderText := "How should I call this save?",
                    onM #changed (\e -> SaveNameChanged . Text.unpack <$> editableGetChars e 0 (-1))]
              ]
          , BoxChild defaultBoxChildProperties { padding = 15, expand = True, fill = True } $
              container Box [#orientation := OrientationHorizontal, #halign := AlignCenter] $
                case loadResultSuccess of
                  Nothing -> [BoxChild defaultBoxChildProperties $ widget Image [ #file := "resources/GUI/reload.png"]]
                  Just result ->
                    let 
                      img = case result of
                        Nothing  -> "tick.png"
                        Just err -> "cross.png"
                    in 
                      [
                        BoxChild defaultBoxChildProperties $ 
                          widget Image [ #file := Text.append "resources/GUI/" img, #marginRight := 40]
                      , BoxChild defaultBoxChildProperties $ 
                          widget Image [ #file := "resources/GUI/reload.png"]
                      ]
          , BoxChild defaultBoxChildProperties {fill = True} $
              container Box [#orientation := OrientationVertical, #valign := AlignCenter, #marginTop := 5, #marginBottom := 15] $
              [
                BoxChild defaultBoxChildProperties $
                  widget Label
                  [ 
                    #label := loadLabel,
                    #marginBottom := 5
                  ]
              , BoxChild defaultBoxChildProperties { fill = True } $
                  container Box [#orientation := OrientationHorizontal, #valign := AlignCenter, #marginTop := 15, #marginBottom := 15] $
                  [
                    BoxChild defaultBoxChildProperties $
                      widget Button
                      [ 
                        #label := "Quick load",
                        #marginLeft  := 5,
                        #marginRight := 10,
                        on #clicked QuickReloadPressed
                      ]
                  , BoxChild defaultBoxChildProperties {fill = True} $ 
                      widget FileChooserButton 
                        [ onM #selectionChanged (fmap LoadPathChanged . fileChooserGetFilename),
                          #action := FileChooserActionOpen, #expand := True ]
                  ]
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

update CommResources{..} e@Emulating{saveRomName = ""} SaveButtonPressed 
  = Transition e (return . Just $ MessageText "You need to give a name to your save file.")

update CommResources{..} e@Emulating{savePath = Nothing} QuickSavePressed
  = Transition e (return . Just $ MessageText "You need to choose a save folder first.")

update CommResources{..} e@Emulating{saveRomName = "quick"} SaveButtonPressed 
  = Transition e (return . Just$ MessageText saveAsQuick)

update CommResources{..} e@Emulating{savePath = Just path, saveRomName = saveName } SaveButtonPressed
  = Transition e (just . atomically $ writeTChan toSDLWindow (Communication.SaveVM (path ++ "/" ++ saveName ++".purenes")))

update CommResources{..} e@Emulating{savePath = Just path} QuickSavePressed
  = Transition e (just . atomically $ writeTChan toSDLWindow (Communication.SaveVM (path ++ "/quick.purenes")))

update _ e@Emulating{} (SavePathChanged s)
  = Transition (e {savePath = s}) noop

update _ e@Emulating{} (SaveNameChanged s)
  = Transition e{saveRomName = s} noop

update _ e@Emulating{} (SaveError b)
  = Transition e{saveResultSuccess = Just $ b} noop

update _ e@Emulating{} (LoadError b)
  = Transition e{loadResultSuccess = Just $ b} noop

update CommResources{..} e@Emulating{ savePath = Nothing } QuickReloadPressed
  = Transition e (return . Just $ MessageText "You need to choose a save folder first.")

update CommResources{..} e@Emulating{ savePath = Just path } QuickReloadPressed
  = Transition e (just . atomically $ writeTChan toSDLWindow (Communication.LoadVM (path ++ "/quick.purenes")))

update CommResources{..} e@Emulating{} (LoadPathChanged (Just path))
  = Transition e {loadPath = path} (just . atomically $ writeTChan toSDLWindow (Communication.LoadVM path))

update _  _ Closed 
  = Exit

update comms s@(Started (Just path)) StartEmulator 
  = Transition (Emulating (Text.pack . dropExtension . takeFileName $ path) True Nothing "" "" Nothing Nothing) (just $ launchEmulator path comms)

update _ s@(Started Nothing) StartEmulator
  = Transition s (return . Just $ MessageText "No ROM selected.")

update _ (Message _ stateAfterOk) MessageAck 
  = Transition stateAfterOk noop

update comms (Emulating{..}) CloseEmulator 
  = Transition (Started Nothing) (just . atomically $ writeTChan (toSDLWindow comms) Stop)

update CommResources{..} e SwitchMode
  = Transition (e { running = not (running e) }) (just . atomically $ writeTChan toSDLWindow Communication.Switch)

update _ (Emulating{}) SDLWindowClosed 
  = Transition (Started Nothing) noop

update _ s (Error msg) 
  = Transition (Message (Text.pack msg) (Started Nothing)) noop

update _ s (MessageText msg) 
  = Transition (Message (Text.pack msg) s) noop

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
                      
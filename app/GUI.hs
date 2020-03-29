{-# LANGUAGE OverloadedLabels, OverloadedLists, OverloadedStrings, FlexibleContexts, NamedFieldPuns, TemplateHaskell, QuasiQuotes, GADTs #-}

module Main where

import           Control.Monad                  
import           Control.Monad.IO.Class
import           Control.Concurrent       hiding (yield)
import           Control.Concurrent.STM
import           Data.Char (toUpper)
import           Data.Time
import           Data.List (intercalate)
import           Data.Version
import qualified Data.Vector as V
import           Language.Haskell.TH
import           Data.Text (Text)
import qualified Data.Text                     as Text
import           GI.Gtk                         ( Box(..)
                                                , Button(..)
                                                , FileChooserButton(..)
                                                , Label(..)
                                                , Window(..)
                                                , Grid(..)
                                                , fileChooserGetFilename
                                                )
import           GI.Gtk.Enums
import           GI.Gtk.Declarative
import           GI.Gtk.Objects.Entry
import           GI.Gtk.Objects.Image(Image(..))
import           GI.Gtk.Interfaces.Editable
import           GI.Gtk.Declarative.App.Simple as DAS
import           GI.Gtk.Declarative.Container.Grid
import           System.Info
import           System.FilePath
import           Text.RawString.QQ
import           Communication
import           Pipes
import           Emulator.Window

data State = Started (Maybe FilePath)
           | Message { text :: Text, stateAfterOk :: State }
           | ShowControls
           | Emulating { 
             romName :: Text, 
             running :: Bool,
             savePath :: Maybe FilePath, 
             loadPath :: FilePath, 
             saveRomName :: String,
             saveResultSuccess :: Maybe IOResult,
             loadResultSuccess :: Maybe IOResult
            }
        
mkControlHelpBox controls =
  container 
  Grid
  [#orientation := OrientationVertical, #valign := AlignCenter, #marginBottom := 20] (V.fromList $ concat $ zipWith mkHelpRow [1..] controls)
  where
    orange x = [r|<span weight="bold" foreground="#eb660e">|] <> x <> [r|</span>|]
    blue x   = [r|<span weight="bold" foreground="#033b94">|] <> x <> [r|</span>|]
    mkHelpRow index (action, button) = [
        GridChild defaultGridChildProperties { leftAttach = 1, topAttach = index } $ 
          widget Label [#label := orange action, #useMarkup := True, #xalign := 0, #marginRight := 25, #marginBottom := 15]
      , GridChild defaultGridChildProperties { leftAttach = 2, topAttach = index } $ 
          widget Label [#label := blue button, #useMarkup := True, #xalign := 0, #marginBottom := 15]
      ]

mkResultTimeLabel :: IOResult -> Text.Text
mkResultTimeLabel (IOResult x t) = prefix x <> (Text.pack t) <>  [r| </span> |]
  where
    prefix Nothing  = [r| <span size="larger" foreground="#6FC6AE"> |]
    prefix (Just _) = [r| <span size="larger" foreground="#E24C4B"> |]

saveLabel = [r|  First, select a folder for your saves.
  Quicksaving creates an anonymous save 
  file that you can access with Quickload.
  <span weight="bold" foreground="#6FC6AE">Quicksave shortcut:</span><span foreground="blue"> F5</span> or<span foreground="blue"> LB</span>
  You can create unique saves by giving it 
  a name and then hitting the save button. 
  |]

loadLabel = [r|  Quickload loads the quicksave file located 
  in the selected save folder (if one exists).
  <span weight="bold" foreground="#6FC6AE">Quickload Shortcut:</span><span foreground="blue"> F9</span> or <span foreground="blue">RB</span>
  You can load unique save files using the
  filechooser.
  |]

saveAsQuick = [r|
You can't name your save as 'quick' 
as it would overwrite the previous quicksave.
  |]

supportedMappers = [r|Mapper 0 - NROM
Mapper 2 - UNROM
|]

view' :: Int -> State -> AppView Window Event
view' threadCount s = do
  let 
    height = case s of
      ShowControls -> 400
      _            -> 700
    title = case s of
      Message _ _  -> "Message"
      ShowControls -> "Controls"
      _            -> "Pure-Nes Menu"
  bin
      Window
      [ #title := title
      , on #deleteEvent (const (True, Closed))
      , #heightRequest := height
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
            BoxChild defaultBoxChildProperties { fill = True } $ container Box
            [#orientation := OrientationHorizontal, #valign := AlignCenter, #margin := 10, #expand := True ]
            [
              BoxChild defaultBoxChildProperties { fill = True } $ container Box
              [#orientation := OrientationVertical, #valign := AlignCenter, #margin := 10, #expand := True ]
              [
                BoxChild defaultBoxChildProperties { padding = 10 } $ 
                  widget Image [ #file := Text.append "resources/GUI/" (if running then "pause.png" else "play.png")]
              , BoxChild defaultBoxChildProperties { padding = 10, fill = True } $ 
                  widget Button
                  [ 
                    #label := Text.append (if running then "Pause " else "Resume ") romName,
                    on #clicked (SwitchMode True)
                  ]
              ]

            , BoxChild defaultBoxChildProperties { fill = True } $ container Box
              [#orientation := OrientationVertical, #valign := AlignCenter, #margin := 10, #expand := True ]
              [
                BoxChild defaultBoxChildProperties { padding = 10 } $ 
                  widget Image [ #file := "resources/GUI/back2.png"]
              , BoxChild defaultBoxChildProperties { padding = 10, fill = True } $ 
                  widget Button
                  [ 
                    #label := "Return to ROM selection"
                  , on #clicked ReturnToSelection
                  ]
              ]
            ]
          , BoxChild defaultBoxChildProperties { padding = 15, expand = True, fill = True } $
              case saveResultSuccess of
                Nothing -> 
                  container Box [#orientation := OrientationHorizontal, #halign := AlignCenter] $ 
                    [BoxChild defaultBoxChildProperties $ widget Image [ #file := "resources/GUI/save.png"]]
                Just result -> 
                  let 
                    img = case errorMsg result of
                      Nothing -> "tick.png"
                      Just _  -> "cross.png"
                  in
                    container Box [#orientation := OrientationVertical, #halign := AlignCenter] $
                    [
                      container Box [#orientation := OrientationHorizontal, #halign := AlignCenter] $
                      [
                        BoxChild defaultBoxChildProperties $ 
                          widget Image [ #file := Text.append "resources/GUI/" img, #marginRight := 40]
                      , BoxChild defaultBoxChildProperties $ 
                          widget Image [ #file := "resources/GUI/save.png"]
                      ],
                      BoxChild defaultBoxChildProperties $
                        widget Label [#label := mkResultTimeLabel result, #useMarkup := True, #marginTop := 10]
                    ]
          , BoxChild defaultBoxChildProperties $
              widget Label
              [ 
                #label := saveLabel,
                #useMarkup := True,
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
                case loadResultSuccess of
                  Nothing -> 
                    container Box [#orientation := OrientationHorizontal, #halign := AlignCenter] $
                      [BoxChild defaultBoxChildProperties $ widget Image [ #file := "resources/GUI/reload.png"]]
                  Just result ->
                    container Box [#orientation := OrientationVertical, #halign := AlignCenter] $
                      let 
                        img = case errorMsg result of
                          Nothing -> "tick.png"
                          Just _  -> "cross.png"
                      in 
                        [
                          container Box [#orientation := OrientationHorizontal, #halign := AlignCenter] $
                          [
                            BoxChild defaultBoxChildProperties $ 
                              widget Image [ #file := Text.append "resources/GUI/" img, #marginRight := 40]
                          , BoxChild defaultBoxChildProperties $ 
                              widget Image [ #file := "resources/GUI/reload.png"]
                          ],
                          BoxChild defaultBoxChildProperties $
                            widget Label [#label := mkResultTimeLabel result, #useMarkup := True, #marginTop := 5]
                        ]
          , BoxChild defaultBoxChildProperties $
              widget Label
              [ 
                #label := loadLabel,
                #useMarkup := True,
                #marginBottom := 5
              ]
          , BoxChild defaultBoxChildProperties {fill = True} $
              container Box [#orientation := OrientationVertical, #valign := AlignCenter, #marginTop := 5, #marginBottom := 15] $
              [
                BoxChild defaultBoxChildProperties { fill = True } $
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
          ]
        Started currentFile -> 
          container Box
          [#orientation := OrientationVertical, #valign := AlignCenter]
          [ 
            BoxChild defaultBoxChildProperties { padding = 10 } $ 
              widget Image [#file := "resources/GUI/logo.png", #marginTop := 30, #marginBottom := 25, #halign := AlignCenter]
          , BoxChild defaultBoxChildProperties { padding = 15 } $ 
              widget Label
              [#label := "Please select the ROM you wish to run."]
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
            , BoxChild defaultBoxChildProperties { padding = 15 } $ 
                widget Button [#label := "Show controls", on #clicked ShowControlsPressed]
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
                #label := supportedMappers
              ]
          , BoxChild defaultBoxChildProperties $ 
              widget Label
              [
                #marginTop := 10,
                #label := ("Available OS threads: " <> (Text.pack (show threadCount)))
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
        ShowControls ->
          container Box
          [#orientation := OrientationVertical, #valign := AlignCenter, #halign := AlignCenter, #margin := 10 ]
          [
            BoxChild defaultBoxChildProperties $
              mkControlHelpBox [ 
                ("Up", "Up arrow"),
                ("Down", "Down arrow"),
                ("Left", "Left arrow"),
                ("Right", "Right arrow"),
                ("A", "1"),
                ("B", "2"),
                ("Select", "3"),
                ("Start", "4"),
                ("Fullscreen toggle", "R"),
                ("Pause", "Space"),
                ("Step 100 cpu instructions (when paused)", "C"),
                ("Step one frame (when paused)", "F")
              ]
          , BoxChild defaultBoxChildProperties { padding = 15 } $ 
              widget Button [#label := "Ok", on #clicked MessageAck]
          ]


-- | Launch the emulator on a new, bound thread
launchEmulator :: FilePath -> CommResources -> IO ()
launchEmulator path comms = do
  toSDLWindow' <- atomically $ dupTChan (toSDLWindow comms)
  void . forkOS $ runEmulatorWindow path comms { toSDLWindow = toSDLWindow' }


noop = pure Nothing
only x = do x; noop
emit = return . Just 


resultEvent result op = 
  let prefix = "Oops! Something went wrong during " ++ op ++ ":\n" in
  case errorMsg result of
    Nothing  -> noop
    Just msg -> emit . MessageText $ prefix ++ msg


-- | GUI state transition function
update :: CommResources -> State -> Event -> Transition State Event

update _ (Started _) (FileSelectionChanged p) 
  = Transition (Started p) (return Nothing)

update CommResources{..} e@Emulating{saveRomName = ""} SaveButtonPressed 
  = Transition e (emit $ MessageText "You need to give a name to your save file.")

update CommResources{..} e@Emulating{savePath = Nothing} SaveButtonPressed
  = Transition e (emit $ MessageText "You need to choose a save folder first.")

update CommResources{..} e@Emulating{savePath = Nothing} QuickSavePressed
  = Transition e (emit $ MessageText "You need to choose a save folder first.")

update CommResources{..} e@Emulating{saveRomName = "quick"} SaveButtonPressed 
  = Transition e (emit $ MessageText saveAsQuick)

update comms e@Emulating{savePath = Just path, saveRomName = saveName } SaveButtonPressed
  = Transition e (sendMsg comms (Save (path </> saveName <.> "purenes")))

update comms e@Emulating{savePath = Just path} QuickSavePressed
  = Transition e (sendMsg comms (Save (path </> "quick.purenes")))

update comms e@Emulating{} (SavePathChanged s)
  = Transition (e {savePath = s}) (sendMsg comms (NewSaveFolder s))

update _ e@Emulating{} (SaveNameChanged s)
  = Transition e{saveRomName = s} noop

update _ e@Emulating{} (SaveResult res)
  = Transition e{saveResultSuccess = Just $ res} $ resultEvent res "saving"

update _ e@Emulating{} (LoadResult res)
  = Transition e{loadResultSuccess = Just $ res} $ resultEvent res "loading"

update CommResources{..} e@Emulating{ savePath = Nothing } QuickReloadPressed
  = Transition e (emit $ MessageText "You need to choose a save folder first.")

update comms e@Emulating{ savePath = Just path } QuickReloadPressed
  = Transition e (sendMsg comms (Load (path </> "quick.purenes")))

update comms e@Emulating{} (LoadPathChanged (Just path))
  = Transition e {loadPath = path} (sendMsg comms (Load path))

update _  _ Closed 
  = Exit

update comms s@(Started (Just path)) StartEmulator 
  = Transition (Emulating (Text.pack . takeBaseName $ path) True Nothing "" "" Nothing Nothing) (only $ launchEmulator path comms)

update _ s@(Started Nothing) StartEmulator
  = Transition s (return . Just $ MessageText "No ROM selected.")

update _ m@Message{} (MessageText newMsg)
  = Transition (m {text = Text.pack newMsg}) noop

update _ (Message _ stateAfterOk) MessageAck 
  = Transition stateAfterOk noop

update comms (Emulating{..}) ReturnToSelection 
  = Transition (Started Nothing) (sendMsg comms Quit)

update comms e (SwitchMode shouldForward)
  = Transition (e { running = not (running e) }) (if shouldForward then sendMsg comms (SwitchEmulationMode False) else noop)

update _ (Emulating{}) SDLWindowClosed 
  = Transition (Started Nothing) noop

update _ s (Error msg) 
  = Transition (Message (Text.pack msg) (Started Nothing)) noop

update _ s (MessageText msg) 
  = Transition (Message (Text.pack msg) s) noop

update _ ShowControls MessageAck = Transition (Started Nothing) noop

update _ (Started _) ShowControlsPressed = Transition ShowControls noop

update _ s _ = Transition s noop

  
sendMsg CommResources{..} x = only . atomically $ writeTChan toSDLWindow x


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
                      
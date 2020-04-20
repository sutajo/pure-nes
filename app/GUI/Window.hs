{-# LANGUAGE OverloadedLabels, OverloadedLists, OverloadedStrings, FlexibleContexts, NamedFieldPuns, TemplateHaskell, QuasiQuotes, GADTs #-}

module GUI.Window where

import           Data.Char (toUpper)
import           Data.Time
import           Data.List (intercalate)
import           Data.Version
import           Data.Maybe
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
import           GI.Gtk.Objects.Image(Image(..))
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative.Container.Grid
import           System.Info
import           Text.RawString.QQ
import           GUI.State
import           Communication
import           GUI.InGame


mkResultTimeLabel :: IOResult -> Text
mkResultTimeLabel (IOResult x t) = prefix x <> (Text.pack t) <>  [r| </span> |]
  where
    prefix Nothing  = [r| <span size="larger" foreground="#6FC6AE"> |]
    prefix (Just _) = [r| <span size="larger" foreground="#E24C4B"> |]


saveAsQuick :: String
saveAsQuick = [r|
You can't name your save as 'quick' 
as it would overwrite the previous quicksave.
|]


supportedMappers :: Text
supportedMappers = 
 [r|Mapper 0 - NROM
Mapper 2 - UNROM
|]


messageWidget :: Text -> MessageIcon -> Widget Event
messageWidget text icon = 
    container Box
        [#orientation := OrientationVertical, #valign := AlignCenter, #margin := 10 ]
        [
            let 
                path = 
                    Text.append "resources/GUI/" $ 
                    case icon of
                        Alert -> "alert.png"
                        Info  -> "info.png"
                        Cross -> "cross.png"
            in
              BoxChild defaultBoxChildProperties { padding = 25 } $
                widget Image [#file := path]
            , BoxChild defaultBoxChildProperties { padding = 15 } $ 
                widget Label [#label := text]
            , BoxChild defaultBoxChildProperties { padding = 15 } $ 
                widget Button [#label := "Ok", on #clicked MessageAck]
        ]


startMenu :: Int -> Maybe FilePath -> Widget Event
startMenu threadCount selectedRom =
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
                    widget Button [#label := "Start Emulator", on #clicked StartEmulator, #sensitive := isJust selectedRom ]
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


mkControlHelpBox :: [(Text, Text, Text)] -> Widget Event
mkControlHelpBox controls =
  container 
  Grid
  [#orientation := OrientationVertical, #valign := AlignCenter, #marginBottom := 20, #marginRight := 25, #marginLeft := 25] 
  (V.fromList $ concat $ mkTopRow 1 toprow : zipWith mkHelpRow [2..] controls)
  where
    span attrs x = "<span " <> Text.intercalate " " (map (\(attr, val) -> attr <> [r| ="|] <> val <> [r|"|]) attrs) <> ">" <> x <> [r|</span>|]
    toprow = ("NES input", "Keyboard button", "Controller button")
    orange = span [("weight", "bold"), ("foreground", "#eb660e")]
    blue   = span [("weight", "bold"), ("foreground", "#033b94")]
    underlinedorange = span [("weight", "ultrabold"), ("foreground", "#eb660e"), ("underline", "single")]
    underlinedblue   = span [("weight", "ultrabold"), ("foreground", "#033b94"), ("underline", "single")]
    mkTopRow index (action, key, joybutton) = [
        GridChild defaultGridChildProperties { leftAttach = 1, topAttach = index } $ 
          widget Label [#label := underlinedorange action, #useMarkup := True, #xalign := 0, #marginRight := 25, #marginBottom := 20]
      , GridChild defaultGridChildProperties { leftAttach = 2, topAttach = index } $ 
          widget Label [#label := underlinedblue key, #useMarkup := True, #xalign := 0, #marginRight := 25, #marginBottom := 20]
      , GridChild defaultGridChildProperties { leftAttach = 3, topAttach = index } $ 
          widget Label [#label := underlinedblue joybutton, #useMarkup := True, #xalign := 0, #marginBottom := 20]
      ]
    mkHelpRow index (action, key, joybutton) = [
        GridChild defaultGridChildProperties { leftAttach = 1, topAttach = index } $ 
          widget Label [#label := orange action, #useMarkup := True, #xalign := 0, #marginRight := 25, #marginBottom := 15]
      , GridChild defaultGridChildProperties { leftAttach = 2, topAttach = index } $ 
          widget Label [#label := blue key, #useMarkup := True, #xalign := 0, #marginRight := 25, #marginBottom := 15]
      , GridChild defaultGridChildProperties { leftAttach = 3, topAttach = index } $ 
          widget Label [#label := blue joybutton, #useMarkup := True, #xalign := 0, #marginBottom := 15]
      ]


controlsWidget :: Widget Event
controlsWidget = 
    container Box
        [#orientation := OrientationVertical, #valign := AlignCenter, #halign := AlignCenter, #margin := 10 ]
        [
          BoxChild defaultBoxChildProperties $ 
            widget Image [#file := "resources/GUI/info.png", #marginBottom := 35]
        , BoxChild defaultBoxChildProperties $
            mkControlHelpBox [ 
                ("Up", "Up arrow", "DPAD Up"),
                ("Down", "Down arrow", "DPAD Down"),
                ("Left", "Left arrow", "DPAD Left"),
                ("Right", "Right arrow", "DPAD Right"),
                ("A", "1", "Button 2"),
                ("B", "2", "Button 3"),
                ("Select", "3", "Button 0"),
                ("Start", "4", "Button 1"),
                ("Quick save", "F5", "Button 4"),
                ("Quick load", "F9", "Button 6"),
                ("Fullscreen toggle", "R", "Not available"),
                ("CRT effect toggle", "T", "Not available"),
                ("Pause", "Space", "Not available"),
                ("Step 100 cpu instructions (when paused)", "C", "Not available"),
                ("Step one frame (when paused)", "F", "Not available")
            ]
        , BoxChild defaultBoxChildProperties { padding = 15 } $ 
            widget Button [#label := "Ok", on #clicked MessageAck]
        ]


visualize :: Int -> State -> AppView Window Event
visualize threadCount s = do
  let 
    height = case s of
      ShowControls _ -> 400
      _              -> 700
    title = case s of
      Message _ _ _  -> "Message"
      ShowControls _ -> "Controls"
      _              -> "Pure-Nes Menu"
  bin
      Window
      [ #title := title
      , on #deleteEvent (const (True, Closed))
      , #windowPosition := WindowPositionCenter
      , #heightRequest := height
      , #widthRequest := 400
      ]
    $ windowContent s
  where
    windowContent = \case
        Message {text, icon} ->
            messageWidget text icon

        e@Emulating{} -> 
            inGame e

        Started{selectedRom} -> 
            startMenu threadCount selectedRom

        ShowControls _ ->
            controlsWidget
            

chooseSaveFolderFirst :: Event
chooseSaveFolderFirst = MessageText "You need to choose a save folder first." Alert
                      
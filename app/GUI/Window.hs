{-# LANGUAGE OverloadedLabels, OverloadedLists, OverloadedStrings, FlexibleContexts, NamedFieldPuns, TemplateHaskell, QuasiQuotes, GADTs #-}

module GUI.Window where

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


startMenu :: Int -> Widget Event
startMenu threadCount =
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


mkControlHelpBox :: [(Text, Text)] -> Widget Event
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


controlsWidget :: Widget Event
controlsWidget = 
    container Box
        [#orientation := OrientationVertical, #valign := AlignCenter, #halign := AlignCenter, #margin := 10 ]
        [
          BoxChild defaultBoxChildProperties $ 
            widget Image [#file := "resources/GUI/info.png", #marginBottom := 35]
        , BoxChild defaultBoxChildProperties $
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


visualize :: Int -> State -> AppView Window Event
visualize threadCount s = do
  let 
    height = case s of
      ShowControls -> 400
      _            -> 700
    title = case s of
      Message _ _ _ -> "Message"
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
    windowContent = \case
        Message {text, icon} ->
            messageWidget text icon

        e@Emulating{} -> 
            inGame e

        Started{} -> 
            startMenu threadCount

        ShowControls ->
            controlsWidget
            

chooseSaveFolderFirst :: Event
chooseSaveFolderFirst = MessageText "You need to choose a save folder first." Alert
                      
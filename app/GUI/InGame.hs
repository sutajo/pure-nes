{-# LANGUAGE OverloadedLabels, OverloadedLists, OverloadedStrings, FlexibleContexts, NamedFieldPuns, QuasiQuotes, GADTs #-}

module GUI.InGame where

import qualified Data.Text  as Text

import           Data.HashSet as H
import qualified Data.Vector  as V
import           Data.Maybe
import qualified GI.GObject   as GI
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Attributes.Collected
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.State
import           GI.Gtk.Interfaces.FileChooser
import           GI.Gtk.Enums
import qualified GI.Gtk as QGtk (on)
import           GI.Gtk as Gtk (
                  Box(..)
                , Button(..)
                , FileChooserButton(..)
                , Label(..)
                , widgetGetStyleContext
                , new
                )
import           GI.Gtk.Objects.Entry
import           GI.Gtk.Objects.Image(Image(..))
import           GI.Gtk.Objects.FileChooserButton
import           GI.Gtk.Interfaces.Editable
import           Text.RawString.QQ
import           Communication
import           GUI.State


mkResultTimeLabel :: IOResult -> Text.Text
mkResultTimeLabel (IOResult x t) = prefix x <> (Text.pack t) <>  [r| </span> |]
  where
    prefix Nothing  = [r| <span size="larger" foreground="#6FC6AE"> |]
    prefix (Just _) = [r| <span size="larger" foreground="#E24C4B"> |]


saveLabel :: Text.Text
saveLabel = [r|  First, select a folder for your saves.
  Quicksaving creates an anonymous save 
  file that you can access with Quickload.
  <span weight="bold" foreground="#6FC6AE">Quicksave shortcut:</span><span foreground="blue"> F5</span> or <span foreground="blue">4. controller button</span>
  You can create unique saves by giving it 
  a name and then hitting the save button. 
  |]


loadLabel :: Text.Text
loadLabel = [r|  Quickload loads the quicksave file located 
  in the selected save folder (if one exists).
  <span weight="bold" foreground="#6FC6AE">Quickload Shortcut:</span><span foreground="blue"> F9</span> or <span foreground="blue">6. controller button</span>
  You can load unique save files using the
  filechooser.
  |]


data FileChooserProperties = FileChooserProperties
  { selectedPath       :: Maybe FilePath
  , fileChooserClasses :: ClassSet
  } deriving (Eq, Show)


fileChooserButtonFromUri :: Maybe FilePath -> V.Vector (Attribute FileChooserButton Event) -> Widget Event
fileChooserButtonFromUri maybePath attributes = Widget (CustomWidget {..})
  where
    customAttributes = attributes
    customParams = FileChooserProperties maybePath H.empty
    customWidget = FileChooserButton
    customCreate FileChooserProperties{..} = do
      fileChooser <- Gtk.new FileChooserButton []
      fileChooserSetAction fileChooser FileChooserActionOpen
      fileChooserSetCreateFolders fileChooser True
      case maybePath of
          Nothing   -> pure True
          Just path -> fileChooserSetCurrentFolderUri fileChooser (Text.pack path)
      sc          <- Gtk.widgetGetStyleContext fileChooser
      updateClasses sc mempty fileChooserClasses
      return (fileChooser, SomeState (StateTreeWidget (StateTreeNode fileChooser sc mempty ())))

    customPatch :: FileChooserProperties -> FileChooserProperties -> SomeState -> CustomPatch FileChooserButton SomeState 
    customPatch (old :: FileChooserProperties) (new :: FileChooserProperties) (SomeState st)
      | old == new = CustomKeep
      | otherwise = CustomModify $ \(scale :: Gtk.FileChooserButton) -> do
        updateClasses (stateTreeStyleContext (stateTreeNode st))
                      (fileChooserClasses old)
                      (fileChooserClasses new)
        return (SomeState st)

    customSubscribe _  _ (fileChooser :: Gtk.FileChooserButton) cb = do
      handler <- QGtk.on fileChooser
                  #selectionChanged
                  (cb =<< fmap SavePathChanged (fileChooserGetFilename fileChooser))
      return (fromCancellation (GI.signalHandlerDisconnect fileChooser handler))


inGame :: State -> Widget Event
inGame Emulating{..} = 
    container Box
    [#orientation := OrientationVertical, #valign := AlignCenter, #margin := 10 ]
    [

        -- Pause and return buttons
        BoxChild defaultBoxChildProperties { fill = True } $ container Box
        [#orientation := OrientationHorizontal, #valign := AlignCenter, #margin := 10, #expand := True ]
        [
            BoxChild defaultBoxChildProperties { fill = True } $ container Box
            [#orientation := OrientationVertical, #valign := AlignCenter, #margin := 10, #expand := True ]
            [
                BoxChild defaultBoxChildProperties { padding = 10 } $ 
                    widget Image [ #file := Text.append "resources/GUI/" (if isRunning then "pause.png" else "play.png")]
            ,   BoxChild defaultBoxChildProperties { padding = 10, fill = True } $ 
                    widget Button
                    [ 
                        #label := ((if isRunning then "Pause " else "Resume ") <> romName),
                        on #clicked (TogglePause True)
                    ]
            ]

        ,   BoxChild defaultBoxChildProperties { fill = True } $ container Box
            [#orientation := OrientationVertical, #valign := AlignCenter, #margin := 10, #expand := True ]
            [
                BoxChild defaultBoxChildProperties { padding = 10 } $ 
                    widget Image [ #file := "resources/GUI/back2.png"]
            ,   BoxChild defaultBoxChildProperties { padding = 10, fill = True } $ 
                    widget Button
                    [ 
                        #label := "Return to ROM selection"
                    ,   on #clicked ReturnToSelection
                    ]
            ]
        ]


        -- Saving icons
    ,   BoxChild defaultBoxChildProperties { padding = 15, expand = True, fill = True } $
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
                            ]
                
                        ,   BoxChild defaultBoxChildProperties $
                                widget Label [#label := mkResultTimeLabel result, #useMarkup := True, #marginTop := 10]
                        ]


          -- Save message              
    ,   BoxChild defaultBoxChildProperties $
            widget Label
            [ 
                #label := saveLabel,
                #useMarkup := True,
                #marginBottom := 5
            ]


          -- Save inputs
    ,   BoxChild defaultBoxChildProperties { fill = True } $
            container Box [#orientation := OrientationHorizontal, #valign := AlignCenter, #marginTop := 15, #marginBottom := 15] $
            [
                BoxChild defaultBoxChildProperties $
                    widget Button
                    [ 
                        #label := "   Quicksave   ",
                        #marginLeft  := 5,
                        #marginRight := 10,
                        on #clicked QuickSavePressed,
                        #sensitive := isJust savePath
                    ]
            ,   BoxChild defaultBoxChildProperties { fill = True } $ 
                    fileChooserButtonFromUri savePath
                    [   onM #selectionChanged (fmap SavePathChanged . fileChooserGetFilename),
                        #action := FileChooserActionSelectFolder, #expand := True, #createFolders := True
                    ]
            ]

    ,   BoxChild defaultBoxChildProperties { fill = True } $
            container Box [#orientation := OrientationHorizontal, #valign := AlignCenter, #marginTop := 15, #marginBottom := 15] $
            [
                BoxChild defaultBoxChildProperties $
                    widget Button
                    [ 
                        #label := "Save progress",
                        #marginLeft  := 5,
                        #marginRight := 10,
                        on #clicked SaveButtonPressed,
                        #sensitive := (isJust savePath && saveRomName /= "")
                    ]

    ,   BoxChild defaultBoxChildProperties { fill = True } $ 
            widget Entry
                [ #expand := True, 
                  #placeholderText := "How should I call this save?",
                  onM #changed (\e -> SaveNameChanged . Text.unpack <$> editableGetChars e 0 (-1)),
                  #sensitive := isJust savePath,
                  #text := Text.pack saveRomName
                ]
            ]


        -- Loading
    ,   BoxChild defaultBoxChildProperties { padding = 15, expand = True, fill = True } $
            case loadResultSuccess of
                Nothing -> 
                    container Box [#orientation := OrientationHorizontal, #halign := AlignCenter] $
                    [BoxChild defaultBoxChildProperties $ widget Image [ #file := "resources/GUI/reload.png"]]
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
                        ,   BoxChild defaultBoxChildProperties $ 
                                widget Image [ #file := "resources/GUI/reload.png"]
                        ]
                    
                    ,   BoxChild defaultBoxChildProperties $
                            widget Label [#label := mkResultTimeLabel result, #useMarkup := True, #marginTop := 5]
                    ]

    ,   BoxChild defaultBoxChildProperties $
            widget Label
            [ 
                #label := loadLabel,
                #useMarkup := True,
                #marginBottom := 5
            ]

    ,   BoxChild defaultBoxChildProperties {fill = True} $
            container Box [#orientation := OrientationVertical, #valign := AlignCenter, #marginTop := 5, #marginBottom := 15] $
            [
                BoxChild defaultBoxChildProperties { fill = True } $
                container Box [#orientation := OrientationHorizontal, #valign := AlignCenter, #marginTop := 15, #marginBottom := 15] $
                [
                    BoxChild defaultBoxChildProperties $
                        widget Button
                        [ 
                            #label := "Quickload",
                            #marginLeft  := 5,
                            #marginRight := 10,
                            on #clicked QuickReloadPressed,
                            #sensitive := isJust savePath
                        ]
                ,   BoxChild defaultBoxChildProperties {fill = True} $ 
                        widget FileChooserButton 
                        [ 
                            onM #selectionChanged (fmap LoadPathChanged . fileChooserGetFilename),
                            #action := FileChooserActionOpen, #expand := True 
                        ]
                ]
            ]
    ]


inGame _ = error "This widget can only be constructed from the Emulating state."
module Main where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import FFI.Drag as Drag
import FFI.WebSocket as WS
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent as Keyboard
import Web.UIEvent.MouseEvent as Mouse
import Web.UIEvent.WheelEvent as Wheel

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  void $ runUI component unit body

type SongInfo =
  { id :: Int
  , name :: String
  }

type TrackInfo =
  { id :: Int
  , name :: String
  , position :: Int
  }

type ParamState =
  { value :: Int
  , name :: Maybe String
  }

type SnapshotParam =
  { cc :: Int
  , value :: Int
  , name :: Maybe String
  }

type ParamNameEdit =
  { cc :: Int
  , draft :: String
  }

data Theme
  = Light
  | Dark

derive instance eqTheme :: Eq Theme

type State =
  { songs :: Array SongInfo
  , tracks :: Array TrackInfo
  , currentSong :: Maybe Int
  , currentTrack :: Maybe Int
  , params :: Map Int ParamState
  , newSongName :: String
  , newTrackName :: String
  , duplicateTrackTargetSong :: Maybe Int
  , editingParamName :: Maybe ParamNameEdit
  , connection :: Maybe WS.WebSocketConnection
  , theme :: Theme
  }

type TaggedMessage =
  { tag :: String }

type SnapshotPayload =
  { songs :: Array SongInfo
  , tracks :: Array TrackInfo
  , currentSong :: Maybe Int
  , currentTrack :: Maybe Int
  , params :: Array SnapshotParam
  }

type ParamUpdatePayload =
  { cc :: Int
  , value :: Int
  }

data ServerMessage
  = Snapshot SnapshotPayload
  | ParamUpdate Int Int

data ClientMessage
  = SelectSongMessage Int
  | SelectTrackMessage Int
  | SetParamMessage Int Int
  | RenameParamMessage Int String
  | CreateSongMessage String
  | CreateTrackMessage Int String
  | DuplicateSongMessage Int
  | DuplicateTrackMessage Int Int

data Action
  = Initialize
  | ReceiveMessage String
  | SocketOpened
  | SocketClosed
  | SelectSong Int
  | SelectTrack Int
  | UpdateNewSongName String
  | UpdateNewTrackName String
  | SelectDuplicateTrackTarget Int
  | CreateSong
  | CreateTrack
  | DuplicateSong
  | DuplicateTrack
  | ToggleTheme
  | IgnoreSelection
  | StartParamNameEdit Int
  | UpdateParamNameDraft String
  | CommitParamNameEdit
  | CommitParamNameEditOnKey String
  | StartParamDrag Int Int Int
  | SetParamFromDrag Int Int
  | SetParamFromWheel Int Wheel.WheelEvent
  | EndParamDrag H.SubscriptionId

component :: forall query input output m. MonadEffect m => H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  }

initialState :: forall input. input -> State
initialState _ =
  { songs: []
  , tracks: []
  , currentSong: Nothing
  , currentTrack: Nothing
  , params: Map.empty
  , newSongName: ""
  , newTrackName: ""
  , duplicateTrackTargetSong: Nothing
  , editingParamName: Nothing
  , connection: Nothing
  , theme: Dark
  }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.main
    [ HP.style (pageStyle st.theme) ]
    [ HH.div
        [ HP.style contentStyle ]
        [ headerBar st.theme
        , HH.div
            [ HP.style "display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 24px; align-items: start;" ]
            [ songsPanel st
            , tracksPanel st
            ]
        , paramsPanel st
        ]
    ]

headerBar :: forall m. Theme -> H.ComponentHTML Action () m
headerBar theme =
  HH.header
    [ HP.style (headerStyle theme) ]
    [ HH.h1
        [ HP.style (titleStyle theme) ]
        [ HH.text "Medibox" ]
    , HH.button
        [ HP.type_ HP.ButtonButton
        , HP.style (themeToggleStyle theme)
        , HE.onClick \_ -> ToggleTheme
        ]
        [ HH.text (themeToggleLabel theme) ]
    ]

songsPanel :: forall m. State -> H.ComponentHTML Action () m
songsPanel st =
  HH.section
    [ HP.style (cardStyle st.theme) ]
    [ HH.h2
        [ HP.style (panelTitleStyle st.theme) ]
        [ HH.text "Songs" ]
    , HH.div
        [ HP.style controlRowStyle ]
        [ songSelect st.theme st.currentSong st.songs
        , HH.input
            [ HP.type_ HP.InputText
            , HP.placeholder "New song"
            , HP.value st.newSongName
            , HP.style (textInputStyle st.theme false)
            , HE.onValueInput UpdateNewSongName
            ]
        , HH.button
            [ HP.type_ HP.ButtonButton
            , HP.disabled (st.newSongName == "")
            , HP.style (createButtonStyle st.theme (st.newSongName == ""))
            , HE.onClick \_ -> CreateSong
            ]
            [ HH.text "Create Song" ]
        , HH.button
            [ HP.type_ HP.ButtonButton
            , HP.disabled (st.currentSong == Nothing)
            , HP.style (createButtonStyle st.theme (st.currentSong == Nothing))
            , HE.onClick \_ -> DuplicateSong
            ]
            [ HH.text "Duplicate" ]
        ]
    ]

songSelect :: forall m. Theme -> Maybe Int -> Array SongInfo -> H.ComponentHTML Action () m
songSelect theme currentSong songs =
  HH.select
    [ HP.value (selectedValue currentSong)
    , HP.style (selectStyle theme)
    , HE.onValueChange songSelectionAction
    ]
    ([ placeholderOption (currentSong == Nothing) "Select a song..." ] <> map (songOption currentSong) songs)

songOption :: forall m. Maybe Int -> SongInfo -> H.ComponentHTML Action () m
songOption currentSong song =
  HH.option
    [ HP.value (show song.id)
    , HP.selected (isSelected currentSong song.id)
    ]
    [ HH.text song.name ]

tracksPanel :: forall m. State -> H.ComponentHTML Action () m
tracksPanel st =
  HH.section
    [ HP.style (cardStyle st.theme) ]
    [ HH.h2
        [ HP.style (panelTitleStyle st.theme) ]
        [ HH.text "Tracks" ]
    , HH.div
        [ HP.style controlRowStyle ]
        [ trackSelect st.theme st.currentSong st.currentTrack visibleTracks
        , HH.input
            [ HP.type_ HP.InputText
            , HP.placeholder "New track"
            , HP.value st.newTrackName
            , HP.disabled noSongSelected
            , HP.style (textInputStyle st.theme noSongSelected)
            , HE.onValueInput UpdateNewTrackName
            ]
        , HH.button
            [ HP.type_ HP.ButtonButton
            , HP.disabled (noSongSelected || st.newTrackName == "")
            , HP.style (createButtonStyle st.theme (noSongSelected || st.newTrackName == ""))
            , HE.onClick \_ -> CreateTrack
            ]
            [ HH.text "Create Track" ]
        , duplicateTargetSongSelect st.theme noTrackSelected (duplicateTrackTargetSongId st) st.songs
        , HH.button
            [ HP.type_ HP.ButtonButton
            , HP.disabled noTrackSelected
            , HP.style (createButtonStyle st.theme noTrackSelected)
            , HE.onClick \_ -> DuplicateTrack
            ]
            [ HH.text "Duplicate" ]
        ]
    ]
  where
  noSongSelected = st.currentSong == Nothing

  noTrackSelected = st.currentTrack == Nothing

  visibleTracks = case st.currentSong of
    Nothing -> []
    Just _ -> st.tracks

trackSelect
  :: forall m
   . Theme
  -> Maybe Int
  -> Maybe Int
  -> Array TrackInfo
  -> H.ComponentHTML Action () m
trackSelect theme currentSong currentTrack tracks =
  HH.select
    [ HP.value selectedTrackValue
    , HP.disabled noSongSelected
    , HP.style (selectStyle theme)
    , HE.onValueChange trackSelectionAction
    ]
    ([ placeholderOption (selectedTrackValue == "") placeholderText ] <> map (trackOption currentTrack) tracks)
  where
  noSongSelected = currentSong == Nothing

  selectedTrackValue =
    if noSongSelected then "" else selectedValue currentTrack

  placeholderText =
    if noSongSelected then "Select a song first..." else "Select a track..."

trackOption :: forall m. Maybe Int -> TrackInfo -> H.ComponentHTML Action () m
trackOption currentTrack track =
  HH.option
    [ HP.value (show track.id)
    , HP.selected (isSelected currentTrack track.id)
    ]
    [ HH.text (show track.position <> ". " <> track.name) ]

duplicateTargetSongSelect
  :: forall m
   . Theme
  -> Boolean
  -> Maybe Int
  -> Array SongInfo
  -> H.ComponentHTML Action () m
duplicateTargetSongSelect theme disabled targetSong songs =
  HH.select
    [ HP.value (selectedValue targetSong)
    , HP.disabled disabled
    , HP.style (selectStyle theme)
    , HP.title "Duplicate track target song"
    , HE.onValueChange duplicateTargetSongSelectionAction
    ]
    ([ placeholderOption (targetSong == Nothing) "Duplicate to..." ] <> map (duplicateTargetSongOption targetSong) songs)

duplicateTargetSongOption :: forall m. Maybe Int -> SongInfo -> H.ComponentHTML Action () m
duplicateTargetSongOption targetSong song =
  HH.option
    [ HP.value (show song.id)
    , HP.selected (isSelected targetSong song.id)
    ]
    [ HH.text song.name ]

placeholderOption :: forall m. Boolean -> String -> H.ComponentHTML Action () m
placeholderOption selected label =
  HH.option
    [ HP.value ""
    , HP.disabled true
    , HP.selected selected
    ]
    [ HH.text label ]

paramsPanel :: forall m. State -> H.ComponentHTML Action () m
paramsPanel st =
  HH.section
    [ HP.style (paramsPanelStyle st.theme) ]
    [ HH.div
        [ HP.style "display: grid; grid-template-columns: repeat(8, minmax(82px, 1fr)); gap: 12px; overflow-x: auto;" ]
        (map (knobControl st.theme st.params st.editingParamName) physicalCCs)
    ]

knobControl
  :: forall m
   . Theme
  -> Map Int ParamState
  -> Maybe ParamNameEdit
  -> Int
  -> H.ComponentHTML Action () m
knobControl theme params editingParamName cc =
  HH.div
    [ HP.style (knobButtonStyle theme)
    , HP.title ("CC " <> show cc <> " value " <> show value)
    , HP.attr (HH.AttrName "aria-label") ("CC " <> show cc <> ", value " <> show value)
    , HE.onWheel (SetParamFromWheel cc)
    ]
    [ paramNameControl theme params editingParamName cc
    , HH.button
        [ HP.type_ HP.ButtonButton
        , HP.style (knobDialButtonStyle theme)
        , HP.title ("Adjust " <> paramDisplayName params cc)
        , HP.attr (HH.AttrName "aria-label") ("Adjust " <> paramDisplayName params cc <> ", value " <> show value)
        , HE.onMouseDown \event -> StartParamDrag cc value (Mouse.clientY event)
        ]
        [ knobSvg theme value ]
    , HH.span
        [ HP.style (knobValueStyle theme) ]
        [ HH.text (show value) ]
    ]
  where
  value = paramValue params cc

paramNameControl
  :: forall m
   . Theme
  -> Map Int ParamState
  -> Maybe ParamNameEdit
  -> Int
  -> H.ComponentHTML Action () m
paramNameControl theme params editingParamName cc =
  case editingParamName of
    Just edit ->
      if edit.cc == cc then
        HH.input
          [ HP.type_ HP.InputText
          , HP.value edit.draft
          , HP.autofocus true
          , HP.placeholder ("CC " <> show cc)
          , HP.style (paramNameInputStyle theme)
          , HE.onValueInput UpdateParamNameDraft
          , HE.onBlur \_ -> CommitParamNameEdit
          , HE.onKeyDown \event -> CommitParamNameEditOnKey (Keyboard.key event)
          ]
      else
        paramNameButton theme params cc
    _ ->
      paramNameButton theme params cc

paramNameButton
  :: forall m
   . Theme
  -> Map Int ParamState
  -> Int
  -> H.ComponentHTML Action () m
paramNameButton theme params cc =
  HH.button
    [ HP.type_ HP.ButtonButton
    , HP.style (knobLabelButtonStyle theme)
    , HP.title "Rename parameter"
    , HE.onClick \_ -> StartParamNameEdit cc
    ]
    [ HH.text (paramDisplayName params cc) ]

knobSvg :: forall m. Theme -> Int -> H.ComponentHTML Action () m
knobSvg theme value =
  svgEl "svg"
    [ svgAttr "viewBox" "0 0 72 72"
    , svgAttr "role" "presentation"
    , HP.style (knobSvgStyle theme)
    ]
    [ svgEl "circle"
        [ svgAttr "cx" "36"
        , svgAttr "cy" "36"
        , svgAttr "r" "31"
        , svgAttr "fill" (knobOuterFill theme)
        , svgAttr "stroke" (knobOuterStroke theme)
        , svgAttr "stroke-width" "2"
        ]
        []
    , svgEl "circle"
        [ svgAttr "cx" "36"
        , svgAttr "cy" "36"
        , svgAttr "r" "25"
        , svgAttr "fill" (knobInnerFill theme)
        , svgAttr "stroke" (knobInnerStroke theme)
        , svgAttr "stroke-width" "1.5"
        ]
        []
    , svgEl "line"
        [ svgAttr "x1" "36"
        , svgAttr "y1" "36"
        , svgAttr "x2" "36"
        , svgAttr "y2" "14"
        , svgAttr "stroke" (knobIndicatorStroke theme)
        , svgAttr "stroke-width" "4"
        , svgAttr "stroke-linecap" "round"
        , svgAttr "transform" rotation
        ]
        []
    , svgEl "circle"
        [ svgAttr "cx" "36"
        , svgAttr "cy" "14"
        , svgAttr "r" "3.5"
        , svgAttr "fill" (knobIndicatorDotFill theme)
        , svgAttr "transform" rotation
        ]
        []
    ]
  where
  rotation = "rotate(" <> show (knobAngle value) <> " 36 36)"

svgEl
  :: forall r w i
   . String
  -> Array (HH.IProp r i)
  -> Array (HH.HTML w i)
  -> HH.HTML w i
svgEl name = HH.elementNS svgNamespace (HH.ElemName name)

svgAttr :: forall r i. String -> String -> HH.IProp r i
svgAttr name = HP.attr (HH.AttrName name)

svgNamespace :: HH.Namespace
svgNamespace = HH.Namespace "http://www.w3.org/2000/svg"

physicalCCs :: Array Int
physicalCCs = Array.range 0 31

paramsFromSnapshot :: Array SnapshotParam -> Map Int ParamState
paramsFromSnapshot snapshotParams =
  Map.fromFoldable (map snapshotParamTuple snapshotParams)

snapshotParamTuple :: SnapshotParam -> Tuple Int ParamState
snapshotParamTuple param =
  Tuple param.cc
    { value: clampParam param.value
    , name: normalizeParamName param.name
    }

paramValue :: Map Int ParamState -> Int -> Int
paramValue params cc = case Map.lookup cc params of
  Just param -> clampParam param.value
  Nothing -> 0

paramDisplayName :: Map Int ParamState -> Int -> String
paramDisplayName params cc = case Map.lookup cc params of
  Just param -> case param.name of
    Just name ->
      if name == "" then fallback else name
    _ -> fallback
  Nothing -> fallback
  where
  fallback = "CC " <> show cc

paramRawName :: Map Int ParamState -> Int -> String
paramRawName params cc = case Map.lookup cc params of
  Just param -> fromMaybe "" param.name
  Nothing -> ""

normalizeParamName :: Maybe String -> Maybe String
normalizeParamName = case _ of
  Just "" -> Nothing
  other -> other

setParamValueInMap :: Int -> Int -> Map Int ParamState -> Map Int ParamState
setParamValueInMap cc rawValue params =
  Map.insert cc { value: clampParam rawValue, name: currentName } params
  where
  currentName = case Map.lookup cc params of
    Just param -> param.name
    Nothing -> Nothing

setParamNameInMap :: Int -> String -> Map Int ParamState -> Map Int ParamState
setParamNameInMap cc name params =
  Map.insert cc { value: paramValue params cc, name: normalizeParamName (Just name) } params

duplicateTrackTargetSongId :: State -> Maybe Int
duplicateTrackTargetSongId st = case st.duplicateTrackTargetSong of
  Just songId -> Just songId
  Nothing -> st.currentSong

wheelParamStep :: Wheel.WheelEvent -> Int
wheelParamStep event
  | Wheel.deltaY event < 0.0 = 1
  | Wheel.deltaY event > 0.0 = (-1)
  | otherwise = 0

knobAngle :: Int -> Int
knobAngle value = (-135) + Int.quot (clampParam value * 270) 127

clampParam :: Int -> Int
clampParam = clamp 0 127

selectedValue :: Maybe Int -> String
selectedValue = case _ of
  Just itemId -> show itemId
  Nothing -> ""

songSelectionAction :: String -> Action
songSelectionAction value = case Int.fromString value of
  Just songId -> SelectSong songId
  Nothing -> IgnoreSelection

trackSelectionAction :: String -> Action
trackSelectionAction value = case Int.fromString value of
  Just trackId -> SelectTrack trackId
  Nothing -> IgnoreSelection

duplicateTargetSongSelectionAction :: String -> Action
duplicateTargetSongSelectionAction value = case Int.fromString value of
  Just songId -> SelectDuplicateTrackTarget songId
  Nothing -> IgnoreSelection

toggleTheme :: Theme -> Theme
toggleTheme = case _ of
  Light -> Dark
  Dark -> Light

themeToggleLabel :: Theme -> String
themeToggleLabel = case _ of
  Light -> "Dark mode"
  Dark -> "Light mode"

pageStyle :: Theme -> String
pageStyle theme =
  "min-height: 100vh; background: " <> pageBg theme <> "; color: " <> textColor theme <> ";"

contentStyle :: String
contentStyle =
  "font-family: system-ui, sans-serif; max-width: 980px; margin: 0 auto; padding: 24px; line-height: 1.4; box-sizing: border-box;"

headerStyle :: Theme -> String
headerStyle theme =
  "display: flex; align-items: center; justify-content: space-between; gap: 16px; margin-bottom: 24px; padding-bottom: 16px; border-bottom: 1px solid " <> borderColor theme <> ";"

titleStyle :: Theme -> String
titleStyle theme =
  "margin: 0; color: " <> textColor theme <> "; font-size: 1.75rem; line-height: 1.1;"

themeToggleStyle :: Theme -> String
themeToggleStyle theme =
  "height: 36px; padding: 0 12px; border: 1px solid " <> controlBorder theme <> "; border-radius: 6px; background: " <> buttonBg theme <> "; color: " <> textColor theme <> "; cursor: pointer;"

cardStyle :: Theme -> String
cardStyle theme =
  "padding: 18px; background: " <> panelBg theme <> "; border: 1px solid " <> borderColor theme <> "; border-radius: 8px; color: " <> textColor theme <> "; box-shadow: " <> panelShadow theme <> ";"

paramsPanelStyle :: Theme -> String
paramsPanelStyle theme =
  "margin-top: 24px; " <> cardStyle theme

panelTitleStyle :: Theme -> String
panelTitleStyle theme =
  "margin: 0 0 16px; color: " <> textColor theme <> "; font-size: 1.25rem;"

controlRowStyle :: String
controlRowStyle =
  "display: flex; flex-wrap: wrap; gap: 8px; align-items: center;"

selectStyle :: Theme -> String
selectStyle theme =
  "flex: 1 1 180px; min-width: 160px; height: 36px; padding: 0 10px; border: 1px solid " <> controlBorder theme <> "; border-radius: 6px; background: " <> controlBg theme <> "; color: " <> textColor theme <> ";"

textInputStyle :: Theme -> Boolean -> String
textInputStyle theme disabled =
  "flex: 1 1 160px; min-width: 0; height: 36px; box-sizing: border-box; padding: 0 10px; border: 1px solid " <> controlBorder theme <> "; border-radius: 6px; background: " <> inputBg theme disabled <> "; color: " <> inputText theme disabled <> ";"

createButtonStyle :: Theme -> Boolean -> String
createButtonStyle theme disabled =
  "height: 36px; padding: 0 12px; border: 1px solid " <> controlBorder theme <> "; border-radius: 6px; background: " <> createButtonBg theme disabled <> "; color: " <> createButtonText theme disabled <> "; cursor: " <> (if disabled then "not-allowed;" else "pointer;")

knobButtonStyle :: Theme -> String
knobButtonStyle theme =
  "display: grid; justify-items: center; gap: 8px; min-width: 82px; padding: 10px 8px; border: 1px solid " <> knobButtonBorder theme <> "; border-radius: 8px; background: " <> knobButtonBg theme <> "; color: inherit; user-select: none; touch-action: none; box-sizing: border-box;"

knobDialButtonStyle :: Theme -> String
knobDialButtonStyle _ =
  "width: 72px; height: 72px; padding: 0; border: 0; background: transparent; color: inherit; cursor: ns-resize; touch-action: none;"

knobLabelButtonStyle :: Theme -> String
knobLabelButtonStyle theme =
  "width: 100%; min-width: 0; height: 20px; padding: 0 2px; border: 0; background: transparent; color: " <> mutedTextColor theme <> "; cursor: text; font-size: 0.76rem; font-weight: 700; letter-spacing: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;"

paramNameInputStyle :: Theme -> String
paramNameInputStyle theme =
  "width: 100%; min-width: 0; height: 20px; box-sizing: border-box; padding: 0 4px; border: 1px solid " <> controlBorder theme <> "; border-radius: 4px; background: " <> controlBg theme <> "; color: " <> textColor theme <> "; font-size: 0.76rem; font-weight: 700; text-align: center;"

knobValueStyle :: Theme -> String
knobValueStyle theme =
  "font-size: 0.82rem; color: " <> textColor theme <> "; font-variant-numeric: tabular-nums;"

knobSvgStyle :: Theme -> String
knobSvgStyle theme =
  "width: 72px; height: 72px; display: block; filter: " <> knobShadow theme <> ";"

isSelected :: Maybe Int -> Int -> Boolean
isSelected selected itemId = case selected of
  Just selectedId -> selectedId == itemId
  Nothing -> false

pageBg :: Theme -> String
pageBg = case _ of
  Dark -> "#101418"
  Light -> "#eef1f5"

panelBg :: Theme -> String
panelBg = case _ of
  Dark -> "#181b20"
  Light -> "#f4f6f9"

textColor :: Theme -> String
textColor = case _ of
  Dark -> "#f4f7fb"
  Light -> "#1f2933"

mutedTextColor :: Theme -> String
mutedTextColor = case _ of
  Dark -> "#c8d0dc"
  Light -> "#536271"

borderColor :: Theme -> String
borderColor = case _ of
  Dark -> "#30343c"
  Light -> "#d2d9e3"

controlBg :: Theme -> String
controlBg = case _ of
  Dark -> "#111820"
  Light -> "#ffffff"

controlBorder :: Theme -> String
controlBorder = case _ of
  Dark -> "#343c46"
  Light -> "#c7d0dc"

buttonBg :: Theme -> String
buttonBg = case _ of
  Dark -> "#252b33"
  Light -> "#ffffff"

inputBg :: Theme -> Boolean -> String
inputBg theme disabled =
  if disabled then disabledBg theme else controlBg theme

inputText :: Theme -> Boolean -> String
inputText theme disabled =
  if disabled then disabledText theme else textColor theme

createButtonBg :: Theme -> Boolean -> String
createButtonBg theme disabled =
  if disabled then disabledBg theme else buttonBg theme

createButtonText :: Theme -> Boolean -> String
createButtonText theme disabled =
  if disabled then disabledText theme else textColor theme

disabledBg :: Theme -> String
disabledBg = case _ of
  Dark -> "#20262e"
  Light -> "#e4e9f0"

disabledText :: Theme -> String
disabledText = case _ of
  Dark -> "#7d8793"
  Light -> "#7a8795"

panelShadow :: Theme -> String
panelShadow = case _ of
  Dark -> "inset 0 1px 0 rgba(255, 255, 255, 0.05)"
  Light -> "0 1px 2px rgba(15, 23, 42, 0.08)"

knobButtonBorder :: Theme -> String
knobButtonBorder = case _ of
  Dark -> "#343c46"
  Light -> "#c7d0dc"

knobButtonBg :: Theme -> String
knobButtonBg = case _ of
  Dark -> "linear-gradient(#252b33, #1d2229)"
  Light -> "linear-gradient(#ffffff, #e8edf4)"

knobShadow :: Theme -> String
knobShadow = case _ of
  Dark -> "drop-shadow(0 8px 10px rgba(0, 0, 0, 0.35))"
  Light -> "drop-shadow(0 6px 8px rgba(15, 23, 42, 0.18))"

knobOuterFill :: Theme -> String
knobOuterFill = case _ of
  Dark -> "#111820"
  Light -> "#dce3ec"

knobOuterStroke :: Theme -> String
knobOuterStroke = case _ of
  Dark -> "#46515e"
  Light -> "#9ca8b8"

knobInnerFill :: Theme -> String
knobInnerFill = case _ of
  Dark -> "#2a343f"
  Light -> "#f8fafc"

knobInnerStroke :: Theme -> String
knobInnerStroke = case _ of
  Dark -> "#0a0d11"
  Light -> "#b5bfcc"

knobIndicatorStroke :: Theme -> String
knobIndicatorStroke = case _ of
  Dark -> "#79f0b0"
  Light -> "#167a4a"

knobIndicatorDotFill :: Theme -> String
knobIndicatorDotFill = case _ of
  Dark -> "#d6ffe7"
  Light -> "#18a360"

handleAction
  :: forall output m
   . MonadEffect m
  => Action
  -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    { emitter, listener } <- H.liftEffect HS.create
    void $ H.subscribe emitter
    connection <- H.liftEffect $
      WS.connect
        "ws://localhost:8080/ws"
        (\message -> HS.notify listener (ReceiveMessage message))
        (HS.notify listener SocketOpened)
        (HS.notify listener SocketClosed)
    H.modify_ \st -> st { connection = Just connection }

  ReceiveMessage raw ->
    case decodeServerMessage raw of
      Left err ->
        H.liftEffect $ log ("Ignoring WebSocket message: " <> err)
      Right message ->
        applyServerMessage message

  SocketOpened ->
    pure unit

  SocketClosed ->
    H.modify_ \st -> st { connection = Nothing }

  SelectSong songId ->
    sendClientMessage (SelectSongMessage songId)

  SelectTrack trackId ->
    sendClientMessage (SelectTrackMessage trackId)

  SelectDuplicateTrackTarget songId ->
    H.modify_ \st -> st { duplicateTrackTargetSong = Just songId }

  UpdateNewSongName name ->
    H.modify_ \st -> st { newSongName = name }

  UpdateNewTrackName name ->
    H.modify_ \st -> st { newTrackName = name }

  CreateSong -> do
    st <- H.get
    if st.newSongName == "" then
      pure unit
    else do
      sendClientMessage (CreateSongMessage st.newSongName)
      H.modify_ \st' -> st' { newSongName = "" }

  CreateTrack -> do
    st <- H.get
    case st.currentSong of
      Nothing ->
        pure unit
      Just songId ->
        if st.newTrackName == "" then
          pure unit
        else do
          sendClientMessage (CreateTrackMessage songId st.newTrackName)
          H.modify_ \st' -> st' { newTrackName = "" }

  DuplicateSong -> do
    st <- H.get
    case st.currentSong of
      Nothing ->
        pure unit
      Just songId ->
        sendClientMessage (DuplicateSongMessage songId)

  DuplicateTrack -> do
    st <- H.get
    case st.currentTrack of
      Nothing ->
        pure unit
      Just trackId ->
        case duplicateTrackTargetSongId st of
          Nothing ->
            pure unit
          Just targetSongId ->
            sendClientMessage (DuplicateTrackMessage trackId targetSongId)

  ToggleTheme ->
    H.modify_ \st -> st { theme = toggleTheme st.theme }

  IgnoreSelection ->
    pure unit

  StartParamNameEdit cc ->
    H.modify_ \st -> st { editingParamName = Just { cc: cc, draft: paramRawName st.params cc } }

  UpdateParamNameDraft name ->
    H.modify_ \st -> st { editingParamName = map (\edit -> edit { draft = name }) st.editingParamName }

  CommitParamNameEdit ->
    commitParamNameEdit

  CommitParamNameEditOnKey key ->
    case key of
      "Enter" ->
        commitParamNameEdit
      "Escape" ->
        H.modify_ \st -> st { editingParamName = Nothing }
      _ ->
        pure unit

  StartParamDrag cc startValue startY ->
    H.subscribe' \subscriptionId ->
      HS.makeEmitter \push ->
        Drag.trackVerticalDrag startY
          (\deltaY -> push (SetParamFromDrag cc (clampParam (startValue - deltaY))))
          (push (EndParamDrag subscriptionId))

  SetParamFromDrag cc value ->
    setParam cc value

  SetParamFromWheel cc event -> do
    H.liftEffect $ Event.preventDefault (Wheel.toEvent event)
    let step = wheelParamStep event
    if step == 0 then
      pure unit
    else do
      st <- H.get
      setParam cc (paramValue st.params cc + step)

  EndParamDrag subscriptionId ->
    H.unsubscribe subscriptionId

applyServerMessage
  :: forall output m
   . ServerMessage
  -> H.HalogenM State Action () output m Unit
applyServerMessage = case _ of
  Snapshot snapshot ->
    H.modify_ \st -> st
      { songs = snapshot.songs
      , tracks = snapshot.tracks
      , currentSong = snapshot.currentSong
      , currentTrack = snapshot.currentTrack
      , params = paramsFromSnapshot snapshot.params
      , duplicateTrackTargetSong = snapshot.currentSong
      , editingParamName = Nothing
      }

  ParamUpdate cc value ->
    H.modify_ \st -> st { params = setParamValueInMap cc value st.params }

setParam
  :: forall output m
   . MonadEffect m
  => Int
  -> Int
  -> H.HalogenM State Action () output m Unit
setParam cc rawValue = do
  let value = clampParam rawValue
  H.modify_ \st -> st { params = setParamValueInMap cc value st.params }
  sendClientMessage (SetParamMessage cc value)

commitParamNameEdit
  :: forall output m
   . MonadEffect m
  => H.HalogenM State Action () output m Unit
commitParamNameEdit = do
  st <- H.get
  case st.editingParamName of
    Nothing ->
      pure unit
    Just edit -> do
      let previousName = paramRawName st.params edit.cc
      if previousName == edit.draft then
        H.modify_ \st' -> st' { editingParamName = Nothing }
      else do
        H.modify_ \st' -> st'
          { editingParamName = Nothing
          , params = setParamNameInMap edit.cc edit.draft st'.params
          }
        sendClientMessage (RenameParamMessage edit.cc edit.draft)

sendClientMessage
  :: forall output m
   . MonadEffect m
  => ClientMessage
  -> H.HalogenM State Action () output m Unit
sendClientMessage message = do
  st <- H.get
  case st.connection of
    Nothing ->
      pure unit
    Just connection ->
      H.liftEffect $ WS.send connection (encodeClientMessage message)

decodeServerMessage :: String -> Either String ServerMessage
decodeServerMessage raw = do
  json <- jsonParser raw
  tagged <- decodeTaggedMessage json
  case tagged.tag of
    "snapshot" -> Snapshot <$> decodeSnapshotPayload json
    "paramUpdate" -> do
      payload <- decodeParamUpdatePayload json
      pure (ParamUpdate payload.cc payload.value)
    other -> Left ("unknown server message tag: " <> other)

decodeTaggedMessage :: Json -> Either String TaggedMessage
decodeTaggedMessage = decodeWith

decodeSnapshotPayload :: Json -> Either String SnapshotPayload
decodeSnapshotPayload = decodeWith

decodeParamUpdatePayload :: Json -> Either String ParamUpdatePayload
decodeParamUpdatePayload = decodeWith

decodeWith :: forall a. DecodeJson a => Json -> Either String a
decodeWith json =
  case decodeJson json of
    Left err -> Left (printJsonDecodeError err)
    Right value -> Right value

encodeClientMessage :: ClientMessage -> String
encodeClientMessage = stringify <<< encodeClientJson

encodeClientJson :: ClientMessage -> Json
encodeClientJson = case _ of
  SelectSongMessage songId ->
    encodeJson { tag: "selectSong", id: songId }
  SelectTrackMessage trackId ->
    encodeJson { tag: "selectTrack", id: trackId }
  SetParamMessage cc value ->
    encodeJson { tag: "setParam", cc: cc, value: value }
  RenameParamMessage cc name ->
    encodeJson { tag: "renameParam", cc: cc, name: name }
  CreateSongMessage name ->
    encodeJson { tag: "createSong", name: name }
  CreateTrackMessage songId name ->
    encodeJson { tag: "createTrack", songId: songId, name: name }
  DuplicateSongMessage songId ->
    encodeJson { tag: "duplicateSong", songId: songId }
  DuplicateTrackMessage trackId targetSongId ->
    encodeJson { tag: "duplicateTrack", trackId: trackId, targetSongId: targetSongId }

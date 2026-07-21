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
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import FFI.WebSocket as WS
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)

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

type State =
  { songs :: Array SongInfo
  , tracks :: Array TrackInfo
  , currentSong :: Maybe Int
  , currentTrack :: Maybe Int
  , params :: Map Int Int
  , newSongName :: String
  , newTrackName :: String
  , connection :: Maybe WS.WebSocketConnection
  }

type TaggedMessage =
  { tag :: String }

type SnapshotPayload =
  { songs :: Array SongInfo
  , tracks :: Array TrackInfo
  , currentSong :: Maybe Int
  , currentTrack :: Maybe Int
  , params :: Map Int Int
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
  | CreateSongMessage String
  | CreateTrackMessage Int String

data Action
  = Initialize
  | ReceiveMessage String
  | SocketOpened
  | SocketClosed
  | SelectSong Int
  | SelectTrack Int
  | UpdateNewSongName String
  | UpdateNewTrackName String
  | CreateSong
  | CreateTrack
  | SetParamFromInput Int String

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
  , connection: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.main
    [ HP.style "font-family: system-ui, sans-serif; max-width: 980px; margin: 0 auto; padding: 24px; line-height: 1.4;" ]
    [ HH.h1_ [ HH.text "Medibox" ]
    , HH.div
        [ HP.style "display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 24px; align-items: start;" ]
        [ songsPanel st
        , tracksPanel st
        ]
    , paramsPanel st
    ]

songsPanel :: forall m. State -> H.ComponentHTML Action () m
songsPanel st =
  HH.section_
    [ HH.h2_ [ HH.text "Songs" ]
    , HH.ul_ (map (songItem st.currentSong) st.songs)
    , HH.div
        [ HP.style "display: flex; gap: 8px; margin-top: 12px;" ]
        [ HH.input
            [ HP.type_ HP.InputText
            , HP.placeholder "New song"
            , HP.value st.newSongName
            , HE.onValueInput UpdateNewSongName
            ]
        , HH.button
            [ HP.type_ HP.ButtonButton
            , HP.disabled (st.newSongName == "")
            , HE.onClick \_ -> CreateSong
            ]
            [ HH.text "Create Song" ]
        ]
    ]

songItem :: forall m. Maybe Int -> SongInfo -> H.ComponentHTML Action () m
songItem currentSong song =
  HH.li_
    [ HH.button
        [ HP.type_ HP.ButtonButton
        , HP.style (selectButtonStyle (isSelected currentSong song.id))
        , HE.onClick \_ -> SelectSong song.id
        ]
        [ HH.text song.name ]
    ]

tracksPanel :: forall m. State -> H.ComponentHTML Action () m
tracksPanel st =
  HH.section_
    [ HH.h2_ [ HH.text "Tracks" ]
    , HH.ul_ (map (trackItem st.currentTrack) visibleTracks)
    , HH.div
        [ HP.style "display: flex; gap: 8px; margin-top: 12px;" ]
        [ HH.input
            [ HP.type_ HP.InputText
            , HP.placeholder "New track"
            , HP.value st.newTrackName
            , HP.disabled noSongSelected
            , HE.onValueInput UpdateNewTrackName
            ]
        , HH.button
            [ HP.type_ HP.ButtonButton
            , HP.disabled (noSongSelected || st.newTrackName == "")
            , HE.onClick \_ -> CreateTrack
            ]
            [ HH.text "Create Track" ]
        ]
    ]
  where
  noSongSelected = st.currentSong == Nothing

  visibleTracks = case st.currentSong of
    Nothing -> []
    Just _ -> st.tracks

trackItem :: forall m. Maybe Int -> TrackInfo -> H.ComponentHTML Action () m
trackItem currentTrack track =
  HH.li_
    [ HH.button
        [ HP.type_ HP.ButtonButton
        , HP.style (selectButtonStyle (isSelected currentTrack track.id))
        , HE.onClick \_ -> SelectTrack track.id
        ]
        [ HH.text (show track.position <> ". " <> track.name) ]
    ]

paramsPanel :: forall m. State -> H.ComponentHTML Action () m
paramsPanel st =
  HH.section
    [ HP.style "margin-top: 28px;" ]
    [ HH.h2_ [ HH.text "Parameters" ]
    , HH.div
        [ HP.style "display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 12px;" ]
        (map paramControl (paramEntries st.params))
    ]

paramControl :: forall m. Tuple Int Int -> H.ComponentHTML Action () m
paramControl (Tuple cc value) =
  HH.label
    [ HP.style "display: grid; gap: 6px; padding: 10px; border: 1px solid #ddd;" ]
    [ HH.span_ [ HH.text ("CC " <> show cc <> ": " <> show value) ]
    , HH.input
        [ HP.type_ HP.InputRange
        , HP.min 0.0
        , HP.max 127.0
        , HP.value (show value)
        , HE.onValueInput (SetParamFromInput cc)
        ]
    ]

paramEntries :: Map Int Int -> Array (Tuple Int Int)
paramEntries params =
  Array.mapMaybe
    (\cc -> map (Tuple cc) (Map.lookup cc params))
    (Array.range 0 127)

isSelected :: Maybe Int -> Int -> Boolean
isSelected selected itemId = case selected of
  Just selectedId -> selectedId == itemId
  Nothing -> false

selectButtonStyle :: Boolean -> String
selectButtonStyle selected =
  "display: block; width: 100%; text-align: left; padding: 6px 8px; margin: 2px 0;"
    <> if selected then " font-weight: 700; background: #eef2ff;" else ""

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

  SetParamFromInput cc rawValue ->
    case Int.fromString rawValue of
      Nothing ->
        pure unit
      Just parsed -> do
        let value = clamp 0 127 parsed
        H.modify_ \st -> st { params = Map.insert cc value st.params }
        sendClientMessage (SetParamMessage cc value)

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
      , params = snapshot.params
      }

  ParamUpdate cc value ->
    H.modify_ \st -> st { params = Map.insert cc value st.params }

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
  CreateSongMessage name ->
    encodeJson { tag: "createSong", name: name }
  CreateTrackMessage songId name ->
    encodeJson { tag: "createTrack", songId: songId, name: name }

-- | JSON wire protocol between the backend and the PureScript
-- frontend. Kept intentionally small and flat.
module Medibox.Protocol
    ( SongInfo (..)
    , TrackInfo (..)
    , fromSong
    , fromTrack
    , ServerMsg (..)
    , ClientMsg (..)
    ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)
import Medibox.Store (Song (..), Track (..))

data SongInfo = SongInfo {siId :: Int, siName :: Text}
    deriving (Eq, Show, Generic)

data TrackInfo = TrackInfo {tiId :: Int, tiName :: Text, tiPosition :: Int}
    deriving (Eq, Show, Generic)

fromSong :: Song -> SongInfo
fromSong (Song i n) = SongInfo i n

fromTrack :: Track -> TrackInfo
fromTrack (Track i _ n p) = TrackInfo i n p

instance ToJSON SongInfo where
    toJSON (SongInfo i n) = object ["id" .= i, "name" .= n]

instance ToJSON TrackInfo where
    toJSON (TrackInfo i n p) = object ["id" .= i, "name" .= n, "position" .= p]

-- | Messages the backend pushes to a connected browser.
data ServerMsg
    = -- | Full state after connecting or switching song/track.
      Snapshot
        { snSongs :: [SongInfo]
        , snTracks :: [TrackInfo]
        , snCurrentSong :: Maybe Int
        , snCurrentTrack :: Maybe Int
        , snParams :: [(Int, Int)]
        }
    | -- | One parameter changed (from the device or another client).
      ParamUpdate {puCC :: Int, puValue :: Int}
    deriving (Eq, Show)

instance ToJSON ServerMsg where
    toJSON Snapshot{..} =
        object
            [ "tag" .= ("snapshot" :: Text)
            , "songs" .= snSongs
            , "tracks" .= snTracks
            , "currentSong" .= snCurrentSong
            , "currentTrack" .= snCurrentTrack
            , "params" .= snParams
            ]
    toJSON ParamUpdate{..} =
        object ["tag" .= ("paramUpdate" :: Text), "cc" .= puCC, "value" .= puValue]

-- | Messages a browser sends to the backend.
data ClientMsg
    = SelectSong Int
    | SelectTrack Int
    | SetParam {spCC :: Int, spValue :: Int}
    | CreateSong Text
    | CreateTrack {ctSongId :: Int, ctName :: Text}
    deriving (Eq, Show)

instance FromJSON ClientMsg where
    parseJSON = withObject "ClientMsg" $ \o -> do
        tag <- o .: "tag" :: Parser Text
        case tag of
            "selectSong" -> SelectSong <$> o .: "id"
            "selectTrack" -> SelectTrack <$> o .: "id"
            "setParam" -> SetParam <$> o .: "cc" <*> o .: "value"
            "createSong" -> CreateSong <$> o .: "name"
            "createTrack" -> CreateTrack <$> o .: "songId" <*> o .: "name"
            other -> fail $ "unknown client message tag: " ++ show other

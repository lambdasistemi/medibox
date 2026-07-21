{- | JSON wire protocol between the backend and the PureScript
frontend. Kept intentionally small and flat.
-}
module Medibox.Protocol (
    SongInfo (..),
    TrackInfo (..),
    ParamInfo (..),
    fromSong,
    fromTrack,
    fromParam,
    ServerMsg (..),
    ClientMsg (..),
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)
import Medibox.Store (Param (..), Song (..), Track (..))

data SongInfo = SongInfo {siId :: Int, siName :: Text}
    deriving (Eq, Show, Generic)

data TrackInfo = TrackInfo {tiId :: Int, tiName :: Text, tiPosition :: Int}
    deriving (Eq, Show, Generic)

data ParamInfo = ParamInfo {piCC :: Int, piValue :: Int, piName :: Maybe Text}
    deriving (Eq, Show, Generic)

fromSong :: Song -> SongInfo
fromSong (Song i n) = SongInfo i n

fromTrack :: Track -> TrackInfo
fromTrack (Track i _ n p) = TrackInfo i n p

fromParam :: Param -> ParamInfo
fromParam (Param cc v n) = ParamInfo cc v n

instance ToJSON SongInfo where
    toJSON (SongInfo i n) = object ["id" .= i, "name" .= n]

instance ToJSON TrackInfo where
    toJSON (TrackInfo i n p) = object ["id" .= i, "name" .= n, "position" .= p]

instance ToJSON ParamInfo where
    toJSON (ParamInfo cc v n) = object ["cc" .= cc, "value" .= v, "name" .= n]

-- | Messages the backend pushes to a connected browser.
data ServerMsg
    = -- | Full state after connecting or switching song/track.
      Snapshot
        { snSongs :: [SongInfo]
        , snTracks :: [TrackInfo]
        , snCurrentSong :: Maybe Int
        , snCurrentTrack :: Maybe Int
        , snParams :: [ParamInfo]
        }
    | {- | One parameter's value changed (from the device or another
      client). Names never change via this message; renaming
      triggers a full 'Snapshot' instead.
      -}
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
    | RenameParam {rpCC :: Int, rpName :: Text}
    | CreateSong Text
    | CreateTrack {ctSongId :: Int, ctName :: Text}
    | DuplicateSong {dsSongId :: Int}
    | DuplicateTrack {dtTrackId :: Int, dtTargetSongId :: Int}
    deriving (Eq, Show)

instance FromJSON ClientMsg where
    parseJSON = withObject "ClientMsg" $ \o -> do
        tag <- o .: "tag" :: Parser Text
        case tag of
            "selectSong" -> SelectSong <$> o .: "id"
            "selectTrack" -> SelectTrack <$> o .: "id"
            "setParam" -> SetParam <$> o .: "cc" <*> o .: "value"
            "renameParam" -> RenameParam <$> o .: "cc" <*> o .: "name"
            "createSong" -> CreateSong <$> o .: "name"
            "createTrack" -> CreateTrack <$> o .: "songId" <*> o .: "name"
            "duplicateSong" -> DuplicateSong <$> o .: "songId"
            "duplicateTrack" -> DuplicateTrack <$> o .: "trackId" <*> o .: "targetSongId"
            other -> fail $ "unknown client message tag: " ++ show other

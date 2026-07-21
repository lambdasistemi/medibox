{- | SQLite persistence: songs, tracks, and per-track CC parameter
values.
-}
module Medibox.Store (
    Store,
    openStore,
    Song (..),
    Track (..),
    Param (..),
    listSongs,
    listTracks,
    createSong,
    createTrack,
    duplicateSong,
    duplicateTrack,
    loadTrackParams,
    trackSongOf,
    setParam,
    renameParam,
    currentTrackId,
    setCurrentTrackId,
) where

import Control.Monad (forM_, unless, when)
import Data.Text (Text)
import Database.SQLite.Simple

newtype Store = Store Connection

{- | Number of physical CC controls on the BCR2000, seeded to 0 for
the auto-created default song/track.
-}
physicalCCCount :: Int
physicalCCCount = 32

data Song = Song {songId :: Int, songName :: Text}
    deriving (Eq, Show)

data Track = Track {trackId :: Int, trackSongId :: Int, trackName :: Text, trackPosition :: Int}
    deriving (Eq, Show)

{- | One stored CC value, with an optional user-given label specific
to the track it belongs to.
-}
data Param = Param {paramCC :: Int, paramValue :: Int, paramName :: Maybe Text}
    deriving (Eq, Show)

{- | Open (creating if needed) the sqlite database at the given path
and ensure the schema exists.
-}
openStore :: FilePath -> IO Store
openStore path = do
    conn <- open path
    execute_ conn "CREATE TABLE IF NOT EXISTS songs (id INTEGER PRIMARY KEY, name TEXT NOT NULL)"
    execute_ conn $
        Query
            "CREATE TABLE IF NOT EXISTS tracks \
            \(id INTEGER PRIMARY KEY, song_id INTEGER NOT NULL REFERENCES songs(id), \
            \name TEXT NOT NULL, position INTEGER NOT NULL)"
    execute_ conn $
        Query
            "CREATE TABLE IF NOT EXISTS parameters \
            \(track_id INTEGER NOT NULL, cc INTEGER NOT NULL, value INTEGER NOT NULL, \
            \name TEXT, PRIMARY KEY (track_id, cc))"
    execute_ conn "CREATE TABLE IF NOT EXISTS app_state (key TEXT PRIMARY KEY, value TEXT)"
    migrateParamNameColumn conn
    let store = Store conn
    ensureDefaultSong store
    pure store

{- | Databases created before per-track param names existed have a
@parameters@ table without the @name@ column; add it if missing.
-}
migrateParamNameColumn :: Connection -> IO ()
migrateParamNameColumn conn = do
    cols <- query_ conn "PRAGMA table_info(parameters)" :: IO [(Int, Text, Text, Int, Maybe Text, Int)]
    let hasName = any (\(_, colName, _, _, _, _) -> colName == "name") cols
    unless hasName $
        execute_ conn "ALTER TABLE parameters ADD COLUMN name TEXT"

{- | On a fresh database, create a "Default" song/track with every
physical CC seeded to 0, so the UI always has something to select
and the device gets a known-zero starting state.
-}
ensureDefaultSong :: Store -> IO ()
ensureDefaultSong store = do
    songs <- listSongs store
    when (null songs) $ do
        song <- createSong store "Default"
        track <- createTrack store (songId song) "Default"
        forM_ [0 .. physicalCCCount - 1] $ \cc -> setParam store (trackId track) cc 0
        setCurrentTrackId store (trackId track)

listSongs :: Store -> IO [Song]
listSongs (Store conn) =
    map (uncurry Song) <$> query_ conn "SELECT id, name FROM songs ORDER BY id"

listTracks :: Store -> Int -> IO [Track]
listTracks (Store conn) sid =
    map (\(i, s, n, p) -> Track i s n p)
        <$> query
            conn
            "SELECT id, song_id, name, position FROM tracks WHERE song_id = ? ORDER BY position"
            (Only sid)

createSong :: Store -> Text -> IO Song
createSong (Store conn) name = do
    execute conn "INSERT INTO songs (name) VALUES (?)" (Only name)
    sid <- fromIntegral <$> lastInsertRowId conn
    pure $ Song sid name

createTrack :: Store -> Int -> Text -> IO Track
createTrack (Store conn) sid name = do
    [Only nextPos] <-
        query
            conn
            "SELECT COALESCE(MAX(position) + 1, 0) FROM tracks WHERE song_id = ?"
            (Only sid)
    execute
        conn
        "INSERT INTO tracks (song_id, name, position) VALUES (?, ?, ?)"
        (sid, name, nextPos :: Int)
    tid <- fromIntegral <$> lastInsertRowId conn
    pure $ Track tid sid name nextPos

{- | Copy a track's stored parameters (values and names) onto another
track. Internal helper for duplication.
-}
copyParamsTo :: Store -> Int -> Int -> IO ()
copyParamsTo store fromTid toTid = do
    params <- loadTrackParams store fromTid
    forM_ params $ \p -> do
        setParam store toTid (paramCC p) (paramValue p)
        forM_ (paramName p) $ renameParam store toTid (paramCC p)

{- | Deep-copy a song: a new song with the same name (suffixed) and a
copy of every track (with its parameter values and names).
-}
duplicateSong :: Store -> Int -> IO Song
duplicateSong store sid = do
    songs <- listSongs store
    case filter ((== sid) . songId) songs of
        [] -> error "duplicateSong: unknown song id"
        (original : _) -> do
            newSong <- createSong store (songName original <> " copy")
            tracks <- listTracks store sid
            forM_ tracks $ \t -> do
                newTrack <- createTrack store (songId newSong) (trackName t)
                copyParamsTo store (trackId t) (trackId newTrack)
            pure newSong

{- | Deep-copy a single track's parameter values and names into a new
track under the given (possibly different) song.
-}
duplicateTrack :: Store -> Int -> Int -> IO Track
duplicateTrack store tid targetSongId = do
    tracks <- concat <$> (mapM (listTracks store . songId) =<< listSongs store)
    case filter ((== tid) . trackId) tracks of
        [] -> error "duplicateTrack: unknown track id"
        (original : _) -> do
            newTrack <- createTrack store targetSongId (trackName original <> " copy")
            copyParamsTo store tid (trackId newTrack)
            pure newTrack

{- | Every parameter stored for a track (missing CCs are simply
absent, the caller decides on a default).
-}
loadTrackParams :: Store -> Int -> IO [Param]
loadTrackParams (Store conn) tid =
    map (\(cc, v, n) -> Param cc v n)
        <$> query conn "SELECT cc, value, name FROM parameters WHERE track_id = ?" (Only tid)

-- | Which song a track belongs to, if it exists.
trackSongOf :: Store -> Int -> IO (Maybe Int)
trackSongOf (Store conn) tid = do
    rows <- query conn "SELECT song_id FROM tracks WHERE id = ?" (Only tid)
    pure $ case rows of
        [Only sid] -> Just sid
        _ -> Nothing

setParam :: Store -> Int -> Int -> Int -> IO ()
setParam (Store conn) tid cc v =
    execute
        conn
        "INSERT INTO parameters (track_id, cc, value) VALUES (?, ?, ?) \
        \ON CONFLICT (track_id, cc) DO UPDATE SET value = excluded.value"
        (tid, cc, v)

{- | Give a CC a track-specific label, creating a zero-value row for
it first if it hasn't been touched yet.
-}
renameParam :: Store -> Int -> Int -> Text -> IO ()
renameParam (Store conn) tid cc name = do
    execute
        conn
        "INSERT INTO parameters (track_id, cc, value, name) VALUES (?, ?, 0, ?) \
        \ON CONFLICT (track_id, cc) DO UPDATE SET name = excluded.name"
        (tid, cc, name)

currentTrackId :: Store -> IO (Maybe Int)
currentTrackId (Store conn) = do
    rows <- query_ conn "SELECT value FROM app_state WHERE key = 'current_track_id'"
    pure $ case rows of
        [Only v] -> Just (read v)
        _ -> Nothing

setCurrentTrackId :: Store -> Int -> IO ()
setCurrentTrackId (Store conn) tid =
    execute
        conn
        "INSERT INTO app_state (key, value) VALUES ('current_track_id', ?) \
        \ON CONFLICT (key) DO UPDATE SET value = excluded.value"
        (Only (show tid))

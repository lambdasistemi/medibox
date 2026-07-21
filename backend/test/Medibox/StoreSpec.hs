module Medibox.StoreSpec (spec) where

import Data.List (find)
import Data.Text (Text)
import Medibox.Store
import Test.Hspec
import Test.QuickCheck

genCC :: Gen Int
genCC = choose (0, 127)

genValue :: Gen Int
genValue = choose (0, 127)

valueOf :: Int -> [Param] -> Maybe Int
valueOf cc = fmap paramValue . find ((== cc) . paramCC)

nameOf :: Int -> [Param] -> Maybe Text
nameOf cc params = find ((== cc) . paramCC) params >>= paramName

spec :: Spec
spec = around (\act -> openStore ":memory:" >>= act) $ do
    describe "songs and tracks" $ do
        it "lists a created song" $ \store -> do
            song <- createSong store "Test Song"
            songs <- listSongs store
            map songId songs `shouldContain` [songId song]

        it "lists tracks for a song in position order" $ \store -> do
            song <- createSong store "Song"
            t0 <- createTrack store (songId song) "A"
            t1 <- createTrack store (songId song) "B"
            trackPosition t0 `shouldBe` 0
            trackPosition t1 `shouldBe` 1
            tracks <- listTracks store (songId song)
            map trackId tracks `shouldBe` [trackId t0, trackId t1]

        it "does not list tracks belonging to a different song" $ \store -> do
            songA <- createSong store "A"
            songB <- createSong store "B"
            _ <- createTrack store (songId songA) "trackA"
            tracks <- listTracks store (songId songB)
            tracks `shouldBe` []

        it "resolves the song a track belongs to" $ \store -> do
            song <- createSong store "Song"
            track <- createTrack store (songId song) "Track"
            trackSongOf store (trackId track) `shouldReturn` Just (songId song)

        it "has no song for an unknown track id" $ \store ->
            trackSongOf store 999 `shouldReturn` Nothing

    describe "parameters" $ do
        it "persists a set value and reads it back" $ \store ->
            forAll ((,) <$> genCC <*> genValue) $ \(cc, v) -> ioProperty $ do
                song <- createSong store "S"
                track <- createTrack store (songId song) "T"
                setParam store (trackId track) cc v
                params <- loadTrackParams store (trackId track)
                pure $ valueOf cc params === Just v

        it "last write wins for the same cc" $ \store -> do
            song <- createSong store "S"
            track <- createTrack store (songId song) "T"
            setParam store (trackId track) 5 10
            setParam store (trackId track) 5 20
            params <- loadTrackParams store (trackId track)
            valueOf 5 params `shouldBe` Just 20

        it "keeps parameters isolated per track" $ \store -> do
            song <- createSong store "S"
            trackA <- createTrack store (songId song) "A"
            trackB <- createTrack store (songId song) "B"
            setParam store (trackId trackA) 1 42
            paramsB <- loadTrackParams store (trackId trackB)
            paramsB `shouldBe` []

        it "has no name until one is given" $ \store -> do
            song <- createSong store "S"
            track <- createTrack store (songId song) "T"
            setParam store (trackId track) 3 10
            params <- loadTrackParams store (trackId track)
            nameOf 3 params `shouldBe` Nothing

        it "remembers a renamed param's name and value" $ \store -> do
            song <- createSong store "S"
            track <- createTrack store (songId song) "T"
            setParam store (trackId track) 3 10
            renameParam store (trackId track) 3 "Cutoff"
            params <- loadTrackParams store (trackId track)
            nameOf 3 params `shouldBe` Just "Cutoff"
            valueOf 3 params `shouldBe` Just 10

        it "renaming a never-set param creates it with value 0" $ \store -> do
            song <- createSong store "S"
            track <- createTrack store (songId song) "T"
            renameParam store (trackId track) 7 "Resonance"
            params <- loadTrackParams store (trackId track)
            nameOf 7 params `shouldBe` Just "Resonance"
            valueOf 7 params `shouldBe` Just 0

        it "keeps names isolated per track" $ \store -> do
            song <- createSong store "S"
            trackA <- createTrack store (songId song) "A"
            trackB <- createTrack store (songId song) "B"
            renameParam store (trackId trackA) 3 "Cutoff"
            paramsB <- loadTrackParams store (trackId trackB)
            nameOf 3 paramsB `shouldBe` Nothing

    describe "renaming songs and tracks" $ do
        it "renames a song" $ \store -> do
            song <- createSong store "Old"
            renameSong store (songId song) "New"
            songs <- listSongs store
            map songName (filter ((== songId song) . songId) songs) `shouldBe` ["New"]

        it "renames a track" $ \store -> do
            song <- createSong store "S"
            track <- createTrack store (songId song) "Old"
            renameTrack store (trackId track) "New"
            tracks <- listTracks store (songId song)
            map trackName (filter ((== trackId track) . trackId) tracks) `shouldBe` ["New"]

    describe "duplication" $ do
        it "duplicateTrack copies values and names into a new track" $ \store -> do
            song <- createSong store "S"
            track <- createTrack store (songId song) "T"
            setParam store (trackId track) 1 55
            renameParam store (trackId track) 1 "Cutoff"
            newTrack <- duplicateTrack store (trackId track) (songId song)
            newTrack `shouldNotBe` track
            params <- loadTrackParams store (trackId newTrack)
            valueOf 1 params `shouldBe` Just 55
            nameOf 1 params `shouldBe` Just "Cutoff"

        it "duplicateTrack can target a different song" $ \store -> do
            songA <- createSong store "A"
            songB <- createSong store "B"
            track <- createTrack store (songId songA) "T"
            setParam store (trackId track) 1 55
            newTrack <- duplicateTrack store (trackId track) (songId songB)
            trackSongOf store (trackId newTrack) `shouldReturn` Just (songId songB)

        it "duplicateSong copies every track's values and names" $ \store -> do
            song <- createSong store "S"
            trackA <- createTrack store (songId song) "A"
            trackB <- createTrack store (songId song) "B"
            setParam store (trackId trackA) 1 55
            renameParam store (trackId trackA) 1 "Cutoff"
            setParam store (trackId trackB) 2 77
            newSong <- duplicateSong store (songId song)
            newSong `shouldNotBe` song
            newTracks <- listTracks store (songId newSong)
            length newTracks `shouldBe` 2
            paramsPerTrack <- mapM (loadTrackParams store . trackId) newTracks
            let allParams = concat paramsPerTrack
            valueOf 1 allParams `shouldBe` Just 55
            nameOf 1 allParams `shouldBe` Just "Cutoff"
            valueOf 2 allParams `shouldBe` Just 77

    describe "current track" $ do
        it "defaults to the auto-created Default track on a fresh store" $ \store ->
            currentTrackId store `shouldReturn` Just 1

        it "remembers the current track across reads" $ \store -> do
            song <- createSong store "S"
            track <- createTrack store (songId song) "T"
            setCurrentTrackId store (trackId track)
            currentTrackId store `shouldReturn` Just (trackId track)

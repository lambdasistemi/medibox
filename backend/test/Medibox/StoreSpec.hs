module Medibox.StoreSpec (spec) where

import Medibox.Store
import Test.Hspec
import Test.QuickCheck

genCC :: Gen Int
genCC = choose (0, 127)

genValue :: Gen Int
genValue = choose (0, 127)

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
                pure $ lookup cc params === Just v

        it "last write wins for the same cc" $ \store -> do
            song <- createSong store "S"
            track <- createTrack store (songId song) "T"
            setParam store (trackId track) 5 10
            setParam store (trackId track) 5 20
            params <- loadTrackParams store (trackId track)
            lookup 5 params `shouldBe` Just 20

        it "keeps parameters isolated per track" $ \store -> do
            song <- createSong store "S"
            trackA <- createTrack store (songId song) "A"
            trackB <- createTrack store (songId song) "B"
            setParam store (trackId trackA) 1 42
            paramsB <- loadTrackParams store (trackId trackB)
            paramsB `shouldBe` []

    describe "current track" $ do
        it "defaults to the auto-created Default track on a fresh store" $ \store ->
            currentTrackId store `shouldReturn` Just 1

        it "remembers the current track across reads" $ \store -> do
            song <- createSong store "S"
            track <- createTrack store (songId song) "T"
            setCurrentTrackId store (trackId track)
            currentTrackId store `shouldReturn` Just (trackId track)

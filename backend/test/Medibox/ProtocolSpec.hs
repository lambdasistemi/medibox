module Medibox.ProtocolSpec (spec) where

import Data.Aeson
import Medibox.Protocol
import Test.Hspec

spec :: Spec
spec = do
    describe "ServerMsg JSON shape" $ do
        it "encodes a ParamUpdate with a stable tag and field names" $
            toJSON (ParamUpdate 19 64)
                `shouldBe` object
                    [ "tag" .= ("paramUpdate" :: String)
                    , "cc" .= (19 :: Int)
                    , "value" .= (64 :: Int)
                    ]

        it "encodes an empty Snapshot with nulls for unset selections" $
            toJSON (Snapshot [] [] Nothing Nothing [])
                `shouldBe` object
                    [ "tag" .= ("snapshot" :: String)
                    , "songs" .= ([] :: [Value])
                    , "tracks" .= ([] :: [Value])
                    , "currentSong" .= (Nothing :: Maybe Int)
                    , "currentTrack" .= (Nothing :: Maybe Int)
                    , "params" .= ([] :: [Value])
                    ]

        it "encodes song and track info with id/name(/position) fields" $ do
            toJSON (SongInfo 1 "Test Song")
                `shouldBe` object ["id" .= (1 :: Int), "name" .= ("Test Song" :: String)]
            toJSON (TrackInfo 2 "Track 1" 0)
                `shouldBe` object
                    [ "id" .= (2 :: Int)
                    , "name" .= ("Track 1" :: String)
                    , "position" .= (0 :: Int)
                    ]

        it "encodes param info with a nullable name" $ do
            toJSON (ParamInfo 19 64 Nothing)
                `shouldBe` object
                    [ "cc" .= (19 :: Int)
                    , "value" .= (64 :: Int)
                    , "name" .= (Nothing :: Maybe String)
                    ]
            toJSON (ParamInfo 19 64 (Just "Cutoff"))
                `shouldBe` object
                    [ "cc" .= (19 :: Int)
                    , "value" .= (64 :: Int)
                    , "name" .= Just ("Cutoff" :: String)
                    ]

    describe "ClientMsg JSON shape" $ do
        it "decodes setParam" $
            decode "{\"tag\":\"setParam\",\"cc\":19,\"value\":64}"
                `shouldBe` Just (SetParam 19 64)

        it "decodes selectSong" $
            decode "{\"tag\":\"selectSong\",\"id\":1}" `shouldBe` Just (SelectSong 1)

        it "decodes selectTrack" $
            decode "{\"tag\":\"selectTrack\",\"id\":2}" `shouldBe` Just (SelectTrack 2)

        it "decodes createSong" $
            decode "{\"tag\":\"createSong\",\"name\":\"My Song\"}"
                `shouldBe` Just (CreateSong "My Song")

        it "decodes createTrack" $
            decode "{\"tag\":\"createTrack\",\"songId\":1,\"name\":\"Track 1\"}"
                `shouldBe` Just (CreateTrack 1 "Track 1")

        it "decodes renameParam" $
            decode "{\"tag\":\"renameParam\",\"cc\":19,\"name\":\"Cutoff\"}"
                `shouldBe` Just (RenameParam 19 "Cutoff")

        it "decodes duplicateSong" $
            decode "{\"tag\":\"duplicateSong\",\"songId\":1}"
                `shouldBe` Just (DuplicateSong 1)

        it "decodes duplicateTrack" $
            decode "{\"tag\":\"duplicateTrack\",\"trackId\":1,\"targetSongId\":2}"
                `shouldBe` Just (DuplicateTrack 1 2)

        it "rejects an unknown tag" $
            decode "{\"tag\":\"bogus\"}" `shouldBe` (Nothing :: Maybe ClientMsg)

module Language.Quell.Data.BufferedConduitTSpec (spec) where

import           Language.Quell.Prelude
import           Test.Hspec

import qualified Conduit
import qualified Data.Conduit.List                   as ConduitList
import qualified Language.Quell.Data.BufferedConduit as BufferedConduit


spec :: Spec
spec = do
    describe "No back mode" do
        it "behaves usual conduit" do
            let go :: BufferedConduit.T s Int Int (ST s) ()
                go = BufferedConduit.await >>= \case
                    Nothing ->
                        pure ()
                    Just i -> do
                        BufferedConduit.yield do 10 - i
                        go
            runConduit go [1,2,3,4] `shouldBe` [9,8,7,6]

        it "allow to seek future positions" do
            let go0 :: BufferedConduit.T s Int Int (ST s) ()
                go0 = do
                    forM_ [1::Int] \_ -> BufferedConduit.await >>= \case
                        Nothing ->
                            pure ()
                        Just i -> do
                            BufferedConduit.yield i
                    BufferedConduit.seekToPosition 3
                    go

                go :: BufferedConduit.T s Int Int (ST s) ()
                go = BufferedConduit.await >>= \case
                    Nothing ->
                        pure ()
                    Just i -> do
                        BufferedConduit.yield do 10 - i
                        go
            runConduit go0 [1,2,3,4,5,6] `shouldBe` [1,6,5,4]

    describe "Need back mode" do
        it "behaves usual conduit" do
            let go0 :: BufferedConduit.T s Int Int (ST s) ()
                go0 = do
                    BufferedConduit.setBufferMode do BufferedConduit.NeedBack 1
                    go

                go :: BufferedConduit.T s Int Int (ST s) ()
                go = BufferedConduit.await >>= \case
                    Nothing ->
                        pure ()
                    Just i -> do
                        BufferedConduit.yield do 10 - i
                        go
            runConduit go0 [1,2,3,4,5] `shouldBe` [9,8,7,6,5]

        it "allow to seek future positions" do
            let go0 :: BufferedConduit.T s Int Int (ST s) ()
                go0 = do
                    BufferedConduit.setBufferMode do BufferedConduit.NeedBack 1
                    forM_ [1::Int] \_ -> BufferedConduit.await >>= \case
                        Nothing ->
                            pure ()
                        Just i -> do
                            BufferedConduit.yield i
                    BufferedConduit.seekToPosition 3
                    go

                go :: BufferedConduit.T s Int Int (ST s) ()
                go = BufferedConduit.await >>= \case
                    Nothing ->
                        pure ()
                    Just i -> do
                        BufferedConduit.yield do 10 - i
                        go
            runConduit go0 [1,2,3,4,5,6] `shouldBe` [1,6,5,4]

        it "allow to seek past positions buffered" do
            let go0 :: BufferedConduit.T s Int Int (ST s) ()
                go0 = do
                    BufferedConduit.setBufferMode do BufferedConduit.NeedBack 1
                    forM_ [1::Int,2,3,4] \_ -> do
                        BufferedConduit.await >>= \case
                            Nothing ->
                                pure ()
                            Just i -> do
                                BufferedConduit.yield i
                    BufferedConduit.seekToPosition 2
                    go

                go :: BufferedConduit.T s Int Int (ST s) ()
                go = BufferedConduit.await >>= \case
                    Nothing ->
                        pure ()
                    Just i -> do
                        BufferedConduit.yield do 10 - i
                        go
            runConduit go0 [1,2,3,4,5,6] `shouldBe` [1,2,3,4,7,6,5,4]

    describe "getCurrentPosition" do
        it "should be return current positions" do
            let go0 :: BufferedConduit.T s Char Int (ST s) ()
                go0 = do
                    BufferedConduit.getCurrentPosition >>= BufferedConduit.yield
                    BufferedConduit.setBufferMode do BufferedConduit.NeedBack 1
                    forM_ [1::Int,2,3,4] \_ -> do
                        p <- BufferedConduit.getCurrentPosition
                        BufferedConduit.await >>= \case
                            Nothing ->
                                pure ()
                            Just{} -> do
                                BufferedConduit.yield p
                    BufferedConduit.seekToPosition 1
                    BufferedConduit.setBufferMode BufferedConduit.NoBack
                    go

                go :: BufferedConduit.T s Char Int (ST s) ()
                go = BufferedConduit.await >>= \case
                    Nothing ->
                        pure ()
                    Just{} -> do
                        BufferedConduit.getCurrentPosition >>= BufferedConduit.yield
                        go
            runConduit go0 ['1','2','3','4','5','6'] `shouldBe` [0,0,1,2,3,2,3,4,5,6]

runConduit :: (forall s. BufferedConduit.T s i o (ST s) ()) -> [i] -> [o]
runConduit pipeline inputs = runST
    do Conduit.runConduit
        do ConduitList.sourceList inputs
            Conduit..| BufferedConduit.runConduitT pipeline
            Conduit..| ConduitList.consume

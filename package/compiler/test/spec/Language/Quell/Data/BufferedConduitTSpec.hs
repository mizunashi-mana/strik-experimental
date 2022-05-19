module Language.Quell.Data.BufferedConduitTSpec (spec) where

import           Language.Quell.Prelude
import           Test.Hspec

import qualified Language.Quell.Data.BufferedConduit as BufferedConduit
import qualified Conduit
import qualified Data.Conduit.List                     as ConduitList


spec :: Spec
spec = do
    describe "No back mode" do
        it "behaves usual conduit" do
            let go :: BufferedConduit.T s Int Int (ST s) ()
                go = BufferedConduit.await >>= \case
                    Nothing ->
                        pure ()
                    Just i -> do
                        BufferedConduit.yield i
                        go
            runConduit go [1,2,3,4] `shouldBe` [1,2,3,4]

runConduit :: (forall s. BufferedConduit.T s i o (ST s) ()) -> [i] -> [o]
runConduit pipeline inputs = runST
    do Conduit.runConduit
        do ConduitList.sourceList inputs
            Conduit..| BufferedConduit.runConduitT pipeline
            Conduit..| ConduitList.consume

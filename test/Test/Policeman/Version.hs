{-# LANGUAGE NumericUnderscores #-}

module Test.Policeman.Version
    ( versionSpec
    , versionRoundtripText
    , versionRoundtripInts
    ) where

import Hedgehog (MonadGen, Property, forAll, property, (===))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Policeman.Core.Version (Version (..), versionFromIntList, versionFromText, versionToIntList,
                               versionToText)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- | Version parsing unit tests.
versionSpec :: Spec
versionSpec = describe "Version parsing" $ do
    describe "From Text" $ do
        it "parses 1.2.3.4" $
            versionFromText "1.2.3.4" `shouldBe` (Just $ Version 1 2 3 4)
        it "parses 1.2.3" $
            versionFromText "1.2.3" `shouldBe` (Just $ Version 1 2 3 0)
        it "parses 1.2" $
            versionFromText "1.2" `shouldBe` (Just $ Version 1 2 0 0)
        it "parses 1" $
            versionFromText "1" `shouldBe` (Just $ Version 1 0 0 0)
        it "parses 00" $
            versionFromText "00" `shouldBe` (Just $ Version 0 0 0 0)
        it "does not parse letters" $
            versionFromText "1.2.3.a" `shouldSatisfy` isNothing
        it "does not parse trailing dot" $
            versionFromText "1.2.3." `shouldSatisfy` isNothing
        it "does not parse leading dot" $
            versionFromText ".1.2.3" `shouldSatisfy` isNothing
    describe "From List of Ints" $ do
        it "parses [1,2,3,4]" $
            versionFromIntList [1,2,3,4] `shouldBe` (Just $ Version 1 2 3 4)
        it "parses [1,2,3]" $
            versionFromIntList [1,2,3] `shouldBe` (Just $ Version 1 2 3 0)
        it "parses [1,2]" $
            versionFromIntList [1,2] `shouldBe` (Just $ Version 1 2 0 0)
        it "parses [1]" $
            versionFromIntList [1] `shouldBe` (Just $ Version 1 0 0 0)
        it "parses [1,2,3,4,5]" $
            versionFromIntList [1,2,3,4,5] `shouldBe` (Just $ Version 1 2 3 4)

-- | Parsing to/from 'Text' works properly.
versionRoundtripText :: Property
versionRoundtripText = property $ do
    version <- forAll genVersion
    versionFromText (versionToText version) === Just version

-- | Parsing to/from '[Int]' works properly.
versionRoundtripInts :: Property
versionRoundtripInts = property $ do
    version <- forAll genVersion
    versionFromIntList (versionToIntList version) === Just version

-- | Generates random version.
genVersion :: forall m . (MonadGen m) => m Version
genVersion = do
    versionA <- genInt
    versionB <- genInt
    versionC <- genInt
    versionD <- genInt
    pure Version {..}
  where
    genInt :: m Int
    genInt = Gen.int (Range.constant 0 20_000)

module Main (main) where

import Hedgehog (Group (..), checkParallel)
import Test.Hspec (describe, hspec)

import Test.Policeman.Cabal (cabalSpec)
import Test.Policeman.Version (versionRoundtripInts, versionRoundtripText, versionSpec)


main :: IO ()
main = do
    hspec $ describe "Policeman" $ do
        cabalSpec
        versionSpec

    -- property tests
    checkParallel hedgehogTests >>= \p -> if p then exitSuccess else exitFailure

hedgehogTests :: Group
hedgehogTests = Group "Roundtrip properties"
    [ versionRoundtripText  `named` "versionFromText . versionToText ≡ Just"
    , versionRoundtripInts  `named` "versionFromIntList . versionToIntList ≡ Just"
    ]
  where
    named :: a -> b -> (b, a)
    named = flip (,)

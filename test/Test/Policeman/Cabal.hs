{- | Unit tests for the cabal spec
-}

module Test.Policeman.Cabal
    ( cabalSpec
    ) where

import Distribution.PackageDescription (GenericPackageDescription)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, runIO, shouldReturn, shouldSatisfy)

import Policeman.Cabal (CabalError (..), findCabalDescription)


cabalSpec :: Spec
cabalSpec = describe "Cabal parsing" $ do
    curDir <- runIO getCurrentDirectory

    it "finds .cabal for policeman" $
        runCabal curDir >>= (`shouldSatisfy` isRight)
    it "doesn't find .cabal in src/" $ do
        let srcDir = curDir </> "src"
        runCabal srcDir `shouldReturn` Left (NoCabalFile srcDir)
    it "fails on non-existent directory" $ do
        let abcDir = curDir </> "abc"
        runCabal abcDir `shouldReturn` Left (NoSuchDirectory abcDir)
  where
    runCabal :: FilePath -> IO (Either CabalError GenericPackageDescription)
    runCabal = runExceptT . findCabalDescription

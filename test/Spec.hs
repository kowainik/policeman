module Main (main) where

import Test.Hspec (describe, hspec)

import Test.Policeman.Cabal (cabalSpec)


main :: IO ()
main = hspec $ describe "Policeman" $
    cabalSpec

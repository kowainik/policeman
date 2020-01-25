module Main (main) where

import System.IO (hSetEncoding, utf8)

import Policeman (policeman)


main :: IO ()
main = hSetEncoding stdout utf8 >> policeman

{- | Common utilities for downloading.
-}

module Policeman.Download.Common
    ( DownloadError (..)
    , evidenceDir
    ) where

import System.IO.Error (IOError)

import Policeman.Core.Package (PackageName (..))


data DownloadError
    = NoSuchPackage PackageName
    | SystemError IOError
    deriving stock (Show)

evidenceDir :: FilePath
evidenceDir = ".policeman-evidence"

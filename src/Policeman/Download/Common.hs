{- | Common utilities for downloading.
-}

module Policeman.Download.Common
    ( DownloadError (..)
    , evidenceDir
    ) where

import Policeman.Core.Package (PackageName (..))


data DownloadError
    = NoSuchPackage PackageName

evidenceDir :: FilePath
evidenceDir = ".policeman-evidence"

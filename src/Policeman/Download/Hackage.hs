{- | Functions to download Haskell packages from Hackage.
-}

module Policeman.Download.Hackage
    ( PackageName (..)
    , downloadFromHackage
    ) where

import Control.Exception (catch, throwIO)
import Shellmet (($?))
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Error (IOError, isDoesNotExistError)

import Policeman.Core.Package (PackageName (..), Version (..))
import Policeman.Download.Common (DownloadError (..), evidenceDir)


{- | This function takes 'PackageName' and previous package
'Version', downloads @.tar.gz@ archive from Hackage and unpacks it in
the current directory.
-}
downloadFromHackage :: PackageName -> Version -> IO (Either DownloadError ())
downloadFromHackage packageName@(PackageName name) (Version version) = do
    let fullName = name <> "-" <> version
    let tarName = fullName <> ".tar.gz"
    let tarUrl = mconcat
            [ "http://hackage.haskell.org/package/"
            , name
            , "/"
            , tarName
            ]

    let tarPath = toText $ evidenceDir </> toString tarName
    let srcPath = evidenceDir </> toString fullName
    createDirectoryIfMissing True evidenceDir
    removeDirIfExists srcPath

    downloadRes <- (Right <$> "curl" [tarUrl, "--output", tarPath])
        $? pure (Left $ NoSuchPackage packageName)

    case downloadRes of
        Left err -> pure (Left err)
        Right () -> Right <$> "tar" ["-xf", tarPath, "-C", toText evidenceDir]

removeDirIfExists :: FilePath -> IO ()
removeDirIfExists fileName =
    removeDirectoryRecursive fileName `catch` handleExists
  where
    handleExists :: IOError -> IO ()
    handleExists e
        | isDoesNotExistError e = pass
        | otherwise = throwIO e

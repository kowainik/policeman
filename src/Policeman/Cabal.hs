{- | Functions to work with Cabal file and its content.
-}

module Policeman.Cabal
    ( CabalError (..)
    , parseCabalFile
    , findCabalDescription
    , extractExposedModules
    , extractPackageName
    , extractPackageVersion
    ) where

import Control.Monad.Except (throwError)
import Distribution.ModuleName (ModuleName, components)
import Distribution.PackageDescription (CondTree (..), GenericPackageDescription (..), Library (..),
                                        PackageDescription (..))
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.Version (versionNumbers)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import Policeman.Core.Package (Module (..), PackageName (..))
import Policeman.Core.Version (Version, versionFromIntList)

import qualified Distribution.Types.PackageName as Cabal


{- | Errors about dealing with @.cabal@ files.
-}
data CabalError
    -- | Directory where we're looking for cabal file, not found.
    = NoSuchDirectory FilePath
    -- | No @.cabal@ file under given path.
    | NoCabalFile FilePath
    -- | Multiple cabal files found.
    | MultipleCabalFiles (NonEmpty FilePath)
    -- | Error parsing cabal file.
    | CabalParseError
    deriving stock (Show, Eq)

{- | This function takes a path to a directory, finds a cabal file
under this directory, parses it and returns parsed content.
-}
findCabalDescription :: FilePath -> ExceptT CabalError IO GenericPackageDescription
findCabalDescription dirPath = do
    unlessM (liftIO $ doesDirectoryExist dirPath) $ throwError $ NoSuchDirectory dirPath
    dirContent <- liftIO $ listDirectory dirPath
    let cabalFiles = filter (\p -> takeExtension p == ".cabal") dirContent
    case cabalFiles of
        [] -> throwError $ NoCabalFile dirPath
        [cabalFile] -> do
            let cabalPath = dirPath </> cabalFile
            cabalContent <- readFileBS cabalPath
            parseCabalFile cabalContent
        x:xs -> throwError $ MultipleCabalFiles (x :| xs)

-- | Parses the given cabal file source and returns 'GenericPackageDescription'
parseCabalFile :: ByteString -> ExceptT CabalError IO GenericPackageDescription
parseCabalFile cabalContent = whenNothing (parseGenericPackageDescriptionMaybe cabalContent) $
    throwError CabalParseError  -- TODO: better error

extractPackageName :: GenericPackageDescription -> PackageName
extractPackageName =
    PackageName
    . toText
    . Cabal.unPackageName
    . pkgName
    . package
    . packageDescription

extractPackageVersion :: GenericPackageDescription -> Maybe Version
extractPackageVersion =
    versionFromIntList
    . versionNumbers
    . pkgVersion
    . package
    . packageDescription

extractExposedModules :: GenericPackageDescription -> [Module]
extractExposedModules =
    map toModule
    . concatMap (exposedModules . condTreeData)
    . maybeToList
    . condLibrary
  where
    toModule :: ModuleName -> Module
    toModule = Module . toText . intercalate "." . components

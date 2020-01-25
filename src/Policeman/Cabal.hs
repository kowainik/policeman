{- | Functions to work with Cabal file and its content.
-}

module Policeman.Cabal
    ( CabalError (..)
    , findCabalDescription
    ) where

import Control.Monad.Except (throwError)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))


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
    | CabalParseError FilePath
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
            whenNothing (parseGenericPackageDescriptionMaybe cabalContent) $
                throwError $ CabalParseError cabalPath  -- TODO: better error
        x:xs -> throwError $ MultipleCabalFiles (x :| xs)

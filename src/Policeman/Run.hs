module Policeman.Run
    ( runPoliceman
    ) where

import Control.Monad.Except (throwError)
import Control.Monad.Trans.Except (withExceptT)
import System.Directory (getCurrentDirectory)

import Policeman.Cabal (CabalError (..), extractPackageName, extractPackageVersion,
                        findCabalDescription)
import Policeman.Cli (CliArgs (..))
import Policeman.Core.Package (PackageName)
import Policeman.Core.Version (Version, versionFromText)
import Policeman.Download.Common (DownloadError)
import Policeman.Download.Hackage (downloadFromHackage)


data PolicemanError
    = DError DownloadError
    | CError CabalError
    deriving stock (Show)

-- | Runs the tool based on the CLI input.
runPoliceman :: CliArgs -> IO ()
runPoliceman CliArgs{..} = case cliArgsPrev >>= versionFromText of
    -- TODO: check for invalid version separately
    Nothing   -> putTextLn "Auto detection of previous Hackage version is not supported yet"
    Just prev -> whenLeftM_ (runExceptT $ diffWith prev) print

diffWith :: Version -> ExceptT PolicemanError IO ()
diffWith prevVersion = do
    curPackagePath <- liftIO getCurrentDirectory
    (curName, curVersion) <- getPackageInfo curPackagePath
    liftIO $ print (curName, curVersion)

    prevPackagePath <- withExceptT DError $ downloadFromHackage curName prevVersion
    (parsedPrevName, parsedPrevVersion) <- getPackageInfo prevPackagePath
    liftIO $ print (parsedPrevName, parsedPrevVersion)

getPackageInfo :: FilePath -> ExceptT PolicemanError IO (PackageName, Version)
getPackageInfo path = withExceptT CError $ do
    packageDesc <- findCabalDescription path
    let name = extractPackageName packageDesc
    version <- whenNothing (extractPackageVersion packageDesc) $
        throwError $ CabalParseError path

    pure (name, version)

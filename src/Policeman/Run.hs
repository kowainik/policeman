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
import Policeman.Hie (createHieFiles)


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

    prevPackagePath <- withExceptT DError $ downloadFromHackage curName prevVersion
    (parsedPrevName, parsedPrevVersion) <- getPackageInfo prevPackagePath

    prevHieFiles <- liftIO $ createHieFiles prevPackagePath
    curHieFiles  <- liftIO $ createHieFiles "."

    putStrLn $ "Currently:  " ++ show (curName, curVersion, length curHieFiles)
    putStrLn $ "Previously: " ++ show (parsedPrevName, parsedPrevVersion, length prevHieFiles)

getPackageInfo :: FilePath -> ExceptT PolicemanError IO (PackageName, Version)
getPackageInfo path = withExceptT CError $ do
    packageDesc <- findCabalDescription path
    let name = extractPackageName packageDesc
    version <- whenNothing (extractPackageVersion packageDesc) $
        throwError $ CabalParseError path

    pure (name, version)

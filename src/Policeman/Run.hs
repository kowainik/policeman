module Policeman.Run
    ( runPoliceman
    ) where

import Control.Monad.Trans.Except (withExceptT)
import System.Directory (getCurrentDirectory)

import Policeman.Cabal (CabalError (..), extractExposedModules, extractPackageName,
                        findCabalDescription)
import Policeman.Cli (CliArgs (..))
import Policeman.Core.Hie (hieFilesToHashMap)
import Policeman.Core.Package (Module, PackageName, PackageStructure (..))
import Policeman.Core.Version (Version, versionFromText, versionToText)
import Policeman.Diff (comparePackageStructures, prettyPrintDiff)
import Policeman.Download.Common (DownloadError)
import Policeman.Download.Hackage (downloadFromHackage)
import Policeman.Evaluate (eval)
import Policeman.Hie (createHieFiles)

import qualified Data.Set as Set


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
    (packageName, curModules) <- getPackageInfo curPackagePath

    prevPackagePath <- withExceptT DError $ downloadFromHackage packageName prevVersion
    (_, prevModules) <- getPackageInfo prevPackagePath

    prevHieFiles <- liftIO $ createHieFiles prevPackagePath
    curHieFiles  <- liftIO $ createHieFiles "."

    let prevPackageStructure = PackageStructure
            { psModules    = Set.fromList prevModules
            , psModulesMap = hieFilesToHashMap prevHieFiles
            }

    let curPackageStructure = PackageStructure
            { psModules    = Set.fromList curModules
            , psModulesMap = hieFilesToHashMap curHieFiles
            }

    let diff = comparePackageStructures prevPackageStructure curPackageStructure
    liftIO $ prettyPrintDiff diff
    let newVersion = eval prevVersion diff
    putTextLn $ "New version: " <> versionToText newVersion

getPackageInfo :: FilePath -> ExceptT PolicemanError IO (PackageName, [Module])
getPackageInfo path = withExceptT CError $ do
    packageDesc <- findCabalDescription path
    let name = extractPackageName packageDesc
    let modules = extractExposedModules packageDesc
    pure (name, modules)

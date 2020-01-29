module Policeman.Run
    ( runPoliceman
    ) where

import Control.Monad.Except (throwError)
import Control.Monad.Trans.Except (withExceptT)
import System.Directory (getCurrentDirectory)

import Policeman.Cabal (CabalError (..), extractExposedModules, extractPackageName,
                        extractPackageVersion, findCabalDescription, parseCabalFile)
import Policeman.Cli (CliArgs (..))
import Policeman.Core.Hie (hieFilesToHashMap)
import Policeman.Core.Package (Module, PackageName (..), PackageStructure (..))
import Policeman.Core.Version (Version, versionFromText)
import Policeman.Diff (comparePackageStructures, prettyPrintDiff)
import Policeman.Download.Common (DownloadError)
import Policeman.Download.Hackage (downloadFromHackage, getLatestHackageCabalFileContent)
import Policeman.Evaluate (eval)
import Policeman.Hie (createHieFiles)

import qualified Data.Set as Set


data PolicemanError
    = DError DownloadError
    | CError CabalError
    deriving stock (Show)

-- | Runs the tool based on the CLI input.
runPoliceman :: CliArgs -> IO ()
runPoliceman CliArgs{..} = whenLeftM_ (runExceptT findNextVersion) print
  where
    findNextVersion :: ExceptT PolicemanError IO ()
    findNextVersion = do
        curPackagePath <- liftIO getCurrentDirectory
        (packageName, curModules) <- getPackageInfo curPackagePath

        let runDiff :: Version -> ExceptT PolicemanError IO ()
            runDiff prevVer = diffWith prevVer packageName curModules

        -- TODO: check for invalid version separately
        case cliArgsPrev >>= versionFromText of
            Nothing   -> getLatestHackageVersion packageName >>= runDiff
            Just prev -> runDiff prev

getLatestHackageVersion :: PackageName -> ExceptT PolicemanError IO Version
getLatestHackageVersion packageName = do
    cabalContent <- withExceptT DError $ getLatestHackageCabalFileContent packageName
    packageDesc  <- withExceptT CError $ parseCabalFile cabalContent
    whenNothing (extractPackageVersion packageDesc) $
        throwError $ CError CabalParseError

diffWith
    :: Version
    -> PackageName
    -> [Module]
    -> ExceptT PolicemanError IO ()
diffWith prevVersion packageName curModules = do
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
    let evaluation = eval prevVersion diff
    liftIO $ prettyPrintDiff prevVersion evaluation diff

getPackageInfo :: FilePath -> ExceptT PolicemanError IO (PackageName, [Module])
getPackageInfo path = withExceptT CError $ do
    packageDesc <- findCabalDescription path
    let name = extractPackageName packageDesc
    let modules = extractExposedModules packageDesc
    pure (name, modules)

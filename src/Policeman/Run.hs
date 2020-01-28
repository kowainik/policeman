module Policeman.Run
    ( runPoliceman
    ) where

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
runPoliceman CliArgs{..} = do
    curPackagePath <- liftIO getCurrentDirectory
    runExceptT (getPackageInfo curPackagePath) >>= \case
        Left err -> print err
        Right (packageName, curModules) -> do
            let runDiff :: Version -> IO ()
                runDiff prevVer = whenLeftM_
                    (runExceptT $ diffWith prevVer packageName curModules)
                    print

            case cliArgsPrev >>= versionFromText of
                -- TODO: check for invalid version separately
                Nothing   -> getLatestHackageVersion packageName >>=
                    either print runDiff
                Just prev -> runDiff prev

getLatestHackageVersion :: PackageName -> IO (Either PolicemanError Version)
getLatestHackageVersion packageName = do
    eitherGenPackDesc <- runExceptT $
        withExceptT DError (getLatestHackageCabalFileContent packageName)
        >>=  withExceptT CError . parseCabalFile
    pure $ case eitherGenPackDesc of
        Left err -> Left err
        Right genPackDesc -> case extractPackageVersion genPackDesc of
            Just ver -> Right ver
            Nothing  -> Left $ CError CabalParseError

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

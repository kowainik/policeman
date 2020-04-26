module Policeman.Diff
    ( comparePackageStructures
    , prettyPrintDiff
    ) where

import Data.Set (member, (\\))

import Policeman.ColorTerminal (boldText, errorMessage, infoMessage, skipMessage, successMessage)
import Policeman.Core.Diff (Diff (..), PackageDiff (..), emptyDiff, hasDiffAdded, hasDiffDeleted)
import Policeman.Core.Package (Export, Module (..), ModuleStructure (..), PackageStructure (..))
import Policeman.Core.Version (Version, versionText, versionToText)
import Policeman.Evaluate (Evaluation (..))

import qualified Data.HashMap.Strict as HM


{- | Compares the structure of the previous version with the current one.
Returns the `PackageDiff` between them.
-}
comparePackageStructures :: PackageStructure -> PackageStructure -> PackageDiff
comparePackageStructures psPrev psCur = PackageDiff {..}
  where
    setAddedRemoved :: (Ord a) => Set a -> Set a -> Diff a
    setAddedRemoved x y = Diff
        { diffAdded   = y \\ x
        , diffDeleted = x \\ y
        }

    pdModule :: Diff Module
    pdModule = setAddedRemoved (psModules psPrev) (psModules psCur)

    pdExport :: HashMap Module (Diff Export)
    pdExport = foldl' go HM.empty oldExposedModules
      where
        go :: HashMap Module (Diff Export) -> (Module, ModuleStructure) -> HashMap Module (Diff Export)
        go hm (modul, msPrev) = case HM.lookup modul (psModulesMap psCur) of
            Just msCur -> HM.insert modul (moduleDiffExport msPrev msCur) hm
            Nothing    -> hm

        oldModules :: [(Module, ModuleStructure)]
        oldModules = HM.toList (psModulesMap psPrev)

        wasExposed :: Module -> Bool
        wasExposed x = x `member` psModules psCur

        oldExposedModules :: [(Module, ModuleStructure)]
        oldExposedModules = filter (wasExposed . fst) oldModules

    moduleDiffExport :: ModuleStructure -> ModuleStructure -> Diff Export
    moduleDiffExport msPrev msCur = setAddedRemoved (msExports msPrev) (msExports msCur)

{- | This function takes previous version of package, diff evaluation
result, the diff itself, and prints new suggested version based on
diff and evaluation.
-}
prettyPrintDiff :: Version -> Evaluation -> PackageDiff -> IO ()
prettyPrintDiff prevVersion Evaluation{..} PackageDiff{..} = do
    when (hasDiffDeleted pdModule) $ do
        errorMessage "Deleted modules:"
        printModule $ diffDeleted pdModule

    when (hasDiffAdded pdModule) $ do
        successMessage "New modules:"
        printModule $ diffAdded pdModule

    infoMessage "Per module diff:"
    forM_ (HM.toList pdExport) $ \(moduleName, diff@Diff{..}) ->
        when (diff /= emptyDiff) $ do
            skipMessage $ "  * " <> unModule moduleName

            when (hasDiffDeleted diff) $ do
                errorMessage "    Deleted exports:"
                printExport diffDeleted

            when (hasDiffAdded diff) $ do
                successMessage "    Added exports:"
                printExport diffAdded

    putTextLn ""
    putText     "🔄 Type of change:        " *> boldText (show evaluationChange <> "\n")
    putTextLn $ "👵 Previous version:      " <> versionText prevVersion
    putTextLn $ "💎 Suggested new version: " <> versionToText evaluationVersion
  where
    printModule :: Set Module -> IO ()
    printModule = mapM_ (putTextLn . ("    " <>) . unModule)

    printExport :: Set Export -> IO ()
    printExport = mapM_ (putTextLn . ("        " <> ) . show)

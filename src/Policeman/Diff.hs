module Policeman.Diff
    ( comparePackageStructures
    , prettyPrintDiff
    ) where

import Data.Set ((\\))

import Policeman.ColorTerminal (errorMessage, infoMessage, skipMessage, successMessage)
import Policeman.Core.Diff (Diff (..), PackageDiff (..), emptyDiff, hasDiffAdded, hasDiffDeleted)
import Policeman.Core.Package (Export, Module (..), ModuleStructure (..), PackageStructure (..))

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
    pdExport = foldl' go HM.empty oldModules
      where
        go :: HashMap Module (Diff Export) -> (Module, ModuleStructure) -> HashMap Module (Diff Export)
        go hm (modul, msPrev) = case HM.lookup modul (psModulesMap psCur) of
            Just msCur -> HM.insert modul (moduleDiffExport msPrev msCur) hm
            Nothing    -> hm

        oldModules :: [(Module, ModuleStructure)]
        oldModules = HM.toList $ psModulesMap psPrev

    moduleDiffExport :: ModuleStructure -> ModuleStructure -> Diff Export
    moduleDiffExport msPrev msCur = setAddedRemoved (msExports msPrev) (msExports msCur)

prettyPrintDiff :: PackageDiff -> IO ()
prettyPrintDiff PackageDiff{..} = do
    when (hasDiffDeleted pdModule) $ do
      errorMessage "Deleted modules:"
      mapM_ (putTextLn . ("    " <>) . unModule) $ diffDeleted pdModule

    when (hasDiffAdded pdModule) $ do
      successMessage "New modules:"
      mapM_ (putTextLn . ("    " <>) . unModule) $ diffAdded pdModule

    infoMessage "Per module diff:"
    forM_ (HM.toList pdExport) $ \(moduleName, diff@Diff{..}) -> do
        when (diff /= emptyDiff) $ do
            skipMessage $ "  * " <> unModule moduleName

            when (hasDiffDeleted diff) $ do
              errorMessage "    Deleted exports:"
              mapM_ (putTextLn . ("        " <> ) . show) diffDeleted

            when (hasDiffAdded diff) $ do
              successMessage "    Added exports:"
              mapM_ (putTextLn . ("        " <> ) . show) diffAdded

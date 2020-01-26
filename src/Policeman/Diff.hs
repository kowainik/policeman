module Policeman.Diff
    ( comparePackageStructures
    ) where

import Data.Set ((\\))

import Policeman.Core.Diff (Diff (..), PackageDiff (..))
import Policeman.Core.Package (Export, Module, ModuleStructure (..), PackageStructure (..))

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

module Policeman.Diff
    ( comparePackageStructures
    ) where

import Data.Set ((\\))

import Policeman.Core.Diff (PackageDiff (..))
import Policeman.Core.Package (PackageStructure (..))


{- | Compares the structure of the previous version with the current one.
Returns the `PackageDiff` between them.
-}
comparePackageStructures :: PackageStructure -> PackageStructure -> PackageDiff
comparePackageStructures psPrev psCur = PackageDiff {..}
  where
    setAddedRemoved :: (Ord a) => Set a -> Set a -> (Set a, Set a)
    setAddedRemoved x y = (y \\ x, x \\ y)

    (pdModuleAdded, pdModuleDeleted) =
        setAddedRemoved (psModules psPrev) (psModules psCur)
    (pdExportAdded, pdExportDeleted) =
        setAddedRemoved (psExports psPrev) (psExports psCur)

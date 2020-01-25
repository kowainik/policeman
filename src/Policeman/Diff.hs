module Policeman.Diff
    ( comparePackageStructures
    ) where

import Data.Set ((\\))

import Policeman.Core.Diff (Diff (..), PackageDiff (..))
import Policeman.Core.Package (Export, Module, PackageStructure (..))


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
    pdExport :: Diff Export
    pdExport = setAddedRemoved (psExports psPrev) (psExports psCur)

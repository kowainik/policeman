{- | This is the evaluation step.

We now need to calculate the next version based on the given 'PackageDiff' and
modify the previous version accordingly.
-}
module Policeman.Evaluate
    ( eval
    ) where

import Policeman.Core.Diff (PackageDiff (..), emptyDiff, hasDiffAdded, hasDiffDeleted)
import Policeman.Core.Version (Version, bumpMajorVersion, bumpMinorVersion)


{- | Generates the next version in agreement with PVP.
-}
eval :: Version -> PackageDiff -> Version
eval ver PackageDiff{..}
    | moduleDiffNonEmpty = bumpMajorVersion ver
    | removedExport = bumpMajorVersion ver
    | newExport = bumpMinorVersion ver
    | otherwise = ver
  where
    moduleDiffNonEmpty :: Bool
    moduleDiffNonEmpty = pdModule /= emptyDiff

    removedExport :: Bool
    removedExport = all hasDiffDeleted pdExport

    newExport :: Bool
    newExport = all hasDiffAdded pdExport

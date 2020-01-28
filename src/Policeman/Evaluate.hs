{- | This is the evaluation step.

We now need to calculate the next version based on the given 'PackageDiff' and
modify the previous version accordingly.
-}
module Policeman.Evaluate
    ( ChangeType (..)
    , Evaluation (..)
    , eval
    ) where

import Policeman.Core.Diff (PackageDiff (..), emptyDiff, hasDiffAdded, hasDiffDeleted)
import Policeman.Core.Version (Version, bumpMajorVersion, bumpMinorVersion)


{- | Defines whether a change is breaking or non-breaking or not needed at all.
-}
data ChangeType
    = NoChange
    | Minor
    | Major
    deriving (Show)

{- | Result of package evaluation: new version with the change type.
-}
data Evaluation = Evaluation
    { evaluationChange  :: !ChangeType
    , evaluationVersion :: !Version
    }

majorChange :: Version -> Evaluation
majorChange = Evaluation Major . bumpMajorVersion

minorChange :: Version -> Evaluation
minorChange = Evaluation Minor . bumpMinorVersion

noChange :: Version -> Evaluation
noChange = Evaluation NoChange

{- | Generates the next version in agreement with PVP.
-}
eval :: Version -> PackageDiff -> Evaluation
eval ver PackageDiff{..}
    | moduleDiffNonEmpty = majorChange ver
    | removedExport      = majorChange ver
    | newExport          = minorChange ver
    | otherwise          = noChange    ver
  where
    moduleDiffNonEmpty :: Bool
    moduleDiffNonEmpty = pdModule /= emptyDiff

    removedExport :: Bool
    removedExport = any hasDiffDeleted pdExport

    newExport :: Bool
    newExport = any hasDiffAdded pdExport

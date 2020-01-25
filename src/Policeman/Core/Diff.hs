{- |
-}

module Policeman.Core.Diff
    ( PackageDiff (..)
    , Diff (..)
    , emptyDiff
    , hasDiffAdded
    , hasDiffDeleted
    ) where

import Policeman.Core.Package (Export, Module)

import qualified Data.Set as Set


data PackageDiff = PackageDiff
    { pdModule :: !(Diff Module)
    , pdExport :: !(Diff Export)
    }

data Diff a = Diff
    { diffAdded   :: !(Set a)
    , diffDeleted :: !(Set a)
    } deriving stock (Eq)

emptyDiff :: Diff a
emptyDiff = Diff
    { diffAdded   = Set.empty
    , diffDeleted = Set.empty
    }

hasDiffDeleted :: Diff a -> Bool
hasDiffDeleted = not . Set.null . diffDeleted

hasDiffAdded :: Diff a -> Bool
hasDiffAdded = not . Set.null . diffAdded

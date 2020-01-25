{- |
-}

module Policeman.Core.Diff
    ( PackageDiff (..)
    , Diff (..)
    ) where

import Policeman.Core.Package (Export, Module)


data PackageDiff = PackageDiff
    { pdModule :: !(Diff Module)
    , pdExport :: !(Diff Export)
    }

data Diff a = Diff
    { diffAdded   :: !(Set a)
    , diffDeleted :: !(Set a)
    }

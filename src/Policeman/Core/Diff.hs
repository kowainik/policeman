{- |
-}

module Policeman.Core.Diff
    ( PackageDiff (..)
    ) where

import Policeman.Core.Package (Export, Module)


data PackageDiff = PackageDiff
    { pdModuleAdded   :: !(Set Module)
    , pdModuleDeleted :: !(Set Module)
    , pdExportAdded   :: !(Set Export)
    , pdExportDeleted :: !(Set Export)
    }

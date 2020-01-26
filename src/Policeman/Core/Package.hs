{- | Package metadata.
-}

module Policeman.Core.Package
    ( PackageName (..)

      -- * Package Structure
    , PackageStructure (..)
    , Export (..)
      -- ** Modules
    , Module (..)
    , ModuleStructure (..)
    ) where


{- | Package name, like @relude@.
-}
newtype PackageName = PackageName
    { unPackageName :: Text
    } deriving newtype (Show)

{- | Overall structure of the package.
Generally speaking, this is the resulting data type of custom parsing the
project by @policeman@.
-}
data PackageStructure = PackageStructure
    { psModules    :: !(Set Module)  -- ^ List of the exposed modules
    , psModulesMap :: !(HashMap Module ModuleStructure)  -- ^ The pairs of the module and its parsed structure.
    }

newtype Module = Module
  { unModule :: Text
  } deriving newtype (Eq, Ord, Hashable)

data Export
    = ExportedFunction Text
    | ExportedType Text
    deriving stock (Show, Eq, Ord)

newtype ModuleStructure = ModuleStructure
    { msExports :: Set Export  -- ^ The summary list of the exported stuff.
    }

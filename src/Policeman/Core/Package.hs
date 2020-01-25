{- | Package metadata.
-}

module Policeman.Core.Package
    ( PackageName (..)
    , Version (..)

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
    }

newtype Version = Version
    { unVersion :: Text
    }

{- | Overall structure of the package.
Generally speaking, this is the resulting data type of custom parsing the
project by @policeman@.
-}
data PackageStructure = PackageStructure
    { psModules    :: !(Set Module)  -- ^ List of the exposed modules
    , psExports    :: !(Set Export)  -- ^ The summary list of the exported stuff.
    , psModulesMap :: !(HashMap Module ModuleStructure)  -- ^ The pairs of the module and its parsed structure.
    }

newtype Module = Module
  { unModule :: Text
  } deriving newtype (Eq, Ord)

data Export
    = ExportedFunction Text
    | ExportedType Text
    | Exported Module
    deriving stock (Eq, Ord)

data ModuleStructure = ModuleStructure
    { msSmth :: !Text
    , msAsdf :: !Text
    }

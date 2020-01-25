{- | Package metadata.
-}

module Policeman.Core.Package
    ( PackageName (..)

      -- * Versioning
    , Version (..)
    , versionToText
      -- ** Bumping versions
    , bumpMarketingMajorVersion
    , bumpMajorVersion
    , bumpMinorVersion
    , bumpTinyMinorVersion

      -- * Package Structure
    , PackageStructure (..)
    , Export (..)
      -- ** Modules
    , Module (..)
    , ModuleStructure (..)
    ) where

import qualified Data.Text as Text


{- | Package name, like @relude@.
-}
newtype PackageName = PackageName
    { unPackageName :: Text
    }

{- | Package version that should satisfy the PVP.
<https://pvp.haskell.org/>
-}
data Version = Version
    { versionA :: !Int  -- ^ Marketing major.
    , versionB :: !Int  -- ^ Major.
    , versionC :: !Int  -- ^ Minor.
    , versionD :: !Int  -- ^ Tiny minor.
    }

nullVersion :: Version
nullVersion = Version
     { versionA = 0
     , versionB = 0
     , versionC = 0
     , versionD = 0
     }

{- | Shows 'Version' in the standard format:

@
A.B.C.D
1.0.2.1
@
-}
versionToText :: Version -> Text
versionToText Version{..} = Text.intercalate "."
    [ show versionA
    , show versionB
    , show versionC
    , show versionD
    ]

bumpMarketingMajorVersion :: Version -> Version
bumpMarketingMajorVersion Version{..} = nullVersion
    { versionA = versionA + 1 }

bumpMajorVersion :: Version -> Version
bumpMajorVersion Version{..} = nullVersion
    { versionA = versionA
    , versionB = versionB + 1
    }

bumpMinorVersion :: Version -> Version
bumpMinorVersion Version{..} = nullVersion
    { versionA = versionA
    , versionB = versionB
    , versionC = versionC + 1
    }

bumpTinyMinorVersion :: Version -> Version
bumpTinyMinorVersion ver@Version{..} = ver
    { versionD =  versionD + 1 }

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

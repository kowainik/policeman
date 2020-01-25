{- | Package metadata.
-}

module Policeman.Core.Package
    ( PackageName (..)
    , Version (..)
    ) where


{- | Package name, like @relude@.
-}
newtype PackageName = PackageName
    { unPackageName :: Text
    }

newtype Version = Version
    { unVersion :: Text
    }

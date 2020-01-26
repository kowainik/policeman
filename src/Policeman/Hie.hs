module Policeman.Hie
    ( createHieFiles
    ) where

import Shellmet ()
import System.Directory (getCurrentDirectory, setCurrentDirectory)


{- | Generates HIE files for the project in the given folder.
The generated files are going to be created in the @.hie/@ folder inside
the directory.
-}
createHieFiles :: FilePath -> IO ()
createHieFiles projectDir = do
    curDir <- getCurrentDirectory
    setCurrentDirectory projectDir
    getCurrentDirectory >>= putStrLn
    "cabal" ["clean"]
    "cabal" ["build", "--ghc-options=-fwrite-ide-info", "--ghc-options=-hiedir=.hie"]
    setCurrentDirectory curDir

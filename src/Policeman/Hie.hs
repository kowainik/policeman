module Policeman.Hie
    ( createHieFiles
    , readHieFiles
    ) where

import HieBin (HieFileResult (hie_file_result), readHieFile)
import HieTypes (HieFile)
import NameCache (NameCache, initNameCache)
import Shellmet ()
import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.Directory.Recursive (getDirRecursive)
import System.FilePath ((</>))
import UniqSupply (mkSplitUniqSupply)


{- | Generates HIE files for the project in the given folder and
returns their parsed content. The generated files are going to be
created in the @.hie/@ folder inside the given directory.
-}
createHieFiles :: FilePath -> IO [HieFile]
createHieFiles projectDir = do
    curDir <- getCurrentDirectory
    setCurrentDirectory projectDir
    "cabal" ["clean"]
    "cabal" ["build", "--ghc-options=-fwrite-ide-info", "--ghc-options=-hiedir=.hie"]
    readHieFiles <* setCurrentDirectory curDir

{- | Returns content of all @.hie@ files recursively in the current
directory from the @.hie/@ folder.
-}
readHieFiles :: IO [HieFile]
readHieFiles = do
    hieDir <- (</> ".hie") <$> getCurrentDirectory
    nameCache <- createNameCache
    hieContent <- getDirRecursive hieDir
    hieFiles <- filterM doesFileExist hieContent
    forM hieFiles $ \hiePath -> do
        (hieFileResult, _newCache) <- readHieFile nameCache hiePath
        pure $ hie_file_result hieFileResult

createNameCache :: IO NameCache
createNameCache = do
    uniqSupply <- mkSplitUniqSupply 'z'
    pure $ initNameCache uniqSupply []

{-# LANGUAGE TemplateHaskell #-}

{- | Command line interface for @policeman@ executable.
-}

module Policeman.Cli
    ( policemanParser
    ) where

import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitHash)
import Options.Applicative (Parser, ParserInfo, fullDesc, help, helper, info, infoOption, long,
                            metavar, progDesc, short, strArgument)

import Policeman.ColorTerminal (blueCode, boldCode, resetCode)

import qualified Paths_policeman as Meta (version)


-- | Main parser of the @policeman@ command line tool.
policemanParser :: ParserInfo (Maybe Text)
policemanParser = info ( helper <*> versionP <*> policemanP ) $
    fullDesc <> progDesc "Haskell PVP Helper"
  where
    policemanP :: Parser (Maybe Text)
    policemanP = optional $ strArgument (metavar "GIT_LINK")

-- | Show the version of the tool.
versionP :: Parser (a -> a)
versionP = infoOption policemanVersion
    $ long "version"
   <> short 'v'
   <> help "Show policeman's version"

policemanVersion :: String
policemanVersion = toString $ intercalate "\n" $
    [ sVersion
    , sHash
    , sDate
    ]
  where
    sVersion, sHash, sDate :: String
    sVersion = blueCode <> boldCode
        <> "👮 Policeman " <> "v" <>  showVersion Meta.version <> resetCode
    sHash = " ➤ " <> blueCode <> boldCode
        <> "Git revision: " <> resetCode <> $(gitHash)
    sDate = " ➤ " <> blueCode <> boldCode
        <> "Commit date:  " <> resetCode <> $(gitCommitDate)

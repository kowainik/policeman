{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Command line interface for @policeman@ executable.
-}

module Policeman.Cli
    ( CliArgs (..)
    , policemanParser
    ) where

import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitHash)
import Options.Applicative (Parser, ParserInfo, fullDesc, help, helper, info, infoOption, long,
                            long, metavar, progDesc, short, strOption)

import Policeman.ColorTerminal (blueCode, boldCode, resetCode)

import qualified Paths_policeman as Meta (version)


newtype CliArgs = CliArgs
    { cliArgsPrev :: Maybe Text  -- ^ Optional previous version to compare with
    }

-- | Main parser of the @policeman@ command line tool.
policemanParser :: ParserInfo CliArgs
policemanParser = info (helper <*> versionP <*> policemanP) $
    fullDesc <> progDesc "Haskell PVP Version Adviser"

policemanP :: Parser CliArgs
policemanP = do
    cliArgsPrev <- optional hackageVersionP
    pure CliArgs{..}

hackageVersionP :: Parser Text
hackageVersionP = strOption
    $ long "previous"
   <> short 'p'
   <> metavar "VERSION"
   <> help "Previous version of the package to compare to"

-- | Show the version of the tool.
versionP :: Parser (a -> a)
versionP = infoOption policemanVersion
    $ long "version"
   <> short 'v'
   <> help "Show Policeman's badge number"

policemanVersion :: String
policemanVersion = toString $ intercalate "\n"
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

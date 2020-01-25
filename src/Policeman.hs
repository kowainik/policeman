module Policeman
       ( policeman
       ) where

import Options.Applicative (execParser)

import Policeman.Cli (policemanParser)
import Policeman.Run (runPoliceman)


-- | Main tool exec function.
policeman :: IO ()
policeman = execParser policemanParser >>= runPoliceman

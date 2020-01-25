module Policeman.Run
    ( runPoliceman
    ) where


-- | Runs the tool based on the CLI input.
runPoliceman :: Maybe Text -> IO ()
runPoliceman = \case
    Just link -> putTextLn link
    Nothing -> putTextLn "Not implemented yet"

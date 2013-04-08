-- We have two command line frontends (one with GNU-style arguments, and one
-- that follows the requirements for COP5555). There are a lot of common
-- components. They go here.

module OptionHandler ( versionNumber
                     , versionString
                     , Opt(..)
                     , optDefaults
                     , optProcess
                     ) where

import Paths_hs_rpal (version)
import Data.Version (showVersion)
import Control.Monad

import Lexer
import Parser
import Standardizer

-- This auto-updates from the value in cabal! Isn't that cool?
versionNumber :: String
versionNumber = showVersion version
versionString :: String
versionString = "hs-rpal " ++ versionNumber

-- Define a record for all the arguments
data Opt = Opt { optVersion :: Bool
               , optAst :: Bool
               , optPartialSt :: Bool
               , optFullSt :: Bool
               , optLex :: Bool
               , optListing :: Bool
               , optQuiet :: Bool
               , optFile :: Maybe String
               }

-- Define the default settings
optDefaults :: Opt
optDefaults = Opt { optVersion = False
                  , optAst = False
                  , optPartialSt = False
                  , optFullSt = False
                  , optLex = False
                  , optListing = False
                  , optQuiet = False
                  , optFile = Nothing
                  }

-- For each argument, evaluate it and execute subtasks
optProcess :: Opt -> IO ()
optProcess opt = do
    source <- case optFile opt of
        Just path -> readFile path  -- From file
        Nothing   -> getContents    -- From stdin
    if optVersion opt then putStrLn versionString else
        (when (optListing opt) $ putStr source) >>
        (when (optLex opt) $ putStr $ unlines $ fmap show $ getTokens source) >>
        (when (optAst opt) $ putStr $ show $ parse source) >>
        (when (optPartialSt opt) $
            putStr $ show $ standardizePartially $ parse source) >>
        (when (optFullSt opt) $
            putStr $ show $ standardizeFully $ parse source)

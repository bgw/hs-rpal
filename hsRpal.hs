module Main (main) where

import System.Environment
import System.Console.GetOpt
import Control.Monad

import Lexer
import Parser

-- Define a record for all the arguments
data Opt = Opt { optVersion :: Bool
               , optAst :: Bool
               , optLex :: Bool
               , optListing :: Bool
               }

-- Define the default settings
optDefaults :: Opt
optDefaults = Opt { optVersion = False
                  , optAst = False
                  , optLex = False
                  , optListing = False
                  }

-- Each description is a function that modifies an Opt record. Selected flags
-- get applied in order.
optDescr :: [OptDescr (Opt -> Opt)]
optDescr =
    [ Option ['v'] ["version"]
        (NoArg $ \o -> o { optVersion = True })
      "Shows the version number"
    , Option ['l'] ["listing"]
        (NoArg $ \o -> o { optListing = True })
      "Print the raw program to stdout"
    , Option ['x'] ["lex"]
        (NoArg $ \o -> o { optLex = True })
      "Print all the tokens to stdout"
    , Option ['a'] ["ast"]
        (NoArg $ \o -> o { optAst = True })
      "Print the Abstract Syntax Tree to stdout"
    ]

-- Define our argument parser using getOpt
optParse :: [String] -> IO (Opt, [String])
optParse argv =
    case getOpt Permute optDescr argv of
        (options, nonOptions, []  ) -> -- No errors
            -- Start with the default options, and progressively apply modifiers
            return (foldr ($) optDefaults options, nonOptions)
        (_      , _         , errs) -> -- Errors
            -- Print the error and the program usage
            ioError (userError (concat errs ++ usageInfo header optDescr))
    where header = "Usage: hsRpal [OPTION...] files..."

main :: IO ()
main = do
    (options, nonOptions) <- getArgs >>= optParse 
    optProcess options nonOptions

-- For each argument, evaluate it and execute subtasks
optProcess :: Opt -> [String] -> IO ()
optProcess opt nopt = do
    source <-
        if length nopt > 0 then readFile (head nopt) -- From file
        else getContents                             -- From stdin
    if optVersion opt then putStrLn "hsRpal v0.0.1-dev" else do
        when (optListing opt) (putStr source)
        when (optLex opt)     (putStr $ unlines $ fmap show $ getTokens source)
        when (optAst opt)     (putStr $ generateAst source)

generateAst :: String -> String
generateAst source = show $ parse source

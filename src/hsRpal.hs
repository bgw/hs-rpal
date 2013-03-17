-- This is the primary command line argument parser frontend. It takes GNU-style
-- arguments and has some error handling.

module Main (main) where

import System.Environment
import System.Console.GetOpt
import Control.Monad

import OptionHandler

-- Printed whenever there is an error or when `-h` is called
optUsageInfo :: String
optUsageInfo = usageInfo "Usage: hs-rpal [OPTION...] files..." optDescr

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

-- Take sequential arguments and pack them into an Opt record. Called implicitly
-- by optParse.
optNooptParse :: Opt -> [String] -> Opt
optNooptParse baseOpt argv =
    case getOpt Permute optDescr argv of
        (_, [file], []  ) -> baseOpt { optFile = Just file }
        (_, [],     []  ) -> baseOpt
        (_, _,      errs) -> error $ concat errs ++ optUsageInfo

-- Define our argument parser using getOpt
optParse :: Opt -> [String] -> Opt
optParse baseOpt argv =
    case getOpt Permute optDescr argv of
        -- Start with the default options, and progressively apply modifiers
        (options, _, []  ) -> foldr ($) (optNooptParse baseOpt argv) options
        (_,       _, errs) -> error $ concat errs ++ optUsageInfo

main :: IO ()
main = (liftM $ optParse optDefaults) getArgs >>= optProcess


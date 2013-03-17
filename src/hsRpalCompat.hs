-- This is the alternate command line argument parser frontend. It takes
-- arguments compatible with the reference parser for COP5555. It is compatible
-- with class requirements. If you're actually looking to use the program, I'd
-- recommend the GNU-style frontend. It's better.

module Main (main) where

import System.Environment
import Control.Monad

import OptionHandler

optUsageInfo :: String
optUsageInfo =
    unlines [ "Usage: hs-rpal [OPTION...] FILE"
            , "    -version:  Shows the version number"
            , "    -l:        Print the raw program to stdout"
            , "    -ast:      Print the Abstract Syntax Tree to stdout"
            , "    -lex:      Print all the tokens to stdout"
            , "    -noout:    Skip evaluation (useful with -ast, -lex, etc)"
            ]

-- Processes arguments prefixed with a dash
optParseArg :: Opt -> String -> Opt
optParseArg o "version" = o { optVersion = True }
optParseArg o "l"       = o { optListing = True }
optParseArg o "ast"     = o { optAst     = True }
optParseArg o "lex"     = o { optLex     = True }
optParseArg o "noout"   = o { optQuiet   = True }
optParseArg _ arg       = error $ "Unrecognized option: '-" ++ arg ++ "'\n"
                                  ++ optUsageInfo

optParse :: Opt -> [String] -> Opt
optParse o (argHead : argTail) =
    case argHead :: String of
        '-' : a -> optParse (optParseArg o a) argTail
        a       -> optParse (o {optFile = Just a }) argTail
optParse o [] = o

main :: IO ()
main = (liftM $ optParse optDefaults) getArgs >>= optProcess

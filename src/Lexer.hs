-- This lexer is not completely to spec. We accept certain characters that the
-- spec would not allow, simplifying our lexer as suggested by Bermudez during
-- lecture. For example, our strings could contain any arbitrary Unicode
-- code-point, while the spec's can only handle a certain subset of ASCII.
--
-- We use POSIX regular expressions because regex is awesome. Sure it slows down
-- the lexer, but this isn't part of the critical path, so who cares?
module Lexer ( Token(..)
             , getToken
             , getTokens
             ) where

import Text.Regex.TDFA -- Text.Regex.Posix sucks
import Data.Maybe

data Token
    = TokenKeyword String
    | TokenIdentifier String
    | TokenInteger Integer
    | TokenOperator String
    | TokenString String
    | TokenPunction String
    | TokenDelete String  -- Temporary placeholder, gets removed in the end
    | TokenComment String -- Discarded now, treated separately in case we might
                          -- ever want to use this in the AST later
    deriving (Show, Eq)

-- A given matcher should consume the first token it sees in the string, and
-- then stop. Getting all the tokens can be done recursively (and lazily).
tokenMatchers :: [(Regex, String -> Token)]
tokenMatchers = [ (mr "[A-Za-z][A-Za-z0-9_]*", TokenIdentifier)
                , (mr "[0-9]+", \s -> TokenInteger $ read s)
                -- Stolen from http://stackoverflow.com/a/1016356/130598
                , (mr "'(\\\\.|[^\'])*'", TokenString)
                , (mr "//.*", TokenComment) -- Must be processed before operator
                -- You have to be really careful with character ordering in the
                -- regex, so that `]`, `"`, and `-` get treated properly
                , (mr "[][+*<>&.@/:=~|$!#%^_{}\"`?-]+", TokenOperator)
                , (mr "[();,]", TokenPunction)
                , (mr "[ \t\n\r]+", TokenDelete)
                ]
                where mr m = makeRegex $ "\\`" ++ m

-- Get applied in sequence, and can perform simple one-to-one transformations on
-- tokens
tokenTransformers :: [Token -> Token]
tokenTransformers = [ transformKeyword ]

transformToken :: Token -> Token
transformToken token = foldr ($) token tokenTransformers

-- Certain `TokenIdentifier`s should be be treated as keywords
transformKeyword :: Token -> Token
transformKeyword (TokenIdentifier name) =
    if isKeyword then TokenKeyword name else TokenIdentifier name
    where
        isKeyword = case name of
            "let"    -> True
            "in"     -> True
            "fn"     -> True
            "aug"    -> True
            "or"     -> True
            "gr"     -> True
            "ge"     -> True
            "ls"     -> True
            "le"     -> True
            "eq"     -> True
            "ne"     -> True
            "not"    -> True
            "where"  -> True
            "within" -> True
            "and"    -> True
            "rec"    -> True
            "true"   -> True
            "false"  -> True
            "nil"    -> True
            "dummy"  -> True
            _        -> False
transformKeyword token = token

-- These terminals should be discarded by getToken
isIgnored :: Token -> Bool
isIgnored (TokenDelete _) = True
isIgnored (TokenComment _) = True
isIgnored _ = False

getToken :: String -> Maybe (Token, String)
getToken source =
    if null source then Nothing
    else let (token, sourceTail) = getTokenPartial tokenMatchers source in
        if isIgnored token then getToken sourceTail
        else Just (token, sourceTail)

getTokens :: String -> [Token]
getTokens source =
    let r = getToken source in
        if isJust r then
            let (token, sourceTail) = fromJust r in
                [token] ++ getTokens sourceTail
        else []

getTokenPartial :: [(Regex, String -> Token)] -> String -> (Token, String)
getTokenPartial matchers source =
    let (m, t) = head matchers in
        if match m source then 
            (transformToken $ t (match m source :: String),
            drop (length (match m source :: String)) source)
        else getTokenPartial (tail matchers) (source)

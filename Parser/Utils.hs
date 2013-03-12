-- Various utility functions that make writing the parser functions easier and
-- cleaner by separating out common patterns
module Parser.Utils ( Parser(..)
                    , ParserResult
                    , ParserResultList
                    , ParserConsumed
                    , consume
                    , advance
                    , many
                    , manySep
                    , notFound
                    ) where

import Lexer
import Parser.Ast

-- Define the Parser Monad
newtype Parser a = Parser a
    -- Use the values for `a` as defined below
    deriving Show

type ParserResult = Parser (Ast, [Token])
type ParserResultList = Parser ([Ast], [Token])
type ParserConsumed = Parser [Token]

instance Monad Parser where
    return = Parser
    (Parser tokens) >>= subtreeParser = subtreeParser tokens


-- Discard a specific token
consume :: [Token] -> Token -> ParserConsumed
consume (tHead:tTail) matched
    | tHead == matched = return tTail
    | otherwise        = error $ "Can't match expected '" ++ (show matched)
                                 ++ "' with '" ++ (show tHead) ++ "'"
consume _ matched = error $ "Expecting '" ++ (show matched) ++ "' but got EOF"


-- Discard the next token, not checking what it actually is
-- This is like consume, but less safe. Use it instead of consume only after you
-- already checked it yourself (in a conditional block or something)
advance :: [Token] -> ParserConsumed
advance (_:tTail) = return tTail
-- An error should not occur here if you already checked for the token yourself
advance _ = error $ "Expecting discardable symbol but got EOF"


-- Repeatedly call a parser **until** a end conditional is matched. There may be
-- zero or more results.
many :: [Token] -> ([Token] -> ParserResult) -> ([Token] -> Bool)
        -> ParserResultList
many ta par cond =
    if cond ta then return ([], ta)
    else do
        (ab, tb) <- par ta
        (ac, tc) <- many tb par cond
        return (ab : ac, tc)


-- Like many, but instead of taking a conditional function, it takes an expected
-- separator. When the separator is no longer matched, it returns a
-- ParserResultList. Because of parsing limitations, we assume one or more
-- results. An empty list will fail.
manySep :: [Token] -> ([Token] -> ParserResult) -> Token -> ParserResultList
manySep tokens parsingFunc separator = do
    (aa, ta) <- parsingFunc tokens -- we assume at least one match
    (ab, tb) <- many ta consumingParsingFunc sepCond
    return (aa : ab, tb)
    where
        -- Consume a separator and then parse an element
        consumingParsingFunc partialTokens =
            consume partialTokens separator >>= parsingFunc
        -- Stop when the separator is no longer matched or on EOF
        sepCond (t:_) = t /= separator -- the separator is no longer matched
        sepCond _     = True           -- we hit an EOF


-- Generate error handler when a matcher fails
notFound :: String -> [Token] -> b
notFound expected (token:_) =
    error $ "Expected " ++ expected ++ " but instead found (" ++ (show token)
            ++ ")"
notFound expected _ =
    error $ "Expected " ++ expected ++ " but instead found EOF"

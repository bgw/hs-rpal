module Parser ( module Parser.Ast, parse ) where

import Parser.Ast
import Lexer

-- Define the top-level `parse` function
class Parsable p where
    parse :: [p] -> Ast

instance Parsable Char where
    parse s = parse $ getTokens s

instance Parsable Token where
    parse t = case parseE t of
        Parser (astRoot, []) -> astRoot
        Parser (_, tokens)   -> error $ "Expected EOF but found tokens: "
                                        ++ (show tokens)

-- Define the Parser Monad
newtype Parser a = Parser a
    -- Use the values for `a` as defined in ParserResult and ParserConsumed
    deriving Show

type ParserResult = Parser (Ast, [Token])
type ParserConsumed = Parser [Token]

instance Monad Parser where
    return = Parser
    (Parser tokens) >>= subtreeParser = subtreeParser tokens

-- Discard a specific token (utility)
consume :: [Token] -> Token -> ParserConsumed
consume (tHead:tTail) matched
    | tHead == matched = return tTail
    | otherwise        = error $ "Can't match expected '" ++ (show matched)
                                 ++ "' with '" ++ (show tHead) ++ "'"
consume _ matched = error $ "Expecting '" ++ (show matched) ++ "' but got EOF"

notFound :: String -> [Token] -> b
notFound expected (token:_) =
    error $ "Expected " ++ expected ++ " but instead found (" ++ (show token)
            ++ ")"
notFound expected _ =
    error $ "Expected " ++ expected ++ " but instead found EOF"

-- Parsers for each grammar component
parseE :: [Token] -> ParserResult
parseE (TokenIdentifier "let" : tokens) = do
    (aa, ta) <- parseD tokens
    tb <- consume ta $ TokenIdentifier "in"
    (ac, tc) <- parseE tb
    return (AstLet aa ac, tc)
parseE t = notFound "expression" t

parseD :: [Token] -> ParserResult
parseD = parseE

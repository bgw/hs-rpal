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
    -- Use the values for `a` as defined below
    deriving Show

type ParserResult = Parser (Ast, [Token])
type ParserResultList = Parser ([Ast], [Token])
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

-- Repeatedly call a parser until a end conditional is matched
many :: [Token] -> ([Token] -> ParserResult) -> ([Token] -> Bool)
        -> ParserResultList
many ta par cond =
    if cond ta then return ([], ta)
    else do
        (ab, tb) <- par ta
        (ac, tc) <- many tb par cond
        return (ab : ac, tc)

-- Generate error handler when a matcher fails
notFound :: String -> [Token] -> b
notFound expected (token:_) =
    error $ "Expected " ++ expected ++ " but instead found (" ++ (show token)
            ++ ")"
notFound expected _ =
    error $ "Expected " ++ expected ++ " but instead found EOF"

-- Parsers for each grammar component
parseE :: [Token] -> ParserResult

-- E -> 'let' D 'in' E
parseE (TokenIdentifier "let" : tokens) = do
    (aa, ta) <- parseD tokens
    tb <- consume ta $ TokenIdentifier "in"
    (ac, tc) <- parseE tb
    return (AstLet aa ac, tc)

-- E -> 'fn' Vb+ '.' E
parseE (TokenIdentifier "fn" : tokens) = do
    (aa, ta) <- parseVb tokens -- There should be at least one Vb
    (ab, tb) <- many ta parseVb (\t -> (head t) == (TokenOperator "."))
    tc <- consume tb $ TokenOperator "."
    (ad, td) <- parseE tc
    return (AstLambda (aa : ab) ad, td)

parseE t = parseEw t

-- Stuff below is not yet implemented
parsePlaceholder :: [Token] -> ParserResult
parsePlaceholder (TokenIdentifier "placeholder" : tokens) =
    return (AstDummy, tokens)
parsePlaceholder t = notFound "placeholder" t

parseEw :: [Token] -> ParserResult
parseEw = parsePlaceholder

parseD :: [Token] -> ParserResult
parseD = parsePlaceholder

parseVb :: [Token] -> ParserResult
parseVb = parsePlaceholder

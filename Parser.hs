module Parser ( module Parser.Ast, module Parser.Utils, parse ) where

import Parser.Ast
import Parser.Utils
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

-- Parsers for each grammar component
parseE :: [Token] -> ParserResult

-- E -> 'let' D 'in' E
parseE (TokenIdentifier "let" : tokens) = do
    (aa, ta) <- parseD tokens
    tb       <- consume ta $ TokenIdentifier "in"
    (ac, tc) <- parseE tb
    return (AstLet aa ac, tc)

-- E -> 'fn' Vb+ '.' E
parseE (TokenIdentifier "fn" : tokens) = do
    (aa, ta) <- parseVb tokens -- There should be at least one Vb
    (ab, tb) <- many ta parseVb (\t -> head t == TokenOperator ".")
    tc       <- advance tb
    (ad, td) <- parseE tc
    return (AstLambda (aa : ab) ad, td)

parseE t = parseEw t

-- Ew -> T 'where' Dr
--    -> T
parseEw :: [Token] -> ParserResult
parseEw tokens =
    parseT tokens >>= \(aa, ta) ->
        if length ta > 0 && head ta == TokenIdentifier "where" then do
            tb       <- advance ta
            (ac, tc) <- parseDr tb
            return (AstWhere aa ac, tc)
        else return (aa, ta)

-- T -> Ta ( ',' Ta )+
--   -> Ta
parseT :: [Token] -> ParserResult
parseT tokens = do
    (aa, ta) <- manySep tokens parseTa (TokenPunction ",")
    return (AstTau aa, ta)

-- Ta -> Ta 'aug' Tc
--    -> Tc
parseTa :: [Token] -> ParserResult
parseTa tokens = do
    -- Build as a list, then turn into a tree
    (aa, ta) <- manySep tokens parseTc (TokenIdentifier "aug")
    return (buildTree aa, ta)
    where
        buildTree tcList =
            if length tcList == 1 then head tcList
            else AstAug (buildTree $ init tcList) (last tcList)

-- Stuff below is not yet implemented
parsePlaceholder :: [Token] -> ParserResult
parsePlaceholder (TokenIdentifier "placeholder" : tokens) =
    return (AstDummy, tokens)
parsePlaceholder t = notFound "placeholder" t

parseTc :: [Token] -> ParserResult
parseTc = parsePlaceholder

parseD :: [Token] -> ParserResult
parseD = parsePlaceholder

parseDr :: [Token] -> ParserResult
parseDr = parsePlaceholder

parseVb :: [Token] -> ParserResult
parseVb = parsePlaceholder

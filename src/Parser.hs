module Parser ( module Parser.Ast, module Parser.Utils, parse ) where

import Data.Maybe

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
parseE (TokenKeyword "let" : tokens) = do
    (aa, ta) <- parseD tokens
    tb       <- consume ta $ TokenKeyword "in"
    (ac, tc) <- parseE tb
    return (AstLet aa ac, tc)

-- E -> 'fn' Vb+ '.' E
parseE (TokenKeyword "fn" : tokens) = do
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
    parseT tokens >>= \(aa, ta) -> case ta of
        (TokenKeyword "where" : tb) -> do
            (ac, tc) <- parseDr tb
            return (AstWhere aa ac, tc)
        _ ->
            return (aa, ta)

-- T -> Ta ( ',' Ta )+
--   -> Ta
parseT :: [Token] -> ParserResult
parseT tokens = do
    (aa, ta) <- manySep tokens parseTa (TokenPunction ",")
    return ( if length aa > 1 then (AstTau aa, ta) else (head aa, ta) )

-- Ta -> Ta 'aug' Tc
--    -> Tc
parseTa :: [Token] -> ParserResult
parseTa tokens =
    leftRecursive tokens parseTc (TokenKeyword "aug") AstAug

-- Tc -> B '->' Tc '|' Tc
--    -> B
parseTc :: [Token] -> ParserResult
parseTc tokens = do
    parseB tokens >>= \(aa, ta) -> case ta of
        (TokenOperator "->" : tb) -> do
            (ac, tc) <- parseTc tb
            td       <- consume tc $ TokenOperator "|"
            (ae, te) <- parseTc td
            return (AstCond aa ac ae, te)
        _ ->
            return (aa, ta)

-- B -> B 'or' Bt
--   -> Bt
parseB :: [Token] -> ParserResult
parseB tokens =
    leftRecursive tokens parseBt (TokenKeyword "or") AstOr

-- Bt -> Bt '&' Bs
--    -> Bs
parseBt :: [Token] -> ParserResult
parseBt tokens =
    leftRecursive tokens parseBs (TokenOperator "&") AstAmp

-- Bs -> 'not' Bp
parseBs :: [Token] -> ParserResult
parseBs (TokenKeyword "not" : tokens) = do
    (aa, ta) <- parseBp tokens
    return (AstNot aa, ta)

-- Bs -> Bp
parseBs tokens = parseBp tokens

-- Bp -> A ( 'gr' | '>'  ) A
--    -> A ( 'ge' | '>=' ) A
--    -> A ( 'ls' | '<'  ) A
--    -> A ( 'le' | '<=' ) A
--    -> A 'eq' A
--    -> A 'ne' A
--    -> A
parseBp :: [Token] -> ParserResult
parseBp tokens =
    parseA tokens >>= \(aa, ta) ->
        let
            combTypeMaybe = if length ta == 0 then Nothing else case head ta of
                -- My eyes are starting to hurt...
                TokenKeyword "gr" -> Just AstGr
                TokenOperator   ">"  -> Just AstGr
                TokenKeyword "ge" -> Just AstGe
                TokenOperator   ">=" -> Just AstGe
                TokenKeyword "ls" -> Just AstLs
                TokenOperator   "<"  -> Just AstLs
                TokenKeyword "le" -> Just AstLe
                TokenOperator   "<=" -> Just AstLe
                TokenKeyword "eq" -> Just AstEq
                TokenKeyword "ne" -> Just AstNe
                _                    -> Nothing
        in case combTypeMaybe of
            Just combType -> do
                tb       <- advance ta
                (ac, tc) <- parseA tb
                return (combType aa ac, tc)
            Nothing ->
                return (aa, ta)

-- A -> A '+' At
--   -> A '-' At
--   ->   '+' At
--   ->   '-' At
--   -> At
-- There is actually (In my opinion) a bug in the grammar spec here, which we
-- replicate for compatibility. `-5 * 5` is valid grammar, but `5 * -5` is not.
parseA :: [Token] -> ParserResult
parseA tokens = do
    (aa, ta) <- parseAtNeg tokens
    parseAPartial aa ta
    where
        parseAPartial leftAst partialTokens =
            case partialTokens of
                (TokenOperator "+" : ta) -> do
                    (ab, tb) <- parseAtNeg ta
                    parseAPartial (AstPlus leftAst ab) tb
                (TokenOperator "-" : ta) -> do
                    (ab, tb) <- parseAtNeg ta
                    parseAPartial (AstMinus leftAst ab) tb
                _ ->
                    return (leftAst, partialTokens)
        -- handle possible '+' or '-' uniary operators
        parseAtNeg (TokenOperator "-" : partialTokens) = do
            (aa, ta) <- parseAt partialTokens
            return (AstNeg aa, ta)
        parseAtNeg (TokenOperator "+" : partialTokens) =
            parseAt partialTokens
        parseAtNeg partialTokens =
            parseAt partialTokens

-- At -> At '*' Af
--    -> At '/' Af
--    -> Af
parseAt :: [Token] -> ParserResult
parseAt tokens = do
    (aa, ta) <- parseAf tokens
    parseAtPartial aa ta
    where
        parseAtPartial leftAst partialTokens =
            case partialTokens of
                (TokenOperator "*" : ta) -> do
                    (ab, tb) <- parseAf ta
                    parseAtPartial (AstMult leftAst ab) tb
                (TokenOperator "/" : ta) -> do
                    (ab, tb) <- parseAf ta
                    parseAtPartial (AstDiv leftAst ab) tb
                _ ->
                    return (leftAst, partialTokens)

-- Af -> Ap '**' Af
--    -> Ap
parseAf :: [Token] -> ParserResult
parseAf tokens =
    parseAp tokens >>= \(aa, ta) -> case ta of
        (TokenOperator "**" : tb) -> do
            (ac, tc) <- parseAf tb
            return (AstPow aa ac, tc)
        _ ->
            return (aa, ta)

-- Ap -> Ap '@' '<IDENTIFIER>' R
--    -> R
parseAp :: [Token] -> ParserResult
parseAp tokens = do
    (aa, ta) <- parseR tokens
    parseApPartial aa ta
    where
        parseApPartial leftAst partialTokens =
            case partialTokens of
                (TokenOperator "@" : TokenIdentifier functionName: ta) -> do
                    (ab, tb) <- parseR ta
                    parseApPartial
                        (AstInfix leftAst (AstIdentifier functionName) ab) tb
                _ ->
                    return (leftAst, partialTokens)

-- R -> R Rn
--   -> Rn
parseR :: [Token] -> ParserResult
parseR tokens = do
    (aa, ta) <- parseRn tokens -- There should be at least one Rn
    -- parseRnMaybe gets implicitly called twice for each Rn. I hope ghc
    -- optimizes this...
    (ab, tb) <- many ta parseRn (\t -> isNothing $ parseRnMaybe t)
    return (buildTree (aa : ab), tb)
    where
        buildTree subList =
            if length subList == 1 then head subList
            else AstGamma (buildTree $ init subList) (last subList)

-- Rn -> '<IDENTIFIER>'
--    -> '<INTEGER>'
--    -> '<STRING>'
--    -> 'true'
--    -> 'false'
--    -> 'nil'
--    -> '(' E ')'
--    -> 'dummy'
parseRnMaybe :: [Token] -> Maybe ParserResult
parseRnMaybe (TokenKeyword "true" : tokens) =
    Just $ return (AstTrue, tokens)
parseRnMaybe (TokenKeyword "false" : tokens) =
    Just $ return (AstFalse, tokens)
parseRnMaybe (TokenKeyword "nil" : tokens) =
    Just $ return (AstNil, tokens)
parseRnMaybe (TokenKeyword "dummy" : tokens) =
    Just $ return (AstDummy, tokens)
parseRnMaybe (TokenIdentifier idName : tokens) =
    Just $ return (AstIdentifier idName, tokens)
parseRnMaybe (TokenInteger intValue : tokens) =
    Just $ return (AstInteger intValue, tokens)
parseRnMaybe (TokenString strValue : tokens) =
    Just $ return (AstString strValue, tokens)
parseRnMaybe (TokenPunction "(" : tokens) = Just $ do
    (aa, ta) <- parseE tokens
    tb       <- consume ta $ TokenPunction ")"
    return (aa, tb)
parseRnMaybe _ = Nothing

parseRn :: [Token] -> ParserResult
parseRn tokens = case parseRnMaybe tokens of
    Just r  -> r
    Nothing ->
        notFound ("true, false, nil, dummy, integer, identifier, string or "
                  ++ "parenthesized expression") tokens

-- D -> Da 'within' D
--   -> Da
parseD :: [Token] -> ParserResult
parseD tokens =
    rightRecursive tokens parseDa (TokenKeyword "within") AstWithin

-- Da -> Dr ( 'and' Dr )+
--    -> Dr
parseDa :: [Token] -> ParserResult
parseDa tokens = do
    (aa, ta) <- manySep tokens parseDr (TokenKeyword "and")
    return (if length aa > 1 then AstAnd aa else head aa, ta)

-- Dr -> 'rec' Db
--    -> Db
parseDr :: [Token] -> ParserResult
parseDr (TokenKeyword "rec" : tokens) = do
    (aa, ta) <- parseDb tokens
    return (AstRec aa, ta)
parseDr tokens = parseDb tokens

-- Db -> Vl '=' E
--    -> '<IDENTIFIER>' Vb+ '=' E
--    -> '(' D ')'
--
-- This grammar is potentially ambiguous. The reference parser actually parses
-- this *wrong*. The input `let a, b, c d e f = dummy in dummy` should fail, as
-- you cannot have `Db -> Vl Vb+ = E`. The reference parser accepts it as such.
--
-- We can solve this by performing a small non-LL(1) lookahead. We should look
-- at the first *two* tokens in the stream (it should always exist):
-- id  '=' -> Parse with first rule
-- id  ',' -> Parse with first rule
-- id  '(' -> Parse with second rule
-- id  id  -> Parse with second rule
-- '(' _   -> Parse with third rule
parseDb :: [Token] -> ParserResult
parseDb (ta:tb:tc) = case (ta, tb) of
    (TokenIdentifier _,   TokenOperator   "=") -> parseDbFirst  $ ta:tb:tc
    (TokenIdentifier _,   TokenPunction   ",") -> parseDbFirst  $ ta:tb:tc
    (TokenIdentifier _,   TokenPunction   "(") -> parseDbSecond $ ta:tb:tc
    (TokenIdentifier _,   TokenIdentifier _  ) -> parseDbSecond $ ta:tb:tc
    (TokenPunction   "(", _                  ) -> parseDbThird  $ ta:tb:tc
    _                                          -> parseDb       $ [ta] -- error
parseDb tokens = notFound ("variable list, function form, or parenthesized "
                           ++ "expression") tokens

parseDbFirst :: [Token] -> ParserResult
parseDbFirst tokens = do
    (aa, ta) <- parseVl tokens
    tb       <- consume ta $ TokenOperator "="
    (ac, tc) <- parseE tb
    return (AstDef aa ac, tc)

parseDbSecond :: [Token] -> ParserResult
parseDbSecond (TokenIdentifier idName : tokens) = do
    (aa, ta) <- many tokens parseVb (\t -> head t == TokenOperator "=")
    tb       <- consume ta $ TokenOperator "="
    (ac, tc) <- parseE tb
    return (AstFcnForm (AstIdentifier idName) aa ac, tc)
parseDbSecond tokens = notFound "identifier" tokens

parseDbThird :: [Token] -> ParserResult
parseDbThird (TokenPunction "(" : tokens) = do
    (aa, ta) <- parseD tokens
    tb       <- consume ta $ TokenPunction ")"
    return (aa, tb)
parseDbThird tokens = notFound "open parenthesis" tokens

-- Vb -> '<IDENTIFIER>'
--    -> '(' Vl ')'
--    -> '(' ')'
parseVb :: [Token] -> ParserResult
parseVb (TokenPunction "(" : TokenPunction ")" : tokens) =
    return (AstEmpty, tokens)
parseVb (TokenPunction "(" : tokens) = do
    (aa, ta) <- parseVl tokens
    tb       <- consume ta $ TokenPunction ")"
    return (aa, tb)
parseVb (TokenIdentifier idName : tokens) =
    return (AstIdentifier idName, tokens)
parseVb tokens = notFound "open parenthesis or identifier" tokens

-- Vl -> '<IDENTIFIER>' ( ',' '<IDENTIFIER>' )+
--    -> '<IDENTIFIER>'
-- N.B. I didn't like how the grammar words this definition, so I reworded it
parseVl :: [Token] -> ParserResult
parseVl tokens = do
    (aa, ta) <- manySep tokens parseIdentifier (TokenPunction ",")
    return (if length aa > 1 then AstComma aa else head aa, ta)
    where
        parseIdentifier (TokenIdentifier idName : t) =
            return (AstIdentifier idName, t)
        parseIdentifier t = notFound "identifier" t

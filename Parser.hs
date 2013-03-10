module Parser ( parse, Ast(..) ) where

import Lexer

-- Define all the treenodes, grouped as they are in the RPAL grammar. On the
-- right are the simplified matching types of Ast
data Ast
    -- Expressions
    = AstLet Ast Ast            -- definition, expression
    | AstLambda [Ast] Ast       -- arguments, expression
    | AstWhere Ast Ast          -- tuple or lower, definition

    -- Tuple expressions
    | AstTau [Ast]              -- tuple elements
    | AstAug Ast Ast            -- tuple-like, augmented element
    | AstCond Ast Ast Ast       -- boolean, true condition, false condition

    -- Boolean expressions
    | AstOr Ast Ast             -- boolean, boolean expression
    | AstAmp Ast Ast            -- boolean, boolean expression
    | AstNot Ast                -- boolean
    | AstGr Ast                 -- arithmetic, arithmetic
    | AstGe Ast                 -- arithmetic, arithmetic
    | AstLs Ast                 -- arithmetic, arithmetic
    | AstLe Ast                 -- arithmetic, arithmetic
    | AstEq Ast                 -- arithmetic, arithmetic
    | AstNe Ast                 -- arithmetic, arithmetic

    -- Arithmetic expressions
    | AstPlus Ast Ast           -- arithmetic, arithmetic (binary)
    | AstMinus Ast Ast          -- arithmetic, arithmetic (binary)
    | AstNeg Ast                -- arithmetic (unary)

    -- Rators and Rands (some not defined as a node in the grammar)
    | AstGamma Ast Ast          -- rator, rand
    | AstIdentifier String      -- from TokenIdentifier
    | AstInteger Integer        -- from TokenInteger
    | AstString String          -- from TokenString
    | AstTrue                   -- from TokenIdentifier
    | AstFalse                  -- from TokenIdentifier
    | AstNil                    -- from TokenIdentifier
    | AstDummy                  -- from TokenIdentifier

    -- Definitions
    | AstWithin Ast Ast         -- definition, definition
    | AstAnd [Ast]              -- definition list
    | AstRec Ast                -- definition
    | AstDef Ast Ast            -- commmaified variable list, expression
    | AstFcnForm Ast [Ast] Ast  -- identifier, curried arguments, expression

    -- Variables
    | AstEmpty                  -- from two TokenOperators, ()
    | AstComma [Ast]            -- identifier list
    deriving (Eq, Show)

-- TODO: write custom `Show` instance for Ast nodes

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

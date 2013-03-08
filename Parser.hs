module Parser ( Parser, Ast(..) ) where

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
-- TODO: write parser monad and parsers for it

data Parser
    = SimpleParser Ast [Token] [([Token] -> Parser)]
    | EmptyParser [Token] ([Token] -> Ast)
    deriving Show

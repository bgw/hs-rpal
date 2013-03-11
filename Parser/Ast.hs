module Parser.Ast ( Ast(..), children ) where

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
    | AstGr Ast Ast             -- arithmetic, arithmetic
    | AstGe Ast Ast             -- arithmetic, arithmetic
    | AstLs Ast Ast             -- arithmetic, arithmetic
    | AstLe Ast Ast             -- arithmetic, arithmetic
    | AstEq Ast Ast             -- arithmetic, arithmetic
    | AstNe Ast Ast             -- arithmetic, arithmetic

    -- Arithmetic expressions
    | AstPlus Ast Ast           -- arithmetic, arithmetic (binary)
    | AstMinus Ast Ast          -- arithmetic, arithmetic (binary)
    | AstNeg Ast                -- arithmetic (unary)
    | AstMult Ast Ast           -- arithmetic, arithmetic (binary)
    | AstDiv Ast Ast            -- arithmetic, arithmetic (binary)
    | AstPow Ast Ast            -- arithmetic, arithmetic (binary)
    | AstInfix Ast Ast Ast      -- rator, identifier, rator

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
    deriving (Eq)

-- Define `show` for the Ast
instance Show Ast where
    show a =
        -- Recursively print all children with '.' characters for indentation
        unlines $ (showName a) : (         -- prepend current name and rejoin
            map ('.':) $                   -- prefix each line with '.'
                lines $ concat $           -- split up each line
                    map show (children a)  -- call recursively
        )

-- Our type names are different from those printed by Bermudez's parser, so we
-- have to convert them (ugh)
showName :: Ast -> String

-- Expressions
showName (AstLet _ _) = "let"
showName (AstLambda _ _) = "lambda"
showName (AstWhere _ _) = "where"

-- Tuple expressions
showName (AstTau _) = "tau"
showName (AstAug _ _) = "aug"
showName (AstCond _ _ _) = "->"

-- Boolean expressions
showName (AstOr _ _) = "or"
showName (AstAmp _ _) = "&"
showName (AstNot _) = "not"
showName (AstGr _ _) = "gr"
showName (AstGe _ _) = "ge"
showName (AstLs _ _) = "ls"
showName (AstLe _ _) = "le"
showName (AstEq _ _) = "eq"
showName (AstNe _ _) = "ne"

-- Arithmetic expressions
showName (AstPlus _ _) = "+"
showName (AstMinus _ _) = "-"
showName (AstNeg _) = "neg"
showName (AstMult _ _) = "*"
showName (AstDiv _ _) = "/"
showName (AstPow _ _) = "**"
showName (AstInfix _ _ _) = "@"

-- Rators and Rands
showName (AstGamma _ _) = "gamma"
showName (AstIdentifier n) = "<ID:" ++ n ++ ">"
showName (AstInteger n) = "<INT:" ++ (show n) ++ ">"
showName (AstString n) = "<STR:" ++ n ++ ">"
showName (AstTrue) = "<true>"
showName (AstFalse) = "<false>"
showName (AstNil) = "<nil>"
showName (AstDummy) = "<dummy>"

-- Definitions
showName (AstWithin _ _) = "within"
showName (AstAnd _) = "and"
showName (AstRec _) = "rec"
showName (AstDef _ _) = "="
showName (AstFcnForm _ _ _) = "function_form"

-- Variables
showName (AstEmpty) = "<()>"
showName (AstComma _)= ","

-- Given the format that our Ast types are in, it isn't easy to iterate over the
-- children in the tree. This should help with that.
children :: Ast -> [Ast]

-- Expressions
children (AstLet a b) = [a, b]
children (AstLambda a b) = a ++ [b]
children (AstWhere a b) = [a, b]

-- Tuple expressions
children (AstTau a) = a
children (AstAug a b) = [a, b]
children (AstCond a b c) = [a, b, c]

-- Boolean expressions
children (AstOr a b) = [a, b]
children (AstAmp a b) = [a, b]
children (AstNot a) = [a]
children (AstGr a b) = [a, b]
children (AstGe a b) = [a, b]
children (AstLs a b) = [a, b]
children (AstLe a b) = [a, b]
children (AstEq a b) = [a, b]
children (AstNe a b) = [a, b]

-- Arithmetic expressions
children (AstPlus a b) = [a, b]
children (AstMinus a b) = [a, b]
children (AstNeg a) = [a]
children (AstMult a b) = [a, b]
children (AstDiv a b) = [a, b]
children (AstPow a b) = [a, b]
children (AstInfix a b c) = [a, b, c]

-- Rators and Rands
children (AstGamma a b) = [a, b]
children (AstIdentifier _) = []
children (AstInteger _) = []
children (AstString _) = []
children (AstTrue) = []
children (AstFalse) = []
children (AstNil) = []
children (AstDummy) = []

-- Definitions
children (AstWithin a b) = [a, b]
children (AstAnd a) = a
children (AstRec a) = [a]
children (AstDef a b) = [a, b]
children (AstFcnForm a b c) = a : b ++ [c]

-- Variables
children (AstEmpty) = []
children (AstComma a) = a

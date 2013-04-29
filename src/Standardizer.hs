-- Turn the AST into a simplified tree format, useful for the CSE machine. In
-- addition to partial standardization (needed for the CSE machine), this
-- supports full standardization because it wasn't that much more work.
--
-- Standardizing converts complex AST nodes like `let` or `fcn_form` into a
-- series of `gamma` and `lambda` nodes. This makes implementing the CSE machine
-- easier and later programmatic tree transformations (if we did any) easier.
--
-- The implementation of this module is heavily dependent on pattern matching,
-- which we use to identify subtrees that we can then transform. Actual
-- standardization is done in a bottom-up fashion, as some certain
-- transformations are dependent on the standardized form of their children.
module Standardizer ( standardize
                    , standardizePartially
                    , standardizeFully
                    , StandardizationLevel(..)
                    ) where

import Parser.Ast

-- `Full` will standardize everything. `Partial` will standardize just enough to
-- get code working with the extended CSE machine.
data StandardizationLevel = Full | Partial

-- Calls `standardizeNode` recursively, standardizing the entire tree (partially
-- or fully).
standardize :: StandardizationLevel -> Ast -> Ast
standardize level ast =
    snd $ partial (0, ast)
    where
        partial :: (Integer, Ast) -> (Integer, Ast)
        partial (nodeId, partialAst) =
            (newId, standardizeNode level newId partialAst standardizedChildren)
            where
                childrenTuples :: (Integer, [Ast])
                childrenTuples = foldl partialWrapper
                                       (nodeId, [])
                                       (children partialAst)
                partialWrapper :: (Integer, [Ast]) -> Ast -> (Integer, [Ast])
                partialWrapper (a, b) c = (fst r, b ++ [snd r])
                    where r = partial (a, c)
                standardizedChildren :: [Ast]
                standardizedChildren = snd childrenTuples
                newId :: Integer
                newId = (fst childrenTuples) + 1

-- Nice little shortcut functions for `standardize`:
standardizePartially :: Ast -> Ast
standardizePartially = standardize Partial

standardizeFully :: Ast -> Ast
standardizeFully = standardize Full

-- ## Node standardization

-- Given an AST and it's standardized children (as a list), gives out the
-- standardized node. This is done to simplify pattern matching (in most cases)
-- and to keep each function from having to work recursively themselves
-- (simplifying function body).
standardizeNode :: StandardizationLevel -> Integer -> Ast -> [Ast] -> Ast

-- ### Expressions

{-
       let     =>    gamma
       / \           /   \
      =   P       lambda  E
     / \           /  \
    X   E         X    P
-}
standardizeNode level nodeId (AstLet _ _) [AstDef key value, expr] =
    AstGamma (commaHelper level nodeId $ AstLambda [key] expr) value

{-
     lambda    =>   ++lambda
     /    \           /    \
    V++    E         V     .E
-}
standardizeNode _ _ (AstLambda _ _) [argument, expression] =
    AstLambda [argument] expression

standardizeNode level nodeId (AstLambda _ _) nodeChildren =
    let
        argument = head nodeChildren
        argumentsTail = init $ tail nodeChildren
        expression = last nodeChildren
    in AstLambda [argument] $
                 standardizeNode level nodeId
                                 (AstLambda argumentsTail expression)
                                 (argumentsTail ++ [expression])

{-
     where     =>     gamma
     /   \            /   \
    P     =        lambda  E
         / \        /  \
        X   E      X    P
-}
standardizeNode level nId (AstWhere _ _) [innerExpression, AstDef key value] =
    AstGamma (commaHelper level nId $ AstLambda [key] innerExpression) value

-- ### Tuple expressions

{-
    tau   =>    ++gamma
     |            /  \
    E++        gamma  E
                /  \
              aug  .nil
-}
standardizeNode Full _ (AstTau _) [] = AstNil
standardizeNode Full nodeId (AstTau _) (expression:expressionTail) =
    AstGamma (AstGamma (AstOp AstAug)
                       (standardizeNode Full nodeId
                                        (AstTau expressionTail)
                                        expressionTail))
             expression
standardizeNode Partial _ (AstTau _) nodeChildren = AstTau nodeChildren

standardizeNode level _ (AstAug _ _) [a, b] = op AstAug level a b

{-
       ->        =>      gamma
     / | \               /   \
    B  T  E          gamma   nil
                     /   \
                 gamma   lambda
                 /  \        / \
             gamma  lambda  ()  E
              /  \    /  \
            Cond  B  ()   T
-}
standardizeNode Partial _ (AstCond _ _ _) [b, t, e] = AstCond b t e
-- This is a hellish one
standardizeNode Full _ (AstCond _ _ _) [boolean, onTrue, onFalse] =
    AstGamma (AstGamma (AstGamma (AstGamma AstCondOp boolean)
                                 (AstLambda [AstEmpty] onTrue))
                       (AstLambda [AstEmpty] onFalse))
             AstNil

-- ### Boolean expressions
-- The `op` and `uop` helpers are defined later.

standardizeNode level _ (AstOr  _ _) [a, b] =  op AstOr  level a b
standardizeNode level _ (AstAmp _ _) [a, b] =  op AstAmp level a b
standardizeNode level _ (AstNot _  ) [a   ] = uop AstNot level a
standardizeNode level _ (AstGr  _ _) [a, b] =  op AstGr  level a b
standardizeNode level _ (AstGe  _ _) [a, b] =  op AstGe  level a b
standardizeNode level _ (AstLs  _ _) [a, b] =  op AstLs  level a b
standardizeNode level _ (AstLe  _ _) [a, b] =  op AstLe  level a b
standardizeNode level _ (AstEq  _ _) [a, b] =  op AstEq  level a b
standardizeNode level _ (AstNe  _ _) [a, b] =  op AstNe  level a b

-- ### Arithmetic expressions

standardizeNode level _ (AstPlus  _ _) [a, b] =  op AstPlus  level a b
standardizeNode level _ (AstMinus _ _) [a, b] =  op AstMinus level a b
standardizeNode level _ (AstNeg   _  ) [a   ] = uop AstNeg   level a
standardizeNode level _ (AstMult  _ _) [a, b] =  op AstMult  level a b
standardizeNode level _ (AstDiv   _ _) [a, b] =  op AstDiv   level a b
standardizeNode level _ (AstPow   _ _) [a, b] =  op AstPow   level a b

{-
        @       =>      gamma
      / | \             /   \
    E1  N  E2        gamma   E2
                      /  \
                     N   E1
-}
standardizeNode _ _ (AstInfix _ _ _) [left, identifier, right] =
    AstGamma (AstGamma identifier left) right

-- ### Rators and Rands
-- These are all simple no-op translations.

standardizeNode _ _ (AstGamma      _ _  ) [a, b]        = AstGamma      a b
standardizeNode _ _ (AstIdentifier name ) []            = AstIdentifier name
standardizeNode _ _ (AstInteger    value) []            = AstInteger    value
standardizeNode _ _ (AstString     value) []            = AstString     value
standardizeNode _ _ (AstTrue            ) []            = AstTrue
standardizeNode _ _ (AstFalse           ) []            = AstFalse
standardizeNode _ _ (AstNil             ) []            = AstNil
standardizeNode _ _ (AstDummy           ) []            = AstDummy

-- ### Definitions

{-
       within     =>     =
       /    \           / \
      =      =         X2 gamma
     / \    / \           /   \
    X1 E1  X2 E2      lambda   E1
                       /  \
                      X1  E2
-}
standardizeNode level nodeId (AstWithin _ _) [AstDef x1 e1, AstDef x2 e2] =
    AstDef x2 $ AstGamma (commaHelper level nodeId $ AstLambda [x1] e2) e1

{-
     and     =>    =
      |           / \
     =++         ,  tau
     / \         |   |
    X   E       X++  E++
-}
standardizeNode _ _ (AstAnd _) nodeChildren =
    AstDef (AstComma $ fmap defFst nodeChildren)
           (AstTau   $ fmap defSnd nodeChildren)
    where 
        defFst :: Ast -> Ast
        defFst (AstDef a _) = a
        defFst _ = error "expected AstDef"
        defSnd :: Ast -> Ast
        defSnd (AstDef _ b) = b
        defSnd _ = error "expected AstDef"

{-
     rec      =>     =
      |             / \
      =            X  gamma
     / \              /   \
    X   E            Y*  lambda
                          /  \
                         X    E
-}
standardizeNode level nodeId (AstRec _) [AstDef key value] =
    AstDef key
           (AstGamma AstYstar
                     (commaHelper level nodeId $ AstLambda [key] value))


{-
     fcn_form     =>    =
     /  |   \          / \
    P   V++  E        P  ++lambda
                          /   \
                         P    .E
-}
standardizeNode level nodeId (AstFcnForm _ _ _) nodeChildren =
    let
        name = head nodeChildren
        arguments = init $ tail nodeChildren
        expression = last nodeChildren
    in AstDef name $
              standardizeNode level nodeId
                              (AstLambda arguments expression)
                              (arguments ++ [expression])

-- ### Variables

standardizeNode _ _ (AstEmpty) [] = AstEmpty

-- ### Things that get handled later

standardizeNode _ _ (AstDef _ _) [a, b] = AstDef a b
standardizeNode _ _ (AstComma _) nodeChildren = AstComma nodeChildren

-- ### Error Handling

standardizeNode _ _ node _ =
    error $ "Could not standardize invalid AST subtree:\n" ++ (show node)


-- ## Uop and Op helpers
-- These are very common conversions, so these are some helper functions that we
-- can reuse. These conversions only happen with full standardization.

{-
    Uop     =>     gamma
     |             /   \
     E           Uop    E
-}
uop :: (Ast -> Ast) -> StandardizationLevel -> Ast -> Ast
uop constructor Full child    = AstGamma (AstUop constructor) child
uop constructor Partial child = constructor child

{-
      Op      =>     gamma
     /  \            /   \
    E1  E2        gamma  E2
                  /  \
                 Op  E1
-}
op :: (Ast -> Ast -> Ast) -> StandardizationLevel -> Ast -> Ast -> Ast
op constructor Full childA childB =
    AstGamma (AstGamma (AstOp constructor) childA) childB
op constructor Partial childA childB = constructor childA childB

-- ## Extra helper for `and`
-- There's a complicated case that occurs with `and`. The `and` node transforms:
{-
     and     =>     =
      |            / \
     =++          ,  tau
     / \          |   |
    X   E        X++  E++
-}
-- Unfortunately, `,` nor `tau` are part of the fully standardized tree.
-- The `tau` can be immediately processed, but the only way to get rid of a `,`
-- node is after the parent `=` node is processed:
{-
     lambda     =>     lambda
      /  \              /  \
     ,    E          Temp  ++gamma
     |                      /   \
    X++i                lambda  gamma
                         /  \    /  \
                       X.i  .E Temp  <INTEGER:i>
-}
-- Because of this, we have to reprocess the top of the subtree if `AstDef` gets
-- converted to AstLambda. This is the callback function:
commaHelper :: StandardizationLevel -> Integer -> Ast -> Ast
commaHelper Partial _  a = a
commaHelper Full nodeId (AstLambda [(AstComma elements)] expression) =
    AstLambda [temp] (subtree elements 1)
    where
        subtree :: [Ast] -> Integer -> Ast
        subtree [] _ = expression
        subtree (h:t) tupleIndex =
            AstGamma (AstLambda [h] (subtree t $ tupleIndex + 1))
                     (AstGamma temp (AstInteger tupleIndex))

        -- The temporary variable is contructed with a uuid (for this tree)
        temp :: Ast
        temp = AstTemp nodeId 0
commaHelper Full _ a = a

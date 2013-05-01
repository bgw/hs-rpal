module Evaluator.Environment ( Environment(..)
                             , environmentLookup ) where

import Data.List (intersperse)
import Parser.Ast

data Environment = EnvironmentPrimative
                 | Environment Ast Ast Environment

type Env = Environment

environmentLookup :: Environment -> Ast -> Ast
-- Passthrough types
environmentLookup _ (AstInteger v) = AstInteger v
environmentLookup _ (AstString v) = AstString v
environmentLookup _ AstTrue = AstTrue
environmentLookup _ AstFalse = AstFalse
environmentLookup _ AstNil = AstNil
environmentLookup _ AstDummy = AstDummy
environmentLookup _ AstEmpty = AstEmpty

-- Recursive identifier lookup
environmentLookup (Environment (AstIdentifier key) value next)
                  (AstIdentifier query) =
    if query == key then
        value
    else
        environmentLookup next (AstIdentifier query)

environmentLookup (EnvironmentPrimative) key = primativeLookup key
environmentLookup _ _ = error "lookups not yet implemented"

primativeLookup :: Ast -> Ast
-- Operations
primativeLookup (AstOp  a) = primativeOpLookup  $ a AstDummy AstDummy
primativeLookup (AstUop a) = primativeUopLookup $ a AstDummy

-- Functions
primativeLookup (AstIdentifier "Print") =
    AstEvaluatable $ \a -> (putStr $ f a) >> (return AstDummy)
    where
        f (AstString  a) = a
        f (AstInteger a) = show a
        f (AstTrue     ) = "true"
        f (AstFalse    ) = "false"
        f (AstNil      ) = "nil"
        f (AstTau     a) = "(" ++ (concat $ intersperse ", " $ fmap f a) ++ ")"
        f a = error $ "Can't print node:\n" ++ (show a)
primativeLookup (AstIdentifier "print") =
    primativeLookup $ AstIdentifier "Print"

-- Error case
primativeLookup key = error $ "Could not find:\n" ++ (show key)



-- Operation helper functions
primativeIntOp :: (Integer -> Integer -> Integer) -> Ast
primativeIntOp f = AstEvaluatable $ return . fa
    where
        errorMessage = "Non-integer value applied to integer binary operation"
        fa (AstInteger a) = AstEvaluatable $ return . fb
            where
                fb (AstInteger b) = AstInteger $ f a b
                fb _ = error errorMessage
        fa _ = error errorMessage

primativeIntBoolOp :: (Integer -> Integer -> Bool) -> Ast
primativeIntBoolOp f = AstEvaluatable $ return . fa
    where
        errorMessage = "Non-integer value applied to integer binary operation"
        fa (AstInteger a) = AstEvaluatable $ return . fb
            where
                fb (AstInteger b) = toAstBool $ f a b
                fb _ = error errorMessage
        fa _ = error errorMessage
        toAstBool True = AstTrue
        toAstBool False = AstFalse

primativeIntUop :: (Integer -> Integer) -> Ast
primativeIntUop f = AstEvaluatable $ return . g
    where
        g (AstInteger a) = AstInteger $ f a
        g _ = error "Non-integer value applied to integer unary operation"

primativeBoolOp :: (Bool -> Bool -> Bool) -> Ast
primativeBoolOp f =
    AstEvaluatable (\a -> return $ AstEvaluatable $ return . toAstBool . g a)
    where
        g AstTrue  AstTrue  = f True  True
        g AstTrue  AstFalse = f True  False
        g AstFalse AstTrue  = f False True
        g AstFalse AstFalse = f False False
        g _ _ = error "Non-boolean value applied to boolean binary operation"
        toAstBool True = AstTrue
        toAstBool False = AstFalse

primativeBoolUop :: (Bool -> Bool) -> Ast
primativeBoolUop f = AstEvaluatable $ return . toAstBool . g
    where
        g AstTrue  = f True
        g AstFalse = f False
        g _ = error "Non-boolean value applied to boolean unary operation"
        toAstBool True = AstTrue
        toAstBool False = AstFalse

primativeOpLookup :: Ast -> Ast
primativeOpLookup  (AstPlus _ _)  = primativeIntOp (+)
primativeOpLookup  (AstMinus _ _) = primativeIntOp (-)
primativeOpLookup  (AstMult _ _)  = primativeIntOp (*)
primativeOpLookup  (AstDiv _ _)   = primativeIntOp quot
primativeOpLookup  (AstPow _ _)   = primativeIntOp (^)
primativeOpLookup  (AstGr _ _)    = primativeIntBoolOp (>)
primativeOpLookup  (AstGe _ _)    = primativeIntBoolOp (>=)
primativeOpLookup  (AstLs _ _)    = primativeIntBoolOp (<)
primativeOpLookup  (AstLe _ _)    = primativeIntBoolOp (<=)
primativeOpLookup  (AstEq _ _)    = primativeIntBoolOp (==)
primativeOpLookup  (AstNe _ _)    = primativeIntBoolOp (/=)
primativeOpLookup  (AstOr  _ _)   = primativeBoolOp (||)
primativeOpLookup  (AstAmp _ _)   = primativeBoolOp (&&)
primativeOpLookup  _ = error "Unknown binary operation"

primativeUopLookup :: Ast -> Ast
primativeUopLookup (AstNeg _)     = primativeIntUop $ (*) (-1)
primativeUopLookup (AstNot _)     = primativeBoolUop not
primativeUopLookup  _ = error "Unknown unary operation"

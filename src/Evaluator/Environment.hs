module Evaluator.Environment ( Environment(..)
                             , environmentLookup ) where

import Data.List (intersperse)
import Parser.Ast

data Environment = EnvironmentPrimative
                 | Environment Ast Ast Environment
                 deriving Show

environmentLookup :: Environment -> Ast -> Ast
-- Passthrough types
environmentLookup _ (AstInteger v) = AstInteger v
environmentLookup _ (AstString v) = AstString v
environmentLookup _ AstTrue = AstTrue
environmentLookup _ AstFalse = AstFalse
environmentLookup _ AstNil = AstNil
environmentLookup _ AstDummy = AstDummy
environmentLookup _ AstEmpty = AstEmpty

-- Operations can only ever be defined in the primative environment
environmentLookup (Environment _ _ _) (AstOp query) =
    primativeLookup (AstOp query)

environmentLookup (Environment (AstIdentifier key) value next)
                  (AstIdentifier query) =
    if query == key then
        value
    else
        environmentLookup next (AstIdentifier query)

environmentLookup (Environment (AstTemp keyA keyB) value next)
                  (AstTemp queryA queryB) =
    if keyA == queryA && keyB == queryB then
        value
    else
        environmentLookup next (AstTemp queryA queryB)

-- Not found, go to the next environment
environmentLookup (Environment _ _ next) query =
    environmentLookup next query

-- Fell down to the primative environment
environmentLookup EnvironmentPrimative key = primativeLookup key

primativeLookup :: Ast -> Ast
-- Operations
primativeLookup (AstOp  a) = primativeOpLookup  $ a AstDummy AstDummy
primativeLookup (AstUop a) = primativeUopLookup $ a AstDummy
primativeLookup AstCondOp  =
    AstEvaluatable $ \a ->
        return $ AstEvaluatable $ \b ->
            return $ AstEvaluatable $ f a b
    where
        f AstTrue  (AstEvaluatable a) _ = a AstEmpty
        f AstFalse _ (AstEvaluatable a) = a AstEmpty
        f _ _ _ = error "Cond was passed non-boolean"

-- Functions
primativeLookup (AstIdentifier "Print") =
    AstEvaluatable $ \a -> (putStr $ f a) >> (return AstDummy)
    where
        f (AstString  a) = init $ tail a
        f (AstInteger a) = show a
        f (AstTrue     ) = "true"
        f (AstFalse    ) = "false"
        f (AstNil      ) = "nil"
        f (AstTau     a) = "(" ++ (concat $ intersperse ", " $ fmap f a) ++ ")"
        f a = error $ "Can't print node:\n" ++ (show a)

primativeLookup AstYstar = AstYstar

primativeLookup (AstIdentifier "print") =
    primativeLookup $ AstIdentifier "Print"

primativeLookup (AstIdentifier "Istuple") =
    AstEvaluatable f
    where
        f (AstTau _) = return AstTrue
        f AstNil     = return AstTrue
        f _          = return AstFalse

primativeLookup (AstIdentifier "Isfunction") =
    AstEvaluatable f
    where
        f (AstLambda _ _) = return AstTrue
        f _               = return AstFalse

primativeLookup (AstIdentifier "Isdummy") =
    AstEvaluatable f
    where
        f AstDummy = return AstTrue
        f _        = return AstFalse

primativeLookup (AstIdentifier "Isstring") =
    AstEvaluatable f
    where
        f (AstString _) = return AstTrue
        f _             = return AstFalse

primativeLookup (AstIdentifier "Isinteger") =
    AstEvaluatable f
    where
        f (AstInteger _) = return AstTrue
        f _              = return AstFalse

primativeLookup (AstIdentifier "Istruthvalue") =
    AstEvaluatable f
    where
        f AstTrue  = return AstTrue
        f AstFalse = return AstTrue
        f _        = return AstFalse

primativeLookup (AstIdentifier "Conc") =
    AstEvaluatable $ \a -> return $ AstEvaluatable $ f a
    where
        f (AstString a) (AstString b) = return $ AstString $ a ++ b
        f _ _                         = error "non-string value passed to Conc"

primativeLookup (AstIdentifier "conc") =
    primativeLookup $ AstIdentifier "Conc"

primativeLookup (AstIdentifier "ItoS") =
    AstEvaluatable f
    where
        f (AstInteger a) = return $ AstString $ show a
        f _              = error "non-integer value passed to ItoS"

primativeLookup (AstIdentifier "Order") =
    AstEvaluatable f
    where
        f (AstTau a) = return $ AstInteger $ toInteger $ length a
        f AstNil     = return $ AstInteger 0
        f _          = error "non-tuple value passed to Order"

primativeLookup (AstIdentifier "Stem") =
    AstEvaluatable f
    where
        f (AstTau    (a:_))     = return $ a
        f (AstString (_:a:_:_)) = return $ AstString ['\'', a, '\'']
        f _ = error "Stem requires a tuple of order 1 or more"

primativeLookup (AstIdentifier "Stern") =
    AstEvaluatable f
    where
        f (AstTau    (_:b))   = return $ AstTau b
        f (AstString (_:_:b)) = return $ AstString $ "'" ++ (init b) ++ "'"
        f _ = error "Stern requires a tuple of order 1 or more"

primativeLookup (AstIdentifier "Null") =
    AstEvaluatable f
    where
        f (AstTau []) = return AstTrue
        f AstNil      = return AstTrue
        f _           = return AstFalse

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
primativeOpLookup  (AstAug _ _)   =
    AstEvaluatable (\a -> return $ AstEvaluatable $ f a)
    where
        f (AstTau a) b = return $ AstTau $ a ++ [b]
        f AstNil     b = return $ AstTau [b]
        f _          _ = error "aug called with non-tuple type"
primativeOpLookup  _ = error "Unknown binary operation"

primativeUopLookup :: Ast -> Ast
primativeUopLookup (AstNeg _)     = primativeIntUop $ (*) (-1)
primativeUopLookup (AstNot _)     = primativeBoolUop not
primativeUopLookup  _ = error "Unknown unary operation"

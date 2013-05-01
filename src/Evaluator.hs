module Evaluator ( evaluateSimple ) where

import Parser.Ast
import Evaluator.Control
import Evaluator.Environment

data StackElement = StackEnvironment Environment
                  | StackValue Ast
                  | StackLambda Ast ControlStructure Environment
type Stack = [StackElement]

type CSE = (ControlStructure, Stack, Environment)

evaluateSimple :: ControlStructure -> IO ()
evaluateSimple c = 
    (evaluateHelper (c, [], EnvironmentPrimative)) >> putStrLn ""

evaluateHelper :: CSE -> IO ()
evaluateHelper cse =
    case step cse of
        Just r  -> r >>= evaluateHelper
        Nothing -> return ()

step :: CSE -> Maybe (IO CSE)
-- Rule 1 (Stacking a name)
step (((ControlName name):c), s, e) =
    Just $ return (c, (StackValue $ environmentLookup e name):s, e)
-- Rule 2 (Stacking a lambda)
step (((ControlLambda ast struct):c), s, e) =
    Just $ return (c, (StackLambda ast struct e):s, e)
-- Rule 3 (Applying a rator)
step ( (ControlGamma:c)
     , ((StackValue (AstEvaluatable f)):(StackValue v):s)
     , e) =
    Just $ do
        r <- f v
        return (c, (StackValue r):s, e)
-- Rule 4 (Applying a lambda closure)
step ((ControlGamma:c), ((StackLambda skey sc se):(StackValue svalue):s), e) =
    Just $ return ( sc ++ (ControlEnvironment nextEnvironment) : c
                  , (StackEnvironment nextEnvironment):s
                  , e
                  )
    where
        nextEnvironment = Environment skey svalue se
-- Rule 5 (Exit from environment)
step ( ((ControlEnvironment _):c)
     , ((StackValue sv):(StackEnvironment _):s)
     , (Environment _ _ e)) =
    Just $ return (c, (StackValue sv):s, e)

-- Exit handler
step ([], [StackValue _], EnvironmentPrimative) = Nothing

-- Error handler
step _ = error "Bad CSE machine state"

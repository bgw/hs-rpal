module Evaluator ( evaluateSimple ) where

import Parser.Ast
import Evaluator.Control
import Evaluator.Environment

data StackElement = StackEnvironment Environment
                  | StackValue Ast
                  | StackLambda Ast ControlStructure Environment
                  | StackMetaLambda Ast ControlStructure Environment
                  deriving Show
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
-- Rule 12 (Y-Combinator)
step ((ControlGamma:c),
      ((StackValue AstYstar):(StackLambda lAst lStruct lEnv):s),
      e) =
    Just $ return (c, (StackMetaLambda lAst lStruct lEnv):s, e)
-- Rule 13 (Y-Combinator)
step ((ControlGamma:c), ((StackMetaLambda lAst lStruct lEnv):s), e) =
    Just $ return
        ( ControlGamma:ControlGamma:c
        , (StackLambda lAst lStruct lEnv):(StackMetaLambda lAst lStruct lEnv):s
        , e
        )
-- Rule 1 (Stacking a name)
step (((ControlName name):c), s, e) =
    Just $ return (c, (StackValue $ environmentLookup e name):s, e)
-- Rule 2 (Stacking a lambda)
step (((ControlLambda ast struct):c), s, e) =
    Just $ return (c, (StackLambda ast struct e):s, e)
-- Rule 3 (Applying a rator)
step ( (ControlGamma:c), ((StackValue f):(StackValue v):s), e) =
    Just $ do
        r <- eval f v
        return (c, (StackValue r):s, e)
    where
        eval (AstTau el) (AstInteger index) =
            return $ el !! (fromIntegral $ index - 1)
        eval (AstEvaluatable func) a        = func a
        eval _ _ = error "You can only call a function or a tuple"
-- Rule 4 (Applying a lambda closure)
step ((ControlGamma:c), ((StackLambda skey sc se):(StackValue svalue):s), e) =
    Just $ return ( sc ++ (ControlEnvironment e) : c
                  , (StackEnvironment e):s
                  , nextEnvironment
                  )
    where
        nextEnvironment = Environment skey svalue se
-- Rule 5 (Exit from environment)
step ( ((ControlEnvironment _):c)
     , ((StackValue sv):(StackEnvironment e):s)
     , (Environment _ _ _)) =
    Just $ return (c, (StackValue sv):s, e)

-- Exit handler
step ([], [StackValue _], EnvironmentPrimative) = Nothing

-- Error handler
step (_, _, _) = error $ "Bad CSE machine state"

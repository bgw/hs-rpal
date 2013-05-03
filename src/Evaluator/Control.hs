module Evaluator.Control ( generateControl
                         , ControlElement(..)
                         , ControlStructure ) where

import Evaluator.Environment
import Parser.Ast
import Data.List (elemIndex)

data ControlElement = ControlGamma
                    | ControlLambda Ast ControlStructure
                    | ControlName Ast
                    | ControlPrimativeEnvironment
                    | ControlEnvironment Environment
type ControlStructure = [ControlElement]

-- Pretty mathematical symbols
unicodeLambda :: Char
unicodeLambda = 'λ'
unicodeGamma :: Char
unicodeGamma = '𝛾'
unicodeDelta :: Char
unicodeDelta = 'δ'

instance Show ControlElement where
    show (ControlGamma) = [unicodeGamma]
    show (ControlLambda argument _) =
        "(" ++ [unicodeLambda] ++ " ? " ++ (show $ ControlName argument) ++ ")"
    show (ControlName name) =
        case elemIndex ' ' strName of
            Nothing -> strName
            Just _ -> "(" ++ strName ++ ")" -- parenthesize only when needed
        where
            strName = showName name
    show _ = error "This part of Control not showable"

    -- show for ControlStructure
    showList elements =
        (([unicodeDelta] ++ "0: " ++ showHelper 0 (reverse elements) []) ++)
        where
            showHelper :: Int -> ControlStructure -> [ControlStructure]
                       -> String
            -- No elements left in structure, no queue
            showHelper _ [] [] =
                ""
            -- No el left in structure, but at least one in queue
            showHelper i [] (next:rest) =
                "\n" ++ [unicodeDelta] ++ (show $ i + 1) ++ ": "
                     ++ showHelper (i+1) next rest
            -- Individual element handlers
            showHelper i (ControlGamma:el) queue =
                [unicodeGamma] ++ " " ++ showHelper i el queue
            showHelper i ((ControlLambda argument next):el) queue =
                "(" ++ [unicodeLambda] ++ " "
                    ++ (show $ i + (length queue) + 1) ++ " "
                    ++ (show $ ControlName argument) ++ ") "
                    ++ showHelper i el (queue ++ [reverse next])
            showHelper i ((ControlName name):el) queue =
                (show $ ControlName name) ++ " " ++ showHelper i el queue
            showHelper _ _ _ = error "Error showing ControlStructure"

-- Generate the (recursively nested) control structure for the program
generate :: Ast -> ControlStructure
generate (AstGamma a b) = (generate b) ++ (generate a) ++ [ControlGamma]
generate (AstLambda [a] b) = [ControlLambda a (generate b)]
-- All the simple ControlName types
generate (AstIdentifier a  ) = [ControlName $ AstIdentifier a  ]
generate (AstInteger    a  ) = [ControlName $ AstInteger    a  ]
generate (AstString     a  ) = [ControlName $ AstString     a  ]
generate (AstTrue          ) = [ControlName $ AstTrue          ]
generate (AstFalse         ) = [ControlName $ AstFalse         ]
generate (AstNil           ) = [ControlName $ AstNil           ]
generate (AstDummy         ) = [ControlName $ AstDummy         ]
generate (AstEmpty         ) = [ControlName $ AstEmpty         ]
generate (AstOp         a  ) = [ControlName $ AstOp         a  ]
generate (AstUop        a  ) = [ControlName $ AstUop        a  ]
generate (AstCondOp        ) = [ControlName $ AstCondOp        ]
generate (AstYstar         ) = [ControlName $ AstYstar         ]
generate (AstTemp       a b) = [ControlName $ AstTemp       a b]
generate a = error ("not an identifier or value:\n" ++ (show a))

generateControl :: Ast -> ControlStructure
generateControl = generate

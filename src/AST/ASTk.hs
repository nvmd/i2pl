module AST.ASTk
(   LLang (LLangVariable, LLangConstant,
           Plus, Minus, Mult, Div, Mod,
           Greater,
           If, Fun, Apply)
,   eval
) where

data LLang =
            -- X - variables
            LLangVariable String
            -- Z - constants
            | LLangConstant Integer
            -- O - operations
            | Plus LLang LLang
            | Minus LLang LLang
            | Mult LLang LLang
            | Div LLang LLang
            | Mod LLang LLang
            | Greater LLang LLang
            -- R
            | If LLang LLang LLang
            | Fun LLang LLang   -- must be smth. like Fun LLangVariable LLang
            | Apply LLang LLang
            deriving (Eq)
--            deriving (Show)

instance Show LLang where
    show (LLangVariable x) = x
    show (LLangConstant x) = show x
    show (Plus x y)        = "(" ++ show x ++ "+" ++ show y ++ ")"
    show (Minus x y)       = "(" ++ show x ++ "-" ++ show y ++ ")"
    show (Mult x y)        = "(" ++ show x ++ "*" ++ show y ++ ")"
    show (Div x y)         = "(" ++ show x ++ "div" ++ show y ++ ")"
    show (Mod x y)         = "(" ++ show x ++ "mod" ++ show y ++ ")"
    show (If x y z)        = "(" ++ show x ++ "?" ++ show y ++ ":" ++ show z ++ ")"
    show (Fun x y)         = "{" ++ show x ++ "->" ++ show y ++ "}"
    show _                 = "<unknown LLang>"

llang_bool :: LLang -> Bool
llang_bool (LLangConstant x) = x /= 0
llang_bool _ = False

eval :: LLang -> LLang
-- This kind of tree can't be reduced - return as is
eval (LLangVariable v) = LLangVariable v
eval (LLangConstant v) = LLangConstant v
-- variable-constant, constant-variable
eval (Plus (LLangConstant x) (LLangVariable y)) = Plus (LLangConstant x) (LLangVariable y)
eval (Plus (LLangVariable x) (LLangConstant y)) = Plus (LLangVariable x) (LLangConstant y)
-- variable evaluators
eval (Plus (LLangVariable x) (LLangVariable y))  = Plus (LLangVariable x) (LLangVariable y)
eval (Minus (LLangVariable x) (LLangVariable y)) = Minus (LLangVariable x) (LLangVariable y)
eval (Mult (LLangVariable x) (LLangVariable y))  = Mult (LLangVariable x) (LLangVariable y)
eval (Div (LLangVariable x) (LLangVariable y))   = Div (LLangVariable x) (LLangVariable y)
eval (Mod (LLangVariable x) (LLangVariable y))   = Mod (LLangVariable x) (LLangVariable y)
-- Reducible trees
-- constant evaluators
eval (Plus (LLangConstant x) (LLangConstant y))  = LLangConstant (x + y)
eval (Minus (LLangConstant x) (LLangConstant y)) = LLangConstant (x - y)
eval (Mult (LLangConstant x) (LLangConstant y))  = LLangConstant (x * y)
eval (Div (LLangConstant x) (LLangConstant y))   = LLangConstant (x `div` y)
eval (Mod (LLangConstant x) (LLangConstant y))   = LLangConstant (x `mod` y)
-- recursive evaluators
eval (Plus x y)
    | same_arg1 && same_arg2 = eval op_wargs
    | otherwise              = op_wargs
    where same_arg1 = x /= eval x
          same_arg2 = y /= eval y
          op_wargs = Plus (eval x) (eval y)
eval (Minus x y)
    | same_arg1 && same_arg2 = eval op_wargs
    | otherwise              = op_wargs
    where same_arg1 = x /= eval x
          same_arg2 = y /= eval y
          op_wargs = Minus (eval x) (eval y)
eval (Mult x y)
    | same_arg1 && same_arg2 = eval op_wargs
    | otherwise              = op_wargs
    where same_arg1 = x /= eval x
          same_arg2 = y /= eval y
          op_wargs = Mult (eval x) (eval y)
eval (Div x y)
    | same_arg1 && same_arg2 = eval op_wargs
    | otherwise              = op_wargs
    where same_arg1 = x /= eval x
          same_arg2 = y /= eval y
          op_wargs = Div (eval x) (eval y)
eval (Mod x y)
    | same_arg1 && same_arg2 = eval op_wargs
    | otherwise              = op_wargs
    where same_arg1 = x /= eval x
          same_arg2 = y /= eval y
          op_wargs = Mod (eval x) (eval y)

eval (Fun (LLangVariable x) y) = eval y
eval (If x y z)
    | llang_bool (eval x)   = eval y
    | otherwise             = eval z
eval (Apply x y) = eval y

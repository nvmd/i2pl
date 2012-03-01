main = print (eval ctx fact6)

i = Integral 2
l = Logical True
x = Var "variable"
_if = If (Logical (2 > 1)) (PLUS (Var "variable") (Var "asd")) (Var "asd")
e = PLUS (Integral 1) (MULT (Integral 3) (Var "asdd"))

fact6 = CALL "fact" (Integral 6)

ctx = [
        ("asdd",        Integral 2),
        ("variable",    Integral 1),
        ("asd",         Integral 3),
        ("Var",         (Fun "x" (Var "x"))),
        ("Bool",        (Fun "x" (EQ0 (Var "x")))),
        ("fun1",        (Fun "x" (MULT (MINUS (Var "x") (Integral 1)) (Integral 2)))),
        ("fun2",        (Fun "x" (MULT (Var "x") (Integral 1)))),
        ("fun",         (Fun "x" (CALL "fun1" (PLUS (Var "x") (Integral 1))))),
        ("funn",        (Fun "x" (MULT (CALL "fun1" (MINUS (Var "x") (Integral 1))) (Integral 2)))),

        ("fact",        (Fun "x"
                                (If (EQ0 (Var "x"))
                                        (Integral 1)
                                        (MULT 
                                                (Var "x") 
                                                (CALL "fact" 
                                                        (MINUS 
                                                                (Var "x") 
                                                                (Integral 1)))))))
      ]

data Expr =
        -- константы
          Integral Integer
        | Logical Bool
        -- переменная
        | Var String
        -- лямбда-выражение
        | Fun String Expr
        -- применение функции
--        | Apply Expr Expr
        -- условное выражение
        | If Expr Expr Expr        
        -- операторы
        | PLUS Expr Expr
        | MINUS Expr Expr
        | MULT Expr Expr
        | DIV Expr Expr
        -- вызов лямбды(функции)
        | CALL String Expr

        | EQ0 Expr
        -- блоки
--      | Def [(String, Expr)] Expr | DefRec [(String, Expr)] Expr
        	deriving (Show, Eq)

type Context = [(String, Expr)]

eval :: Context -> Expr -> Expr

eval _ e@(Integral _) = e
eval _ e@(Logical _)  = e

eval ctx (Var x) = assoc x ctx

eval ctx (PLUS (Integral a) (Integral b)) = Integral (a + b)
eval ctx (MINUS (Integral a) (Integral b)) = Integral (a - b)
eval ctx (MULT (Integral a) (Integral b)) = Integral (a * b)
eval ctx (DIV (Integral a) (Integral b)) = Integral (a `div` b)

eval ctx (PLUS a b) = binOp ctx PLUS a b
eval ctx (MINUS a b) = binOp ctx MINUS a b
eval ctx (MULT a b) = binOp ctx MULT a b
eval ctx (DIV a b) = binOp ctx DIV a b

eval ctx (CALL x p) = callFun ctx (assoc x ctx) (eval ctx p)
eval ctx (Fun _ e) = eval ctx e

eval ctx (If cond t e) = eval ctx (if (eval ctx cond) == (Logical True) then t else e)

eval ctx (EQ0 e) = Logical (eval ctx e == (Integral 0))

--eval ctx (Apply a b) = apply (eval ctx a) (eval ctx b)

--apply :: Expr -> Expr -> Expr
--apply e a = 

-- поиск по контексту:
assoc x ((y, e):ctx) | x == y    = e
                     | otherwise = assoc x ctx
--
binOp :: Context -> (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
binOp ctx op a b = eval ctx (op (eval ctx a) (eval ctx b))
--
callFun ctx f param = eval ([(getParamName f, param)] ++ ctx) f
getParamName (Fun x _) = x

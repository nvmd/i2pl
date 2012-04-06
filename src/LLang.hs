import AST.AST

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

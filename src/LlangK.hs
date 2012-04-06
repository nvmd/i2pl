import AST.ASTk

main =
    putStrLn (show llang_ast_list) >>
    putStrLn (show (map (eval) llang_ast_list))
    where
    llang_ast_list = [(Plus (LLangConstant 3) (LLangConstant 2)),
                      (Plus (LLangConstant 3) (LLangVariable "x")),
                      (Plus (LLangVariable "x") (LLangConstant 3)),
                      (Plus (LLangVariable "x") (LLangVariable "y")),
                      (Plus (Plus (LLangVariable "x") (LLangVariable "y"))
                            (Plus (LLangVariable "z") (LLangVariable "v"))),
                      (Plus (Plus (Plus (LLangVariable "x") (LLangVariable "w")) (LLangVariable "y"))
                            (Plus (LLangVariable "z") (LLangVariable "v"))),
                      (Plus (Plus (Plus (LLangVariable "x") (LLangVariable "w")) (LLangVariable "y"))
                            (Plus (LLangConstant 8) (LLangVariable "v"))),
                      (Mult (Plus (LLangConstant 3) (LLangConstant 2)) (LLangConstant 8))]

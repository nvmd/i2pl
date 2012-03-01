
import Data.Set
import NFA

main =
    putStrLn (show nfa_list)
    where nfa_list = [(NFA (fromList [0..3])
                           (fromList [(Delta 0 'a' 0), (Delta 0 'a' 1), (Delta 0 'b' 0),
                                      (Delta 1 'b' 2),
                                      (Delta 2 'b' 3)])
                           0
                           (fromList [3]))]

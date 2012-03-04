
import Data.Set
import NFA

check_word_for_nfa nfa word = (toList (nfa_final_states nfa word), nfa_accepts_word nfa word)

main =
    putStrLn (foldl (++) "" (Prelude.map show nfa_list)) >>
--    putStrLn (show (Prelude.map (\nfa -> nfa_delta nfa 0 'a') nfa_list)) >>
--    putStrLn (show (Prelude.map (\nfa -> nfa_next_states nfa (fromList [0]) 'a') nfa_list))
    putStrLn (show (Prelude.map (\nfa -> Prelude.map (\word -> check_word_for_nfa nfa word) word_list) nfa_list))
    where nfa_list = [(NFA (fromList [0..3])
                           (fromList [(Delta 0 'a' 0), (Delta 0 'a' 1), (Delta 0 'b' 0),
                                      (Delta 1 'b' 2),
                                      (Delta 2 'b' 3)])
                           0
                           (fromList [3]))]
          word_list = ["abb", "abba", "abab", "aaabb", "ototo"]

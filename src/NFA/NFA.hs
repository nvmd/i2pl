
module NFA.NFA
(   NFA (NFA)
,   Delta (Delta, EDelta)
,   nfa_next_states
,   nfa_delta
,   nfa_final_states
,   nfa_accepts_word
) where

import Data.Set (Set, toList, difference, fromList, member)

data NFA q = NFA             -- Non-deterministic Finite state Automation (Char - set of input symbols)
             (Set q)         -- set of states (Q)
             (Set (Delta q)) -- set of transitions \delta: (Q x (Char \cup \epsilon)) -> Q
             q               -- start state (q_0 \in Q)
             (Set q)         -- set of terminal/final states (F \subset Q)

-- \delta: (Q x (Char \cup \epsilon)) -> Q
data Delta q = Delta q Char q  -- \delta: (Q x Char) -> Q
              | EDelta q q     -- \delta: (Q x \epsilon) -> Q
              deriving (Eq,Ord)

instance (Show q, Ord q) => Show (NFA q) where
--    show (NFA q delta q0 f) = "(" ++ show (toList q)
--                              ++ ", " ++ show (toList delta)
--                              ++ ", " ++ show q0
--                              ++ ", " ++ show (toList f)
--                              ++ ")"
    show (NFA q delta q0 f) = "digraph Nfa {\n"
                              ++ "\t\"\" [shape=none];\n"
                              ++ foldl (\s qi -> s ++ "\t\"" ++ qi ++ "\" [shape=circle];\n")
                                       ""
                                       (map (show) (toList (difference q f)))
                              ++ foldl (\s qi -> s ++ "\t\"" ++ qi ++ "\" [shape=doublecircle];\n")
                                       ""
                                       (map (show) (toList f))
                              ++ "\n\t\"\" -> " ++ show q0 ++ ";\n"
                              ++ foldl (++) "" (map (show) (toList delta))
                              ++ "}\n"

instance Show q => Show (Delta q) where
--    show (Delta q1 c q2) = "(" ++ show q1 ++ "," ++ show c ++ "," ++ show q2 ++ ")"
--    show (EDelta q1 q2)  = "(" ++ show q1 ++ ",e," ++ show q2 ++ ")"
    show (Delta q1 c q2) = "\t" ++ show q1 ++ " -> " ++ show q2 ++ " [label=" ++ show (c:[]) ++ "];\n"
    show (EDelta q1 q2)  = "\t" ++ show q1 ++ " -> " ++ show q2 ++ " [label=\\epsilon];\n"

nfa_delta :: (Ord q) => (NFA q) -> q -> Char -> (Set q)
nfa_delta (NFA _ delta _ _) state symbol = fromList [ q2 | (Delta q1 c q2) <- (toList delta), q1 == state, c == symbol]

nfa_next_states :: (Ord q) => (NFA q) -> (Set q) -> Char -> (Set q)
nfa_next_states nfa states symbol = fromList (foldl (\state_list qi -> (toList (nfa_delta nfa qi symbol)) ++ state_list)
                                              []
                                              (toList states))

-- Returns set of states that NFA will be in after reading the whole word
nfa_final_states :: (Ord q) => (NFA q) -> String -> (Set q)
nfa_final_states (NFA q delta q0 f) word = nfa_final_states_from_state (NFA q delta q0 f) (fromList [q0]) word

-- Returns set of states that NFA will be in after reading the whole word, assuming the supplied set of start states
nfa_final_states_from_state :: (Ord q) => (NFA q) -> (Set q) -> String -> (Set q)
nfa_final_states_from_state _ states []       = states
nfa_final_states_from_state nfa states (w:ws) = nfa_final_states_from_state nfa (nfa_next_states nfa states w) ws

-- Check, whether NFA accepts the word, or not
nfa_accepts_word :: (Ord q) => (NFA q) -> String -> Bool
nfa_accepts_word (NFA q delta q0 f) word = elem True
                                           (map (\q -> member q f) (toList (nfa_final_states (NFA q delta q0 f) word)))

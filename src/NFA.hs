
module NFA
(   NFA(NFA)
,   Delta(Delta, EDelta)
) where

import Data.Set (Set,toList,difference)

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
                                  ++ foldl (\s q -> s ++ "\t\"" ++ q ++ "\" [shape=circle];\n")
                                           ""
                                           (map (show) (toList (difference q f)))
                                  ++ foldl (\s q -> s ++ "\t\"" ++ q ++ "\" [shape=doublecircle];\n")
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

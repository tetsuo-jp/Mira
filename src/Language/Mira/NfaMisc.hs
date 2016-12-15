--------------------------------------------------------------------------
--									--
--	NfaMisc.hs							--
--									--
--	Misecllaneous definitions for the NFA system. Includes		--
--	examples of machines, and functions to print an NFA and the	--
--	equivalence classes produces by minimisation.			--
--									--
--	Regular expressions are defined in RegExp, and the type of	--
--	NFAs in NfaTypes. The implementation of sets used is in	--
--	Sets.								--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--									--
--------------------------------------------------------------------------

module Language.Mira.NfaMisc where

import qualified Data.Set as Set
import Data.Set ( Set, singleton )

import Language.Mira.NfaTypes

--------------------------------------------------------------------------
--									--
--	Examples							--
--									--
--------------------------------------------------------------------------

machM, machN :: Nfa Int Char
machM = NFA
        (Set.fromList [0..3])
        (Set.fromList [ Move 0 'a' 0 ,
                        Move 0 'a' 1,
                        Move 0 'b' 0,
                        Move 1 'b' 2,
                        Move 2 'b' 3 ] )
        0
        (singleton 3)
machN = NFA
        (Set.fromList [0..5])
        (Set.fromList [ Move 0 'a' 1 ,
                        Move 1 'b' 2,
                        Move 0 'a' 3,
                        Move 3 'b' 4,
                        Emove 3 4,
                        Move 4 'b' 5 ] )
        0
        (Set.fromList [2,5])

--------------------------------------------------------------------------
--									--
--	Printing an NFA.						--
--									--
--------------------------------------------------------------------------

print_nfa :: (Show a, Show b) => Nfa a b -> String
print_nfa (NFA myStates myMoves start finish) =
        "digraph Mira {\n" ++
        show_states (Set.toList myStates) ++
        (concat (map print_move (Set.toList myMoves))) ++
        show_final (Set.toList finish) ++
        show start ++ "[shape=box]\n" ++
        "}\n"

show_states :: Show a => [a] -> String
show_states = concat . (map ((++"\n") . show))

show_final :: Show a => [a] -> String
show_final = concat . (map ((++"[shape=doublecircle]\n") . show))

print_move :: (Show a, Show b) => Move a b -> String
print_move (Move s1 c s2) = "\t" ++ show s1 ++ "\t->\t"
                            ++ show s2 ++ "\t[label=" ++ show c ++ "]\n"

print_move (Emove s1 s2) = "\t" ++ show s1 ++ "\t->\t" ++ show s2 ++ "[label=\"@\"]\n"

--------------------------------------------------------------------------
--									--
--	Printing a set of equivalence classes.				--
--									--
--------------------------------------------------------------------------

print_classes :: Show a => Set (Set a) -> String
print_classes ss = pcs (map Set.toList (Set.toList ss))
                   where
                   pcs = concatMap pc
                   pc  = (++"\n") .  concat . (map ((++"\t").show))

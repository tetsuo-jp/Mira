--------------------------------------------------------------------------
--									--
--	NfaLib.hs							--
--									--
--	Useful functions used in the implementation of an NFA and	--
--	the conversion of an NFA to a DFA.				--
--	Therefore used in ImplementNfa and NfaToFDfa.			--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--									--
--------------------------------------------------------------------------

module Language.Mira.NfaLib where

import qualified Data.Set as Set

import Data.List hiding ( union )
import Data.Set ( Set, union )

import Language.Mira.NfaTypes

-- | The epsilon closure of a set of states in an NFA. This is
--   found by finding the limit of the function which adds to a
--   set all those states accessible by a single epsilon move.
closure :: Ord a => Nfa a b -> Set a -> Set a
closure (NFA _ myMoves _ _)
      = setlimit add
        where
        add stateset = union stateset (Set.fromList accessible)
                       where
                       accessible
                         = [ s | x <- Set.toList stateset,
                                 Emove y s <- Set.toList myMoves,
                                 y==x ]

setlimit :: Eq a => (Set a -> Set a) -> Set a -> Set a
setlimit f s
  | s==next     = s
  | otherwise   = setlimit f next
    where
    next = f s

-- | 'onemove' finds the set of states accessible from a given set
-- by a single move on the given character.
onemove :: (Ord a, Eq b) => Nfa a b -> b -> Set a -> Set a
onemove (NFA _ myMoves _ _) c x
      = Set.fromList [ s | t <- Set.toList x,
                      Move z d s <- Set.toList myMoves,
                      z==t, c==d ]

-- | 'onetrans' performs one move (by onemove) and then takes the
--   epsilon closure of the result.
onetrans :: (Ord a, Eq b) => Nfa a b -> b -> Set a -> Set a
onetrans mach c x = closure mach (onemove mach c x)

-- | 'alphabet' returns the alphabet of the machine, by finding a list of
--   the characters mentioned in the Moves.
alphabet :: Eq b => Nfa a b -> [b]
alphabet (NFA _ myMoves _ _)
  = nub [ c | Move _ c _ <- Set.toList myMoves ]

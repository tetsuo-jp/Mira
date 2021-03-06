--------------------------------------------------------------------------
--									--
--	NfaToDfa.hs							--
--									--
--			NFA to DFA					--
--									--
--	Regular expressions are defined in RegExp, and the type of	--
--	NFAs in NfaTypes. The implementation of sets used is in	--
--	Sets.								--
--	NfaLib contains functions used here and in the implementation:	--
--	closure, startstate etc.					--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--									--
--------------------------------------------------------------------------

module Language.Mira.NfaToDfa where

import qualified Data.Set as Set
import Data.Set ( Set, empty, intersection, singleton, union )

import Language.Mira.RegExp
import Language.Mira.NfaTypes
import Language.Mira.NfaLib

-- |    Conversion of an nfa produced by build to a dfa.
--	Note that a ``dead'' state is produced by the process as
--	implemented here.
make_deterministic :: Ord b => Nfa Int b -> Nfa Int b

--	First make a (nfa (set num)) using make_deter, and then
--	convert to a numeric dfa by means of the numbering functio
--	number.
make_deterministic = number . make_deter

-- | 'number' @m@ extracts a list of the states from @m@ (statelist),
--   and returns machine where each state is replaced by its position in the
--   statelist, given by the function change
number :: Ord b => Nfa (Set Int) b -> Nfa Int b

number (NFA states moves start finish)
  = NFA states' moves' start' finish'
    where
    statelist = Set.toList states
    lookup l a = look 0 l a
    look n [] a = error "lookup"
    look n (b:y) a
      | (b==a)      = n
      | otherwise   = look (n+1) y a
    change = lookup statelist
    states' = Set.map change states
    moves'  = Set.map newmove moves
              where
              newmove (Move s c t) = Move (change s) c (change t)
              newmove (Emove s t)  = Emove (change s) (change t)
    start' = change start
    finish' = Set.map change finish


-- | 'make_deter' calls the crucial function 'deterministic' on a machine and
--   its alphabet.
make_deter :: Ord b => Nfa Int b -> Nfa (Set Int) b

make_deter mach = deterministic mach (alphabet mach)

-- | 'deterministic' @mach alpha@ is the result of forming the dfa based on
-- sets of states of @mach@, using the alphabet @alpha@.
--
-- Calculated by taking the limit of the function 'addstep' which adds all
-- the states accessible by one transition on one of the characters of the
-- alphabet. The starting machine has one (start) state, the closure of the
-- start state of mach. Note that this may be terminal - test for this by
-- taking an intersection of this state set with the set term of terminal
-- states of mach.
deterministic :: Ord b => Nfa Int b -> [b] -> Nfa (Set Int) b

deterministic mach alpha
    = nfa_limit (addstep mach alpha) startmach
      where
      startmach = NFA
                  (singleton starter)
                  empty
                  starter
                  finish
      starter = closure mach (singleton start)
      finish
        | (term `intersection` starter) == empty     = empty
        | otherwise                           = singleton starter
      (NFA sts mvs start term) = mach

-- | 'addstep' adds all the new states which can be made by a single
-- transition on one of the characters of the alphabet.
addstep :: Ord b => Nfa Int b -> [b] -> Nfa (Set Int) b -> Nfa (Set Int) b

addstep mach alpha dfa
  = add_aux mach alpha dfa (Set.toList states)
    where
    (NFA states m s f) = dfa
    add_aux mach alpha dfa [] = dfa
    add_aux mach alpha dfa (st:rest)
        = add_aux mach alpha (addmoves mach st alpha dfa) rest

-- | 'addmoves' @mach x alpha dfa@ will add to @dfa@ all the moves from
--   state set @x@ over alphabet @alpha@.
--
--   Defined by iterating 'addmove'.
addmoves :: Ord b => Nfa Int b -> Set Int -> [b] -> Nfa (Set Int) b -> Nfa (Set Int) b

addmoves _ _ [] dfa    = dfa

addmoves mach x (c:r) dfa = addmoves mach x r (addmove mach x c dfa)

-- | 'addmove' @mach x c dfa@ will add to @dfa@ the moves from state set @x@ on
-- character @c@.
addmove :: Ord b => Nfa Int b -> Set Int -> b -> Nfa (Set Int) b -> Nfa (Set Int) b

addmove mach x c (NFA _states _moves _start _finish)
  = NFA states' moves' _start finish'
    where
    states' = _states `union` singleton new
    moves'  = _moves  `union` singleton (Move x c new)
    finish'
     | empty /= (term `intersection` new) = _finish `union` singleton new
     | otherwise                          = _finish
    new = onetrans mach c x
    (NFA _ _ _ term) = mach

-- | 'nfa_limit' finds the limit of an nfa transforming function. Just like
-- 'limit' except for the change of equality test.
nfa_limit :: (Eq a, Eq b) => (Nfa a b -> Nfa a b) -> Nfa a b -> Nfa a b
nfa_limit f n
  | n == next = n
  | otherwise = nfa_limit f next
                where
                next = f n

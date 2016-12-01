--------------------------------------------------------------------------
--									--
--	MinimiseDfa.hs							--
--									--
--	Minimising a DFA.						--
--									--
--	Regular expressions are defined in RegExp, and the type of	--
--	NFAs in NfaTypes. The implementation of sets used is in	--
--	Sets.								--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--									--
--------------------------------------------------------------------------

module Language.Mira.MinimiseDfa where

import qualified Data.Set as Set
import Data.Set ( Set, member )

import Language.Mira.NfaTypes

--------------------------------------------------------------------------
--									--
--	Minimising the nfa - uses the equivalence classes generated	--
--	by the function eqclasses. Replaces each state by the minimum	--
--	state equivalent to it. The set functions clear up repeats etc. --
--									--
--------------------------------------------------------------------------

minimise :: Ord a => Nfa a -> Nfa a
minimise mach@(NFA myStates _ _ _)
  | Set.null myStates = mach
minimise mach = replace mini mach
                where
                replace f (NFA myStates myMoves start finish)
                  = NFA states' moves' start' finish'
                    where
                    states' = Set.map f myStates
                    moves' = Set.map (\(Move a c b) -> Move (f a) c (f b)) myMoves
                    start' = f start
                    finish' = Set.map f finish
                mini a = Set.findMin (eqclass a)
                eqclass a = case [ b | b <- Set.toList classes , a `member` b ] of
                            []    -> error "minimise eqclass"
                            (x:_) -> x
                classes = eqivclasses mach

--------------------------------------------------------------------------
--									--
--	Partition takes a binary predicate, represented by a function	--
--	of type							--
--		a -> a -> Bool						--
--	assumed to represent an equivalence relation, and a (Set a),	--
--	and returns the set of the equivalence classes under the	--
--	relation.							--
--									--
--	Implemented using the function part which does the same	--
--	operation, except that it works over sets.			--
--									--
--------------------------------------------------------------------------

partition :: Ord a => (a -> a -> Bool) -> Set a -> Set (Set a)
partition (<=>) s = Set.fromList $ map Set.fromList $
                    part (<=>) (Set.toList s)

--------------------------------------------------------------------------
--									--
--	Partitions a list into a list of equivalence classes (lists)	--
--	by folding in the addtoclass function.				--
--									--
--------------------------------------------------------------------------

part :: (a -> a -> Bool) -> [a] -> [[a]]
part (<=>) = foldr (addtoclass (<=>)) []

--------------------------------------------------------------------------
--									--
--	addtoclass will add an element to the (first) equivalence	--
--	class to which the element belongs, creating a new class if	--
--	necessary.							--
--									--
--------------------------------------------------------------------------

addtoclass :: (a -> a -> Bool) -> a -> [[a]] -> [[a]]
addtoclass _     a []    = [[a]]
addtoclass (<=>) a (c:r)
  | a <=> head c = (a:c):r
  | otherwise    = c : addtoclass (<=>) a r

--------------------------------------------------------------------------
--									--
--	Given an nfa will return the set of sets of indistinguishable	--
--	states, from which the minimal DFA can be constructed.		--
--									--
--	This function simply strips off one half of the pair		--
--	returned by eqclasses.						--
--									--
--------------------------------------------------------------------------

eqivclasses :: Ord a => Nfa a -> Set (Set a)

eqivclasses = fst . eqclasses

--------------------------------------------------------------------------
--									--
--	eqclasses returns a pair, which consists of two		--
--	representations of the partition of the states into indistin-	--
--	guishable classes:						--
--		the set of classes, as sets				--
--		the boolean valued function representing the		--
--			relation.					--
--									--
--	These are found by iterating the function step which takes one	--
--	such pair to the next, where at the next stage we distinguish	--
--	previously indistinguishable states if and only if a transition --
--	from them on a particular character gives states distinguished	--
--	by the previous partition.					--
--	Can see from this that we generate the stages simply from the	--
--	function representation; we carry the set representation so	--
--	that we can compare two partitions for equality: can't compare	--
--	functions for equality, so compare the set representations.	--
--									--
--	set representations of the partitions are compared by the	--
--	function eqpart, which compares sets of sets for (true) set	--
--	equality.							--
--									--
--	The starting value for the iteration is given by the function	--
--	firstpartfun, which distinguishes the states in finish, i.e.	--
--	the terminal states, from the rest.				--
--									--
--------------------------------------------------------------------------

eqclasses :: Ord a => Nfa a -> ( Set (Set a) , a -> a -> Bool )
eqclasses mach = to_limit step start
          where

          start = ( firstpart , firstpartfun )

          firstpart = partition firstpartfun myStates

          firstpartfun a b = ( (a `member` finish) == (b `member` finish) )

          (NFA myStates myMoves _ finish) = mach

          step ( _ , partfun )
                  = ( newpart , newpartfun )
                    where
                    newpart = partition newpartfun myStates
                    newpartfun a b
                      = and [ partfun c d | (Move a' y c) <- movelist , a==a' ,
                                            (Move b' z d) <- movelist , b==b' ,
                                            y==z ]
                        && partfun a b
                    movelist = Set.toList myMoves

          to_limit f (a,b)
            | (eqpart a a') = (a,b)
            | otherwise = to_limit f next
              where
              next = f (a,b)
              (a',_) = next

          eqpart a a' = and ( Set.toList (Set.map (setmemSet a') a) ) &&
                        and ( Set.toList (Set.map (setmemSet a) a') )

          setmemSet x a = or ( Set.toList (Set.map (== a) x) )

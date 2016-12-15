--------------------------------------------------------------------------
--									--
--	RegExp.hs							--
--									--
--	A type of regular expressions.					--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--									--
--------------------------------------------------------------------------

module Language.Mira.RegExp where

data Reg b = Epsilon |
             Literal b |
             Or (Reg b) (Reg b) |
             And (Reg b) (Reg b) |
             Then (Reg b) (Reg b) |
             Star (Reg b) |
             Not (Reg b)
             deriving Eq

--------------------------------------------------------------------------
--	Definitions of ? and +						--
--------------------------------------------------------------------------

opt,plus :: Reg b -> Reg b

opt re = Or re Epsilon

plus re = Then (Star re) re

--------------------------------------------------------------------------
--	Expanding a character range into a regular expression.		--
--									--
--	range 'a' 'c'							--
--	  = Or (Literal 'a') (Or (Literal 'b') (Literal 'c'))		--
--------------------------------------------------------------------------

rangeChar :: Char -> Char -> Reg Char

rangeChar c1 c2
      = foldr1 Or (map Literal [c1 .. c2])

--------------------------------------------------------------------------
--	Examples							--
--------------------------------------------------------------------------

a, b :: Reg Char
a = Literal 'a'
b = Literal 'b'

rex1 = a `Or` (a `Then` b)
rex2 = (a `Then` b) `Or` (Epsilon `Or` (Star a))

regexp0 = Then b (Then (Star regexp2) a)
regexp1 = Then a (Then (Star regexp2) b)
regexp2 = Or (Then a b) (Then b a)

--------------------------------------------------------------------------
--	Which literals occur in a regular expression?			--
--------------------------------------------------------------------------

literals :: Reg b -> [b]

literals Epsilon      = []
literals (Literal ch) = [ch]
literals (Or r1 r2)   = literals r1 ++ literals r2
literals (And r1 r2)  = literals r1 ++ literals r2
literals (Then r1 r2) = literals r1 ++ literals r2
literals (Star r)     = literals r

--------------------------------------------------------------------------
--	Pretty printing a regular expression.				--
--									--
--	@ is used instead for the epsilon character.			--
--------------------------------------------------------------------------

instance Show b => Show (Reg b) where
  show = printRE

printRE :: Show b => Reg b -> String

printRE Epsilon = "@"
printRE (Literal ch) = show ch
printRE (Or r1 r2) = "(" ++ printRE r1 ++ "|" ++ printRE r2 ++ ")"
printRE (And r1 r2) = "(" ++ printRE r1 ++ "&" ++ printRE r2 ++ ")"
printRE (Then r1 r2) = "(" ++ printRE r1 ++ printRE r2 ++ ")"
printRE (Star r) = "(" ++ printRE r ++")*"
printRE (Not r) = "~(" ++ printRE r ++ ")"

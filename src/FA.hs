import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type Symbol = Char
type Symbol' = Maybe Char
type Symbols = Set Symbol
data FA a = DFA (Set a, Symbols, a -> Symbol -> a, a, Set a) |
            NFA (Set a, Symbols, a -> Symbol' -> (Set a), a, Set a)

accept (DFA (_,_,d,q0,f)) w = Set.member (d' q0 w) f 
  where d' = foldl d
accept (NFA (_,_,d,q0,f)) w = any (\e -> Set.member e f) (d' q0 w')
  where
    w' = map Just w
    unionMap f = Set.foldr' (Set.union . f) Set.empty
    e q = f $ iterate visit (Set.singleton q)
      where 
        visit s = Set.union s $ unionMap (\x -> d x Nothing) s
        f [] = Set.empty
        f (x:y:[]) = Set.empty
        f (x:y:xs) = if x==y then x else f (y:xs)
    d' q w = foldl t (e q) w
      where
        t a b = unionMap (\x -> unionMap e $ d x b) a
        --t = \a b -> (Set.foldr' (Set.union . e) Set.empty) . t'

reject m = not . (accept m)

language (DFA m) = filter (accept (DFA m)) s' 
  where
    DFA (_,s,_,_,_) = DFA m
    s' = [c:t | t <- "":s', c <- let s' = Set.toList s in s']
language (NFA m) = filter (accept (NFA m)) s' 
  where
    NFA (_,s,_,_,_) = NFA m
    s' = [c:t | t <- "":s', c <- let s' = Set.toList s in s']

d 0 (Just '0') = Set.singleton 0
d 0 (Just '1') = Set.singleton 1
d 0 Nothing = Set.empty
d 1 (Just '0') = Set.singleton 1
d 1 (Just '1') = Set.singleton 1
d 1 Nothing = Set.empty

m = NFA (Set.fromAscList [0, 1], Set.fromAscList ['0', '1'], d, 0, Set.fromAscList [1])

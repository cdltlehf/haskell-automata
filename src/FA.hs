import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type Symbol = Char
type Symbol' = Maybe Char
type Symbols = Set Symbol
data FA a = DFA (Set a, Symbols, a -> Symbol -> a, a, Set a) |
            NFA (Set a, Symbols, a -> Symbol' -> (Set a), a, Set a)

accepts (DFA (_,_,d,q0,f)) w = Set.member (d' q0 w) f 
  where d' = foldl d
accepts (NFA (_,_,d,q0,f)) w = any (\e -> Set.member e f) (d' q0 w')
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

reject m = not . (accepts m)

language (DFA m) = filter (accepts (DFA m)) s' 
  where
    DFA (_,s,_,_,_) = DFA m
    s' = [c:t | t <- "":s', c <- let s' = Set.toList s in s']
language (NFA m) = filter (accepts (NFA m)) s' 
  where
    NFA (_,s,_,_,_) = NFA m
    s' = [c:t | t <- "":s', c <- let s' = Set.toList s in s']

d 'a' (Just '0') = Set.singleton 'a'
d 'a' (Just '1') = Set.singleton 'a'
d 'a' Nothing = Set.fromAscList ['b', 'e']
d 'b' (Just '0') = Set.singleton 'c'
d 'b' (Just '1') = Set.empty
d 'b' Nothing = Set.empty
d 'c' (Just '0') = Set.empty
d 'c' (Just '1') = Set.singleton 'd'
d 'c' Nothing = Set.empty
d 'd' (Just '0') = Set.empty
d 'd' (Just '1') = Set.empty
d 'd' Nothing = Set.empty
d 'e' (Just '0') = Set.empty
d 'e' (Just '1') = Set.singleton 'f'
d 'e' Nothing = Set.empty
d 'f' (Just '0') = Set.singleton 'g'
d 'f' (Just '1') = Set.empty
d 'f' Nothing = Set.empty
d 'g' (Just '0') = Set.empty
d 'g' (Just '1') = Set.empty
d 'g' Nothing = Set.empty

m = NFA (Set.fromAscList ['a'..'g'], Set.fromAscList ['0', '1'], d, 'a', Set.fromAscList ['d','g'])

import Euterpea

{-|

        Chapter 3

        length :: [a] -> Integer
        length[] = 0
        length(x:xs) = 1 + length xs

        head :: [a] -> a
        head (x:_) = x -- ( "_" wildcard pattern. anything goes, binds no variables)

        tail :: [a] -> [a]
        tail (_:xs) = xs

        toAbsPitches :: [Pitch] -> [AbsPitch]
        toAbsPitches [] = []
        toAbsPitches(p:ps) = absPitch p : toAbsPitches ps

        map f [] = []
        map f (x:xs) = f x : map f xs

        toAbsPitches :: [Pitch] -> [AbsPitch]
        toAbsPitchs ps = map absPitch ps
        
-}

-- exercise 1
f1 :: Int -> [Pitch] -> [Pitch]
f1 ap [] = []
f1 ap ps = map (trans ap) ps


-- exercise 2
f2 :: [Dur] -> [Music a]
f2 [] = []
f2 ds = map rest ds


-- exercise 3
fn (Prim (Note d p )) = Prim (Note (d/2) p ) :+: rest (d/2)
 
f3 :: [Music Pitch] -> [Music Pitch]
f3 [] = []
f3 ns = map fn ns



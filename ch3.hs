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


{-|
        3.3

        (++) :: [a] -> [a] -> [a]
        [] ++ ys = ys
        (x:xs) ++ ys = x:(xs++ys)

        3.4
        line, chord :: [Music a] -> Music a
        line ms = foldr (:+:) ms
        chord ms = foldr (:=:) ms

        maxPitch1 :: [Pitch] -> Pitch
        maxPitch ps = foldr1 (!!!) ps


        3.6
        note qn p1 :+: note qn p2 :+: ... :+: note qn pn
        ==
        line (map (note qn ) [p1, p2, ..., pn]

        toAbsPitches = map absPitch
        toPitches = map pitch

-}

-- 3.2

-- 3.3  [Integer -> Integer]

-- 3.4


 
-- TODO TYPE -- can I use flip?
applyEach :: [(a -> b)] -> a -> [b]
applyEach functions val =
          let f5 val function = function val
          in map (f5 val ) functions 

applyEach2 :: [(a -> b)] -> a -> [b]
applyEach2 fns val = map ($ val) fns

-- 3.5
--applyAll :: [a -> b] -> a -> a
applyAll [] val = val
applyAll (f:functions) val = f (applyAll functions val)


-- 3.6 foldl ?

--3.7

length2 :: [a] -> Integer
length2 [] = 0
length2 xs = foldl (\ n _ -> n+1) 0 xs        

--3.8

doubleEach :: Num a => [a] -> [a]
doubleEach [] = []
doubleEach (x:xs) = x*2 : doubleEach xs

pairAndOne :: Num a => [a] -> [(a,a)]
pairAndOne xs = map (\ n -> (n, n+1)) xs
                       
addEachPair :: Num a => [(a,a)] -> [a]
addEachPair xs = map (\ (x,y) -> x + y ) xs

--  TODO: THIS DOES NOT WORK !!!! addPairsPointwise :: Num a => [(a,a)] => (a,a)
addPairsPointwise xs = foldl (\ (x,y) (a, b) -> (x+a, y+b)) (0,0) xs


-- 3.9 

fuse :: [Dur] -> [ Dur -> Music a ] -> [Music a]
fuse dns nts = if length dns /= length nts
                  then error "Prelude.fuse: length must match"
                  else let comb = zip nts dns
                       in map (\ (note, dur) -> note dur ) comb


--3.10 TODO: make recursive

getBiggerNum :: Ord a => a -> a -> a
getBiggerNum a b = if a > b
                   then a
                   else b

getSmallerNum :: Ord a => a -> a -> a
getSmallerNum a b = if a > b
                    then b
                    else a

maxAbsPitch :: (Ord a, Num a) => [a] -> a
maxAbsPitch [] = error "Prelude.maxAbsPitch: empty list"
maxAbsPitch ps = foldl getBiggerNum 0 ps

minAbsPitch :: (Ord a, Num a) => [a] -> a
minAbsPitch [] = error ""
minAbsPitch ps = foldl getSmallerNum (2^100) ps

--3.11  TODO: MAKE RECURSIVE

chrom :: Pitch -> Pitch -> Music Pitch
chrom p1 p2 = let ap1 = absPitch p1
                  ap2 = absPitch p2
                  absPitches = [ap1, ap1 + (signum $ ap2 - ap1) .. ap2 ]
                  comparePitches (p:pitcharray) ap = if p == pitch ap
                                                     then p:pitcharray
                                                     else (pitch ap):p:pitcharray
                  relevantPitches = foldl comparePitches [p1] absPitches
                  in foldr (\ val acc -> acc :+: note qn val) (rest 0) relevantPitches 

-- 3.12


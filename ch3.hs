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

addPairsPointwise :: Num a => [(a,a)] -> (a,a)
addPairsPointwise xs = foldl (\ (x,y) (a, b) -> (x+a, y+b)) (0,0) xs


-- 3.9 

fuse :: [Dur] -> [ Dur -> Music a ] -> [Music a]
fuse dns nts = if length dns /= length nts
                  then error "fuse: length must match"
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
maxAbsPitch [] = error "maxAbsPitch: empty list"
maxAbsPitch ps = foldl getBiggerNum 0 ps

minAbsPitch :: (Ord a, Num a) => [a] -> a
minAbsPitch [] = error "maxAbsPitch: empty list"
minAbsPitch ps = foldl getSmallerNum (2^100) ps

--3.11

chrom :: Pitch -> Pitch -> Music Pitch
chrom p1 p2 = let ap1 = absPitch p1
                  ap2 = absPitch p2
                  absPitches = [ap1, ap1 + (signum $ ap2 - ap1) .. ap2 ]
                  in foldl (\ acc val -> acc :+: note qn (pitch val)) (rest 0) absPitches

newPitch :: Pitch -> Pitch -> Pitch
newPitch p1 p2
  | absPitch p1 > absPitch p2 = trans (-1) p1
  | absPitch p1 < absPitch p2 = trans 1 p1
  | otherwise = p1

chrom2 :: Pitch -> Pitch -> Music Pitch
chrom2 p1 p2 
  | p1 == p2 = note qn p2
  | otherwise = note qn p1 :+: chrom2 (newPitch p1 p2) p2

            
-- 3.12

mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p [] = note qn p
mkScale p (interval:intervals) = note qn p :+: mkScale (trans interval p) intervals 


-- 3.13

data MajorScaleMode = Ionian
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Aeolian
    | Locrian

genScale :: Pitch -> MajorScaleMode -> Music Pitch
genScale p mode = 
  let intervals  = cycle [2, 2, 1, 2, 2, 2, 1]
      getIntervals n = take 7 $ drop n $ intervals
      getModeIntervals Ionian = getIntervals 0
      getModeIntervals Dorian = getIntervals 1
      getModeIntervals Phrygian = getIntervals 2
      getModeIntervals Lydian = getIntervals 3
      getModeIntervals Mixolydian = getIntervals 4
      getModeIntervals Aeolian = getIntervals 5
      getModeIntervals Locrian = getIntervals 6
  in mkScale p (getModeIntervals mode)


-- 3.14
fj =  
  let notes = [[c 3 qn, d 3 qn, e 3 qn, c 3 qn], [e 3 qn, f 3 qn, g 3 hn], [ g 3 en, a 3 en, g 3 en, f 3 en, e 3 qn, c 3 qn], [ c 3 qn, g 2 qn, c 3 hn]]
  in foldl (\ mel noteslist -> mel :+: times 2 (line noteslist) ) (rest 0) notes


-- 3.15 

encrypt :: String -> String
encrypt str = map (\s -> toEnum ((fromEnum s) + 1)) str

decrypt :: String -> String
decrypt str = map (\s -> toEnum ((fromEnum s) - 1)) str






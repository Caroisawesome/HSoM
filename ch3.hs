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





                             
                            
                       
        
         

import Euterpea

{-|
        1.4
        modifiy the definitions of hNote and hList so that they
        each take an extra argument that specifies the interval of harmonization.
        rewrite the definition of mel to take these changes into account 
-}

hNote   :: Dur -> Pitch -> Music Pitch
hNote d p i = note d p :=: note d (trans i p)

hList   :: Dur -> Int -> [Pitch] -> Music Pitch
hList d i [] = rest 0
hList d i (p:ps) = hNote d p i :+: hList d i ps

-- define p1, p2, p3 ?
mel :: Int -> Music Pitch
mel i = hList qn i [p1, p2, p3]

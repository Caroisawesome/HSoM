{-|

        Chapter 3

        higher-order functions: functions that take one or more functions as arguments or return a function as a result (can be placed in data structures)

        polymorphic types

        length :: [a] -> Integer
        length[] = 0
        length(x:xs) = 1 + length xs

        head :: [a] -> a
        head (x:_) = x -- "_" is a wildcard pattern. anything goes, binds no variables

        tail :: [a] -> [a]
        tal (_:xs) = xs

        toAbsPitches :: [Pitch] -> [AbsPitch]
        toAbsPitches [] = []
        toAbsPitches(p:ps) = absPitch p : toAbsPitches ps

        map f [] = []
        map f (x:xs) = f x : map f xs

        toAbsPitches :: [Pitch] -> [AbsPitch]
        toAbsPitchs ps = map absPitch ps

        
-}

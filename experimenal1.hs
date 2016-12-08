import Euterpea

{-|
        
                
         one = g
         two = a
         three = b 
         four = c 
         five = d
         six = e 
         seven = fs

         SteelDrums   
-}



mel1 =    let oct = 3
              chordy1 = chord $ map ($qn) [g oct, b oct, d oct]
              chordy2 = chord $ map ($qn) [a oct, b oct, d (oct+1)]
              chordy3 = chord $ map ($hn) [b oct, cs oct, fs (oct+2)]
              progression = chordy3 :+: chordy1 :+: chordy2
              melody1 = instrument Lead4Chiff progression
          in times 3 melody1 :+: chordy3
--          in  (phrase [Dyn $ Crescendo 2.0] $ addVolume 50 (times 3 melody1))
  --            :+: (phrase [Dyn $ Diminuendo 0.7] $ addVolume 100 (times 3 melody1))


bells1 = let fnn acc val
                  | val `mod` 4 == 0 = acc :+: times 3 (g 5 sn)
                  | val `mod` 3 == 0 = acc :+: b 5 en
                  | val `mod` 2 == 0 = acc :+: fs 5 sn
                  | val `mod` 5 == 0 = acc :+: a 5 qn
                  | val `mod` 7 == 0 = acc :+: rest sn :+: b 5 den 
                  | otherwise = acc :+: rest sn
          in instrument Agogo (foldl fnn (rest 0) [1..31])

    
song = mel1 :=: bells1 :=: times 4 (instrument Helicopter (d 2 wn))
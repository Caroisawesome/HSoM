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



melody1 = let oct = 3
              chordy1 = chord $ map ($qn) [g oct, b oct, d oct]
              chordy2 = chord $ map ($qn) [a oct, b oct, d (oct+1)]
              chordy3 = chord $ map ($hn) [b oct, cs oct, fs (oct+2)]
              progression = chordy3 :+: chordy1 :+: chordy2
              melody1 = instrument Lead2Sawtooth progression
          in  (phrase [Dyn $ Crescendo 2.0] $ addVolume 50 (times 3 melody1))
              :+: (phrase [Dyn $ Diminuendo 0.7] $ addVolume 100 (times 3 melody1))


-- play melody1

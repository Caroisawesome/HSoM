{-|
  	CH 4 NOTES

  	timesM 	:: Int -> Music a -> Music a
  	timesM 0 m 	= rest 0
  	timesM n m 	= m :+: timesM (n - 1) m

  	addDur 	:: Dur -> [Dur -> Music a] -> Music a
  	addDur d ns = let f n = n d
  					in line (map f ns)

	graceNote	:: Int -> Music Pitch -> Music Pitch
	graceNote n ( Prim (Note d p)) = 
		note (d/8) (trans n p) :+: note (7 * d/8) p
	graceNote n _  =
		error "Can only add a grace note to a note"

		-- graceNote (-1) (d 4 qn)

	b1 = addDur dqn [b 3, fs 4, g 4, fs 4]
	bassLine = timesM 3 b1 :+: timsM 2 b2 ...

	4.1
-}

import Euterpea

addDur 	:: Dur -> [Dur -> Music a] -> Music a
addDur d ns = let f n = n d
				in line (map f ns)

mt1 = addDur hn [c 4, g 4]
mt2 = (addDur dsn [f 4, e 4, d 4]) :+: c 5 hn :+: g 4 qn
mt3 = (addDur dsn [ f 4, e 4, f 4]) :+: d 4 hn :+: g 3 qn :+: g 3 en
mainmel1 = times 2 (mt1 :+: times 2 mt2 :+: mt3)


-- 4.2

prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x:xs) = let f pf = x:pf
				  in [x]:map f (prefixes xs)

prefix :: [Music a] -> Music a
prefix mels = m :+: transpose 5 m :+: m
  where m1 = line (concat (prefixes mels))
        m2 = transpose 12 (line (concat (prefixes (reverse mels))))
        m  = instrument Flute m1 :=: instrument VoiceOohs m2

mel1 = [ c 5 hn, d 5 sn, e 5 en, f 5 qn, g 5 hn, a 5 sn, b 5 en, c 6 qn]
mel2 = [ rest qn, c 4 sn, rest qn, d 4 sn, e 4 qn, f 4 qn, rest en, g 4 hn, a 4 sn, rest sn]


-- 4.3 

prefix2 :: [Music a] -> Music a
prefix2 mels =  m :+: tempo 2 (transpose 5 m :=: transpose 3 m) :+: m
  where m1 = line (concat (prefixes mels))
        m2 = transpose 12 (line (concat (prefixes (reverse mels))))
        m  = instrument Timpani m1 :=: transpose 4 (instrument TinkleBell m1) :=: instrument TaikoDrum m2 


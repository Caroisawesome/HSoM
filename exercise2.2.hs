import Euterpea

data BluesPitchClass = BluesRoot | BluesMinorThird | BluesFourth | BluesFifth | BluesMinorSeventh

type BluesPitch = (BluesPitchClass, Octave)

ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
ro o d = note d (BluesRoot, o)
mt o d = note d (BluesMinorThird, o)
fo o d = note d (BluesFourth, o)
fi o d = note d (BluesFifth, o)
ms o d = note d (BluesMinorSeventh, o)

fromBlues :: Music BluesPitch -> Music Pitch
fromBlues (Prim (Note d (BluesRoot, o)))         = Prim (Note d (C, o))
fromBlues (Prim (Note d (BluesMinorThird, o)))   = Prim (Note d (Ef, o))
fromBlues (Prim (Note d (BluesFourth, o)))       = Prim (Note d (F, o))
fromBlues (Prim (Note d (BluesFifth, o)))        = Prim (Note d (G, o))
fromBlues (Prim (Note d (BluesMinorSeventh, o))) = Prim (Note d (Bf, o))
fromBlues (Prim (Rest d)) = Prim (Rest d)
fromBlues (m1 :+: m2) = fromBlues m1 :+: fromBlues m2
fromBlues (m1 :=: m2) = fromBlues m1 :=: fromBlues m2
fromBlues (Modify cntrl m) = Modify cntrl (fromBlues m)

melodyA :: Music BluesPitch
melodyA = ro 3 qn :+: mt 3 hn :+: fo 3 qn :+: fi 3 hn :+: ms 3 wn

melodyB :: Music Pitch
melodyB = let chordA = transpose (3) (fi 3 qn) :=: fi 1 qn :=: fi 2 qn
              chordB = ro 3 qn :=: ro 1 qn :=: ro 2 qn
              chordC = mt 3 qn :=: mt 1 qn :=: mt 2 qn
          in fromBlues (chordA :+: chordB :+: chordC)
             :+: qnr :+:
             fromBlues (chordC :+: chordA :+: chordC :+: chordB :+: chordC :+: chordA :+: chordC :+: chordB)

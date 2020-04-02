module Signal.Notes where

pitch :: Int -> Double -> Double
pitch n frequency = frequency * 2 ** (fromIntegral n / 12)

c, dflat, d, eflat, e, f, fsharp, g, aflat, a, bflat, b :: Double
c', dflat', d', eflat', e', f', fsharp', g', aflat', a', bflat', b' :: Double
c'', dflat'', d'', eflat'', e'', f'', fsharp'', g'', aflat'', a'', bflat'', b'' :: Double
c''', dflat''', d''', eflat''', e''', f''', fsharp''', g''', aflat''', a''', bflat''', b''' :: Double
c         = pitch (- 45) 440
dflat     = pitch  1 c
d         = pitch  2 c
eflat     = pitch  3 c
e         = pitch  4 c
f         = pitch  5 c
fsharp    = pitch  6 c
g         = pitch  7 c
aflat     = pitch  8 c
a         = pitch  9 c
bflat     = pitch 10 c
b         = pitch 11 c
c'        = pitch 12 c
dflat'    = pitch 13 c
d'        = pitch 14 c
eflat'    = pitch 15 c
e'        = pitch 16 c
f'        = pitch 17 c
fsharp'   = pitch 18 c
g'        = pitch 19 c
aflat'    = pitch 20 c
a'        = pitch 21 c
bflat'    = pitch 22 c
b'        = pitch 23 c
c''       = pitch 24 c
csharp''  = dflat''
dflat''   = pitch 25 c
d''       = pitch 26 c
eflat''   = pitch 27 c
e''       = pitch 28 c
f''       = pitch 29 c
fsharp''  = pitch 30 c
gflat''   = fsharp''
g''       = pitch 31 c
aflat''   = pitch 32 c
a''       = pitch 33 c
bflat''   = pitch 34 c
b''       = pitch 35 c
c'''      = pitch 36 c
csharp''' = dflat'''
dflat'''  = pitch 37 c
d'''      = pitch 38 c
eflat'''  = pitch 39 c
e'''      = pitch 40 c
f'''      = pitch 41 c
fsharp''' = pitch 42 c
g'''      = pitch 43 c
aflat'''  = pitch 44 c
a'''      = pitch 45 c
bflat'''  = pitch 46 c
b'''      = pitch 47 c

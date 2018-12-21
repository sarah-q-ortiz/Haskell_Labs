module RasterGraphics where
import RenderBMP

---------------

type Space  = (Double,Double)
type Color  = Int
type Pict   = Space -> Color
type Figure = Space -> Maybe Color
[black, white, red, orange, yellow, green, blue, purple] =
	[0x000000, 0xffffff, 0xff0000, 0xff9900, 0xffff00, 0x00ff00, 0x0000ff, 0x6600cc] :: [Int]

---------------

blank = \(x,y) -> Nothing
when b y = if b then Just y else Nothing
f `orr` b = \x -> case f x of Nothing -> b   ; Just y -> y
f `on`  g = \x -> case f x of Nothing -> g x ; Just y -> Just y
layer figs = foldl on blank figs

---------------

range n = [-m..m] where m = (n-1) / 2
space n m = map (\y -> (map (\x -> (x,y)) (range n))) (reverse (range m))
render fig w h = map (map (scale mag mag (fig `orr` white))) (space w h)
try p = test (round x) (round y) (concat (render p x y))
        where size = base*mag
              x = 2*size-1
              y =   size-1
base = 50.0
mag  =  8.0		-- use (30,3) for Hugs, (50,8) for GHCi (on my Mac)

------transformations---------

dist x y = sqrt (x^2 + y^2)
shift n m f = \(x,y) -> f (x-n, y-m)
scale n m f = \(x,y) -> f (x/n, y/m)
skew  n m f = \(x,y) -> f (x-(n*y/m), y)
swap (x,y) = (y,x)
diag f = f . swap
flipv f = \(x,y) -> f (x,-y)
fliph f = \(x,y) -> f (-x,y)

-------pattern--------

within x n = -m<=x && x<=m where m = (n-1)/2
rect w h c = \(x,y) -> when (x `within` w && y `within` h) c
circ r c   = \(x,y) -> when (dist x y < r) c
square r c = rect (2*r) (2*r) c
radial  c = \(x,y) -> when (p x y) c
            where p x y = if y==0 then True else even (round (x / y))
stripe k c = skew k k (\(x,y) -> when (round x `mod` round k == 0) c)
checker c = \(x,y) -> when (even (round x + round y)) c
--vstrip  c = \(x,y) -> when (|x| <= 1/2) c
---------------

squirc n = circ n yellow `on` square n blue

tree   = shift 0 3 (circ 9 green) `on` shift 0 (-8) (rect 5 11 black)
target = layer (zipWith circ [1,3..13] (cycle [red,white]))
wagon  = layer [shift 0 5 (rect 25 5 red), shift (-6) 0 wheel, shift 6 0 wheel]
         where wheel = circ 4 black
mickey = layer [circ 9 black, shift 8 8 ear, shift (-8) 8 ear]
         where ear = circ 5 black
dizzy  = skew 2 3 (mickey `on` radial orange `on` checker yellow)
scene  = tree `on` sun `on` field
         where sun   = shift 17 10   (circ 5 yellow)
               field = shift 0 (-13) (\(x,y) -> when (y `within` 3) green)
scene2 = shift (-18) (-10) (scale 0.75 0.75 wagon) `on` scene
galaxy = skew 3 3 (target `on` scale 2 2 (checker green))
stripy = scale 2 1 (stripe 4 blue)
messy  = skew 2 3 (layer [scale (-2) 2 (stripe 5 purple), target, stripy])

simple = (circ 5 red `on` shift 4 0 (rect 10 20 blue)) `on` radial yellow



---- my new stuff ----
--translations--
rotate  theta  f = \(x,y) -> (x*cos(theta) - y*sin(theta), y*cos(theta) + x*sin(theta))

-------myshapes-----
sinwavef y a x = -m <= y && y <= m 
		where m = a*sin(x)
sinwave a  c = \(x,y) -> when ( sinwavef y a x) c

coswavef y a x = -m <= y && y <= m 
		where m = a*cos(x)
coswave a c =  \(x,y) -> when (coswavef y a x) c

tanwavef y a  x = -m <= y && y <= m 
		where m = a*tan(x)
tanwave a c = \(x,y) -> when (tanwavef y a x) c

topsinwave a c = \(x,y) -> when (topsinwavef y a x) c
topsinwavef y a x = -m <= y && -y <= m 
		where m = a*sin(x)

botsinwavef y a x = -m <= -y && y <= m  
		where m = a*sin(x)
botsinwave a c = \(x,y) -> when (botsinwavef y a x) c
vsinwavef  y a x = -m <= x && x <= m 
		where m = a*sin(y)
vsinwave a c = \(x,y) -> when (vsinwavef y a x) c

vcoswavef y a x = -m <= x && x <= m 
		where m = a*cos(y)

vcoswave a c = \(x,y) -> when (vcoswavef y a x) c 

-----my transformations -----

enlarge n f = \(x,y) -> f (x*n,y*n)

newtrans0  n f = \(x,y) -> f (x* cos(n), y * sin(n))

newtrans1 n f = \(x,y) -> f (n*cos(x), n*sin(y))

newtrans2 n f = \(x,y) -> f (x /cos(n), y / sin(n))

--trials--
trialsinwave:: (Double, Double) -> Maybe Int 
trialsinwave  = shift 10 10 (sinwave  10  green)
trialtopsinwave:: (Double, Double) -> Maybe Int 
trialtopsinwave  = shift 10 10 (topsinwave  10  red)
trialbotsinwave:: (Double, Double) -> Maybe Int
trialbotsinwave = shift 10 10 (botsinwave 10 blue)
trialcoswave:: (Double, Double) -> Maybe Int
trialcoswave = shift 10 10 (coswave 10 blue)
trialtanwave:: (Double, Double) -> Maybe Int
trialtanwave =  shift 10 10 (tanwave 10 purple)
trialvsinwave:: (Double, Double) -> Maybe Int
trialvsinwave = shift 10 10 (vsinwave 10 yellow)
trialvcoswave:: (Double, Double) -> Maybe Int
trialvcoswave = shift 10 10 (vcoswave 10 orange)
testcirc = shift 0 3 (circ 9 blue)
trialcombo = trialcoswave `on` trialsinwave
trialvcombo = trialvcoswave `on` trialvsinwave
trialboth = trialcombo `on` trialvcombo
trialsincombo = trialtopsinwave `on` trialbotsinwave
enlargetest = enlarge 4 testcirc
newtrans0test = newtrans0 pi testcirc
newtrans1test = newtrans1 0.05 testcirc
newtrans2test = newtrans2 pi testcirc
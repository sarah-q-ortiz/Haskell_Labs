same3 :: Integer -> Integer -> Integer -> Bool
same3 a b c = (a==b) && (b==c)
poly :: Integer-> Integer
poly x = - 2 + 3*x + 5*x^2
nor :: Bool -> Bool -> Bool
nor a b = (a==False) && (b== False)
echo :: Int -> String -> String
echo n s = let list = replicate n s in concat list
reverb :: Int -> String -> String
reverb n s = let list = map (replicate n) s in concat list
--twice ::
twice f x = f (f x)
twice' f = (f.f)
-- #7
--nice :: Integer -> Integer
nice  f = twice'  twice' f
--slice ::  Integer-> Integer
slice f =  twice'  (twice' twice' f  )
--dice :: Integer-> Integer
dice f  =  (twice'  twice' ) twice' f
foo n = take 10 n
bar n = drop 20 n

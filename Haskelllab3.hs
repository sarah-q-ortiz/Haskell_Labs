--prob 1 done
lcp :: Eq a => [a] -> [a] -> [a]
lcp [] _=[]
lcp _ [] = []
lcp (x:xs) (y:ys) = if x==y then x: lcp xs ys else []

--prob 2 done
shortest [y]= y
shortest (x:y:xs)
      | length x > length y = shortest (y:xs)
      | otherwise = shortest (x:xs)
lcps ::[[Char]] -> [Char]
lcps []=[]
lcps (_:[])= []
lcps (x:y:xs) = shortest (words (lcp x y ++ " " ++ lcps (y:xs)))

--prob 3 done
lcs :: Eq a => [a] -> [a] -> [a]
lcs xs ys = reverse (lcp (reverse xs) (reverse ys))

--prob 4 done
lcss ::[[Char]] -> [Char]
lcss []=[]
lcss (_:[])= []
lcss (x:y:xs) =  shortest(words(lcs x y ++ " " ++ lcss (y:xs)))

--prob 5 done
evens :: [a]->[a]
evens [] = []
evens xs =  [x | (x,y) <- zip xs [0..], even y]

--prob 6 done
while :: (a -> Bool) -> (a -> a) -> a -> a
while p f = go
  where
     go x | p x = go(f x)
          | otherwise = x

--prob 7 done
during :: (a -> Bool) -> (a -> a) -> a -> [a]
during p f = go
  where
    go x | p x = x: go(f x)
         | otherwise = []

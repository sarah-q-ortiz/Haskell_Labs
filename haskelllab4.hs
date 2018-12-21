import Data.Maybe
data BTree a = Tip | BNode (a) (BTree a) (BTree a) deriving (Show, Ord, Eq)
data Rose a = Branch a [Rose a] deriving Show
leaf x = BNode x Tip Tip
samplebin = BNode 'A' (BNode 'B' (leaf 'C')
                                 (leaf 'D'))
                      (BNode 'E' (BNode 'F' (leaf 'G') Tip)
                                 (BNode 'H' (BNode 'I' Tip (leaf 'J'))
                                 (leaf 'K')))
samplebst = BNode 37 (BNode 25 (leaf 12) (BNode 28 Tip (leaf 32)))
                     (BNode 51 (BNode 42 (leaf 39) (BNode 47 (leaf 43) (leaf 49)))
                               (BNode 69 Tip (BNode 81 (BNode 72 (leaf 71) Tip) Tip)))
sampler = Branch "Amarantha"
            [ Branch "Bethesda" [Branch "Edna" []],
              Branch "Callipygia" [ Branch "Flegma" [Branch "Indicia"[],
                                                    Branch "Jaundice" [],
                                                    Branch "Kalahari" []
                                                    ],
                                   Branch "Ganache" []
                                   ],
              Branch "Depilitoria" [Branch "Hildegarde" [Branch "Ludmilla" [],
                                                         Branch "Miasma" []]
                                   ]
              ]
--problem 1 done
postord Tip  =  []
postord (BNode node left right)   = postord left ++ postord right ++ [node]
--problem 2 done
inord Tip = []
inord (BNode node left right) = inord left ++ [node] ++ inord right
mint Tip = Nothing
mint (BNode node left right) = Just x
                                  where
                                   x = head (inord (BNode node left right))
maxt Tip = Nothing
maxt (BNode node left right) =  Just z
                                   where
                                      search = inord (BNode node left right)
                                    
                                      z = last search
--problem 3 done
data Dir = L | R deriving Show
push f Nothing = Nothing
push f (Just x) = Just (f x)
path v Tip = Nothing
path v (BNode node left right) | v == node = Just []
                               | v > node = push (R:) (path v right)
                               | v < node = push (L:) (path v left)

-- problem 4 done
rsize :: Rose a -> Int
rsize (Branch n ts) = 1 +  sum (map rsize ts)

rheight :: Rose a -> Int
rheight (Branch n []) = 1
rheight (Branch n ts) =  1 + maximum (map rheight ts)

rmap :: (a -> b) -> Rose a -> Rose b
rmap f (Branch n rs) = Branch (f n)  (map (rmap f) rs)

rzip :: Rose a -> Rose b -> Rose (a,b)
rzip (Branch n rs) (Branch tn trs) =  Branch (n,tn) (zipWith rzip rs trs)

rmirror  :: Rose a -> Rose a
rmirror (Branch node rose) = Branch node (map rmirror(reverse rose))

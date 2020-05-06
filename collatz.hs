data Poly a = Coef [a]  deriving (Show,Ord,Eq,Read)

class Numseq a where
  (+++) :: a -> a ->  a

instance Numseq (Poly a) where
  (Coef x) +++ (Coef y) = (Coef (x++y))

instance (Num a, Eq a) => Num (Poly a) where
  (Coef x) + (Coef [])          = (Coef x)
  (Coef []) + (Coef x)          = (Coef x)
  (Coef (x:xs)) + (Coef (y:ys)) = reduce ((Coef [x + y]) +++ ((Coef xs) + (Coef ys)))
  (Coef x) * (Coef [])          = (Coef [0])
  (Coef []) * (Coef x)          = (Coef [0])
  (Coef [g]) * (Coef [h])       = (Coef [g*h])
  (Coef x) * (Coef [0,1])       = (Coef (0:x))
  (Coef [0,1]) * (Coef x)       = (Coef (0:x))
  (Coef (a:as)) * (Coef (b:bs)) = reduce (f + i + o + l)
    where
      f = (Coef [a*b])
      i = x * ((Coef [b]) * (Coef as))
      o = x * ((Coef [a]) * (Coef bs))
      l = x * (x * ((Coef as) * (Coef bs)))
      x = Coef [0,1]
  fromInteger i   = (Coef [fromInteger i])
  abs (Coef x)    = (Coef [abs (foldr (+) 0 (zipWith (*) x x))])
  signum (Coef x) = (Coef [signum (head x)])
  negate (Coef x) = Coef (map negate x)

reduce :: (Num a, Eq a) => Poly a -> Poly a
reduce (Coef [])  = Coef []
reduce (Coef [x]) = Coef [x]
reduce (Coef p)   = if (head (reverse p)) == 0
  then reduce (Coef (reverse (tail (reverse p))))
  else Coef p

instance (Fractional a, Eq a) => Fractional (Poly a) where
  fromRational x   = (Coef [fromRational x])
  recip (Coef [x]) = (Coef [recip x])

eval :: (Num a, Eq a) => a -> Poly a -> a
eval x (Coef [y])   = y
eval x (Coef [z,y]) = y*x + z
eval x (Coef p)     = (head p) + x * ((eval x) (Coef (tail p)))

gen_collatz :: (Fractional a, Eq a) => [a] -> (Poly a) -> (Poly a)
gen_collatz [0] (Coef [a,b]) = (Coef [(recip 2) * a, (recip 2) * b])
gen_collatz [1] (Coef [a,b]) = (Coef [3 * a + 1, 3 * b])  
gen_collatz (0:l) p          = (gen_collatz l) ((recip 2) * p)
gen_collatz (1:l) p          = (gen_collatz l) (3 * p + 1)

collatz :: (Fractional a, Eq a) => [a] -> (Poly a)
collatz [0]   = gen_collatz [0] (Coef [0,1])
collatz [1]   = gen_collatz [1] (Coef [0,1])
collatz (0:l) = (gen_collatz l) (gen_collatz [0] (Coef [0,1]))
collatz (1:l) = (gen_collatz l) (gen_collatz [1] (Coef [0,1]))

collatz_seq :: (Fractional a, Eq a) => a -> [a] -> [a]
collatz_seq n [0]   = [(eval n) (collatz [0])]
collatz_seq n [1]   = [(eval n) (collatz [1])]
collatz_seq n (0:l) = [(eval n) (collatz [0])] ++ (collatz_seq ((eval n) (collatz [0])) l)
collatz_seq n (1:l) = [(eval n) (collatz [1])] ++ (collatz_seq ((eval n) (collatz [1])) l)

prinpoly :: (Show a) => Int -> (Poly a) -> String
prinpoly 0 (Coef [x])    = show x
prinpoly n (Coef [x])    = (show x) ++ ("x^" ++ (show n))
prinpoly 0 (Coef (p:ps)) = (show p) ++ " + " ++ (prinpoly 1 (Coef ps))
prinpoly n (Coef (p:ps)) = (show p) ++ ("x^" ++ (show n)) ++ " + " ++ (prinpoly (n+1) (Coef ps))

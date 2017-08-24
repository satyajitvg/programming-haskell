import Data.Char
import Data.List

{- Higher order functions -}

map' :: (a -> b) -> [a] -> [b]
map' f [] = []    
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) 
  | p x = x : filter' p xs
  | otherwise = filter' p xs

{- foldr -}

-- foldr can be defined using recursion
-- a is an element in [a] and b is an accumulator
foldr' :: (a->b->b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr f v xs)

{-  Best to think of foldr in a non-recursive manner
    each cons operator in a list is replaced by f
    and the empty list is at the end by the value v -}

-- sum defined by foldr
sum' :: Num a => [a] -> a
--sum' = foldr (+) 0
sum' = foldr (\x acc -> x + acc) 0

-- length defined using foldr
length' :: [a] -> Int
length' = foldr (\_ n -> n + 1) 0

-- revesre using foldr
-- snoc is cons backwards !
-- adds an element to the end of a list
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse' :: [a] -> [a]
--reverse' = foldr snoc []
reverse' = foldr (\x acc -> acc ++ [x]) []

{- foldl -}
sum'' :: Num a => [a] -> a
sum'' = suml 0
    where 
        suml v [] = v
        suml v (x:xs) = suml (v+x) xs

{- map expressed as foldr -}
map'' :: (a->b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x : acc) []

{- filter expressed as foldr -}
filter'' :: (a->Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x:acc else acc) []

{- Binary string transmitter -}

type Bit = Int


{- assume binary numbers are written in reverse -}

bin2int' :: [Bit] -> Int
bin2int' bits = sum [ w*bit | (w, bit) <- zip weights bits ]
    where
        weights = iterate (*2) 1

{- bin2int as foldr -}
{- 1*a + 2*b + 4*c + 8*d = 1*a + 2*(b + 2*(c + 2*(d + 0))) -}
bin2int :: [Bit] -> Int
bin2int = foldr (\x acc -> x + 2*acc) 0 

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2: int2bin (n `div` 2)

{- truncate binary numbers to 8 bits -}
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

{- Transmission -}
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin. ord)

{- chop bit string into 8 bit chunks -}

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 xs = take 8 xs : chop8 (drop 8 xs)

{- Decode -}
decode :: [Bit] -> String
decode  =   map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

-- perfect channel
channel :: [Bit] -> [Bit]
channel = id

{- voting algorithms -}

count :: Eq a => a -> [a] -> Int
count x = length . filter(== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter(/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

{- alternative voting system -}

rmempty :: Eq a => [[a]] -> [[a]]
rmempty  = filter (/= []) 

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head 

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
               [c] -> c
               (c:cs) -> winner' (elim c bs)

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

{- add parity bit -}
parity :: [Bit] -> Bit
parity bits =  head bits 

addParity :: [Bit] -> [Bit]
addParity bits = parity bits : bits

{- Transmission -}
encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin. ord)

{- chop bit string into 8 bit chunks -}

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 xs = take 8 xs : chop8 (drop 8 xs)

-- chop with parity 
chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 xs = take 9 xs : chop9 (drop 9 xs)

checkError :: [Bit] -> [Bit]
checkError bits
  | head bits == parity (drop 1 bits) = drop 1 bits
  | otherwise = error "Error in parity"

{- Decode -}
decode :: [Bit] -> String
decode  =   map (chr . bin2int . checkError)  . chop9

transmit :: String -> String
transmit = decode . channel . encode

-- perfect channel
channel :: [Bit] -> [Bit]
channel = id

-- faulty channel
faultyChannel :: [Bit] -> [Bit]
faultyChannel = drop 1

faultyTransmit :: String -> String
faultyTransmit = decode . faultyChannel . encode

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

{- Exercises -}

-- list comprehension as map & filter
lcomp :: [a] -> (a -> b) -> (a -> Bool) -> [b]
lcomp xs f p = map f (filter p  xs)

-- all elements in list satisfy a predicate
all' :: (a -> Bool) -> [a] -> Bool
all' p  = foldr (\x acc -> p x && acc) True

-- all redux
all'' :: (a -> Bool) -> [a] -> Bool
all'' p = and . map p

-- any element in list satisfies a predicate
any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

-- select elements while they satisfy a predicate
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) 
  | p x = x : takeWhile' p xs
  | otherwise =  []

-- remove elements from a list while they satisfy a predicate
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
  | p x = dropWhile' p xs
  | otherwise = x:xs

-- dec to int using foldl
dec2int :: [Int] -> Int
dec2int dec = foldl (\acc (digit, power) -> acc + (digit*power)) 0 digitPowers
    where
        powers = reverse [10^x | x <-[0..length dec -1]]
        digitPowers = zip dec powers

-- much simpler dec2int !!
dec2int' :: [Int] -> Int
dec2int' = foldl (\acc x -> 10*acc + x) 0

-- curry
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x,y)

-- uncurry
uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (x,y) = f x y

{- unfold -}
unfold p h t x 
  | p x = []
  | otherwise = h x : unfold p h t (t x)

{- unfold can be usd to write int2bin 
    int2bin :: Int -> [Bit]
    int2bin 0 = []
    int2bin n = n `mod` 2 : int2bin (n `div` 2)
    -}
int2bin' :: Int -> [Bit]
int2bin' = unfold (==0) (`mod` 2) (`div` 2) 


{-
    chop8 :: [Bit] -> [[Bit]]
    chop8 [] = []
    chop8 xs = take 8 xs : chop8 (drop 8 xs) 
    -}

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold (==[]) (take 8) (drop 8)

-- map f using unfold
mapF :: Eq a => (a -> b) -> [a] -> [b]
mapF f = unfold (==[]) (f . head) ( tail )

-- iterate f using unfold
iterateF :: (a -> a) -> a -> [a]
iterateF f  = unfold (const False) id f

-- alternately apply two functions to sucessive elements of list
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:[]) = [f x]
altMap f g (x: x':xs) = f x : g x' : altMap f g xs

-- luhns check for credit card

luhnDouble :: Int -> Int
luhnDouble x  = case (2*x > 9) of
                  True -> 2*x - 9
                  False -> 2*x

luhn :: [Int] -> Bool
luhn ns = (sum (altMap id luhnDouble (reverse ns)) `mod` 10) == 0



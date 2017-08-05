{- Recursion -}

-- factorial
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

-- multiplication is recursive addition
(***) :: Int -> Int -> Int
m *** 0 = 0
m *** n = m + (m *** (n-1))

-- product
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product xs

-- length
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- list concatenation 
(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)

-- insert into a sorted list
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

-- insertion sort
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

-- drop
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (x:xs) =  drop'(n-1) xs

-- init
init' ::  [a] -> [a]
init' (_:[]) = []
init' (x:xs) = x : init' xs

{- Exercises -}

-- sumdown
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- exponentiation is repeated multiplication
(^^^) :: Int -> Int -> Int
m ^^^ 0 = 1
m ^^^ n = m * (m ^^^ (n-1))

-- euclids gcd
gcd' :: Int -> Int -> Int
gcd' x y 
  | x == y = x
  | x > y = gcd' y (x-y)
  | y > x = gcd' x (y-x)

-- check all logical values are true
and' :: [Bool] -> Bool
and' (x:[]) = x
and' (x:xs) = x && and' xs

-- concatenate a list of lists
concat' :: [[a]] -> [a]
concat' (xs:[]) = xs
concat' (xs:xss) = xs ++ concat' xss

-- produce a list with n identical elements
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- select nth element from a list
(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

-- decide if a value is an element in the list
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) 
  | e == x = True
  | otherwise = elem' e xs

-- merge two sorted lists
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | x > y = y : merge (x:xs) ys

-- halve 
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
    where
        n = length xs `div` 2

-- merge sort
msort ::  Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort xs = merge (msort firstHalf) (msort secondHalf)
    where 
        halves = halve xs
        firstHalf = fst halves
        secondHalf = snd halves


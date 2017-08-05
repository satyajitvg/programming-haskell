import Data.Char

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

pairs xs = zip xs (tail xs)
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs(xs)]

-- ceaser cipher

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr ( n + ord 'a')

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <-xs]

-- cracking
 
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

count :: Eq a => [a] -> a -> Int
count xs x = sum [ 1 | x' <- xs, x' == x]

freqs :: String -> [Float]
freqs xs = [ percent (count xs x) n | x <- ['a'..'z']]
           where 
            lowers = [ x | x <- xs, isLower x]
            n = length lowers

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ ((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ i | (x',i) <- zip xs [0..], x' == x ]
 
-- freq table for a-z in english
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0,
         6.1, 7.0, 0.2, 0.8,  4.0, 2.4, 6.7,
         7.5, 1.9, 0.1, 6.0,  6.3, 9.1, 2.8,
         1.0, 2.4, 0.2, 2.0,  0.1]

crack :: String -> String
crack xs = encode (-factor) xs
    where
        factor = head (positions (minimum chitabs) chitabs)
        chitabs = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs

-- pythagorean numbers
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <-[1..n], x^2 + y^2 == z^2]


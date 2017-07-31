double x  = x + x
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns
my_sum [] = 0
my_sum (x : xs) = x + my_sum xs

qsort [] = []
qsort (x : xs) = qsort larger ++ [x] ++ qsort smaller
    where
        smaller = [ a | a <- xs, a <= x]
        larger = [ b | b <-xs, b > x]

abs' :: Int -> Int
abs' n | n >= 0 = n
       | otherwise = -n

signum' n | n < 0 = -1
          | n == 0 = 0
          | otherwise = 1

halve xs = (take n xs, drop n xs)
            where 
                n = length xs `div` 2

third' (x0 : x1 : x2 : xs) = x2

safetail xs | null xs = xs
            | otherwise = tail xs

safetail' [] = []
safetail' (x : xs) = xs

($$) :: Bool -> Bool -> Bool
True $$ b = b
False $$ _ = False

luhnDouble x | x' > 9 = x'-9
             | otherwise =  x'
             where
                x' = 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d | s `mod` 10 == 0 = True
             | otherwise = False
             where
                s = d + luhnDouble c + b + luhnDouble a



import Data.Complex (Complex, realPart)

-- what is n factorial?
fact :: Integer -> Integer 
fact 0 = 1 
fact n = n * fact ( n - 1 ) 

-- what is the area of a circle with a given radius
data Area = Circle Float
surface :: Area -> Float   
surface (Circle r) = pi * r ^ 2  

-- what is the circuleference of a circle with a given radius
circumference :: Float -> Float
circumference (r) = 2*pi*r

-- what are the prime factors of a given number
generatePrimeFactors :: Integral a => a -> [a]
generatePrimeFactors n  = 1:[x | x <- [1..(quot n 2)], (isPrime x) && (n `mod` x == 0)] ++ case (isPrime n) of 
   True -> [n]
   False -> []


-- is a number prime
isPrime :: Integral a => a -> Bool
isPrime k = if k > 1 then null [ x | x <- [2..k - 1], k `mod` x == 0] else False

-- what are the first n primes
generatePrimes :: Integral a => Int -> [a]
generatePrimes n = take n [i | i <- [2..], isPrime i]

-- what are the roots of a quadratic function
type CD = Complex Double
quadraticRoots :: (CD, CD, CD) -> (CD, CD)
quadraticRoots (a, b, c) =
  if realPart b > 0
    then ((2 * c) / (-b - d), (-b - d) / (2 * a))
    else ((-b + d) / (2 * a), (2 * c) / (-b + d))
  where
    d = sqrt(b ^ 2 - 4 * a * c)

-- what is the n-th fibonacci number
-- what are the first n numbers of the fibonacci sequence
fib :: (Eq a, Num a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)
generateFibonacci :: (Enum a1, Eq a1, Num a1, Num a2) => a1 -> [a2]
generateFibonacci n = [fib i | i <- [0..n]]

-- what is the (approximate) golden ratio of the n-th fibonacci number
phi :: (Fractional a1, Eq a2, Num a2) => a2 -> a1
phi p = fib (p-1) / fib (p-2) -- golden ratio

-- what is the n-th lucas-lehmer number
-- what are the first n numbers of the lucas-lehmer sequence
lucL 0 = 4
lucL n = lucL(n-1)^2-2
generateLucasLehmer :: (Enum t, Num t, Num a, Eq t) => t -> [a]
generateLucasLehmer n = [lucL i | i<-[1..n]]

-- what is the recursive sum of a given list of numbers
recursiveSum :: [Int] -> Int
recursiveSum [] = 0
recursiveSum (x:xs) = x + recursiveSum xs



module RSA where

{- Import Packages -}
import Control.Monad.Fix
import Data.Bits
import Data.Char (ord)
import Math.NumberTheory.Primes.Testing
import System.Random

{- Type handling -}
-- 3-Tuple extraction
get1st :: (Integer, Integer, Integer) -> Integer
get1st (a, _, _) = a

get2nd :: (Integer, Integer, Integer) -> Integer
get2nd (_, a, _) = a

get3rd :: (Integer, Integer, Integer) -> Integer
get3rd (_, _, a) = a

-- Convert a raw string to an Integer
strToInt :: [Char] -> [Int]
strToInt input | length input == 0 = error "Input length is blank"
               | otherwise = map ord input

-- Roll the converted list down to [1..26]
reduceIntString :: [Int] -> [Int]
reduceIntString input = map ((-96) + ) input

processString :: [Char] -> [Int]
processString input = reduceIntString (strToInt input)

{- Backend functions -}
-- Extended Euclidean algorithm for the GCD of a and b, ax+by
  -- (GCD, x, y)
eGCD :: Integer -> Integer -> (Integer,Integer,Integer)
eGCD a b | a == 0 = (b, 0, 1) -- exit condition
eGCD a b | otherwise = let (g, s, t) = eGCD (mod b a) a
                       in (g, t - (div b a) * s, s)

-- Cycles x until it is positive in base m
cyclePositive :: Integer -> Integer -> Integer
cyclePositive x m | x > 0 = x -- exit condition
                  | otherwise = cyclePositive (x+m) m

-- Modular inverse - calculate a (positive) modular inverse of e mod m
modInverse :: Integer -> Integer -> Integer
modInverse e m = cyclePositive (get2nd (eGCD e m)) m

-- Modular exponentiation
modExp :: Integer -> Integer -> Integer -> Integer
modExp b e m | e == 0 = 1 -- b = base, e = exponent, m = mod m
modExp b e m | otherwise = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
               where t = if testBit e 0 then b `mod` m else 1

{- Generation of random primes -}
rndPrime :: Int -> IO Integer
rndPrime bits =
    fix $ \again -> do
        x <- fmap (.|. 1) $ randomRIO (2^(bits - 1), 2^bits - 1)
        if isPrime x then return x else again

rndPrimes :: Int -> IO (Integer, Integer)
rndPrimes bits = do
    p <- rndPrime bits
    fix $ \again -> do
        q <- rndPrime bits
        if p /= q then return (p, q) else again

{- Basic primality and division functions -}
-- Definition of divisibility
  -- d divides n if the remainder of n/d = 0
divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

-- List of all divisors of n
divisorList :: Integer -> [Integer]
divisorList n = [x | x <- [1..n], divides x n]

-- Least natural number that divides n
  -- Existence of prime divisor guaranteed by the Fundamental Theorem of Arithmatic
  -- Existence of least element guaranteed by the Well Ordering of Z
ld :: Integer -> Integer
ld n = ldf 2 n

-- Least divisor of n such that n >= k for k in N
  -- LDF(2)(n) = LD(n), as n > 1
ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k -- if K div n then return k; exit condition
        | k^2 > n = n -- if k > sqrt(n), return n
        | otherwise = ldf (k+1) n -- else, iterate (tail recursion)

-- Prime factorization of the integer n
factors :: Integer -> [Integer]
factors n | n < 1 = error "Argument not positive"
          | n == 1 = [] -- 1 has no prime factors; exit condition
          | otherwise = p : factors (div n p) where p = ld n -- if p divides n,
            -- then divide n by p and find the factors of n/p (tail recursion)

{- Group theory applications -}

-- Count the Sylow p-Subgroups: n = integer, p = prime
countSylowPSubs :: Integer -> Integer -> [Integer]
countSylowPSubs n p = [x | x <- [1..n], divides x n, x `mod` p == 1]

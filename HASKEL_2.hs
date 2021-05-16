import Char
import Ratio

snum :: Int -> Int
snum n = if n>0 then 1 else
          if n==0 then 0 else -1

abs :: Int -> Int
abs n = if n>=0 then n else -n

--fSignum n=(abs(n+1) - abs(n-1))`div`2

abs2 n
   | n >=0 = n
   | otherwise = -n


signum2 n
      | n <0 = -1
      | n == 0 = 0
      | otherwise = 1

fNot :: Bool -> Bool
fNot b
   | b = False
   | otherwise = True 


pred :: Int -> Int
pred(n+1)=n

--fAdd =\x -> (\y -> x+y) 

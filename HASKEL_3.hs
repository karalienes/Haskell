fAdd = \ x -> (\ y -> x+y) 

adds n = map f [0.. n-1]
         where
            f x = x*2 + 1

add2 n = map( \x -> x*2 + 1) [0..n-1]

f n = [ [y] | x <-[1..n] , y <- [1..x]]


fn a = [ [1..x] | x <-[1..a] , y <- [1..x]]

fnn n = [x |  x <- [1..n] , even x ]

factors :: Int -> [Int]
factors n = [x | x<-[1..n], mod n x == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x<-[1..n], isPrime x]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

--sorted :: Ord a => [a] -> Bool
sorted xs = and [x<=y | (x,y)<-pairs xs]
sorted2 xs = null [(x,y) | (x,y)<-pairs xs, x>y]

psts :: Eq a => a -> [a] -> [Int]
psts x xs = [i | (a,i)<-zip xs [0..], x == a]


--lowers :: String -> Int
--lowers xs = length [x | x <- xs , ÝsLower 'x' ]

pyth :: Int -> [(Int,Int,Int)]
pyth n = [(x,y,z) | x <- [3..n] , y <- [x+1..n] , z <- [y+1..n] , x^2+y^2 ==z^2]

pyths2 :: Int -> [(Int,Int,Int)]
pyths2 n =
    [(a,b,c) |
        a<-[3..n],
        b<-[a+1..min n (div (a^2-1) 2)],
        c<-[b+1..min n (div (a^2+1) 2)],
        a^2+b^2==c^2]

factorial n = product [1..n]

twice :: (a -> a) -> a -> a
twice f x = f ( f x )


factors2 n = filter (\x-> mod n x == 0) [1..n]

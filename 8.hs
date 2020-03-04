
import Char
import Ratio

double x = x +x

quad x = double (double x)

fact n = product [1..n]

average ns = sum ns `div` length ns
average2 ns = div (sum ns) (length ns)

a = 10 + b
 where
 b = 20

c = 30

n = a `div` length xs
    where
      a = 10
      xs = [1..5]

myLast ns = head(drop (length ns - 1) ns)
myLast2 ns = head(reverse ns)
myLast3 ns = ns !! (length ns - 1)

myInit ns = take (length ns - 1) ns
myInit2 ns = reverse(tail (reverse ns))

data Day = M | Tu | W | Th | F | Sa | Su deriving (Show,Eq,Ord,Enum)
isWeekend :: Day -> Bool
isWeekend x = (x == Sa || x == Su)

abs2 :: Int -> Int
abs2 n = if n>=0 then n else -n

snum :: Int -> Int
snum n = if n>0 then 1 else
          if n==0 then 0 else -1

snum2 n = div (abs(n+1)-abs(n-1)) 2

abs3 n | n>=0 = n
       | otherwise = -n

snum3 n
    | n > 0 = 1
    | n == 0 = 0
    | True = -1

fxy 0 0 = 1
fxy 0 y = y
fxy x 0 = x
fxy x y = x * y

add x y = x + y

add2 = \x->(\y->x+y)

--add :: (Int,Int)->Int
addT (x,y) = x + y

--zeroto :: Int -> [Int]
zeroto n = [0..n]

mult :: Int -> Int -> Int -> Int
mult a b c = a * b * c

mult2 = mult 2 3
mult3 = mult 2
mult4 = mult

data Element = I Int | FF Float deriving (Show)

inc = (+1)
rec = (1/)

lowers :: String -> Int
lowers xs = length [x | x<-xs, isLower x]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x<-[3..n], y<-[x+1..n], z<-[y+1..n],x^2+y^2==z^2]

pyths2 n = [(x,y,z) | x<-[3..n],
                      y<-[x+1..min (div (x^2-1) 2) n],
                      z<-[y+1..min (div (x^2+1) 2) n],
                      x^2+y^2==z^2]

factors :: Int -> [Int]
factors n = [x | x<-[1..n], mod n x == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [1,n]
isPrime2 n = length(factors n) == 2

primes :: Int -> [Int]
primes n = [p | p<-[2..n], isPrime p]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

--sorted :: Ord a => [a] -> Bool
sorted xs = and [x<=y | (x,y)<-pairs xs]
sorted2 xs = null [(x,y) | (x,y)<-pairs xs, x>y]

psts :: Eq a => a -> [a] -> [Int]
psts x xs = [i | (a,i)<-zip xs [0..], x == a]

zero = 0 : [x+1 | x<-zero]

fib = 0 : 1 : [a+b | (a,b)<-zip fib (tail fib)]

prime = 2 : [p | p<-[3,5..],
              (all (\x->mod p x /= 0) . takeWhile (\x->x*x<=p)) prime]

prime2 n = prime22 [2..n]
    where
     prime22 (x:xs)
        | x^2 > n = x : xs
        | otherwise = x : prime22 [p | p<-xs, mod p x /= 0]

perfects :: Int -> [Int]
perfects n = [x | x<-[1..n], sum(init(factors x))==x]

scalar :: [Int]->[Int]->Int
scalar xs ys = sum [(xs!!i)*(ys!!i) |
                     i<-[0..(min (length(xs)) (length(ys)))-1]]

and2 :: [Bool] -> Bool
and2 [] = True
and2 (x:xs) = x && and2 xs

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (xs:xss) = xs ++ concat2 xss

replicate2 :: Int -> a -> [a]
replicate2 0 _ = []
replicate2 (n+1) a = a : replicate2 n a

getE :: [a] -> Int -> a
getE [] _ = error "Prelude.!!: index too large"
getE (x:_) 0 = x
getE (x:xs) (n+1) = getE xs n

isElem :: Eq a => a -> [a] -> Bool
isElem _ [] = False
isElem a (x:xs) = a==x || isElem a xs

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort(take n xs)) (msort(drop n xs))
    where
     n = div (length(xs)) 2

ftc = (2+) . (5*)

toBin n = (map ((!!) "01") . map (`mod` 2) . map (div n) . reverse . takeWhile (<=n) . map (2^)) [0..]

toDec ns = (sum . map (\(r,n)->n*2^r) . zip [0..] . reverse . map digitToInt) ns

data Nat = Zero | Succ Nat deriving (Show)

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat (n+1) = Succ (int2nat n)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ s) = 1 + nat2int s

addNat :: Nat -> Nat -> Nat
addNat a b = int2nat (nat2int a + nat2int b)

data E = P E E | M2 E E | T E E | N Int deriving (Show)

--1+2*3-4
e :: E
e = M2 (P (N 1) (T (N 2) (N 3))) (N 4)

evalE :: E -> Int
evalE (P a b) = evalE a + evalE b
evalE (M2 a b) = evalE a - evalE b
evalE (T a b) = evalE a * evalE b
evalE (N n) = n

prefE :: E -> String
prefE (P a b) = "+ " ++ prefE a ++ " " ++ prefE b
prefE (M2 a b) = "- " ++ prefE a ++ " " ++ prefE b
prefE (T a b) = "* " ++ prefE a ++ " " ++ prefE b
prefE (N n) = show n

data E2 = T2 String E2 E2 | N2 Int deriving (Show)

e2 :: E2
e2 = T2 "-" (T2 "+" (N2 1) (T2 "*" (N2 2) (N2 3))) (N2 4)

evalE2 :: E2 -> Int
evalE2 (T2 "+" a b) = evalE2 a + evalE2 b
evalE2 (T2 "-" a b) = evalE2 a - evalE2 b
evalE2 (T2 "*" a b) = evalE2 a * evalE2 b
evalE2 (N2 n) = n

postE2 :: E2 -> String
postE2 (T2 "+" a b) = postE2 a ++ " " ++ postE2 b ++ " +"
postE2 (T2 "-" a b) = postE2 a ++ " " ++ postE2 b ++ " -"
postE2 (T2 "*" a b) = postE2 a ++ " " ++ postE2 b ++ " *"
postE2 (N2 n) = show n

data S = S S Int S | L Int deriving (Show)

tree :: S
tree = S (S (L 1) 3 (L 4)) 5 (S (L 6) 7 (L 9))

fromS :: S -> [Int]
fromS (L n) = [n]
fromS (S a n b) = fromS a ++ [n] ++ fromS b

fCh :: IO (Char,Char)
fCh =
    do
     x<-getChar
     getChar
     y<-getChar
     return (x,y)

fCh2 :: IO ()
fCh2 =
    do
     (x,y)<-fCh
     putChar '\n'
     putChar x
     putChar ','
     putChar y

getL :: IO String
getL =
    do
     x<-getChar
     if x=='\n' then return ""
     else
        do
         xs<-getL
         return (x:xs)

getL2 :: IO ()
getL2 =
    do
     xs<-getL
     putChar (last xs)
     putStrLn ""
     putStr(show(length xs))

drawStar :: Int -> IO ()
drawStar 0 = return ()
drawStar n =
    do
     putStr "* "
     drawStar (n-1)

fStar :: IO ()
fStar = fStar2 [5,4..1]

fStar2 :: [Int] -> IO ()
fStar2 [] = return ()
fStar2 (n:ns) =
    do
     putStr (show n ++ ": ")
     drawStar n
     putChar '\n'
     fStar2 ns

type Position = (Int,Int)

origin :: Position
origin = (0,0)

left :: Position -> Position
left (x,y) = (x-1,y)

type Pair a = (a,a)

multP :: Pair Int -> Int
multP (a,b) = a*b

copy :: a -> Pair a
copy a = (a,a)

type Transition = Position -> Position

right :: Transition
right (x,y) = (x+1,y)

data Shape = Circle Float | Rect Float Float
                | Tria Float Float Float deriving (Show)

square :: Float -> Shape
square n = Rect n n

circum, area :: Shape -> Float
circum (Circle r) = 2*pi*r
circum (Rect a b) = 2*(a+b)
circum (Tria a b c) = a+b+c

area (Circle r) = pi*r*r
area (Rect a b) = a*b
area (Tria a b c) = sqrt(u*(u-a)*(u-b)*(u-c))
    where
     u=(a+b+c)/2

data T3 = T3 Int [T3] deriving (Show)

tree3 :: T3
tree3 = T3 1 [T3 2 [T3 3 []], T3 5 []]

data E3 = E3 (Int->Int->Int) E3 E3 | N3 Int

expr :: E3
expr = E3 (*) (N3 3) (E3 (-) (N3 5) (N3 2))

instance Show E3 where
    show (N3 n) = show n
    show (E3 op e1 e2) = ". " ++ show e1 ++ " " ++ show e2

evalE3 :: E3 -> Int
evalE3 (N3 n) = n
evalE3 (E3 op e1 e2) = op (evalE3 e1) (evalE3 e2)



import Char
import Ratio

double x = x + x

quadruple x = double (double x)

fact n = product [1..n]

--average ns = sum ns `div` length ns
average ns = div (sum ns) (length ns)

a = 10 + b
 where
 b = 20

c = 30 + a

fN = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

myLast ns = head (reverse ns)
myLast2 ns = ns !! (length ns - 1)

myInit ns = reverse(tail(reverse ns))
myInit2 ns = take (length ns - 1) ns

add :: (Int, Int) -> Int
add (x,y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

--mult :: Int -> Int -> Int ->Int
mult :: Int -> (Int -> (Int ->Int))
mult a b c = a * b * c

fT = take 3

fL :: [a] -> Int
fL xs = length xs

twice f x = f (f x)

{-
data Day = M | Tu | W | Th | F | Sa | Su
        deriving (Show, Eq, Ord, Enum)

isWeekend :: Day -> Bool
isWeekend d = d == Sa || d == Su

nextDay :: Day -> Day
nextDay d = ([d .. Su] ++ [M]) !! 1

prevDay :: Day -> Day
prevDay d = reverse ([Su] ++ [M .. d]) !! 1
-}

data Direction = North | South | East | West
        deriving (Show, Eq, Ord, Enum)

degree :: Direction -> Int
--degree d = [90, 180, 0, 270] !! (4 - length [d .. West])
degree d = [90, 180, 0, 270] !! length [South .. d]

degree2 :: Direction -> Int
degree2 North = 90
degree2 South = 180
degree2 East = 0
degree2 West = 270

fR [x] = x
fR [x,y] = x * y
fR [x,y,z] = x * y - z
fR xs = sum xs

fP (n+1) = n
fP2 n = n - 1

fSec = (1/)

safetail xs = if null xs then [] else tail xs

safetail2 xs
    | null xs = []
    | otherwise = tail xs

safetail3 [] = []
safetail3 (x:xs) = xs

fCon xss = [x | xs<-xss, x<-xs]

fTn n = [ sum [1..x] | x<-[0..n]]
fTn2 n = [ div (x*(x+1)) 2 | x<-[0..n]]

fLn n = [[x | y<-[1..x]] | x<-[0..n]]
fLn2 n = [take x [x,x..] | x<-[0..n]]

factors :: Int -> [Int]
factors n = [x | x<-[1..n], mod n x == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x<-[1..n], isPrime x]

pairs xs = zip xs (tail xs)

--sorted xs = null [(x,y) | (x,y)<-pairs xs, x>y]
--sorted xs = and [x<y | (x,y)<-pairs xs]
sorted xs = not(or [x>y | (x,y)<-pairs xs])

fDL n = [ ord d - ord '0' | d<-show n]

fLower xs = [x | x<-xs, isLower x]
fDigit xs = [x | x<-xs, isDigit x]
fUpper xs = [toUpper x | x<-xs, isLower x && not(isDigit x)]

pyths :: Int -> [(Int,Int,Int)]
pyths n =
    [(a,b,c) |
        a<-[3..n],
        b<-[a+1..n],
        c<-[b+1..n],
        a^2+b^2==c^2]

pyths2 :: Int -> [(Int,Int,Int)]
pyths2 n =
    [(a,b,c) |
        a<-[3..n],
        b<-[a+1..min n (div (a^2-1) 2)],
        c<-[b+1..min n (div (a^2+1) 2)],
        a^2+b^2==c^2]

perfect n = [x | x<-[1..n], sum(init(factors x))==x]

fScalar xs ys = sum [ x*y | (x,y)<-zip xs ys]

fCon2 [] = []
fCon2 (xs:xss) = xs ++ fCon2 xss

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort [] = []
msort [x] = [x]
msort xs = merge (msort(take n xs))  (msort(drop n xs))
    where
     n = div (length xs) 2

fib = 0 : 1 : [x+y | (x,y)<-zip fib (tail fib)]

digits n = [ord d - ord '0' | d<-show n]

fM = [1..9] ++ [n*10+d | n<-fM, d<-[1..9],
        not(elem d (digits n)),
        mod (n*10+d) (sum[1|x<-show(n*10+d)])==0]

fLen xs = foldl (\y _-> y+1) 0 xs
fLen2 xs = foldr (\_ y-> y+1) 0 xs

fRev xs = foldl (\ys x->x:ys) [] xs
fRev2 xs = foldr (\x ys->ys++[x]) [] xs

fSet xs = foldl (\ys x-> ys++[x|not(elem x ys)]) [] xs

fUnion xs ys = fSet (xs++ys)

toBin = (concat . reverse . map show . map (`mod` 2) .
          map (div 100) . map (2^)) [0..6]

toBin2 = (concat . reverse .
         map (show . (`mod` 2) . (div 100) . (2^))) [0..6]

toBin3 n = (concat . reverse .
         map (show . (`mod` 2) . (div n) . (2^)) .
         takeWhile (\r->2^r<=n)) [0..]

toDec bs = (sum . map (\(r,b)->2^r*b) . zip [0..] . 
             map (\b-> ord b - ord '0') . reverse) bs

primes2 = 2 : [n | n<-[3,5..],
           all (\p-> mod n p /= 0) $
           takeWhile (\p->p^2<=n) primes2]

primes3 = 2 : [n | n<-[3,5..],
           not $ any (\p-> mod n p == 0) $
           takeWhile (\p->p^2<=n) primes3]

fMap f xs = foldr (\x ys->f x : ys) [] xs

fFilter p xs = foldr (\x ys->[x|p x] ++ ys) [] xs

fAct :: IO (Char, Char)
fAct =
    do
     x <- getChar
     getChar
     y <- getChar
     return (x, y)

fAct2 :: IO ()
fAct2 =
    do
     (x, y) <- fAct
     putChar x
     putChar ' '
     putChar y
     putChar '\n'

fLine :: IO String
fLine =
    do
     x <- getChar
     if x == '\n' then
      return []
     else
      do xs <- fLine
         return (x:xs)

fLine2 :: IO ()
fLine2 =
    do
     line <- fLine
     putChar (head line)
     putChar (last line)

fDigitSum :: IO ()
fDigitSum =
    do
     ns <- fLine
     --fSumShow (show(fDigitSum2 ns))
     putStr (show(fDigitSum2 ns))

fDigitSum2 :: String -> Int
fDigitSum2 = sum . map (\n-> ord n - ord '0')

fSumShow :: String -> IO ()
fSumShow [] = return ()
fSumShow (n:ns) =
    do
     putChar n
     fSumShow ns

type Position = (Int, Int)
type Trans = Position -> Position

origin :: Position
origin = (0, 0)

--left :: Position -> Position
left :: Trans
left (x, y) = (x-1, y)


type Pair a = (a, a)

mult2 :: Pair Int -> Int
mult2 (x,y) = x * y

copy :: a -> Pair a
copy x = (x, x)

data Shape = Circle Float |
              Rect Float Float |
               Triangle Float Float Float
                deriving (Show, Eq, Ord)

shapes :: [Shape]
shapes = [Circle 2, Rect 3 4, Triangle 5 6 7]

circum :: Shape -> Float
circum (Circle r) = 2 * pi * r
circum (Rect x y) = 2 * (x + y)
circum (Triangle a b c) = a + b + c

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rect x y) = x * y
area (Triangle a b c) = sqrt(u*(u-a)*(u-b)*(u-c))
    where
     u = (a + b + c) / 2

data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ s) = 1 + nat2int s

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat (n+1) = Succ (int2nat n)

addNat :: Nat -> Nat -> Nat
addNat n1 n2 = int2nat (nat2int n1 + nat2int n2)

data E = P E E | M E E | T E E | D E E | N Int deriving (Show, Eq, Ord)

--2+3*4-6/2
exps :: E
exps = M (P (N 2) (T (N 3) (N 4))) (D (N 6) (N 2))

evalE :: E -> Int
evalE (N n) = n
evalE (P e1 e2) = evalE e1 + evalE e2
evalE (M e1 e2) = evalE e1 - evalE e2
evalE (T e1 e2) = evalE e1 * evalE e2
evalE (D e1 e2) = evalE e1 `div` evalE e2

toPost :: E -> String
toPost (N n) = show n
toPost (P e1 e2) = toPost e1 ++ " " ++ toPost e2 ++ " +"
toPost (M e1 e2) = toPost e1 ++ " " ++ toPost e2 ++ " -"
toPost (T e1 e2) = toPost e1 ++ " " ++ toPost e2 ++ " *"
toPost (D e1 e2) = toPost e1 ++ " " ++ toPost e2 ++ " /"

size :: E -> Int
size (N n) = 1
size (P e1 e2) = size e1 + size e2
size (M e1 e2) = size e1 + size e2
size (T e1 e2) = size e1 + size e2
size (D e1 e2) = size e1 + size e2

data E2 = E2 (Int->Int->Int) E2 E2 | N2 Int

instance Show E2 where
  show (N2 n) = show n
  show (E2 op e1 e2) = "E2 op (" ++ show e1 ++ ") (" ++ show e2 ++ ")"

--2+3*4-6/2
exps2 :: E2
exps2 = E2 (-) (E2 (+) (N2 2) (E2 (*) (N2 3) (N2 4))) (E2 (div) (N2 6) (N2 2))

evalE2 :: E2 -> Int
evalE2 (N2 n) = n
evalE2 (E2 op e1 e2) = op (evalE2 e1) (evalE2 e2)

data Tree = Bin Tree Int Tree | Leaf Int deriving (Show, Eq, Ord)

tree :: Tree
tree = Bin (Bin (Leaf 2) 3 (Leaf 4)) 5 (Bin (Leaf 6) 7 (Leaf 8))

toList :: Tree -> [Int]
toList (Leaf n) = [n]
toList (Bin t1 n t2) = toList t1 ++ [n] ++ toList t2

find :: Int -> Tree -> Bool
find m (Leaf n) = m == n
find m (Bin t1 n t2) = m == n || find m t1 || find m t2


{-
-- nines.hs --
import Ratio

data Op = P | M | T | D deriving (Show, Eq, Ord)

data E = E Op E E | N Int deriving (Show, Eq, Ord)

genE = (N 9) : [E op e (N 9) |
                 e<-takeWhile (\f->numE f<9) genE, op<-[P,M,T,D]]

genE2 = (N 9) : [E op e e2 |
                 e<-takeWhile (\f->numE f<9) genE2, e2<-takeWhile (\f->numE f<9) genE2, op<-[P,M,T,D], op/=D || evalE(e2)/=0]

numE :: E -> Int
numE (N n) = 1
numE (E op e1 e2) = numE e1 + numE e2

infixE :: E -> String
infixE (N n) = show n
infixE (E P e1 e2) = "(" ++ infixE e1 ++ ")+(" ++ infixE e2 ++ ")"
infixE (E M e1 e2) = "(" ++ infixE e1 ++ ")-(" ++ infixE e2 ++ ")"
infixE (E T e1 e2) = "(" ++ infixE e1 ++ ")*(" ++ infixE e2 ++ ")"
infixE (E D e1 e2) = "(" ++ infixE e1 ++ ")/(" ++ infixE e2 ++ ")"

evalE :: E -> Ratio Int
evalE (N n) = n%1
evalE (E P e1 e2) = evalE e1 + evalE e2
evalE (E M e1 e2) = evalE e1 - evalE e2
evalE (E T e1 e2) = evalE e1 * evalE e2
evalE (E D e1 e2) = evalE e1 / evalE e2

findList n = filter ((==n) . evalE) $ filter ((==9) . numE) $ genE
findList2 n = filter ((==n) . evalE) $ filter ((==9) . numE) $ genE2

find = infixE . head . findList
find2 = infixE . head . findList2

search =  filter (not . null . findList) [0..]
search2 =  filter (not . null . findList2) [0..]
-}


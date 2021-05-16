import Ratio
import Char

double x = x + x

quad x = double (double x)

fact n = product [1..n]

average ns = div (sum ns) (length ns)

a = 10 + b
 where
 b = 20 + c + d
 d = 40

c = 30

n = a `div` length xs
 where
   a = 10
   xs = [1,2,3,4,5]

myLast ns = head (drop (length ns - 1) ns)
myLast2 ns = head(reverse ns)
myLast3 ns = ns !! (length ns - 1)

myInit ns = take (length ns - 1) ns
myInit2 ns = reverse(tail (reverse ns))

add :: (Int,Int) -> Int
add (x,y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

add2 :: Int -> Int -> Int
add2 x y = x + y

add3 = add2
add4 = add2 3
add5 = add2 5 7

data Day = M | Tu | W | Th | F | Sa | Su deriving (Show, Eq, Ord, Enum)

isWeekend :: Day -> Bool
isWeekend x = (x == Sa || x == Su)

nextDay d = head (tail [d .. Su] ++ [M])

prevDay d = last ([Su] ++ init [M .. d])

data Direction = North | South | East | West deriving (Show, Eq, Ord, Enum)

degree :: Direction -> Int
degree d = [90,180,0,270] !! (length [North .. d] - 1)

factors n = [x | x<-[1..n], mod n x == 0]

isPrime n = factors n == [1,n]

primes n = [ x | x<-[2..n], isPrime x]

pairs xs = zip xs (tail xs)

sorted xs = and [x <= y | (x,y)<-pairs xs]

pyths n = [(x,y,z) | x<-[3..n], y<-[x+1..n],z<-[y+1..n], x^2+y^2==z^2]

pyths2 n = [(x,y,z) | x<-[3..n], x^2<=2*n-1,
                     y<-[x+1..min n (div (x^2-1) 2)],
                     z<-[y+1..min n (div (x^2+1) 2)], x^2+y^2==z^2]

perfect n = [ x | x<-[1..n], sum(init(factors x))==x]

fScalar xs ys = sum [x*y | (x,y)<-zip xs ys]

fScalar2 xs ys = sum [xs!!i * ys!!i |
			i<-[0 .. min (length xs) (length ys) - 1]]

fMSum xss = sum [x | xs<-xss, x<-xs]
fMSum2 xss = sum [sum xs | xs<-xss]
fMSum3 xss = sum [(xss!!i)!!j | i<-[0..length(xss)-1],
				j<-[0..length(xss!!i)-1]]

fact2 0 = 1
fact2 n = n * fact2 (n-1)

prod [] = 1
--prod [a] = a
--prod [a,b] = a*b
prod (x:xs) = x * prod xs

fAnd [] = True
fAnd (False:_) = False
fAnd (x:xs) = x && fAnd xs

fCon [] = []
fCon (xs:xss) = xs ++ fCon xss

fRep 0 _ = []
fRep (n+1) x = x : fRep n x

fInd [] _ = error "Program error!"
fInd (x:_) 0 = x
fInd (x:xs) (n+1) = fInd xs n

fElem a [] = False
--fElem a (x:xs) = a == x || fElem a xs
fElem a (x:xs)
  | a == x = True
  | otherwise = fElem a xs

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

msort [] = []
msort [x] = [x]
msort xs = merge (msort (take n xs))  (msort (drop n xs))
  where
   n = div (length xs) 2

twice f x = f (f x)

fn = 2 : map (+3) fn

factors2 n = filter (\x-> mod n x == 0) [1..n]

revl = foldl (\x y-> y:x) []
revl2 = foldl (\x y-> [y]++x) []

revr = foldr (\x y-> y++[x]) []

map2 = map (+1) . map (+2)

map3 = map ((+1) . (+2))

factors3 n = filter ((==0).(mod n)) [1..n]

primes2 = 2 : [n | n<-[3,5..], (all ((/=0).(mod n)) . takeWhile ((<=n).(^2))) primes2]

primes3 = 2 : [n | n<-[3,5..], (not . any ((==0).(mod n)) . takeWhile ((<=n).(^2))) primes3]

toBin n = (reverse . concat . map show . map (`mod` 2) .
           map (div n) . takeWhile (<=n) . map (2^)) [0..]


getC :: IO ()
getC =
  do
    c<-getChar
    d<-getChar
    putChar '\n'
    putChar d

getT :: IO (Char, Char)
getT =
  do
    a <- getChar
    getChar
    b <- getChar
    return (a,b)

getT2 :: IO ()
getT2 =
  do
    (x,y)<-getT
    putChar '\n'
    putChar y

getL :: IO String
getL =
  do
   c <- getChar
   if c=='\n' then return []
   else
    do
      cs <- getL
      return (c:cs)

getL2 :: IO ()
getL2 =
  do
    line <- getL
    putChar (last line)

type Pos = (Int, Int)
type Trans = Pos -> Pos

origin :: Pos
origin = (0, 0)

--left :: Pos -> Pos
left :: Trans
left (x,y) = (x-1,y)

type Pair a = (a,a)

copy :: a -> Pair a
copy x = (x,x)

mult :: Pair Int -> Int
mult (x,y) = x * y

data Shape = Circle Float | Rect Float Float | Tria Float Float Float deriving (Show)

circum :: Shape -> Float
circum (Circle r) = 2 * pi * r
circum (Rect a b) = 2 * (a + b)
circum (Tria a b c) = a + b + c

area :: Shape -> Float
area (Circle r) = pi * r ^2
area (Rect a b) = a * b
area (Tria a b c) = sqrt(u*(u-a)*(u-b)*(u-c))
  where
   u = (a + b + c) / 2

data T = T T Int T | N Int deriving (Show)

sumT :: T -> Int
sumT (N n) = n
sumT (T t1 n t2) = sumT t1 + n + sumT t2

listT :: T -> [Int]
listT (N n) = [n]
listT (T t1 n t2) = listT t1 ++ [n] ++ listT t2

maxT :: T -> Int
maxT (N n) = n
maxT (T t1 n t2) = max (maxT t1)  (max n (maxT t2))

toT :: [Int] -> T
toT [] = error "!"
toT [n] = N n
toT ns = T (toT (take n ns)) (ns!!n) (toT (drop (n+1) ns))
  where
   n = div (length ns) 2

data E = A E E | S E E | C E E | D E E | P E E | V Int deriving (Show)

--2+3*(5-1)/2^4
aexp :: E
aexp = A (V 2) (D (C (V 3) (S (V 5) (V 1))) (P (V 2) (V 4)))

postfix :: E -> String
postfix (A e1 e2) = postfix e1 ++ " " ++ postfix e2 ++ " +"
postfix (S e1 e2) = postfix e1 ++ " " ++ postfix e2 ++ " -"
postfix (C e1 e2) = postfix e1 ++ " " ++ postfix e2 ++ " *"
postfix (D e1 e2) = postfix e1 ++ " " ++ postfix e2 ++ " /"
postfix (P e1 e2) = postfix e1 ++ " " ++ postfix e2 ++ " ^"
postfix (V n) = show n

evalE :: E -> Int
evalE (A e1 e2) = evalE e1 + evalE e2
evalE (S e1 e2) = evalE e1 - evalE e2
evalE (C e1 e2) = evalE e1 * evalE e2
evalE (D e1 e2) = evalE e1 `div` evalE e2
evalE (P e1 e2) = evalE e1 ^ evalE e2
evalE (V n) = n

preE :: E -> String
preE (A e1 e2) = "(+) (" ++ preE e1 ++ ") (" ++ preE e2 ++ ")"
preE (S e1 e2) = "(-) (" ++ preE e1 ++ ") (" ++ preE e2 ++ ")"
preE (C e1 e2) = "(*) (" ++ preE e1 ++ ") (" ++ preE e2 ++ ")"
preE (D e1 e2) = "div (" ++ preE e1 ++ ") (" ++ preE e2 ++ ")"
preE (P e1 e2) = "(^) (" ++ preE e1 ++ ") (" ++ preE e2 ++ ")"
preE (V n) = show n

data T2 = T2 T2 T2 (T2 -> Int) | N2 Int

texp :: T2
texp = T2 (T2 (T2 (N2 5) (N2 3) fT2) (T2 (N2 12) (N2 10) fT2) fT2)
          (T2 (N2 4) (T2 (N2 8) (N2 6) fT2) fT2) fT2

fT2 :: T2 -> Int
fT2 (T2 t1 t2 f) = max (fT2 t1) (fT2 t2)
fT2 (N2 n) = n

instance Show T2 where
  show (T2 t1 t2 f) = "T2 (" ++ show t1 ++ ") (" ++ show t2 ++ ") f"
  show (N2 n) = "N2 " ++ show n




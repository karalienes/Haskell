
import Char
import Ratio


double x=x+x
quadruple x= double (double x)
factorial n =product [1..n]
average ns = (sum[1..ns]) `div` ns
fLast ms = head(reverse ms)
fLast2 ms = ms!!(length ms-1)

fInit ns=reverse (tail(reverse ns))
fN cs=10 `div`(length cs)
fInit2 ns=take(length ns-1)ns

n = a `div` length xs
    where
      a = 10
      xs = [1..5]

abs2 :: Int -> Int
abs2 n = if n>=0 then n else -n

add       :: (Int,Int) -> Int 
add (x,y)  = x+y 

zeroto    :: Int -> [Int] 
zeroto n   = [0..n]

mult      :: Int -> (Int -> (Int -> Int)) 
mult x y z = x*y*z

add2      ::(Int,Int,Int)->Int
add2 (x,y,z)=x+y+z

--add3 :: (Int->(Int->Int))->Int
--add3 (x,y,z) =x+y+z

fst ::(Int,Int)->Int
fst(x,y)=x

snd ::(Int,Int)->Int
snd(x,y)=y

--data Day = M | Tu | W | Th | F | Sa | Su deriving (Show,Eq,Ord,Enum)
--isWeekend :: Day -> Bool
--isWeekend x = (x == Sa || x == Su)

--nextDay :: Day -> Day
--nextDay d=[d..Su] !!1

--Nextday :: (Day,Day) -> Day
--Nextday (d,x)=([d..Su] ++ [x]))!!2


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

data Element = I Int | F Float
getFloat :: Element -> Float
getFloat(I n) = read (show n) :: Float
getFloat(F f) = f


Signum :: Int -> Int
Signum n = if n<0 then -1 else
           --if n==0 then 0 else 1


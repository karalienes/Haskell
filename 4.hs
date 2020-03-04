f :: IO (Char)
f = do x <- getChar
       getChar
       y <- getChar
       return x

fAct :: IO (Char, Char)
fAct =
    do
     x <- getChar
     getChar
     y <- getChar
     return (x, y)


getL :: IO String
getL =
  do
   c <- getChar
   if c=='a' then return []
   else
    do
      cs <- getL
      return (c:cs)

--putStr  :: String -> IO ()
--putStr []   = return ()
--putStr (x:xs) = do putChar x
--                   putStr xs

type Pos =(Int,Int)
left  :: Pos -> Pos
left(x,y) =(x-1,y)

type Pair a =(a,a)
mult :: Pair Int -> Int
mult(m,n)=m*n

copy :: a -> Pair a 
copy x = (x,x)

--type Trans = Pos -> Pos
--type P2 = Pair Int -> Pair Int
--P2 :: P2
--P2(x,y) =(x+y,x-y)

data Answer = Yes | No | Unknown deriving (Show)

answers :: [Answer]
answers = [Yes,No,Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown

data Shape = Circle Float | Rect Float Float deriving (Show)
square  :: Float -> Shape
square n = Rect n n

circum :: Shape -> Float
circum (Circle r) = 2*pi*r
circum (Rect a b) = 2*(a+b)

data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1+ nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat(n+1) = Succ (int2nat n)




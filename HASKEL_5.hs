--sumD n =(sum. map ( (modn) . (div n) . (10^)) ns
--  where
--      ns = [0..length(show n)-1]


twice :: ([a] -> [a]) -> [a] -> [a] 
twice f xs = f (f xs) 

--primes  = 2 : [ n | n <- [3,5..], all (\p-> mod n p==0)]        
        

artt   = "2" : [x ++ [y] ++ "2" | x <- artt , y <- "+-*/" ]

func n = take n [n,n..]

app f (x:xs) = map (f x) xs 

funcc ns = [n|n<-ns, mod n 10 /= 0] 


f x = length(show x)

apply f x = f (f x) 

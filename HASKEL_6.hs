sumD n = (sum . map ((`mod` 10) . (div n) . (10^))) [0,1] 

twice :: ([a] -> [a]) -> [a] -> [a]    
twice f xs = f (f xs)

  
toDec n = foldl (\t b-> length(n)*t^2 ) 0

--func as = [a|(a,n)<-zip as [1..] , notElem a (drop n as)] 

 
--funcc n = mod n 10 : func (2  `div` 10) 

func [n]    =n   
func (n:ns) = func [n+x|x<-ns] 

--funcc 0 =[]
--funcc n = 10 `mod` n : func (10 `div` n)


--fMap :: .......    
fMap f xs = map f (filter f xs)


funnn = head ns : null .ns <- ns

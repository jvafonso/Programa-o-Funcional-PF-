--1)

conta_ch :: [Char] -> Int
conta_ch [] = 0
conta_ch (x : xs) = 1 + conta_ch xs

conta ::  [t] -> Int
conta [] = 0
conta (x  : xs) = 1 + conta xs


maior :: [Int] -> Int 
maior [x] = x
maior (x:y:resto)
  |x > y = maior (x : resto)
  |otherwise = maior (y : resto)



primeiros :: Int -> [t] -> [t]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x : xs) = x : primeiros (n-1) xs


pertence :: Eq t => t -> [t] -> Bool
pertence a [] = False
pertence a (x:z) = if (a == x) then True else pertence a z


uniaoR :: Eq t => [t] -> [t] -> [t]
uniaoR [] l = l
uniaoR (x:xs) l = if pertence x l then uniaoR xs l else x : uniaoR xs l


--2)

quanti_par :: [Int] -> Int
quanti_par [] = 0
quanti_par list =  length([x | x<-list, mod x 2 == 0])

--3)
prod_list :: [Int] -> Int
prod_list [] = 1
prod_list (x:xs) = x * prod_list xs


--4)

comprime :: [[t]] -> [t]
comrime [] = []
comprime [x] = x
comprime (x:xs) = x ++ comprime xs


--5)
tamanho :: [t] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

--6)
uniaoR2 ::[Int] -> [Int] -> [Int]
uniaoR2 list1 [] = list1
uniaoR2 [] list2 = list2
uniaoR2 list1 list2 = list1 ++ nao_repet list1 list2

nao_repet :: [Int] -> [Int] -> [Int]
nao_repet list1 list2 = [x | x<-list2, elem x list1 == False]
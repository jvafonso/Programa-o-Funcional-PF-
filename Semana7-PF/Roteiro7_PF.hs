--1)

paridade :: [Int] -> [Bool]
paridade lista =  map (even) lista

--2)

prefixo :: [String] -> [String]
prefixo lista = map  (take 3) lista

--3)

saudacao :: [String] -> [String]
saudacao lista = map ("Oi " ++) lista

--4)

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar p xs = [x | x <- xs, p x]

--5)
pares :: [Int] -> [Int]
pares lista = filter (even) lista

--6)

filtro :: Int -> Bool
filtro x  | (5*x + 6) < (x*x) = True
          | otherwise = False

solucoes :: [Int] -> [Int]
solucoes lista = filter (filtro) lista

--7)

maior :: [Int] -> Int
maior lista = foldr1 max lista

--8)

min10 :: Int -> Int -> Int
min10 x y | x < y && x < 10 = x
          | y < 10 = y
          |otherwise = 10
 
menor_min10 :: [Int] -> Int
menor_min10 (a:x) = foldr (min10) a x


--9)

junta_silabas_plural :: [String] -> [Char]
junta_silabas_plural lista  = foldr (++) "s" lista
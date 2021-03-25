--1) print

--2)print

--3)
--a)
geraL3 :: Int -> Int -> [Int]

geraL3 a b
          |a<b = [a,(a+1)..b]
          |a == b = [a]
          |otherwise = []


--b)
geraL4 :: Int -> Int -> [Int]
geraL4 a b
          | a<b = [x | x<-[a,(a+1)..b], mod x 2 == 0]
          |otherwise = []

--4)print

--5)
quadrados::  Int -> Int -> [Int]
quadrados a b = [x*x | x<-[a,(a+1)..b]]

--6)
seleciona_impares :: [Int] -> [Int]
seleciona_impares n = [x | x<-n, mod x 2 /= 0]

--7)
tabuada :: Int -> [Int]
tabuada a = [x*a| x<-[1..10]]

--8)
bissexto :: Int -> Bool

bissexto a | (mod a 400 == 0) = True
           | (mod a 4 == 0) && (mod a 100 /= 0) = True
           | otherwise = False

bissextoLista :: [Int] -> [Int]
bissextoLista k = [x | x<-k, bissexto x]


--9)
sublistas :: [[Int]] -> [Int]
sublistas p = concat p

--10)
type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo::Emprestimos
bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
 ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
 ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

atrasado :: Emprestimos -> Data -> Emprestimos
atrasado list_emp dma = [x | x<-list_emp, (emprestimo_dia x dma ) == True]


emprestimo_dia :: Emprestimo -> Data -> Bool
emprestimo_dia (codL, codP, (d, m, a), (d1, m1, a1), codS) (d2, m2, a2) = if (precede (d1, m1, a1) (d2, m2, a2)) == True then True else False 

precede :: Data -> Data -> Bool
precede (d1,m1,a1) (d2,m2,a2)
                             | a1 < a2 = True
                             | (m1 < m2) && (a1 == a2) = True
                             | (d1 < d2) && (m1 == m2) && (a1 == a2) = True
                             | otherwise = False



--11)
uniaoNRec :: [Int] -> [Int] -> [Int]
uniaoNRec list1 list2 = list1 ++ [y | y<-list2 , elem y list1 == False]
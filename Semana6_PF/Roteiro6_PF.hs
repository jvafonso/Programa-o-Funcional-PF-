--1)

--a)
type Data = (Int, Int, Int)

valida :: Data -> Bool
valida (d,m,a) 
              | ((d >= 1) && (d <= 31) && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12)) = True
              | ((d >= 1) && (d <= 30) && (m == 4 || m == 6 || m == 9 || m == 11)) = True
              | (d == 28) && (m == 2) = True
              | (d == 29) && (m == 2) && (bissexto2(d,m,a) == True) = True
              | otherwise = False

              where
                bissexto2 :: Data -> Bool
                bissexto2 (d,m,a) = if (bissexto a == True) then True else False
                bissexto :: Int -> Bool
                bissexto a | (mod a 400 == 0) = True
                           | (mod a 4 == 0) && (mod a 100 /= 0) = True
                           | otherwise = False

--b)

bissextos :: [Int] -> [Int]
bissextos k = [x | x<-k, bissextoB x]
  where
      bissextoB :: Int -> Bool
      bissextoB a | (mod a 400 == 0) = True
             | (mod a 4 == 0) && (mod a 100 /= 0) = True
             | otherwise = False



--c)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]



atrasado :: Emprestimos -> Data -> Emprestimos
atrasado bdEmprestimo dma = [x | x<-bdEmprestimo, (emprestimo_dia x dma ) == True]
  where
     bdEmprestimo::Emprestimos
     bdEmprestimo =
       [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
       ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
       ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

     emprestimo_dia :: Emprestimo -> Data -> Bool
     emprestimo_dia (codL, codP, (d, m, a), (d1, m1, a1), codS) (d2, m2, a2) = if (precede (d1, m1, a1) (d2, m2, a2)) == True then True else False 

     precede :: Data -> Data -> Bool
     precede (d1,m1,a1) (d2,m2,a2)
                              | a1 < a2 = True
                              | (m1 < m2) && (a1 == a2) = True
                              | (d1 < d2) && (m1 == m2) && (a1 == a2) = True
                              | otherwise = False


--d)
fibo2 :: Int -> Int
fibo2 x = fiboAux x (1, 1)
   where
      fiboAux :: Int -> (Int, Int) -> Int
      fiboAux x (first, second)
                         | x == 1 = first
                         | otherwise = fiboAux (x-1) (passo(first, second))

      passo :: (Int, Int) -> (Int, Int)
      passo (x, y) = (y, x+y)


--e)

factorial n = prodIntervalo 1 n
     where
        prodIntervalo x y
                 |x > y = 1
                 |otherwise = x * prodIntervalo (x+1) y



--2)


--e)
(let prodIntervalo x y
                 |x > y = 1
                 |otherwise = x * prodIntervalo (x+1) y in  prodIntervalo 1 n)







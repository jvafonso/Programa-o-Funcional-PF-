--1)
dobro :: Int -> Int
dobro n = 2*n


quadruplica :: Int -> Int
quadruplica n = 2*(dobro n)


hipotenusa :: Float -> Float -> Float
hipotenusa c1 c2 = sqrt((c1^2) + (c2^2))


distancia :: Float -> Float -> Float -> Float -> Float
distancia xA yA xB yB = sqrt((xB - xA)^2 + (yB - yA)^2)

--3)

conversao :: Float -> (Float, Float, Float)
conversao r = (r, r*3.96, r*4.45)

--4)

bissexto :: Int -> Bool

bissexto a | (mod a 400 == 0) = True
           | (mod a 4 == 0) && (mod a 100 /= 0) = True
           | otherwise = False


--5)

type Data = (Int, Int, Int)

bissexto2 :: Data -> Bool
bissexto2 (d,m,a) = if (bissexto a == True) then True else False

--6)

valida :: Data -> Bool
valida (d,m,a) 
              | ((d >= 1) && (d <= 31) && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12)) = True
              | ((d >= 1) && (d <= 30) && (m == 4 || m == 6 || m == 9 || m == 11)) = True
              | (d == 28) && (m == 2) = True
              | (d == 29) && (m == 2) && (bissexto2(d,m,a) == True) = True
              | otherwise = False


--7)

precede :: Data -> Data -> Bool
precede (d1,m1,a1) (d2,m2,a2)
                             | a1 < a2 = True
                             | (m1 < m2) && (a1 == a2) = True
                             | (d1 < d2) && (m1 == m2) && (a1 == a2) = True
                             | otherwise = False


--8)
type Livro = (String, String, String, String, Int)
type Pessoa = (String, String, String, String)
type Emprestimo = (String, String, Data, Data, String)


--9)
emp1 :: Emprestimo

emp1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")

emprestimo_dia :: Emprestimo -> Data -> String
emprestimo_dia (codL, codP, (d, m, a), (d1, m1, a1), codS) (d2, m2, a2) = if (precede (d1, m1, a1) (d2, m2, a2)) == True then show "Atrasado" else show "Em dia" 
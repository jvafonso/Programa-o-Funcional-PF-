--1)
--a)
--(||) :: Bool -> Boll -> Bool
-- || True True = True
-- || True False = True
-- || False False = False

-- (||) :: Bool -> Boll -> Bool
-- || False False = False
-- || _   _  = True


-- (||) :: Bool -> Boll -> Bool
-- False || b = b 
-- True || _ = True


-- b)
-- if(x == False && y == False) then False else True

-- if(x == True || y == True) then True else False


--2)

distancia :: (Float,Float) -> (Float,Float) -> Float
distancia (xA, yA) (xB, yB) = sqrt((xB - xA)^2 + (yB - yA)^2)

--3)

factorial1 :: Int -> Int
factorial1 n
            |n == 0 = 1
            |otherwise = n*factorial1(n-1)


factorial2 :: Int -> Int
factorial2 0 = 1
factorial2 n = n*factorial2(n-1)


--4)

fibo:: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo 2 = 1
fibo n = fibo(n-2) + fibo(n-1)


--5)

n_tri :: Int -> Int
n_tri 1 = 1
n_tri n = n + n_tri(n-1)


--6)

potencia2 :: Int -> Int
potencia2 0 = 1
potencia2 1 = 2
potencia2 n = 2 * potencia2(n-1)

--7)
--a)
prodIntervalo x y
                 |x > y = 1
                 |otherwise = x * prodIntervalo (x+1) y

--b)
factorial3 n = prodIntervalo 1 n

--8)
div_inteira m n
               |m < n = 0
               |otherwise = 1 + div_inteira (m - n) n

resto_div m n
             |m< n = m
             |otherwise = resto_div (m - n) n


--9)

mdc1 :: (Int,Int) -> Int
mdc1 (m,n)
         | n == 0 = m
         | otherwise = mdc1 (n, (mod m n))


mdc2 :: (Int,Int) -> Int
mdc2 (m,0) = m
mdc2 (m,n) = mdc2 (n, (mod m n))


--10)
bino1 :: (Int,Int) -> Int
bino1 (n,k)
          | k == 0 = 1
          | k == n = 1
          | otherwise = bino1 (n-1,k) + bino1 (n-1,k-1)


bino2 :: (Int,Int) -> Int
bino2 (n,0) = 1
bino2 (n,k) = if (k == n)
then 1
else bino2 (n-1,k) + bino2 (n-1,k-1)

--11)
passo :: (Int, Int) -> (Int, Int)
passo (x, y) = (y, x+y)

fibo2 :: Int -> Int
fibo2 x = fiboAux x (1, 1)

fiboAux :: Int -> (Int, Int) -> Int
fiboAux x (first, second)
 | x == 1 = first
 | otherwise = fiboAux (x-1) (passo(first, second))
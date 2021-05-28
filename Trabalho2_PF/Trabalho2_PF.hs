--Maxley Soares da Costa 11911BCC038
--Joao Vitor Afonso Pereira 11911BCC037
 
import Data.List (sort)
 
l1=[1..2000]
l2=[2000,1999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]
 
--Questao 1
 
--Original
 
bubbleSort :: (Ord a) => [a] -> ([a], Int)
bubbleSort lista = bolhaOrd (lista, 0) ( length lista )
 
bolhaOrd :: (Ord a) => ([a], Int) -> Int -> ([a], Int)
bolhaOrd (lista, cont) 0 = (lista, cont)
bolhaOrd (lista, cont) n = bolhaOrd (troca (lista, cont)) (n-1)
 
troca :: (Ord a) => ([a], Int) -> ([a], Int)
troca ([x], cont) = ([x], cont)
troca ((x:y:zs), cont)
  | x > y = ((y : lista1), cont1)
  | otherwise = ( (x : lista2), cont2)
  where
    (lista1, cont1) = troca ((x:zs), (cont+1))
    (lista2, cont2) = troca ((y:zs), cont)
 
--Variacao 1
 
bubbleSort2 :: (Ord a) => [a] -> ([a], Int)
bubbleSort2 lista = bolhaOrd2 (lista, 0) ( length lista )
 
bolhaOrd2 :: (Ord a) => ([a], Int) -> Int -> ([a], Int)
bolhaOrd2 (lista, cont) 0 = (lista, cont)
bolhaOrd2 (lista, cont) n
  | tenta_troca == nao_troca  = (lista, cont)
  | otherwise = bolhaOrd2 tenta_troca (n-1)
  where
    nao_troca = (lista, cont)
    tenta_troca = troca2 (lista, cont)
 
troca2 :: (Ord a) => ([a], Int) -> ([a], Int)
troca2 ([x], cont) = ([x], cont)
troca2 ((x:y:zs), cont)
  | x > y = ((y : lista1), cont1)
  | otherwise = ( (x : lista2), cont2)
  where
    (lista1, cont1) = troca2 ((x:zs), (cont+1))
    (lista2, cont2) = troca2 ((y:zs), cont)
 
--Variação 2
 
bubbleSort3 :: (Ord a) => [a] -> ([a], Int)
bubbleSort3 lista = bolhaOrd3 (lista, 0) ( length lista )
 
bolhaOrd3 :: (Ord a) => ([a], Int) -> Int -> ([a], Int)
bolhaOrd3 (lista, cont) 0 = (lista, cont)
bolhaOrd3 (lista, cont) n
  | tenta_troca2 == nao_troca = (lista, cont)
  | otherwise = bolhaOrd3 tenta_troca2 (n-1)
  where
    nao_troca = (lista, cont)
    tenta_troca2 = troca_melhorado (lista, cont) (n-1)
 
troca_melhorado :: (Ord a) => ([a], Int) -> Int -> ([a], Int)
troca_melhorado (lista, cont) 0 = (lista, cont)
troca_melhorado ((x:y:zs), cont) n
  | x > y  = (y : lista1, cont1)
  | otherwise = (x : lista2, cont2)
  where
    (lista1, cont1) = troca_melhorado ((x:zs), cont+1) (n-1)
    (lista2, cont2) = troca_melhorado ((y:zs), cont) (n-1)
 
--Questao 2
 
selectionSort :: (Ord a) => [a] -> ([a], Int)
selectionSort lst = (selecao lst, selecao_contador lst)
 
selecao :: (Ord a) => [a]->[a]
selecao [] = []
selecao lst = [m] ++ selecao (remove m lst)
   where m = minimo lst
 
remove :: (Ord a) => a->[a]->[a]
remove x [] = []
remove x (m:lst)
     |x==m = lst
     |otherwise = m : (remove x lst)
 
 
minimo::(Ord a) => [a]-> a
minimo [] = undefined
minimo [m] = m
minimo (m:lst)
  |m <= (minimo lst) = m
  |otherwise = minimo lst
 
selecao_contador :: (Ord a) => [a] -> Int
selecao_contador [] = 0
selecao_contador lst = troca + selecao_contador (remove m lst)
  where
    m = minimo lst
    troca = verifica_troca m lst
 
verifica_troca :: (Ord a) => a -> [a] -> Int
verifica_troca m (y:ys) 
 | m == y = 0
 | otherwise = 1
 
--Questao 3
 
--Original
 
insertionSort :: (Ord a) => [a] -> ([a], Int)
insertionSort lista = insere (lista, 0)
 
insere :: (Ord a) => ([a], Int) -> ([a], Int)
insere ([x], cont) = ([x], cont)
insere ((x:xs), cont) = insereOrd x lista_ordenada
  where
    lista_ordenada = insere (xs, cont)
 
 
insereOrd :: (Ord a) => a -> ([a], Int) -> ([a], Int)
insereOrd x ([], cont) = ([x], cont)
insereOrd x ((y:ys), cont)
  | x < y = ((x:y:ys), cont)
  | otherwise = ( y : lista, cont1)
  where
    (lista, cont1) = insereOrd x (ys, cont+1)
 
--Questao 4
 
--original chamar a funcao contQuick para retornar a lista ordenada junto com a quantidade de comparacoes
 
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (s:xs) = quicksort [x |x <- xs, x < s] ++ [s] ++ quicksort [x |x <- xs, x >= s]
 
contQuick :: (Ord a) => [a] -> ([a], Int)
contQuick [] = ([], 0)
contQuick (s:xs) = (quicksort (s:xs), length(quicksort [x |x <- xs, x < s]) + length(quicksort [x |x <- xs, x >= s]))
 
 
--variacao 1
quicksortV1 :: (Ord a) => [a] -> ([a], Int)
quicksortV1 [] = ([],0)
quicksortV1 lista  = ( sort (fst (divide (head lista) (tail lista))) ++ [head lista] ++  sort (snd (divide (head lista) (tail lista))), length (sort (fst (divide (head lista) (tail lista))) ++ [head lista] ++  sort (snd (divide (head lista) (tail lista)))) -1)
 
 
divide :: (Ord a) => a -> [a] -> ([a],[a])
divide _ [] = ([],[])
divide s (s1:xs)
       | s1 < s = (s1:z, b)
       |otherwise = (z, s1:b)
   where (z, b) = divide s xs
 
 
--variacao 2
quicksortV2 :: (Ord a) => [a] -> ([a], Int)
quicksortV2 [] = ([],0)
quicksortV2 lista = if (length lista >= 3) then (calcQuikV2 (mediana (take 3 lista)) (remove (mediana (take 3 lista)) lista), length (calcQuikV2 (mediana (take 3 lista)) (remove (mediana (take 3 lista)) lista)) - 1) 
else quicksortV1 lista
 
calcQuikV2 :: (Ord a) => a -> [a] -> [a]
calcQuikV2 _ [] = []
calcQuikV2 n listM = sort (fst (divide n listM)) ++ [n] ++ sort (snd (divide n listM))
 
mediana :: (Ord a) => [a] -> a
mediana x = head (drop 1 (sort x))
 
--Questao 5
 
mergeSort :: (Ord a) => [a] -> ([a], Int)
mergeSort lst = merge (lst, 0)
 
merge :: (Ord a) => ([a], Int) -> ([a], Int)
merge ([x], cont) = ([x], 0)
merge (lst, cont) = intercala_merge (merge esquerda) (merge direita)
  where
    n = (length lst) `div` 2
    esquerda = (lista_esquerda lst n, 0)
    direita = (lista_direita lst n, 0)
 
lista_esquerda :: (Ord a) => [a] -> Int -> [a]
lista_esquerda _ 0 = []
lista_esquerda (x:xs) n = x : lista_esquerda xs (n-1)
 
lista_direita :: (Ord a) => [a] -> Int ->[a]
lista_direita lst 0 = lst
lista_direita (x:xs) n = lista_direita xs (n-1)
 
intercala_merge :: (Ord a) => ([a], Int) -> ([a], Int) -> ([a], Int)
intercala_merge ([], cont1) (lst2, cont2) = (lst2, cont1+cont2)
intercala_merge (lst1, cont1) ([], cont2) = (lst1, cont1+cont2)
intercala_merge ((x:xs), cont1) ((y:ys), cont2)
  | x < y = (x : lst1, cont_esq)
  | otherwise = (y : lst2, cont_dir)
  where
    (lst1, cont_esq) = intercala_merge (xs, cont1) ((y:ys), cont2)
    (lst2, cont_dir) = intercala_merge ((x:xs), cont1) (ys, cont2+1) 
 
--Questao 6)
 
data Exp =
 Val Int -- um numero
 | Add Exp Exp -- soma de duas expressoes
 | Sub Exp Exp  -- subtração de duas expressoes
 | Mult Exp Exp -- multiplicacao de duas expressoes
 | Div Exp Exp -- divisao de duas expressoes
 
 
avalia :: Exp -> Int
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mult exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Div exp1 exp2) = (avalia exp1) `div` (avalia exp2)
 
 
eq1,eq2 :: Exp
eq1 = Mult (Add (Val 3) (Val 12)) (Div (Sub (Val 15) (Val 5)) (Mult (Val 1) (Val 3)))
eq2 = (Sub (Val 0) (Mult (Sub (Add (Val 6) (Val 8)) (Add (Val 5) (Val 1))) (Add (Val 2) (Div (Val 6) (Val 2))))) 
 
--Questao 7
 
--a
data Jogada = Pedra | Papel | Tesoura
  deriving (Eq, Show)
 
--b
vence :: Jogada -> Jogada -> Bool
j1 `vence` j2
  | (j1 == Pedra) && (j2 == Tesoura) = True
  | (j1 == Papel) && (j2 == Pedra) = True
  | (j1 == Tesoura) && (j2 == Papel) = True
  | otherwise = False
 
--c
 
vencedoras :: [(Jogada, Jogada)] -> [Jogada]
vencedoras [] = []
vencedoras ((j1, j2): xs)
  | (j1 == j2) = j1 : vencedoras xs
  | j1 `vence` j2 = j1 : vencedoras xs
  | otherwise = j2 : vencedoras xs
 
 
--Questao 8
data Nebuloso = Verdadeiro | Falso | Talvez Float deriving (Show)
 
--b)
fuzzifica :: Float -> Nebuloso
fuzzifica n
     | n <= 0 = Falso
     | n >= 1 = Verdadeiro
     | otherwise = Talvez n
 
 
--c)
verifica_alto :: Float -> Nebuloso
verifica_alto altura = fuzzifica ((altura - 1.70)/0.20)
 
--d)
verifica_barato :: Float -> Nebuloso
verifica_barato custo = fuzzifica ((50000 - custo)/20000)
 
 
--Questao 9
 
--a
type Ano = Int
type NomeColegio = String
type MatriculaColegio = Int
type Altura = Float
type Peso = Float
 
type NomeUniversidade = String
type NomeCurso = String
type MatriculaUniversidade = Int
type Idade = Int
 
data Estudante = Colegial Ano NomeColegio MatriculaColegio Altura Peso
                |Universitario NomeUniversidade NomeCurso MatriculaUniversidade Altura Idade
                deriving (Show)
 
e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20 :: Estudante
 
e1 = Colegial 1 "Nacional" 12345 1.65 60.5
e2 = Colegial 2 "Olimpo" 14256 1.68 59.3
e3 = Colegial 3 "Gabarito" 16278 1.60 64.9
e4 = Colegial 2 "Nacional" 76362 1.72 80.4
e5 = Colegial 3 "Gabarito" 83632 1.79 86.8
e6 = Colegial 1 "Nacional" 36268 1.70 82.7
e7 = Colegial 2 "Olimpo" 27443 1.88 92.2
e8 = Colegial 1 "Gabarito" 23895 1.85 90.3
e9 = Colegial 3 "Olimpo" 16135 1.93 102.0
e10 = Colegial 3 "Nacional" 12728 1.97 100.5
 
e11 = Universitario "UFU" "Computacao" 166373220 1.65 24
e12 = Universitario "UNITRI" "Medicina" 04856790 1.62 26
e13 = Universitario "UNA" "Direito" 12345678 1.69 23
e14 = Universitario "UNITRI" "Medicina" 45215367 1.72 22
e15 = Universitario "UFU" "Musica" 17425732 1.77 20
e16 = Universitario "UNA" "Medicina" 12462683 1.88 18
e17 = Universitario "UNITRI" "Computacao" 85438326 1.82 20
e18 = Universitario "UFU" "Musica" 35725721 1.89 24
e19 = Universitario "UNA" "Direito" 16265332 1.91 28
e20 = Universitario "UFU" "Computacao" 46268246 1.99 26
 
--b
listaEstudantes :: [Estudante]
listaEstudantes = [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20]
 
descobre_alto :: Estudante -> (Int, Nebuloso)
descobre_alto (Colegial _ _ matricula altura _) = (matricula, verifica_alto altura)
descobre_alto (Universitario _ _ matricula altura _) = (matricula, verifica_alto altura)
 
descobre_altos :: [Estudante] -> [(Int, Nebuloso)]
descobre_altos lista = map descobre_alto lista
 
--Questao 10)
 
data ArvBinInt = Nulo |
                    No Int ArvBinInt ArvBinInt
                    deriving (Show)
 
arvrEx, arvrEx2 :: ArvBinInt
arvrEx = (No 1 
             (No 2 Nulo Nulo) (No 3 Nulo Nulo))
 
 
arvrEx2 = (No 2 
              (No 7 
                  (No 2 Nulo Nulo) (No 6 
                                       (No 5 Nulo Nulo) (No 11 Nulo Nulo ))) 
              (No 5 
                  Nulo (No 9 
                           (No 4 Nulo Nulo) Nulo)))
 
folhas :: ArvBinInt -> [Int]
folhas Nulo = []
folhas (No n Nulo Nulo) = [n]
folhas (No _ esquerda direita) = folhas esquerda ++ folhas direita
 
somaNosInternos :: ArvBinInt -> Int
somaNosInternos Nulo = 0
somaNosInternos (No x Nulo Nulo) = 0
somaNosInternos (No x direita esquerda) = somaNosInternos esquerda + somaNosInternos direita + x
 
 
pertence :: Int -> ArvBinInt -> Bool
pertence x Nulo = False
pertence x (No n esquerda direita)
                   |x == n = True
                   |otherwise = pertence x esquerda || pertence x direita
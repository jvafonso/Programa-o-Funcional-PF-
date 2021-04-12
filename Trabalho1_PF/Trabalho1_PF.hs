--Maxley Soares da Costa 11911BCC038
--Joao Vitor Afonso Pereira 11911BCC037
 
--Questao 1
analisa_raiz :: Int -> Int -> Int -> [Char]
analisa_raiz a b c 
 |a == 0 = "4-equacao degenerada"
 |b*b > 4*a*c = "1-possui duas raizes reais"
 |b*b == 4*a*c = "2-possui uma raiz real"
 |otherwise = "3-nenhuma raiz real"
 
 
--Questao 2
equacao :: Float -> Float -> Float -> (Float, Float)
equacao a b c
 | a /= 0 = ( (-b + sqrt(b^2 - 4*a*c))/(2*a) , (-b - sqrt(b^2 - 4*a*c))/(2*a) )
 | otherwise = (a, (-c)/b )
 
--Questao 3
type Data = (Int, Int, Int)
 
passagem :: Float -> Data -> Data ->  Float
passagem preco (d1,m1,a1) (d2,m2,a2)
 |(a1 - a2) <= 2 = preco*0.15
 |(a1 - a2) <= 10 = preco*0.40
 |(a1 - a2) >= 70 = preco*0.5
 |otherwise = preco
 
--Questao 4
l1 :: [Int]
l1 = [15,16]
 
gera1 = [x^3 | x <- [3..11], even x]
 
gera2 = [(x, y) | x <- [1..5], y <- [x..3*x]]
 
gera3 = [x | x <- [1..l1!!0]] ++ [y | y <- [1..l1!!1]]
 
gera4 = zip [x | x <- [2, 4..10]] [y | y <- [3, 5..9]]
 
gera5 = [fst x + snd x | x <- gera4]
 
 
--Questao 5
--a)
contaNegM2 :: [Int] -> Int
contaNegM2 list1 = length([x | x<-list1, x>0, mod x 3 == 0])
 
 
--b)
listaNegM2 :: [Int] -> [Int]
listaNegM2 list1 = [x | x<-list1, x>0, mod x 3 == 0]
 
 
--Questao 6
fatores :: Int -> [Int]
fatores n = [x | x <- [1..n], mod n x == 0]
 
primos :: Int -> Int -> [Int]
primos a b = [x | x <- [a..b], fatores x == [1, x]]
 
--Questao 7
mdc::Int->Int->Int
mdc a b 
       | a < b = mdc b a
       | b == 0 = a
       | otherwise = mdc b (mod a b)
 
mmc::Int->Int->Int
mmc x y = (x * y) `div` (mdc x y)
 
mmc3::Int->Int->Int->Int
mmc3 x y z = mmc x (mmc y z)
 
--Questao 8
serie :: Float -> Int -> Float
serie x 1 = 1/x
serie x n
 | even n = x/(fromIntegral n) + serie x (n-1)
 | otherwise = (fromIntegral n)/(x) + serie x (n-1)
 
--Questao 9
fizzbuzz :: Int -> [[Char]]
fizzbuzz num = reverse (fizzbuzzT num)
 
fizzbuzzT :: Int ->[[Char]]
fizzbuzzT 0 = []
fizzbuzzT num = if(mod num 2 == 0 && mod num 3 == 0) 
 then ["Fizzbuzz"] ++ fizzbuzzT (num-1)
 
 else if(mod num 2 == 0)
 then ["Fizz"] ++ fizzbuzzT (num-1)
 
 else if(mod num 3 == 0)
 then ["Buzz"] ++ fizzbuzzT (num-1)
 
 else ["No"] ++ fizzbuzzT (num-1)
 
--Questao 10
seleciona_multiplos :: Int -> [Int] -> [Int]
seleciona_multiplos n lista = [x | x <- lista, mod x n == 0]
 
--Questao 11
contador :: Int -> [Int] -> Int
contador _ [] = 0
contador n (x : xs)
 |n==x = 1 + contador n xs
 |otherwise = contador n xs
 
unica_ocorrencia :: Int -> [Int] -> Bool
unica_ocorrencia _ [] = False
unica_ocorrencia n list2
 |contador n list2 /= 1 = False
 |otherwise = True
 
--Questao 12
intercala :: [Int] -> [Int] -> [Int]
intercala [] [] = []
intercala [] lista2 = lista2
intercala lista1 [] = lista1
intercala lista1 lista2 = [(head lista1)] ++ [(head lista2)] ++ intercala (tail lista1) (tail lista2)
 
--Questao 13
zipar :: [Int] -> [Int] -> [[Int]]
zipar [] [] = []
zipar (x : xs) [] = [[x]]
zipar [] (y : ys) = [[y]]
zipar (x : xs) (y : ys) = [[x,y]] ++ zipar xs ys
 
--Questao 14
type Contato = (String, String, String, String)
 
contatos :: [Contato]
contatos = [("Maxley", "Rua A", "91111-1111", "maxley@gmail.com"),
            ("Joao Vitor", "Rua B", "92222-22222", "joao@gmail.com"),
            ("Astolfo", "Rua C", "93333-3333", "astolfinho@gmail.com")]
 
recupera :: String -> [Contato] -> String
recupera email [] = "Email desconhecido"
recupera email ((nome, _, _, email_contato):xs)
 | email == email_contato = nome
 | otherwise = recupera email xs
 
--Questao 15
type Pessoa = (String, Float, Int, Char)
pessoas :: [Pessoa]
pessoas = [ ("Rosa", 1.66, 27,'S'),
 ("João", 1.85, 26, 'C'),
 ("Maria", 1.55, 62, 'S'),
 ("Jose", 1.78, 42, 'C'), 
 ("Paulo", 1.93, 25, 'S'),
 ("Clara", 1.70, 33, 'C'),
 ("Bob", 1.45, 21, 'C'),
 ("Rosana", 1.58,39, 'S'),
 ("Daniel", 1.74, 72, 'S'),
 ("Jocileide", 1.69, 18, 'S') ]
 
type Result1 = (String, Char)
 
--a)
altura_media :: [Pessoa] -> Float
altura_media list_p = sum (altura_mediaL list_p) / (fromIntegral $ length (altura_mediaL list_p))
 
altura_mediaL :: [Pessoa] -> [Float]
altura_mediaL [] = []
altura_mediaL (x : xs) = separa_altura x ++ altura_mediaL xs
 
 
separa_altura :: Pessoa -> [Float]
separa_altura (nome, at, idade, est_civ) = [at]
 
 
--b)
menor_idade :: [Pessoa] -> Int
menor_idade list_i = minimum (idadeL list_i)
 
 
idadeL :: [Pessoa] -> [Int]
idadeL [] = []
idadeL (x : xs) = separa_idade x ++ idadeL xs
 
separa_idade :: Pessoa -> [Int]
separa_idade (nome, at, idade, est_civ) = [idade]
 
--c) 
nome_est_velho :: [Pessoa] -> [String]
nome_est_velho (x : xs) = if (separa_idade2 x == maximum (idadeL (x : xs))) then separa_dados x else nome_est_velho xs
 
separa_idade2 :: Pessoa -> Int
separa_idade2 (nome, at, idade, est_civ) = idade
 
 
separa_dados :: Pessoa -> [String]
separa_dados (nome, at, idade, est_civ) = [nome,[est_civ]]
 
 
--d)
dados_velho :: [Pessoa] -> [Pessoa]
dados_velho [] = []
dados_velho (x : xs) = if (separa_idade2 x >= 50) then [x] ++ dados_velho xs else dados_velho xs
 
--e) 
numero_casados :: [Pessoa] -> Int -> Int
numero_casados list_p id_max= contador_pessoas list_p id_max
 
contador_pessoas :: [Pessoa] -> Int -> Int
contador_pessoas [] id_max = 0
contador_pessoas (x : xs) id_max
 |(separa_idade2 x >= id_max) && (separa_civil x == 'C') = 1 + contador_pessoas xs id_max
 |otherwise = contador_pessoas xs id_max
 
separa_civil :: Pessoa -> Char
separa_civil (nome, at, idade, est_civ) = est_civ
 
 --Questao 16
insere_ord :: (Ord a) => a -> [a] -> [a]
insere_ord n [] = [n]
insere_ord n (x:xs)
 | n < x = [n] ++ (x:xs)
 | otherwise = [x] ++ insere_ord n xs
 
--Questao 17
reverte :: [t] -> [t]
reverte list_r = reverse list_r
 
--Questao 18
elimina_repet:: Eq a => [a] -> [a]
elimina_repet [] = []
elimina_repet (x: xs)
 | elem x xs = elimina_repet xs
 | otherwise = [x] ++ elimina_repet xs
 
--Questao 19
disponiveis :: [Int]
disponiveis = [1,2,5,10,20,50,100]
 
notasTroco :: Int -> [[Int]]
notasTroco 0 = [[]]
notasTroco num_pago = [p:ps | p <- disponiveis, num_pago >= p, ps <- notasTroco (num_pago - p)]
 
--Questao 20
diagonal :: [Int] -> [Int] -> Bool
diagonal [x1, y1] [x2, y2]
 | abs(x1-x2) == abs(y1-y2) = True
 | otherwise = False
 
vertical :: [Int] -> [Int] -> Bool
vertical [x1, _] [x2, _]
 | x1 == x2 = True
 | otherwise = False
 
horizontal :: [Int] -> [Int] -> Bool
horizontal [_, y1] [_, y2]
 | y1 == y2 = True
 | otherwise = False
 
ataca :: [Int] -> [Int] -> Bool
ataca rainha1 rainha2
 | horizontal rainha1 rainha2 || vertical rainha1 rainha2 || diagonal rainha1 rainha2  = True
 | otherwise = False
 
ataca_lista :: [Int] -> [[Int]] -> Bool
ataca_lista [] _ = False
ataca_lista _ [] = False
ataca_lista rainha (x:xs)
 | ataca rainha x = True
 | otherwise = ataca_lista rainha xs
 
posiciona :: Int -> Int -> Int -> [[Int]] -> [[Int]]
posiciona x y n posicionadas
 | x > n || y > n = []
 | ataca_lista [x,y] posicionadas = posiciona (x+1) y n posicionadas
 | otherwise = [[x,y]] ++ posiciona 1 (y+1) n (posicionadas ++ [[x,y]])
 
posicionaN_yFixo :: Int -> Int -> Int -> [[[Int]]]
posicionaN_yFixo x y n
 | x > n || y > n = []
 | length ( posiciona x y n [] ) == n = (posiciona x y n []) : (posicionaN_yFixo (x+1) y n)
 | otherwise = posicionaN_yFixo (x+1) y n
 
quebraLista :: [[[Int]]] -> [[Int]]
quebraLista [] = []
quebraLista (x:xs) = pegaLinha x : quebraLista xs
 
pegaLinha :: [[Int]] -> [Int]
pegaLinha [] = []
pegaLinha ([x,y]:xs) = [x] ++ pegaLinha xs
 
nRainhas :: Int -> [[Int]]
nRainhas n = quebraLista (posicionaN_yFixo 1 1 n)
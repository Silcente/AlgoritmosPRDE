{-Algoritmo de las abejas-}

import System.Random

type Posicion =((Int, Int), Int, Int) -- Posicion consta de la posicion del mallado, la calidad/cantidad del nectar y el numero de veces que ha preservado la misma posicion

numerosAleatorios :: Int -> [Float]
numerosAleatorios n = take n (randomRs (0,10) gen)
    where gen = mkStdGen 2022

mallado :: Int -> Int -> [[Posicion]] -- Rellenamos una malla de n*m con posiciones cada una con sus datos (falta rellenar las calidades de forma arbitraria) esto conforma un campo inicial de estudio 
mallado 0 n = [anadir_fila 0 n]
mallado m n = [(anadir_fila m n)] ++ (mallado (m-1) n)

anadir_fila :: Int -> Int -> [Posicion] --Para hacer el mallado primero rellenamoas las filas y para luego irlas añadiendo en la lista de listas 
anadir_fila m 0 = [((m,0), 30, 1)] --El 30 debe ser un numero aleatorio
anadir_fila m n = [((m,n), 30, 1)] ++ (anadir_fila m (n-1)) 
    --where aleatorios = numerosAleatorios (m**2)
--Falta contemplar el caso en el que m es mas grande de los dos  
{-
{-funcion maximo para la segunda posicion de las tuplas en la lista de tuplas de tres 
Si llega al mayor numero de iteraciones que devuelva una nueva posicion rando-}
buscarmejorvecina :: Int -> Posicion -> Posicion  -- Recibe el maximo de iteraciones con la misma posicion y la correspondiente posicion devuelve la nueva posicion que mejora las condiciones a la anterior 
buscarmejorvecina s ((m,n), y, i)  
    | s==i = ((5,4), 30, i)--Devolvemos una posicion random que tenemos que inventarnos es el caso en el que la abeja en espera rechaza esa zona y la exploradora busca un sitio nuevo 
    | otherwise = maximo (alrededor ([]) ((m,n), y, (i+1))) --Seguro que se puede hacer con funciones de orden superior 

--En lugar de [] hay que poner mallado m n pero lo voy a probar asi para ver los errores 
maximo :: [Posicion] -> Posicion
maximo [x]= x
maximo (x:y:xs) 
    |snd3 x > snd3 y = max x (maximo xs) 
    |otherwise = max y (maximo xs)

snd3 :: Posicion -> Int
snd3 (x,y,z) = y

alrededor :: [[Posicion]] -> Posicion -> [Posicion] --Recibe el mallado y devuelve una lista con las posiciones alrededor 
alrededor (xs:ys:xss) ((m,n), y, i)
    |m==0 && n==0 = [xs!!1] ++ (elemyderecha 0 (xss!!1)) --Esquina superior izq
	|m==0 && n==((length xs)-1) = [(xs!!((length xs)-2))] ++ (elemyizquierda ((length xs)-1) ys)   -- Esquina superior der
	|m==((length xss)-1) && n==0 = [(last xss)!!1] ++ (elemyderecha 0 (xss!!((length xss)-2))) --Esquina inferior izq
	|m==((length xss)-1) && n==((length xs)-1) = [((last xss)!!((length xs)-2))] ++ (elemyizquierda ((length xs)-1) (xss!!((length xss)-2)))  -- Esquina inferior der
	|m ==0 && n /=0 && n /= ((length xs)-1) = (defdos (n-1) (xss!!0)) ++ (deftres (n-1) (xss!!1)) --Primera fila sin las esquinas 
	|m == ((length xss)-1) && n /=0 && n /= ((length xs)-1) = (defdos (n-1) (xss!!(m-1))) ++ (deftres (n-1) (xss!!(m-2))) --Ultima fila sin las esquinas 
	|m /=0 && m/=((length xss)-1) && n ==0 = [((xss!!(m-1))!!1)] ++ (elemyderecha 0 (xss!!(m-2))) ++ (elemyderecha 0 (xss!!(m)))  -- Primera columna sin las esquinas
	|m /=0 && m/=((length xss)-1) && n ==((length xs)-1) = [(xss!!(m-1))!!(n-2)] ++ (elemyizquierda (n-1) (xss!!(m-2))) ++ (elemyizquierda (n-1) (xss!!m))	-- Ultima columna sin las esquinas
	|otherwise = (defdos (n-1) (xss!!m-1)) ++ (deftres (n-1) (xss!!(m-2))) ++ (deftres (n-1) (xss!!m)) --Cualquier cuadrado interior 
	
defdos :: Int -> [Posicion] -> [Posicion] -- Dado el elemento central (la n posicion desde 1) devuelve una lista con las dos posiciones a los lados en horizontal 
defdos n xs = [xs!!(n-2)] ++[xs!!n]

deftres :: Int -> [Posicion] -> [Posicion] -- Dada una posicion devuelve una lista con los tres elementos en horizontal que lo tienen a el por central 
deftres n xs = [xs!!(n-2)] ++ [xs!!(n-1)] ++ [xs!!n]

elemyderecha:: Int -> [Posicion] -> [Posicion] --Dada una posicion devuelve una lista con esa posicion y la contigua a la derecha
elemyderecha n xs = [xs!!(n-1)] ++ [xs!!n]

elemyizquierda:: Int -> [Posicion] -> [Posicion] --Dada una posicion devuelve una lista con esa posicion y la contigua a la izquierda
elemyizquierda n xs = [xs!!(n-1)] ++ [xs!!(n-2)]


--type Enjambre = ([Posicion], Int) -- Enjambre consta de la lista de las posiciones y el tamaño del enjambre
--No necesito el tamaño del enjambre [Posicion] corresponde con el enjambre
algoritmo :: [Posicion] -> Int -> Int -> Posicion -- Recibe el enjambre, el criterio de parada y devuelve la posicion resultado de ejecutar el algoritmo ( la fuente de alimento mas optima)
algoritmo xs 0 s= maximo xs -- funcion auxiliar devuelve la posicion que tenga mayor calidad de nectar
algoritmo xs n s= algoritmo (map (buscarmejorvecina s) xs) (n-1) s --Donde s es el valor para cuando debemos cambiar de zona segun cuando nos hayamos quedado pillados en una posicion

--Obs: Puede que el maximo no funcione porque lo vamos a redefinir para triplas 

--Obs: Hay dos criterios de parada el del algoritmo y el de cuando cambiamos de posicion porque se ha quedado pillado en una posicion

--Obs: Habra que poner seeds para tener el mismo mallado en todo el algoritmo 

--Hay que cambiar el maximo basicamente como hacer referencia a la segunda posicion  de una tripla 
-}

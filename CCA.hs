{-Algoritmo de las abejas-}

type Posicion =((Int, Int), Int, Int) -- Posicion consta de la posicion del mallado, la calidad/cantidad del nectar y el numero de veces que ha preservado la misma posicion

mallado :: Int -> Int -> [[Posicion]] -- Rellenamos una malla de n*m con posiciones cada una con sus datos (falta rellenar las calidades de forma arbitraria) esto conforma un campo inicial de estudio 
mallado 1 n = [añadir_fila 1 n]				--El 30 debe ser un numero aleatorio
mallado m n = (añadir_fila m n) ++ (mallado m-1 n)

añadir_fila :: Int -> Int -> [Posicion] 		--Para hacer el mallado primero rellenamoas las filas y para luego irlas añadiendo en la lista de listas 
añadir_fila m 1 = [((m,1), 30, 1)]					--El 30 debe ser un numero aleatorio
añadir_fila m n = [((m,n), 30, 1)] ++ (añadir_fila m n-1) 

{-funcion maximo para la segunda posicion de las tuplas en la lista de tuplas de tres 
Si llega al mayor numero de iteraciones que devuelva una nueva posicion rando-}
buscarmejorvecina :: Int -> Posicion -> Posicion  -- Recibe el maximo de iteraciones con la misma posicion y la correspondiente posicion devuelve la nueva posicion que mejora las condiciones a la anterior 
buscarmejorvecina s ((m,n), y, i)  
	| s==i = ((5,4), 30, i)--Devolvemos una posicion random que tenemos que inventarnos es el caso en el que la abeja en espera rechaza esa zona y la exploradora busca un sitio nuevo 
	| otherwise = maximo (alrededor ((m,n), y, i+1)) --Seguro que se puede hacer con funciones de orden superior 

maximo :: [Posicion] -> Posicion
maximo [x]= x
maximo (x:y:xs) 
	|snd3 x > snd3 y = maximo x:xs 
	|otherwise = maximo y:xs

snd3 :: (a,a,a) -> a
snd3 (x,y,z) = y

alrededor :: [[Posicion]] -> Posicion -> [Posicion] --Recibe el mallado y devuelve una lista con las posiciones alrededor
alrededor (xs:xss) ((1,1), y, i) = deftake 2 xs ++ elemyderecha 1 (deftake2 2 xss) --Esquina superior izq 
alrededor (xs:ys:xss) ((1, length(xs)), y, i) = deftake (length(xs)-1) xs ++ deftake (length(xs)) ys ++ deftake (length(xs)-1) ys   -- Esquina superior der
alrededor (xs:xss) ((length(xss), 1), y, i) = deftake 2 (last xss) ++ deftake 1 (deftake2 (length(xss)-1) xss) ++ deftake 2 (deftake2 (length(xss)-1) xss)  --Esquina inferior izq
alrededor (xs:xss) ((length(xss), length(xs)), y, i) = deftake (length(xs)-1) (last xss) ++ elemyizquierda (length xs) (deftake2 (length(xss)-1) xss)  -- Esquina inferior der
alrededor (xs:xss) ((m,n), y, i)
	|m ==1 && n \=1 && n \= length(xs) = defdos n (deftake2 1 xss) ++ deftres n (deftake2 2 xss) --Primera fila sin las esquinas 
	|m == (length(xss)) && n \=1 && n \= length(xs) = defdos n (deftake2 m xss) ++ deftres n (deftake2 (m-1) xss) --Ultima fila sin las esquinas 
	|m \=1 && m\=length(xss) && n ==1 = deftake 2 (deftake2 m xss) ++ elemyderecha 1 (deftake2 (m-1) xss) ++ elemyderecha 1 (deftake2 (m+1) xss)  -- Primera columna sin las esquinas
	|m \=1 && m\=length(xss) && n ==length(xs) = deftake (n-1) (deftake2 m xss) ++ elemyizquierda n (deftake2 (m-1) xss) ++ elemyizquierda n (deftake2 (m+1) xss)	-- Ultima columna sin las esquinas
	|otherwise = defdos n (deftake2 m xss) ++ deftres n (deftake2 m-1 xss) ++deftres n (deftake2 m+1 xss) --Cualquier cuadrado interior 
	
deftake :: Int -> [Posicion] -> Posicion --Para las listas 
deftake n xs = last (take n xs) 

deftake2 :: Int -> [[Posicion]] -> [Posicion] --Para las listas de listas 
deftake n xss = last (take n xss) 

defdos :: Int -> [Posicion] -> [Posicion] -- Dado el elemento central (la n posicion desde 1) devuelve una lista con las dos posiciones a los lados en horizontal 
defdos n xs = deftake (n-1) xs ++deftake (n+1) xs

deftres :: Int -> [Posicion] -> [Posicion] -- Dada una posicion devuelve una lista con los tres elementos en horizontal que lo tienen a el por central 
deftres n xs = deftake (n-1) xs ++ deftake n xs ++deftake (n+1) xs

elemyderecha:: Int -> [Posicion] -> [Posicion] --Dada una posicion devuelve una lista con esa posicion y la contigua a la derecha
elemyderecha n xs = deftake n xs ++ deftake (n+1) xs 

elemyizquierda:: Int -> [Posicion] -> [Posicion] --Dada una posicion devuelve una lista con esa posicion y la contigua a la izquierda
elemyizquierda n xs = deftake n xs ++ deftake (n-1) xs





type Enjambre = ([Posicion], Int) -- Enjambre consta de la lista de las posiciones y el tamaño del enjambre
 
algoritmo :: Enjambre -> Int -> Int -> Posicion -- Recibe el enjambre, el criterio de parada y devuelve la posicion resultado de ejecutar el algoritmo ( la fuente de alimento mas optima)
algoritmo xs 0 s= maximo xs -- funcion auxiliar devuelve la posicion que tenga mayor calidad de nectar
altoritmo xs n s= algortimo (map buscarmejorvecina s xs) (n-1) --Donde s es el valor para cuando debemos cambiar de zona segun cuando nos hayamos quedado pillados en una posicion

--Obs: Puede que el maximo no funcione porque lo vamos a redefinir para triplas 

--Obs: Hay dos criterios de parada el del algoritmo y el de cuando cambiamos de posicion porque se ha quedado pillado en una posicion



--Hay que cambiar el maximo basicamente como hacer referencia a la segunda posicion  de una tripla 



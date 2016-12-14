-- Trábajo Practico "Who Watches The Watchmen?"
-- Agustin Devesa

--Definicion de Tipos
--Los vigilantes se definen con un nombre (String), una lista de poderes (lista de Strings) y su anio de aparicion del superheroe. 
--Y todos ellos forman una upla de 3 componentes (String, [String], Int)

import Data.List 

type ListaDeVigilantes = [(String, [String], Integer)]
type UnVigilante = (String, [String], Integer)

algunosVigilantes = [ ("El Comediante", ["Fuerza"], 1942), 
					  ("Buho Nocturno", ["Lucha", "Ingenierismo"], 1963), 
					  ("Rorschach", ["Perseverancia", "Deduccion", "Sigilo"], 1964), 
					  ("Espectro de Seda", ["Lucha", "Sigilo", "Fuerza"], 1962),
					  ("Ozimandias", ["Inteligencia", "Mas Inteligencia Aun"], 1968),
					  ("Buho Nocturno", ["Lucha", "Inteligencia", "Fuerza"], 1939),
					  ("Espectro de Seda", ["Lucha", "Sigilo"], 1940)
					]
					
-- agentesDelGobierno es una lista de tuplas de dos componentes. El primero es de nombres y el segundo de donde provienen. [(String, String)]					

agentesDelGobierno = [("Jack Bauer","24"), ("El Comediante", "Watchmen"), ("Dr. Manhattan", "Watchmen"), ("Liam Neeson", "Taken")]

--Eventos:
--destruccion de NiuShork: muere Rorschach y se retira el Dr Manhattan 
--muerte de un vigilante (es un "retiro" forzoso, digamos... Deja de pertenecer a la agrupacion) (mueren/se retiran todos los que se llamen de igual manera)
--guerra de Vietnam: a los vigilantes que ademas son agentes del gobierno les agrega Cinismo como habilidad, a los restantes no.
--accidente de laboratorio: en un anio dado, aparece un nuevo vigilante, el Doctor Manhattan. Tiene una unica habilidad que es la manipulacion de la materia a nivel atomico. 
--acta de Keene: se van del grupo los heroes viejos (para los que existe al menos un sucesor) mientras que el sucesor permanece.


--Defino nombreVigilante habilidadVigilante y anioVigilante para conseguir el primer, segundo y tercer elemento de una tupla de 3 componentes
nombreVigilante :: (a , b , c ) -> a
nombreVigilante (a , _ , _) = a

habilidadVigilante ::(a,b,c) -> b
habilidadVigilante (_ , b , _) = b

anioVigilante :: (a,b,c) -> c
anioVigilante (_ , _ , c ) = c

--Defino el evento muerteVigilante
muerteVigilante :: ListaDeVigilantes -> String -> ListaDeVigilantes
muerteVigilante superheroes nombre = filter ( (/= nombre) . nombreVigilante ) superheroes

--Defino el evento destruccion de NiuShork a partir de muerteVigilante
destruccionNiuShork :: ListaDeVigilantes -> ListaDeVigilantes
destruccionNiuShork superheroesx = muerteVigilante (muerteVigilante superheroesx "Rorschach") "Dr Manhattan"  

--Agrega UN solo poder a un vigilante
agregarPoder ::  String ->  UnVigilante -> UnVigilante
agregarPoder poder (nombre , poderes , anio) = (nombre , [poder] ++ poderes , anio)

--Indica si el vigilante es agente del gobierno
esAgente :: UnVigilante -> Bool
esAgente (nombre , poderes , anio) = any ( == nombre ) (map fst agentesDelGobierno)

--Defino guerraVietnam
guerraVietnam :: ListaDeVigilantes -> ListaDeVigilantes
guerraVietnam superheroeses = (map (agregarPoder "Cinismo") (filter esAgente superheroeses)) ++ (filter (not . esAgente) superheroeses)

-- agregarSuperHeroe , funcion para modularizar 
agregarSuperHeroe :: String -> [String] -> Integer -> ListaDeVigilantes -> ListaDeVigilantes
agregarSuperHeroe nombre poder anyo listadevigilantes = listadevigilantes ++ [(nombre, poder , anyo )] 

--Defino accidenteDeLaboratorio
accidenteDeLaboratorio :: ListaDeVigilantes -> Integer -> ListaDeVigilantes
accidenteDeLaboratorio listadesuperheroes anio = agregarSuperHeroe "Doctor Manhattan" ["Manipulacion de materia a nivel atomico"] anio listadesuperheroes

--Defino acta de Keene 
anioMasAlto :: ListaDeVigilantes -> ListaDeVigilantes
anioMasAlto [x] = [x]
anioMasAlto (x:xs)
	| ((anioVigilante x)  >= (maximum (map anioVigilante xs)))  =  x : anioMasAlto xs
	| otherwise = anioMasAlto (xs ++ [x])

nombresIguales :: UnVigilante -> UnVigilante -> Bool	
nombresIguales (x,_,_) (y,_,_) = x==y

actaDeKeene :: ListaDeVigilantes -> ListaDeVigilantes
actaDeKeene listaDeVigilantes = nubBy nombresIguales (anioMasAlto listaDeVigilantes)

--nombreDelSalvador: Dado un conjunto de vigilantes, obtener el nombre de aquél que más habilidades tenga luego de la destrucción de NiuShork.
--elElegido: Recibiendo a varios vigilantes, devolver la primera habilidad del vigilante cuyo nombre está formado por más palabras, a continuación de la guerra de Vietnam.
--patriarca: A partir de una lista de vigilantes, saber la edad del más antiguo vigilante, habiendo firmado previamente el acta de Keene

--Modulo cantMaximaPoderes
cantMaximaPoderes :: ListaDeVigilantes -> Int
cantMaximaPoderes listadesuperheroes = maximum (map length (map habilidadVigilante (destruccionNiuShork listadesuperheroes)))

--Funcion todosLosNombres
todosLosNombres :: ListaDeVigilantes -> [String]
todosLosNombres listadevigilantes = (map nombreVigilante listadevigilantes)

--Funcion nombreDelSalvador
nombreDelSalvador :: ListaDeVigilantes -> String
nombreDelSalvador listadesuperheroes = head(map nombreVigilante (filter ( ((<=) (cantMaximaPoderes listadesuperheroes)). length . habilidadVigilante) (destruccionNiuShork listadesuperheroes)))

--Funcion elElegido
elElegido :: ListaDeVigilantes -> String
elElegido listadesuperheroes = devolverPrimerHabilidad (head (filter (((==)(maximum (map (cantDePalabras . nombreVigilante) (guerraVietnam listadesuperheroes)))) . cantDePalabras . nombreVigilante) (listadesuperheroes)))

--Funcion cantDePalabras
cantDePalabras :: String -> Int
cantDePalabras palabra = length (filter ((==) (' ')) palabra) + 1

--Funcion devolverPrimerHabilidad
devolverPrimerHabilidad :: UnVigilante -> String
devolverPrimerHabilidad vector = head (habilidadVigilante vector)

--Funcion patriarca 
patriarca :: ListaDeVigilantes -> Integer
patriarca vigilantesX = 2015 - (minimum (map anioVigilante (actaDeKeene vigilantesX)))

--HISTORIA
--"En 1949 se firmó el acta de Keene, en la que se obligaba a los viejos vigilantes a retirarse. Tiempo después, en 1959, hubo un accidente en un área de investigación de campo intrínseco. Éstos dos eventos mejoraron notablemente la participación de UZA en la Guerra de Vietnam, que sucedió a continuación. En NiuShork el comediante muere y como consecuencia de este nefasto accidente se destruye la ciudad." 

--FUNCION historia

historia :: ListaDeVigilantes -> ListaDeVigilantes
historia listadevigilantes = destruccionNiuShork(guerraVietnam(accidenteDeLaboratorio (actaDeKeene listadevigilantes) 1959))

--Explicar en qué medida usar y definir funciones de orden superior fue útil para resolver el problema
-- Las funciones de orden superior hay que usarse a medida que una funcion requiera multiples operaciones en un mismo parametro, ejemplo, teniendo una lista de vectores, primero tengo que conseguir trabajar con el vector y luego con un parametro interno del vector, y a traves de ese vector realizar la operacion final que quiero. Son muchas funciones una detras de la otra, en un determinado orden.


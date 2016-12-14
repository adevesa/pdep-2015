% jugador(Jugador,Equipo): relaciona cada jugador 
% con su equipo
jugador(heaton,nip).	%nip = Ninjas in Pijamas
jugador(potti,nip).
jugador(element,gol).	%gol = Game OnLine

% equipoTerrorista y equipoAntiTerrorista: 
% en qué bando está cada equipo
equipoTerrorista(nip).
equipoAntiTerrorista(gol).

/* Equipamento de cada jugador, usando estos functores
       casco(resistencia,ventilacion)
       chaleco(resistencia)
       rifle(modelo,velocidad en disparos por segundo)
       pistola(modelo,velocidad)
 */
equipamento(heaton,casco(20,9)).
equipamento(heaton,chaleco(80)).
equipamento(heaton,rifle(ak47,30)).
equipamento(heaton,pistola(usp,12)).

% importanciaRifles: lista de rifles en orden de importancia 
% (el primero es el mas importante)
importanciaRifles([awp,ak47,m4a1,aug,galil,famas]).

% presentoA(EquipoPresentador, EquipoPresentado)
% para entrar al servidor te tiene que presentar otro equipo
presentoA(nip,gol).
presentoA(mTw,nip).
presentoA(mTw,noA).
presentoA(noA,fip).

% mapas (zonas) limítrofes, 
% como la base es generada, ya los arma en ambos sentidos
limitrofe(dust2,inferno).
limitrofe(inferno,dust2).
limitrofe(dust2,train).
limitrofe(train,dust2).
limitrofe(inferno,cbble).
limitrofe(cbble,inferno).

% en qué mapa está cada jugador ahora
estaEn(heaton,dust2).
estaEn(potti,inferno).
estaEn(element,inferno).

% 1.	agregar la siguiente información de la forma más compacta posible: todos los integrantes de equipos terroristas tienen una pistola desert eagle de velocidad 25.

equipamento(Nombre, pistola(desertEagle,25)):-
  jugador(Nombre,Equipo),
  equipoTerrorista(Equipo).
  
%2.	
%a.	definir el predicado resistenciaItem/2 que relacione un item con el grado de resistencia que brinda; para los rifles y las pistolas es 0.
%b.	definir el predicado enemigos/2, que relaciona dos jugadores si son de bandos enemigos, o sea, uno es de un equipo terrorista y el otro de un equipo antiterrorista.

resistenciaItem(casco(Resistencia,_),Resistencia).

resistenciaItem(chaleco(Resistencia),Resistencia).

resistenciaItem(pistola(_,_),0).

resistenciaItem(rifle(_,_),0).

enemigos(Jugador1,Jugador2):-
  jugador(Jugador1, Equipo1),
  jugador(Jugador2, Equipo2),
  sonRivales(Equipo1,Equipo2).
  
 sonRivales(Equipo1,Equipo2):-
  equipoTerrorista(Equipo1),
  equipoAntiTerrorista(Equipo2).  
   
 sonRivales(Equipo1,Equipo2):-
  equipoTerrorista(Equipo2),
  equipoAntiTerrorista(Equipo1). 

%3.	definir el predicado equipoComplicado/1, de acuerdo a estas reglas
%•	un equipo terrorista está complicado si: ninguno de sus integrantes tiene un rifle awp, o tiene algún integrante sin casco ni chaleco.
%•	un equipo antiterrorista está complicado si: ningún integrante tiene un rifle groso o si menos de 5 integrantes tienen casco
%Los rifles grosos son: 
%•	aquellos cuyo modelo sea más importante (i.e. está antes en la lista de importanciaRifles) que el m4a1.
%•	los que disparan 50 o más veces por segundo.
%Este predicado debe ser inversible, y hay que indicar qué tuviste que hacer para lograrlo. 

equipoComplicado(Equipo):-
  equipoTerrorista(Equipo),
  ningunoTieneAwp(Equipo).

equipoComplicado(Equipo):-
  equipoTerrorista(Equipo),
  not(todosTienenCascoyChaleco(Equipo)).

equipoComplicado(Equipo):-
  equipoAntiTerrorista(Equipo),
  ningunoTieneRifleGroso(Equipo).

equipoComplicado(Equipo):-
  equipoAntiterrorista(Equipo),
  integrantesConCasco(Equipo, Cantidad),
  Cantidad < 5.

ningunoTieneAwp(Equipo):-
  jugador(_,Equipo),
  forall(jugador(Jugador,Equipo),noTieneAwp(Jugador)).

noTieneAwp(Jugador):-
  not(equipamento(Jugador, rifle(awp,_))).

todosTienenCascoNiChaleco(Equipo):-
  jugador(_,Equipo),
  forall(jugador(Jugador,Equipo),tieneCascoyChaleco(Jugador)).

tieneCascoyChaleco(Jugador):-
  equipamento(Jugador, casco(_,_)),
  equipamento(Jugador, chaleco(_)).

ningunoTieneRifleGroso(Equipo):-
  jugador(_,Equipo),
  forall(jugador(Jugador,Equipo), noTieneRifleGroso(Jugador)).

noTieneRifleGroso(Jugador):-
  esRifleGroso(RifleGroso),
  not(equipamento(Jugador, rifle(RifleGroso,_))).

esRifleGroso(RifleGroso):-
  importanciaRifles(ListaDeRifles),
  nth0(Direccion,ListaDeRifles,m4a1),
  nth0(DireccionRifleGroso,ListaDeRifles,RifleGroso),
  DireccionRifleGroso < Direccion.

esRifleGroso(RifleGroso):-
  equipamento(_, rifle(RifleGroso, VelocidadDeDisparo)),
  VelocidadDeDisparo > 49.

integrantesConCasco(Equipo,Cantidad):-
  jugador(_,Equipo),
  findall(Jugador, tieneCasco(Jugador,Equipo), JugadoresConCasco),
  length(JugadoresConCasco, Cantidad).

tieneCasco(Jugador,Equipo):-
  jugador(Jugador,Equipo),
  equipamento(Jugador, casco(_,_)).

%Para lograr que el predicado sea inversible lo que tuve que hacer fue generar las variables antes del findall y del not, ya que son predicados de orden superior, y los predicados de orden superior no son totalmente inversibles..


%4.	definir el predicado equipoProtegido/1
%Se dice que un equipo está protegido si la resistencia total de cada uno de sus integrantes es al menos 100. La resistencia de un integrante es la suma de las resistencias de su equipamiento.
%Ayudas: usar algún predicado auxiliar, y también alguno de los ítems anteriores.

equipoProtegido(Equipo):-
  jugador(_,Equipo),
  forall(jugador(Jugador,Equipo), tieneResistencia(Jugador)).

tieneResistencia(Jugador):-
  jugador(Jugador,_),
  findall(Resistencia, elementosResistentes(Jugador,_,Resistencia), Resistencias),
  sumlist(Resistencias,Cantidad),
  Cantidad > 99.

elementosResistentes(Jugador,Item,Resistencia):-
  equipamento(Jugador, Item),
  resistenciaItem(Item,Resistencia).

%5.	definir el predicado regionLlena/1, cuyo argumento es un mapa.
%La región de un mapa está llena si tanto el mapa como todos sus mapas limítrofes están llenos.
%Un mapa está lleno si en total hay al menos 12 ítems, tomando los ítems de los jugadores que están en el mapa.
%P.ej. si en un mapa están roque y mecha, roque tiene tres ítems de equipamiento (p.ej. casco, escudo y pistola) y mecha cuatro (casco, rifle, dos pistolas), entonces en total en el mapa hay 7 ítems (los tres de roque y los cuatro de mecha).
%Ayuda: conviene armar directamente una lista de ítems, no de jugadores.

regionLlena(Mapa):-
 mapaLleno(Mapa),
 limitrofesLlenos(Mapa).

 esMapa(Mapa):-
  estaEn(_,Mapa).
 
mapaLleno(Mapa):-
 tiene12ItemsOMas(Mapa).

tiene12ItemsOMas(Mapa):-
  esMapa(Mapa),
  findall(Items, itemDeUnJugador(_,Mapa,Items),  TotalDeItems),
  sumlist(TotalDeItems,Cantidad),
  Cantidad >= 12.

itemDeUnJugador(Jugador,Mapa,Items):-
  estaEn(Jugador,Mapa),
  findall(Item, equipamento(Jugador, Item), ListadoDeItems),
  length(ListadoDeItems,Items).

limitrofesLlenos(Mapa):-
  forall(limitrofe(Mapa,Limitrofe), mapaLleno(Limitrofe)).

%6.	definir el predicado puedeIrA/2, que relaciona un jugador con un mapa, si el jugador puede ir al mapa.
%Un jugador puede ir a un mapa si puede llegar desde donde está, yendo por limítrofes, sin caer en ningún mapa con más de 3 enemigos.
%Ayudas
%•	definir un predicado auxiliar puedeMoverse/3, que relacione jugador, mapa origen y mapa destino. Un jugador puedeIrA si puedeMoverse desde donde está hasta el destino.
%•	puedeMoverse definirlo recursivamente, el caso base es cuando el origen es igual al destino.

puedeIrA(Jugador,MapaParaIr):-
  estaEn(Jugador,MapaDondeEsta),
  puedeMoverse(Jugador,MapaDondeEsta,MapaParaIr).

puedeMoverse(Jugador,MapaDondeEsta,MapaParaIr):-
  limitrofe(MapaDondeEsta,Limitrofe),
  not(tiene3enemigosOMas(Jugador,Limitrofe)),
  puedeMoverse(Jugador,Limitrofe,MapaParaIr).

puedeMoverse(_,MapaParaIr,MapaParaIr).

tiene3enemigosOMasPara(Jugador,Mapa):-
  estaEn(Jugador,Mapa),  
  findall(Enemigo, enemigos(Jugador,Enemigo), Enemigos),
  length(Enemigos, Cantidad),
  Cantidad > 2.




% Aquí va el código.

nombre(ana).
nombre(beto).
nombre(carola).
nombre(dimitri).

tecnologia(herreria).
tecnologia(forja).
tecnologia(fundicion).
tecnologia(emplumado).
tecnologia(laminas).
tecnologia(puntas).
tecnologia(mallas).
tecnologia(horno).
tecnologia(placas).
tecnologia(molino).
tecnologia(collegamento).
tecnologia(arado).

civilizacion(romanos).
civilizacion(incas).

jugador(ana, romanos, [herreria, forja, emplumado, laminas]).
jugador(beto, incas, [herreria, forja, fundicion]).
jugador(carola, romanos, [herreria]).
jugador(dimitri, romanos, [herreria, fundicion]).


esExpertoMetales(Jugador):- 
    jugador(Jugador, _, [herreria, forja, fundicion]).
esExpertoMetales(Jugador):-
    jugador(Jugador, _, [fundicion]).
esExpertoMetales(Jugador):-
    jugador(Jugador, romanos, _).


esCivilizacionPopular(Civilizacion):-
    jugador(Jugador1, Civilizacion, _),
    jugador(Jugador2, Civilizacion, _),
    Jugador1 \= Jugador2.


tieneAlcanceGlobal(Tecnologia):-
    tecnologia(Tecnologia), 
    forall(jugador(_, _, Tecnologias), member(Tecnologia, Tecnologias)).


esCivilizacionLider(Civilizacion):-
    civilizacion(Civilizacion),
    forall((jugador(_, _, Tecnologias), member(Tecnologia, Tecnologias)),
           civilizacionAlcanzoTecnologia(Civilizacion, Tecnologia)).


civilizacionAlcanzoTecnologia(Civilizacion, Tecnologia):-
    jugador(_, Civilizacion, Tecnologias), member(Tecnologia, Tecnologias).


campeon(Vida):-
    between(1, 100, Vida).

jinete(caballo).
jinete(camello).

piquero(Nivel,escudo):-
    between(1, 3, Nivel).
piquero(Nivel):-
    between(1, 3, Nivel).


unidadJugador(ana,[jinete(caballo), piquero(1,escudo), piquero(2)]).
unidadJugador(beto,[campeon(100), campeon(80), piquero(1, escudo), jinete(camello)]).
unidadJugador(carola,[piquero(3), piquero(2,escudo)]).


vida(jinete(camello), 80).
vida(jinete(caballo), 90).
vida(campeon(Vida), Vida).
vida(piquero(Nivel), Vida):-
    nivelVidaPiquero(Nivel, VidaBase),
    Vida is VidaBase.
vida(piquero(Nivel, escudo), Vida):-
    nivelVidaPiquero(Nivel, VidaBase),
    Vida is VidaBase * 1.1.


nivelVidaPiquero(1, 50).
nivelVidaPiquero(2, 65).
nivelVidaPiquero(3, 70).


unidadConMasVida(Jugador, Unidad):-
   unidadJugador(Jugador, Unidades),
   findall((Vida, UnidadTemp), (member(UnidadTemp, Unidades), vida(UnidadTemp, Vida)), VidaUnidades),
   max_member((_, Unidad), VidaUnidades).


tieneVentaja(jinete(_), campeon(_)).
tieneVentaja(campeon(_), piquero(_)).
tieneVentaja(campeon(_), piquero(_, _)).
tieneVentaja(piquero(_), jinete(_)).
tieneVentaja(piquero(_, _), jinete(_)).
tieneVentaja(jinete(camello), jinete(caballo)).


unidadGana(Unidad1, Unidad2):-
    tieneVentaja(Unidad1, Unidad2), !.

unidadGana(Unidad1, Unidad2):-
    not(tieneVentaja(Unidad1, Unidad2)),
    not(tieneVentaja(Unidad2, Unidad1)),
    vida(Unidad1, Vida1),
    vida(Unidad2, Vida2),
    Vida1 > Vida2.


esPiqueroConEscudo(piquero(_, escudo)).

esPiqueroSinEscudo(piquero(_)).


contarPiqueroConEscudo(Jugador, Cantidad):-
    unidadJugador(Jugador, Unidades),
    findall(1, (member(Unidad, Unidades), esPiqueroConEscudo(Unidad)), ListaConEscudo),
    length(ListaConEscudo, Cantidad).


contarPiqueroSinEscudo(Jugador, Cantidad):-
    unidadJugador(Jugador, Unidades),
    findall(1, (member(Unidad, Unidades), esPiqueroSinEscudo(Unidad), \+ esPiqueroConEscudo(Unidad)), ListaSinEscudo), %con \+ nosa asegfuramos que solo tome a los que no tienen escudo
    length(ListaSinEscudo, Cantidad).


puedeSobrevivirAsedio(Jugador):-
    contarPiqueroConEscudo(Jugador, CantidadConEscudo),
    contarPiqueroSinEscudo(Jugador, CantidadSinEscudo),
    CantidadConEscudo > CantidadSinEscudo.


depende(emplumado, herreria).
depende(forja, herreria).
depende(laminas, herreria).
depende(puntas, emplumado).
depende(fundicion, forja).
depende(mallas, laminas).
depende(horno, fundicion).
depende(placas, mallas).
depende(collegamento, molino).
depende(arado, collegamento).


sinDependencias(Tecnologia) :-
    not(depende(Tecnologia, _)).


ordenDeTecnologia(PrimerTecnologia, SegundaTecnologia):-
    tecnologia(PrimerTecnologia),
    tecnologia(SegundaTecnologia),
    depende(PrimerTecnologia, SegundaTecnologia).

ordenDeTecnologia(PrimerTecnologia, SegundaTecnologia):-
    tecnologia(PrimerTecnologia),
    tecnologia(OtraTecnologia),
    depende(PrimerTecnologia, OtraTecnologia),
    ordenDeTecnologia(OtraTecnologia, SegundaTecnologia).


elementoDeUnaListaPerteneceAOtra([], _).

elementoDeUnaListaPerteneceAOtra([Cabeza|Cola], Lista):-
    member(Cabeza, Lista),
    elementoDeUnaListaPerteneceAOtra(Cola, Lista).


puedeDesarrollarTecnologia(Jugador, Tecnologia):-
    nombre(Jugador),
    jugador(Jugador, _, Tecnologias),
    tecnologia(Tecnologia),
    not(member(Tecnologia, Tecnologias)),
    findall(Dependencia, ordenDeTecnologia(Tecnologia, Dependencia), Dependencias),
    elementoDeUnaListaPerteneceAOtra(Dependencias, Tecnologias).

puedeDesarrollarTecnologia(Jugador, Tecnologia):-
    nombre(Jugador),
    jugador(Jugador, _, Tecnologias),
    tecnologia(Tecnologia),
    not(member(Tecnologia, Tecnologias)),
    sinDependencias(Tecnologia).
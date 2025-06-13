ahorcado_linea(1, ',---+').
ahorcado_linea(2, '|   |').
ahorcado_linea(3, '|   o').
ahorcado_linea(4, '|  /|\\').
ahorcado_linea(5, '|  / \\').
ahorcado_linea(6, '|').


:- dynamic max_intentos/1.
max_intentos(7).  % por defecto

mostrar_ahorcado(0) :-
    write('========='), nl,
    write(',---+'), nl,
    write('|   |'), nl,
    write('|   o'), nl,
    write('|  /|\\'), nl,
    write('|  / \\'), nl,
    write('|'), nl,
    write('========='), nl.

mostrar_ahorcado(Fallos) :-
    write('========='), nl,
    max_intentos(Max),
    TotalLineas = 6,
    LineasMostrar is (Fallos * TotalLineas) // Max,
    mostrar_lineas(1, LineasMostrar),
    mostrar_lineas_vacias(6 - LineasMostrar),
    write('========='), nl.

mostrar_lineas(N, Max) :-
    N =< Max,
    ahorcado_linea(N, Texto),
    write(Texto), nl,
    N1 is N + 1,
    mostrar_lineas(N1, Max).
mostrar_lineas(N, Max) :- N > Max.

mostrar_lineas_vacias(0).
mostrar_lineas_vacias(N) :-
    N > 0,
    write('|'), nl,
    N1 is N - 1,
    mostrar_lineas_vacias(N1).

palabra_escogida_aleatoriamente(Palabra) :-
    palabras_guardadas(Lista_palabras),
    random_member(Palabra, Lista_palabras).

validar_palabra([]).
validar_palabra([Cabeza | Resto]) :-
    char_type(Cabeza, alpha), % verifica que la cabeza de la lista sea una letra
    validar_palabra(Resto). % recursivamente valida el resto de la lista

% Recibe una palabra y verifica si es válida
palabra_ingresada(Palabra) :-
    write('Ingrese una palabra (debe contener solamente letras y no puede incluir espacios): '), nl,
    read_line_to_string(user_input, Entrada),
    string_lower(Entrada, EntradaMin),
    string_chars(EntradaMin, ListaCaracteres),
    (validar_palabra(ListaCaracteres) ->
        atom_chars(Palabra, ListaCaracteres);

        write('Palabra inválida, solo se permiten letras: '), nl,
        palabra_ingresada(Palabra)
    ).

existe_letra(Letra, Palabra, Adivinadas, NAdivinadas, Fallos, NFallos) :-
    atom_chars(Palabra, ListaLetras),
    ( member(Letra, ListaLetras) ->
        write('La letra '),
        write(Letra),
        write(' se encuentra en la palabra'), nl,
        ( member(Letra, Adivinadas) ->
            NAdivinadas = Adivinadas  % Ya estaba
        ;
            append(Adivinadas, [Letra], NAdivinadas)  % Es nueva
        ),
        NFallos = Fallos,
        mostrar_pista(Palabra, NAdivinadas), nl
    ;
        write('La letra '), write(Letra), write(' NO se encuentra en la palabra'), nl,
        NFallos is Fallos + 1,
        %añadir aqui para el muñequito
        append(Adivinadas, [Letra], NAdivinadas),
        mostrar_ahorcado(NFallos),
        mostrar_pista(Palabra, NAdivinadas), nl
    ).

ciclo_juego(Palabra, Adivinadas, Fallos) :-
    atom_chars(Palabra, LetrasPalabra),
    sort(LetrasPalabra, LetrasUnicas),
    sort(Adivinadas, LetrasAdivinadas),
    ( LetrasUnicas == LetrasAdivinadas ->
        write('Felicidades! Adivinaste la palabra.'), nl
    ; max_intentos(Max),
        Fallos >= Max ->
        write('Has perdido. La palabra era: '), write(Palabra), nl,
        mostrar_ahorcado(Max), nl

    ;
        max_intentos(Max),
        Restantes is Max - Fallos,
        write('Intentos restantes: '), write(Restantes), nl,
        write('Letras usadas: '), write(Adivinadas), nl,
        write('Escoja una letra: '), nl,
        read_line_to_string(user_input, LetraStr),
        string_chars(LetraStr, [Letra|_]),
        ( char_type(Letra, alpha) ->
            ( member(Letra, Adivinadas) ->
                write('Ya se intento esa letra. Intente con otra diferente.'), nl,
                ciclo_juego(Palabra, Adivinadas, Fallos)
            ;
                existe_letra(Letra, Palabra, Adivinadas, NAdivinadas, Fallos, NFallos),
                ciclo_juego(Palabra, NAdivinadas, NFallos)
            )
        ;
            write('Por favor ingrese solo una letra.'), nl,
            ciclo_juego(Palabra, Adivinadas, Fallos)
        )
    ).

configurar_intentos :-
    write('Ingrese el numero maximo de intentos: '), nl,
    read_line_to_string(user_input, IntentosStr),
    number_string(N, IntentosStr),
    retractall(max_intentos(_)),
    assertz(max_intentos(N)).


palabras_guardadas([
    ranacalva,
    profe,
    ponganos,
    cien,
    porfa,
    brayan,
    probabilidad,
    calvarana,
    iguanapelona,
    pelonaiguana
]).


% mostrar_pista(Palabra, LetrasAdivinadas)
mostrar_pista(Palabra, Letras) :-
    string_chars(Palabra, Chars),
    mostrar_letras(Chars, Letras).

% mostrar_letras(ListaDeCaracteresDeLaPalabra, LetrasAdivinadas)
mostrar_letras([], _).
mostrar_letras([C|Resto], Letras) :-
    (   memberchk(C, Letras)
    ->  write(C)
    ;   write('_')
    ),
    write(' '),
    mostrar_letras(Resto, Letras).

elegir_inicio_juego(Palabra) :-
    write('Desea jugar con una palabra aleatoria o ingresar una palabra?: '), nl,
    write('1. Palabra aleatoria'), nl,
    write('2. Ingresar palabra'), nl,
    write('3. Configurar numero maximo de intentos'), nl,
    write('Seleccione una opcion: '),
    read_line_to_string(user_input, Opcion),
    (
        Opcion = "1" ->
            palabra_escogida_aleatoriamente(Palabra);

        Opcion = "2" ->
            palabra_ingresada(Palabra);

        Opcion = "3" ->
            configurar_intentos,
            elegir_inicio_juego(Palabra);
        
        write('Opcion invalida'),
        elegir_inicio_juego(Palabra)

    ).

jugar :-
    elegir_inicio_juego(Palabra),
    write('Comienza el juego!'), nl,
    ciclo_juego(Palabra, [], 0).
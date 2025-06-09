mostrar_ahorcado :-
    write('========='), nl,
    write(',---+'), nl,
    write('|   |'), nl,
    write('|   o'), nl,
    write('|  /|\\'), nl,
    write('|  / \\'), nl,
    write('|'), nl,
    write('========='), nl.


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
    read_line_to_string(input, Entrada),
    string_chars(Entrada, ListaCaracteres),
    (validar_palabra(ListaCaracteres) ->
        atom_chars(Palabra, ListaCaracteres);

        write('Palabra inválida, solo se permiten letras: '), nl,
        palabra_ingresada(Palabra)
    ).

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


elegir_inicio_juego(Palabra) :-
    write('Desea jugar con una palabra aleatoria o ingresar una palabra?: '), nl,
    write('1. Palabra aleatoria'), nl,
    write('2. Ingresar palabra'), nl,

    read_line_to_string(input, Opcion),
    (
        Opcion = '1' -> palabra_escogida_aleatoriamente(Palabra) ;
        Opcion = '2' -> palabra_ingresada(Palabra) ;
        write('Opción inválida, por favor intente de nuevo.'), nl, elegir_inicio_juego(Palabra)
    ).
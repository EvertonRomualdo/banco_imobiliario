:- encoding(utf8).
:- module(main, [start_game/0]).

:- use_module(game).
:- use_module(ranking).

start_game :-
    main_menu.

main_menu :-
    nl,
    writeln('--- Menu Inicial ---'),
    writeln('1. Iniciar jogo'),
    writeln('2. Ver ranking'),
    writeln('0. Sair'),
    read_line_to_string(user_input, Input),
    ( Input = "1" ->
        game:start_new_game,
        main_menu
    ; Input = "2" ->
        ranking:show_ranking,
        main_menu
    ; Input = "0" ->
        writeln("Saindo do jogo. Ate logo!")
    ; writeln("Opcao invalida!"), main_menu
    ).

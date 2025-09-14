:- encoding(utf8).
:- module(main, [start_game/0]).
:- use_module(game).

start_game :-
    write('Bem-vindo ao Banco Imobiliário (Prolog)!'), nl,
    game:init_game_state(GameState),
    game:game_loop(GameState).

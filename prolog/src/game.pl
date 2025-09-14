:- encoding(utf8).
:- module(game, [
    init_game_state/2,
    game_loop/1
]).
:- use_module(board).
:- use_module(player).
:- use_module(actions).
:- use_module(ui).
:- use_module(ranking).

% init_game_state(+Players, -State)
init_game_state(Players, state(Board, Players, 1)) :-
    board:initial_board(Board).

% game_loop(+State)
game_loop(state(Board, Players, Turn)) :-
    length(Players, N),
    ( N =< 1 ->
        ( Players = [Winner] ->
            ui:print_message("Jogo terminou! Vencedor:"),
            ui:print_message(Winner),
            ranking:update_ranking(Winner, Players)
        ;
            ui:print_message("Jogo terminou.")
        )
    ;
        actions:take_turn(Players, Turn, Board, NewPlayers, NewBoard),
        next_turn_after_update(NewPlayers, Turn, NextTurn),
        game_loop(state(NewBoard, NewPlayers, NextTurn))
    ).

next_turn_after_update(Players, CurrTurn, NextTurn) :-
    length(Players, L),
    ( L = 0 -> NextTurn = 1
    ; Next0 is CurrTurn + 1,
      ( Next0 =< L -> NextTurn = Next0 ; NextTurn = 1 )
    ).

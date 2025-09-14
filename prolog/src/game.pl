:- encoding(utf8).
:- module(game, [
    init_game_state/1,
    game_loop/1
]).
:- use_module(board).
:- use_module(player).
:- use_module(actions).
:- use_module(ui).

% GameState: state(Board, Players, TurnIndex)

init_game_state(state(Board, Players, 1)) :-
    board:initial_board(Board),
    player:initial_players(Players).

% game_loop(+State)

game_loop(state(Board, Players, Turn)) :-
    % check end condition: only one player remains
    length(Players, N),
    ( N =< 1 ->
        ( Players = [Winner] ->
            ui:print_message("Jogo terminou! Vencedor:"),
            ui:print_message(Winner)
        ;
            ui:print_message("Jogo terminou.")
        )
    ;
        % normal turn
        actions:take_turn(Players, Turn, Board, NewPlayers, NewBoard),
        % after move, prepare next turn index (skip if current removed)
        % if current player was removed, keep same index (because list shrank)
        next_turn_after_update(Players, NewPlayers, Turn, NextTurn),
        game_loop(state(NewBoard, NewPlayers, NextTurn))
    ).

% compute next turn index after update
next_turn_after_update(OldPlayers, NewPlayers, CurrTurn, NextTurn) :-
    length(NewPlayers, LNew),
    ( LNew = 0 -> NextTurn = 1
    ; % if current player still exists (same id at position CurrTurn) then advance, else CurrTurn stays (since list changed)
      ( CurrTurn =< LNew ->
          NextTemp is CurrTurn + 1,
          ( NextTemp =< LNew -> NextTurn = NextTemp ; NextTurn = 1 )
      ;
          % if curr turn index out of range (player removed at end), wrap to 1
          NextTurn = 1
      )
    ).

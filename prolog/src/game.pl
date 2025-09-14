:- encoding(utf8).
:- module(game, [
    start_new_game/0,
    game_loop/1
]).

:- use_module(board).
:- use_module(player).
:- use_module(actions).
:- use_module(ui).
:- use_module(ranking).

% GameState: state(Board, Players, TurnNumber)

start_new_game :-
    board:initial_board(Board),
    player:register_players(Players),
    GameState = state(Board, Players, 1),
    game_loop(GameState).

% Loop principal do jogo
game_loop(state(Board, Players, Turn)) :-
    length(Players, N),
    ( N =< 1 ->
        ( Players = [Winner] ->
            ui:print_message("Jogo terminou! Vencedor:"),
            Winner = player(_,Name,_,_,_),
            format("~w venceu a partida!~n", [Name]),
            ranking:update_stats(Name, win),
            update_losers_stats(Players, Name),
            !
        ;
            ui:print_message("Jogo terminou sem vencedor.")
        )
    ;
        Index is ((Turn - 1) mod N) + 1,
        actions:take_turn(Players, Index, Board, NewPlayers, NewBoard),
        NextTurn is Turn + 1,
        game_loop(state(NewBoard, NewPlayers, NextTurn))
    ).

% Atualiza derrotas de todos os que não venceram
update_losers_stats([], _).
update_losers_stats([player(_,Name,_,_,_)], WinnerName) :-
    ( Name \== WinnerName -> ranking:update_stats(Name, loss) ; true ).

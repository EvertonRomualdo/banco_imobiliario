:- encoding(utf8).
:- module(game, [
    start_new_game/0,
    game_loop/3
]).

:- use_module(board).
:- use_module(player).
:- use_module(actions).
:- use_module(ui).
:- use_module(ranking).

% Inicia novo jogo
start_new_game :-
    writeln("=== Iniciando novo jogo ==="),
    board:initial_board(Board),
    player:register_players(Players),
    game_loop(Board, Players, 1).

% Loop de jogo com contador global de turnos
game_loop(Board, Players, Turn) :-
    ( length(Players, N), N =< 1 ->
        ( Players = [Winner] ->
            Winner = player(_,Name,_,_,_),
            format("Jogo terminou! Vencedor: ~w~n", [Name]),
            ranking:update_stats(Name, win),
            update_losers_stats(Players, Name)
        ;
            writeln("Jogo terminou sem vencedor.")
        )
    ;
        % Seleciona o jogador da vez (fila circular)
        length(Players, Len),
        Index is ((Turn - 1) mod Len) + 1,
        nth1(Index, Players, CurrPlayer),

        % Mostra turno global crescente
        ui:print_turn_global(CurrPlayer, Turn),

        ( CurrPlayer = player(_,_,_,_,Blocked), Blocked > 0 ->
            NewBlocked is Blocked - 1,
            CurrPlayer = player(Id,Name,Pos,Bal,_),
            Updated = player(Id,Name,Pos,Bal,NewBlocked),
            player:update_player_in_list(Players, Updated, NewPlayers),
            NewBoard = Board,
            writeln("Jogador está preso, turno consumido.")
        ;
            actions:take_turn(Players, Index, Board, NewPlayers, NewBoard)
        ),

        NextTurn is Turn + 1,
        game_loop(NewBoard, NewPlayers, NextTurn)
    ).

% Atualiza derrotas de todos exceto vencedor
update_losers_stats([], _).
update_losers_stats([player(_,Name,_,_,_)], WinnerName) :-
    ( Name \== WinnerName -> ranking:update_stats(Name, loss) ; true ).

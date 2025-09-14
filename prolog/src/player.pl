:- encoding(utf8).
:- module(player, [
    register_players/1,
    get_current_player/3,
    update_player_in_list/3,
    remove_player_by_id/3,
    next_turn_index/3,
    is_bankrupt/1,
    pay/3,
    receive/3
]).

% player(Id, Name, Pos, Balance, blockedTurns)

% Cadastro de jogadores interativo
register_players(Players) :-
    writeln("Quantos jogadores deseja cadastrar? (2 a 4)"),
    read_line_to_string(user_input, NStr),
    ( catch(number_string(N, NStr), _, fail),
      N >= 2, N =< 4 ->
        register_loop(N, 1, [], Players)
    ;
        writeln("Número inválido! Digite entre 2 e 4."),
        register_players(Players)
    ).

register_loop(0, _, Acc, Players) :- reverse(Acc, Players).
register_loop(N, Id, Acc, Players) :-
    format("Digite o nome do jogador ~w: ", [Id]),
    read_line_to_string(user_input, Name),
    writeln("Saldo inicial (ex: 500): "),
    read_line_to_string(user_input, BalStr),
    ( catch(number_string(Bal, BalStr), _, fail), Bal > 0 ->
        Player = player(Id, Name, 0, Bal, 0),
        N1 is N - 1,
        Id1 is Id + 1,
        register_loop(N1, Id1, [Player|Acc], Players)
    ;
        writeln("Saldo inválido, tente novamente."),
        register_loop(N, Id, Acc, Players)
    ).

% ======== FUNÇÕES AUXILIARES ========

get_current_player(Players, TurnIndex, Player) :-
    nth1(TurnIndex, Players, Player).

update_player_in_list([], _, []).
update_player_in_list([player(Id,_,_,_,_) | Rest], NewPlayer, [NewPlayer | Rest]) :-
    NewPlayer = player(Id,_,_,_,_), !.
update_player_in_list([H | Rest], NewPlayer, [H | Rest2]) :-
    update_player_in_list(Rest, NewPlayer, Rest2).

remove_player_by_id(Players, Id, NewPlayers) :-
    exclude(is_player_id(Id), Players, NewPlayers).

is_player_id(Id, player(Id,_,_,_,_)).
is_player_id(_, _) :- fail.

next_turn_index(Players, Current, Next) :-
    length(Players, L),
    ( L == 0 -> Next = 1
    ; Next0 is Current + 1,
      ( Next0 =< L -> Next = Next0 ; Next = 1 )
    ).

is_bankrupt(player(_,_,_,Balance,_)) :- Balance =< 0.

pay(player(Id,Name,Pos,Bal,Blk), Amount, player(Id,Name,Pos,NewBal,Blk)) :-
    NewBal is Bal - Amount.

receive(player(Id,Name,Pos,Bal,Blk), Amount, player(Id,Name,Pos,NewBal,Blk)) :-
    NewBal is Bal + Amount.

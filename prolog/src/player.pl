:- encoding(utf8).
:- module(player, [
    register_players/1,
    update_player_in_list/3,
    remove_player_by_id/3,
    is_bankrupt/1,
    pay/3,
    receive/3
]).

% player(Id, Name, Pos, Balance, BlockedTurns)

% Cadastro de jogadores com saldo inicial fixo em 500
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
    Player = player(Id, Name, 0, -1, 0),
    N1 is N - 1,
    Id1 is Id + 1,
    register_loop(N1, Id1, [Player|Acc], Players).

% Substitui jogador na lista
update_player_in_list([], _, []).
update_player_in_list([player(Id,_,_,_,_) | Rest], NewPlayer, [NewPlayer | Rest]) :-
    NewPlayer = player(Id,_,_,_,_), !.
update_player_in_list([H | Rest], NewPlayer, [H | Rest2]) :-
    update_player_in_list(Rest, NewPlayer, Rest2).

% Remove jogador por id
remove_player_by_id(Players, Id, NewPlayers) :-
    exclude(is_player_id(Id), Players, NewPlayers).

is_player_id(Id, player(Id,_,_,_,_)).
is_player_id(_, _) :- fail.

% Jogador faliu
is_bankrupt(player(_,_,_,Balance,_)) :- Balance =< 0.

% Pagamento
pay(player(Id,Name,Pos,Bal,Blk), Amount, player(Id,Name,Pos,NewBal,Blk)) :-
    NewBal is Bal - Amount.

% Recebimento
receive(player(Id,Name,Pos,Bal,Blk), Amount, player(Id,Name,Pos,NewBal,Blk)) :-
    NewBal is Bal + Amount.

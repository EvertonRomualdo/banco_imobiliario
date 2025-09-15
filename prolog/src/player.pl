:- encoding(utf8).
:- module(player, [
    register_players/1,
    update_player_in_list/3,
    remove_player_by_id/3,
    is_bankrupt/1,
    pay/3,
    receive/3,
    is_player_id/2
]).

% player(Id, Name, Pos, Balance, BlockedTurns)

% Registro de jogadores
register_players(Players) :-
    writeln("Quantos jogadores humanos? (0 a 4)"),
    read_line_to_string(user_input, HStr),
    ( catch(number_string(Hnum, HStr), _, fail) -> true ; Hnum = -1 ),
    writeln("Quantos bots? (0 a 4)"),
    read_line_to_string(user_input, BStr),
    ( catch(number_string(Bnum, BStr), _, fail) -> true ; Bnum = -1 ),
    Total is Hnum + Bnum,
    ( integer(Hnum), integer(Bnum), Total >= 2, Total =< 4 ->
        register_humans(Hnum, 1, [], Humans),
        StartId is Hnum + 1,
        register_bots(Bnum, StartId, [], Bots),
        append(Humans, Bots, Players)
    ;
        writeln("Número inválido! Total de jogadores (humanos + bots) deve ser entre 2 e 4."),
        register_players(Players)
    ).

register_humans(0, _, Acc, Players) :- reverse(Acc, Players).
register_humans(N, Id, Acc, Players) :-
    N > 0,
    format("Digite o nome do jogador humano ~w: ", [Id]),
    read_line_to_string(user_input, Name0),
    string_trim(Name0, Name),             % tira espaços em branco nas pontas
    ( Name = "" ->
        writeln("Nome inválido! Tente novamente."),
        register_humans(N, Id, Acc, Players)
    ;
        Player = player(Id, Name, 0, 500, 0),  % saldo inicial = 500
        N1 is N - 1,
        Id1 is Id + 1,
        register_humans(N1, Id1, [Player|Acc], Players)
    ).

register_bots(0, _, Acc, Players) :- reverse(Acc, Players).
register_bots(N, Id, Acc, Players) :-
    N > 0,
    format(string(Name), "BOT~w", [Id]),
    Player = player(Id, Name, 0, 500, 0),  % bots também iniciam com 500
    N1 is N - 1,
    Id1 is Id + 1,
    register_bots(N1, Id1, [Player|Acc], Players).

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

% trim auxiliar de strings (simples)
string_trim(Str, Trimmed) :-
    string_codes(Str, Codes),
    trim_left(Codes, L),
    reverse(L, R),
    trim_left(R, RR),
    reverse(RR, TrimmedCodes),
    string_codes(Trimmed, TrimmedCodes).

trim_left([32|T], R) :- !, trim_left(T, R). % espaço
trim_left([9|T], R) :- !, trim_left(T, R).  % tab
trim_left([10|T], R) :- !, trim_left(T, R). % newline
trim_left(L, L).

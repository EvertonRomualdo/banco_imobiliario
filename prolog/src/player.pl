:- encoding(utf8).
:- module(player, [
    initial_players/1,
    get_current_player/3,
    update_player_in_list/3,
    remove_player_by_id/3,
    next_turn_index/3,
    is_bankrupt/1,
    pay/3,
    receive/3
]).

initial_players([
    player(1, "Everton", 0, 500, 0),
    player(2, "Daniel", 0, 500, 0)
]).

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

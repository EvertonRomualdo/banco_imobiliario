:- module(board, [
    initial_board/1,
    get_house_by_pos/3,
    update_house_in_board/3,
    board_size/2
]).

% house(Id, Name, Type, price, rent, owner, fixedIncreaseInRentPerCivilHousing)
% owner = none | PlayerId (integer)
% Type = city | tax | prison | special

% initial_board(-Board)
initial_board([
    house(0, "Start", special, 0, 0, none, 0),
    house(1, "Texas", city, 100, 20, none, 10),
    house(2, "Feriado", special, 0, 0, none, 0),
    house(3, "Paris", city, 120, 24, none, 12),
    house(4, "Imposto", tax, 0, 0, none, 0),
    house(5, "Prisao", prison, 0, 0, none, 0)
]).

% get_house_by_pos(+Board, +Pos, -House)

get_house_by_pos(Board, Pos, House) :-
    nth0(Pos, Board, House).

% update_house_in_board(+Board, +NewHouse, -NewBoard)

update_house_in_board(Board, NewHouse, NewBoard) :-
    NewHouse = house(Id,_,_,_,_,_,_),
    % remove old with same Id and put NewHouse at end then reorder: simpler replace by index
    nth0(Index, Board, house(Id,_,_,_,_,_,_), !),
    same_length(Board, Board), % ensure proper failure if not found
    replace_nth0(Board, Index, NewHouse, NewBoard).

% board_size(+Board, -Size)

board_size(Board, Size) :-
    length(Board, Size).

% helper: replace element at index N (0-based)

replace_nth0([_|T], 0, X, [X|T]).
replace_nth0([H|T], N, X, [H|R]) :-
    N > 0,
    N1 is N - 1,
    replace_nth0(T, N1, X, R).

:- encoding(utf8).
:- module(board, [
    initial_board/1,
    get_house_by_pos/3,
    update_house_in_board/3,
    board_size/2
]).

% house(Id, Name, Type, Price, Rent, Owner, IncreasePerHouse)

initial_board([
    house(0, "Start", special, 0, 0, none, 0),
    house(1, "Texas", city, 100, 20, none, 10),
    house(2, "Feriado", special, 0, 0, none, 0),
    house(3, "Paris", city, 120, 24, none, 12),
    house(4, "Imposto", tax, 0, 0, none, 0),
    house(5, "Prisao", prison, 0, 0, none, 0),
    house(6, "Rio de Janeiro", city, 150, 30, none, 15),
    house(7, "São Paulo", city, 160, 32, none, 16),
    house(8, "Feriado", special, 0, 0, none, 0),
    house(9, "Salvador", city, 140, 28, none, 14),
    house(10, "Imposto", tax, 0, 0, none, 0),
    house(11, "Brasília", city, 155, 31, none, 15),
    house(12, "Prisao", prison, 0, 0, none, 0),
    house(13, "Fortaleza", city, 130, 26, none, 13),
    house(14, "Curitiba", city, 135, 27, none, 13),
    house(15, "Belém", city, 120, 24, none, 12),
    house(16, "Natal", city, 125, 25, none, 12),
    house(17, "Florianópolis", city, 130, 26, none, 13),
    house(18, "Manaus", city, 140, 28, none, 14),
    house(19, "Recife", city, 145, 29, none, 14)
]).

get_house_by_pos(Board, Pos, House) :-
    nth0(Pos, Board, House).

update_house_in_board(Board, NewHouse, NewBoard) :-
    NewHouse = house(Id,_,_,_,_,_,_),
    nth0(Index, Board, house(Id,_,_,_,_,_,_), _),
    utils:replace_nth0(Board, Index, NewHouse, NewBoard).

board_size(Board, Size) :-
    length(Board, Size).

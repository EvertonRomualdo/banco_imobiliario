:- encoding(utf8).
:- module(ui, [
    print_main_menu/0,
    print_turn/2,
    print_message/1,
    print_roll/2,
    ask_line/1,
    print_player_menu/0,
    print_balance/1,
    print_position/1,
    print_properties/2,
    print_all_info/2,
    print_buy_result/3,
    print_rent_payment/4,
    print_bankrupt/1,
    print_turn_global/2,
    print_board/2
]).


print_turn(player(_,Name,Pos,Bal,_), TurnIndex) :-
    format("Turno ~w - Jogador ~w - Posição: ~w - Saldo: R$~w~n", [TurnIndex, Name, Pos, Bal]).

print_message(Msg) :- writeln(Msg).

print_roll(player(_,Name,_,_,_), Dice) :-
    format("~w rolou: ~w~n", [Name, Dice]).

ask_line(Input) :-
    read_line_to_string(user_input, Raw),
    string_lower(Raw, Input).

print_player_menu :-
    nl,
    writeln('--- Menu do Jogador ---'),
    writeln('1. Ver saldo'),
    writeln('2. Ver posição'),
    writeln('3. Ver propriedades'),
    writeln('4. Ver tudo'),
    writeln('5. Rolar dado (ou apenas ENTER)').

print_balance(player(_,Name,_,Bal,_)) :-
    format("~w possui R$~w~n", [Name, Bal]).

print_position(player(_,Name,Pos,_,_)) :-
    format("~w está na posição ~w~n", [Name, Pos]).

print_properties(player(Id,Name,_,_,_), Board) :-
    findall(HName,
        ( member(house(_,HName,city,_,_,Owner,_), Board),
          Owner \== none,
          Owner == Id
        ),
        Props),
    ( Props = [] ->
        format("~w não possui propriedades.~n", [Name])
    ; format("Propriedades de ~w: ~w~n", [Name, Props])
    ).

print_all_info(Player, Board) :-
    print_balance(Player),
    print_position(Player),
    print_properties(Player, Board).

print_buy_result(_, HName, bought) :-
    format("Você comprou a propriedade ~w!~n", [HName]).
print_buy_result(_, HName, declined) :-
    format("Você recusou comprar a propriedade ~w.~n", [HName]).
print_buy_result(_, HName, not_enough) :-
    format("Você não tem dinheiro suficiente para comprar ~w.~n", [HName]).

print_rent_payment(Payer, Receiver, HName, Amount) :-
    Payer = player(_,PName,_,_,_),
    Receiver = player(_,RName,_,_,_),
    format("~w pagou R$~w de aluguel para ~w pela casa ~w.~n", [PName, Amount, RName, HName]).

print_bankrupt(player(_,Name,_,_,_)) :-
    format("Jogador ~w faliu e saiu do jogo!~n", [Name]).

print_main_menu :-
    writeln('--- Menu Inicial ---'),
    writeln('1. Cadastrar jogadores'),
    writeln('2. Iniciar jogo'),
    writeln('3. Ver ranking'),
    writeln('0. Sair'),
    write('Escolha uma opção: ').



print_turn_global(player(_,Name,Pos,Bal,_), Turn) :-
    format("Turno ~d - Jogador ~w - Posição: ~d - Saldo: R$~d~n",
           [Turn, Name, Pos, Bal]).

%impresão do tabuleiro é limitada a 20 casas.
%Caso ultrapasse limite de 20 casas a execução falhara
%Nas regras do jogo o tabuleiro foi limitado a 20 casas
%Por simplificação apenas a inicial de cada jogador é mostrada

% função principal
print_board(Board, Players) :-
    length(Board, Len),
    ( Len =:= 20 ->
        nl, writeln("=== TABULEIRO ==="),
        print_square_board(Board, Players),
        writeln("=================")
    ).

print_square_board(Board, Players) :-
    TopIdxs = [0,1,2,3,4],
    RightIdxs = [5,6,7,8,9],
    BottomIdxs = [10,11,12,13,14],
    LeftIdxs = [15,16,17,18,19],

    % imprimimos topo (0..4)
    print_top_row(TopIdxs, Board, Players), nl,

    
    reverse(LeftIdxs, LeftRev),            
    print_vertical_rows(LeftRev, RightIdxs, Board, Players),

    
    reverse(BottomIdxs, BottomRev),
    print_top_row(BottomRev, Board, Players), nl.

% largura fixa de célula
cell_width(18).

% imprime uma linha de casas (topo ou base)
print_top_row([], _, _).
print_top_row([Idx|Rest], Board, Players) :-
    cell_text(Board, Players, Idx, Text),
    write("|"), write(Text),
    print_top_row(Rest, Board, Players).
print_top_row_end :- write("|").

% imprime as linhas verticais
print_vertical_rows([], [], _, _).
print_vertical_rows([L|Ls], [R|Rs], Board, Players) :-
    
    cell_text(Board, Players, L, LeftText),
    cell_text(Board, Players, R, RightText),

   
    cell_width(W), MidW is W * 3 + 2,  
    string_spaces(MidW, MidSpaces),

    
    write("|"), write(LeftText),
    write("|"), write(MidSpaces), write("|"),
    write(RightText), write("|"), nl,

    print_vertical_rows(Ls, Rs, Board, Players).


cell_text(Board, Players, Index, Padded) :-
    ( nth0(Index, Board, house(_, Name, _, _, _, _, _)) ->
        players_initials_at(Players, Index, PInit), % e.g. "E" ou "E,B"
        ( PInit == "" -> format(string(Base), "~w", [Name])
        ; format(string(Base), "~w(~w)", [Name, PInit])
        ),
        cell_width(W),
        pad_to_width(Base, W, Padded)
    ;  % índice fora do board: string vazia padded
        cell_width(W),
        pad_to_width("", W, Padded)
    ).


players_initials_at(Players, Index, Str) :-
    findall(Char,
        ( member(player(_, Name, Pos, _, _), Players),
          Pos =:= Index,
          string_codes(Name, Codes),
          Codes = [First|_],
          char_code(Char, First)
        ),
        Chars),
    ( Chars == [] -> Str = "" ; atomic_list_concat(Chars, ",", Str) ).


pad_to_width(Src, W, Out) :-
    string_length(Src, L),
    ( L >= W ->
        sub_string(Src, 0, W, _, Out)
    ;
        Pad is W - L,
        string_spaces(Pad, Spaces),
        string_concat(Src, Spaces, Out)
    ).

% cria string de N espaços
string_spaces(0, "") :- !.
string_spaces(N, Spaces) :-
    N > 0,
    length(L, N),
    maplist(=(' '), L),
    string_chars(Spaces, L).
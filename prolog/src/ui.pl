:- encoding(utf8).
:- module(ui, [
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
    print_bankrupt/1
]).

% Funções básicas que você já tinha
print_turn(player(_,Name,Pos,Bal,_), TurnIndex) :-
    format("Turno ~w - Jogador ~w - Posição: ~w - Saldo: R$~w~n", [TurnIndex, Name, Pos, Bal]).

print_message(Msg) :- writeln(Msg).

print_roll(player(_,Name,_,_,_), Dice) :-
    format("~w rolou: ~w~n", [Name, Dice]).

ask_line(Input) :-
    read_line_to_string(user_input, Raw),
    string_lower(Raw, Input).  % converte tudo para minúsculo para facilitar

% -----------------------------
% NOVO MENU
% -----------------------------
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
          Owner \== none,     % garante que não é 'none'
          Owner == Id         % compara simbolicamente
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

% Mensagens de compra
print_buy_result(_, HName, bought) :-
    format("Você comprou a propriedade ~w!~n", [HName]).
print_buy_result(_, HName, declined) :-
    format("Você recusou comprar a propriedade ~w.~n", [HName]).
print_buy_result(_, HName, not_enough) :-
    format("Você não tem dinheiro suficiente para comprar ~w.~n", [HName]).

% Pagamento de aluguel
print_rent_payment(Payer, Receiver, HName, Amount) :-
    Payer = player(_,PName,_,_,_),
    Receiver = player(_,RName,_,_,_),
    format("~w pagou R$~w de aluguel para ~w pela casa ~w.~n", [PName, Amount, RName, HName]).

print_bankrupt(player(_,Name,_,_,_)) :-
    format("Jogador ~w faliu e saiu do jogo!~n", [Name]).


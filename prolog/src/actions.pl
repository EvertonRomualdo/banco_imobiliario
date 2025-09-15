:- encoding(utf8).
:- module(actions, [
    take_turn/5   % take_turn(+Players, +TurnIndex, +Board, -NewPlayers, -NewBoard)
]).

:- use_module(utils).
:- use_module(board).
:- use_module(player).
:- use_module(ui).

% ============================
% Funções auxiliares de input
% ============================

% Lê "s" ou "n" (sem ponto, case-insensitive) e retorna yes/no
ask_yes_no(Response) :-
    read_line_to_string(user_input, Input),
    string_lower(Input, Lower),
    ( Lower = "s" -> Response = yes
    ; Lower = "n" -> Response = no
    ; writeln("Opção inválida! Digite apenas s ou n."),
      ask_yes_no(Response)
    ).

% ============================
% Menu do jogador
% ============================
print_player_menu :-
    nl,
    writeln('--- Menu do Jogador ---'),
    writeln('1. Ver saldo'),
    writeln('2. Ver posição'),
    writeln('3. Ver propriedades'),
    writeln('4. Ver tudo'),
    writeln('5. Rolar dado (ou apenas ENTER)').

show_player_menu_loop(CurrPlayer, Players, Board) :-
    print_player_menu,
    read_line_to_string(user_input, Input),
    ( Input = "1" ->
        ui:print_balance(CurrPlayer),
        show_player_menu_loop(CurrPlayer, Players, Board)
    ; Input = "2" ->
        ui:print_position(CurrPlayer),
        show_player_menu_loop(CurrPlayer, Players, Board)
    ; Input = "3" ->
        ui:print_properties(CurrPlayer, Board),
        show_player_menu_loop(CurrPlayer, Players, Board)
    ; Input = "4" ->
        ui:print_all_info(CurrPlayer, Board),
        show_player_menu_loop(CurrPlayer, Players, Board)
    ; Input = "" -> true  % ENTER para rolar dado
    ; Input = "5" -> true % opção explícita
    ; writeln("Opção inválida!"), show_player_menu_loop(CurrPlayer, Players, Board)
    ).

% ============================
% Turno do jogador
% ============================
take_turn(Players, TurnIndex, Board, NewPlayers, NewBoard) :-
    nth1(TurnIndex, Players, CurrPlayer),
    ( CurrPlayer = player(_,_,_,_,Blocked), Blocked > 0 ->
        NewBlocked is Blocked - 1,
        CurrPlayer = player(Id,Name,Pos,Bal,_),
        Updated = player(Id,Name,Pos,Bal,NewBlocked),
        update_player_in_list(Players, Updated, NewPlayers),
        NewBoard = Board,
        ui:print_message("Jogador está preso, turno consumido.")
    ; show_player_menu_loop(CurrPlayer, Players, Board),
      utils:roll_dice(Dice),
      ui:print_roll(CurrPlayer, Dice),
      CurrPlayer = player(Id,Name,Pos,Bal,Blk),
      board:board_size(Board, Size),
      NewPos is (Pos + Dice) mod Size,
      Moved = player(Id,Name,NewPos,Bal,Blk),
      handle_landing(Moved, Players, TurnIndex, Board, NewPlayers, NewBoard)
    ).

% ============================
% Ações ao cair em uma casa
% ============================
handle_landing(PlayerMoved, Players, _, Board, PlayersOut, BoardOut) :-
    PlayerMoved = player(Pid,Name,Pos,Bal,Blk),
    ( board:get_house_by_pos(Board, Pos, House) ->
        House = house(Hid,HName,HType,Price,Rent,Owner,Inc),
        ( HType = tax ->
            Tax = 50,
            player:pay(PlayerMoved, Tax, PAfter),
            format("~w pagou imposto de R$~w.~n", [Name, Tax]),
            update_player_in_list(Players, PAfter, PlayersOut),
            BoardOut = Board

        ; HType = prison ->
            P2 = player(Pid,Name,Pos,Bal,2),
            update_player_in_list(Players, P2, PlayersOut),
            BoardOut = Board,
            writeln("Você foi para a prisão por 2 turnos!")

        ; HType = city ->
            ( Owner = none ->
                format("Casa livre: ~w - preço R$~w~n", [HName, Price]),
                format("Deseja comprar? (s/n) "),
                ask_yes_no(Resp),
                ( Resp = yes ->
                    ( Bal >= Price ->
                        player:pay(PlayerMoved, Price, BoughtPlayer),
                        NewHouse = house(Hid,HName,HType,Price,Rent,Pid,Inc),
                        board:update_house_in_board(Board, NewHouse, Board1),
                        update_player_in_list(Players, BoughtPlayer, Players1),
                        ui:print_buy_result(BoughtPlayer, HName, bought),
                        PlayersOut = Players1, BoardOut = Board1
                    ;
                        ui:print_buy_result(PlayerMoved, HName, not_enough),
                        update_player_in_list(Players, PlayerMoved, PlayersOut),
                        BoardOut = Board
                    )
                ;
                    ui:print_buy_result(PlayerMoved, HName, declined),
                    update_player_in_list(Players, PlayerMoved, PlayersOut),
                    BoardOut = Board
                )
            ;
                ( Owner =:= Pid ->
                    update_player_in_list(Players, PlayerMoved, PlayersOut),
                    BoardOut = Board
                ;
                    RentAmount = Rent,
                    player:pay(PlayerMoved, RentAmount, Payer),
                    ( member(player(Owner,ON,OP,OB,OBk), Players) ->
                        player:receive(player(Owner,ON,OP,OB,OBk), RentAmount, Receiver),
                        update_player_in_list(Players, Payer, PlayersTmp),
                        update_player_in_list(PlayersTmp, Receiver, PlayersOut),
                        ui:print_rent_payment(Payer, Receiver, HName, RentAmount),
                        BoardOut = Board
                    ;
                        update_player_in_list(Players, PlayerMoved, PlayersOut),
                        BoardOut = Board
                    )
                )
            )
        ;
            update_player_in_list(Players, PlayerMoved, PlayersOut),
            BoardOut = Board
        )
    ;
        update_player_in_list(Players, PlayerMoved, PlayersOut),
        BoardOut = Board
    ).

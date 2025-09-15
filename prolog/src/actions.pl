:- encoding(utf8).
:- module(actions, [
    take_turn/5   % take_turn(+Players, +TurnIndex, +Board, -NewPlayers, -NewBoard)
]).

:- use_module(utils).
:- use_module(board).
:- use_module(player).
:- use_module(ui).


ask_yes_no(Response) :-
    read_line_to_string(user_input, Input0),
    string_lower(Input0, Input),
    ( Input = "s" -> Response = yes
    ; Input = "n" -> Response = no
    ; writeln("Opção inválida! Digite apenas s ou n."),
      ask_yes_no(Response)
    ).


% Menu do jogador

print_player_menu :-
    nl,
    writeln('--- Menu do Jogador ---'),
    writeln('1. Ver saldo'),
    writeln('2. Ver posição'),
    writeln('3. Ver propriedades'),
    writeln('4. Ver tudo'),
    writeln('5. Rolar dado (ou apenas ENTER)').

%interação com o menu
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


%média dos saldos
avg_balance(Players, Avg) :-
    findall(B, (member(player(_,_,_,B,_), Players)), Balances),
    sum_list(Balances, Sum),
    length(Balances, Count),
    ( Count > 0 -> Avg is Sum / Count ; Avg = 0 ).

% Decisões do BOT
decide_buy(player(_,_,_,Bal,_), Price, Players, Decision) :-
    avg_balance(Players, Avg),
    ( Bal > Avg, Bal >= Price -> Decision = yes ; Decision = no ).

decide_sell(player(_,_,_,Bal,_), Players, Decision) :-
    avg_balance(Players, Avg),
    ( Bal < Avg -> Decision = yes ; Decision = no ).

decide_bid(player(_,_,_,Bal,_), Players, Bid) :-
    avg_balance(Players, Avg),
    HalfAvg is Avg / 2,
    ( Bal > HalfAvg -> Bid is Bal // 2 ; Bid = 0 ).


% Turno principal
%existe sepração entre jogador pessoa e bots
% detecta BOT pelo nome começando com "BOT"
take_turn(Players, TurnIndex, Board, NewPlayers, NewBoard) :-
    nth1(TurnIndex, Players, CurrPlayer),
    CurrPlayer = player(_,Name,_,_,_),
    ( sub_atom(Name, 0, 3, _, "BOT") ->
        bot_turn(CurrPlayer, Players, TurnIndex, Board, NewPlayers, NewBoard)
    ;
        human_turn(CurrPlayer, Players, TurnIndex, Board, NewPlayers, NewBoard)
    ).

% Turno humano
human_turn(CurrPlayer, Players, TurnIndex, Board, NewPlayers, NewBoard) :-
    show_player_menu_loop(CurrPlayer, Players, Board),
    utils:roll_dice(Dice),
    ui:print_roll(CurrPlayer, Dice),
    CurrPlayer = player(Id,Name,Pos,Bal,Blk),
    board:board_size(Board, Size),
    NewPos is (Pos + Dice) mod Size,
    Moved = player(Id,Name,NewPos,Bal,Blk),
    handle_landing(Moved, Players, TurnIndex, Board, NewPlayers, NewBoard).

% Turno bot
bot_turn(CurrPlayer, Players, TurnIndex, Board, NewPlayers, NewBoard) :-
    utils:roll_dice(Dice),
    ui:print_roll(CurrPlayer, Dice),
    CurrPlayer = player(Id,Name,Pos,Bal,Blk),
    board:board_size(Board, Size),
    NewPos is (Pos + Dice) mod Size,
    Moved = player(Id,Name,NewPos,Bal,Blk),
    handle_landing_bot(Moved, Players, TurnIndex, Board, NewPlayers, NewBoard).


% Move player 
move_player(player(Id,Name,Pos,Bal,Blk), Dice, player(Id,Name,NewPos,Bal,Blk), Board, Board) :-
    board:board_size(Board, Size),
    NewPos is (Pos + Dice) mod Size.


% Interação com as casas do tabuleiro
handle_landing(PlayerMoved, Players, _TurnIndex, Board, PlayersOut, BoardOut) :-
    PlayerMoved = player(Pid,Name,Pos,Bal,Blk),
    ( board:get_house_by_pos(Board, Pos, House) ->
        House = house(Hid,HName,HType,Price,Rent,Owner,Inc),
        ( HType = tax ->
            Tax = 50,
            player:pay(PlayerMoved, Tax, PAfter),
            format("~w pagou imposto de R$~w.~n", [Name, Tax]),
            ( player:is_bankrupt(PAfter) ->
                ui:print_bankrupt(PAfter),
                remove_player_by_id(Players, Pid, PlayersOut),
                BoardOut = Board
            ;
                update_player_in_list(Players, PAfter, PlayersOut),
                BoardOut = Board
            )

        ; HType = prison ->
            P2 = player(Pid,Name,Pos,Bal,2),
            update_player_in_list(Players, P2, PlayersOut),
            BoardOut = Board,
            writeln("Você foi para a prisão por 2 turnos!")

        ; HType = city ->
            ( Owner = none ->
                % casa livre: pergunta para comprar
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
                        update_player_in_list(Players, PlayerMoved, Players1),
                        PlayersOut = Players1, BoardOut = Board
                    )
                ;
                    ui:print_buy_result(PlayerMoved, HName, declined),
                    update_player_in_list(Players, PlayerMoved, Players1),
                    PlayersOut = Players1, BoardOut = Board
                )
            ; Owner =:= Pid ->
                % DONO caiu na própria casa
                update_player_in_list(Players, PlayerMoved, Players1),  % garante estado atualizado
                format("Você caiu na sua própria propriedade (~w). Deseja colocá-la em leilão? (s/n) ", [HName]),
                ask_yes_no(SellResp),
                ( SellResp = yes ->
                    start_auction(PlayerMoved, HName, Players1, Board, PlayersOut, BoardOut)
                ;
                    PlayersOut = Players1,
                    BoardOut = Board
                )
            ;
                % casa tem dono diferente
                RentAmount = Rent,
                player:pay(PlayerMoved, RentAmount, Payer),
                ( player:is_bankrupt(Payer) ->
                    ui:print_bankrupt(Payer),
                    remove_player_by_id(Players, Pid, PlayersOut),
                    BoardOut = Board
                ;
                    ( member(player(Owner,ON,OP,OB,OBk), Players) ->
                        player:receive(player(Owner,ON,OP,OB,OBk), RentAmount, Receiver),
                        update_player_in_list(Players, Payer, TempPlayers),
                        update_player_in_list(TempPlayers, Receiver, PlayersOut),
                        BoardOut = Board,
                        ui:print_rent_payment(Payer, Receiver, HName, RentAmount)
                    ;
                        update_player_in_list(Players, PlayerMoved, PlayersOut),
                        BoardOut = Board
                    )
                )
            )
        ;
            % outros tipos
            update_player_in_list(Players, PlayerMoved, PlayersOut),
            BoardOut = Board
        )
    ;
        % posição sem casa *crash
        update_player_in_list(Players, PlayerMoved, PlayersOut),
        BoardOut = Board
    ).


% Tratamento de casa: BOT
handle_landing_bot(PlayerMoved, Players, _TurnIndex, Board, PlayersOut, BoardOut) :-
    PlayerMoved = player(Pid,Name,Pos,Bal,Blk),
    ( board:get_house_by_pos(Board, Pos, House) ->
        House = house(Hid,HName,HType,Price,Rent,Owner,Inc),
        ( HType = tax ->
            Tax = 50,
            player:pay(PlayerMoved, Tax, PAfterTax),
            format("~w pagou imposto de R$~w.~n", [Name, Tax]),
            ( player:is_bankrupt(PAfterTax) ->
                ui:print_bankrupt(PAfterTax),
                remove_player_by_id(Players, Pid, PlayersOut),
                BoardOut = Board
            ;
                update_player_in_list(Players, PAfterTax, PlayersOut),
                BoardOut = Board
            )

        ; HType = prison ->
            P2 = player(Pid,Name,Pos,Bal,2),
            update_player_in_list(Players, P2, PlayersOut),
            BoardOut = Board,
            format("~w foi para a prisão por 2 turnos!~n", [Name])

        ; HType = city ->
            ( Owner = none ->
                % casa livre: BOT decide via decide_buy
                decide_buy(PlayerMoved, Price, Players, Decision),
                ( Decision = yes ->
                    ( Bal >= Price ->
                        player:pay(PlayerMoved, Price, BoughtPlayer),
                        NewHouse = house(Hid,HName,HType,Price,Rent,Pid,Inc),
                        board:update_house_in_board(Board, NewHouse, Board1),
                        update_player_in_list(Players, BoughtPlayer, Players1),
                        BoardOut = Board1,
                        PlayersOut = Players1,
                        format("BOT ~w comprou ~w por R$~w.~n", [Name, HName, Price])
                    ;
                        ui:print_buy_result(PlayerMoved, HName, not_enough),
                        update_player_in_list(Players, PlayerMoved, Players1),
                        PlayersOut = Players1, BoardOut = Board
                    )
                ;
                    update_player_in_list(Players, PlayerMoved, Players1),
                    PlayersOut = Players1, BoardOut = Board,
                    format("BOT ~w recusou comprar ~w.~n", [Name, HName])
                )
            ; Owner =:= Pid ->
                % BOT caiu na própria casa -> pode vender via leilão conforme decide_sell
                update_player_in_list(Players, PlayerMoved, Players1),
                decide_sell(PlayerMoved, Players1, SellDecision),
                ( SellDecision = yes ->
                    format("BOT ~w decidiu colocar ~w em leilão.~n", [Name, HName]),
                    start_auction(PlayerMoved, HName, Players1, Board, PlayersOut, BoardOut)
                ;
                    PlayersOut = Players1, BoardOut = Board
                )
            ;
                % pagar aluguel
                RentAmount = Rent,
                player:pay(PlayerMoved, RentAmount, Payer),
                ( player:is_bankrupt(Payer) ->
                    ui:print_bankrupt(Payer),
                    remove_player_by_id(Players, Pid, PlayersOut),
                    BoardOut = Board
                ;
                    ( member(player(Owner,ON,OP,OB,OBk), Players) ->
                        player:receive(player(Owner,ON,OP,OB,OBk), RentAmount, Receiver),
                        update_player_in_list(Players, Payer, TempPlayers),
                        update_player_in_list(TempPlayers, Receiver, PlayersOut),
                        BoardOut = Board,
                        format("BOT ~w pagou R$~w de aluguel para ~w pela casa ~w.~n", [Name, RentAmount, ON, HName])
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

% Leilão - vendedor é quem colocou a casa em leilão
start_auction(Seller, HName, Players, Board, NewPlayers, NewBoard) :-
    format("Iniciando leilão da propriedade ~w~n", [HName]),
    Seller = player(SellerId, _, _, _, _),
    exclude(is_player_id(SellerId), Players, Bidders),
    collect_bids(Bidders, Seller, HName, Players, Board, RawBids),
    include(bid_positive, RawBids, Bids),
    ( Bids = [] ->
        writeln("Nenhum jogador participou do leilão."),
        NewPlayers = Players,
        NewBoard = Board
    ;
        highest_bid(Bids, player_bid(HighestBidder, Amount)),
        % transfere dinheiro: paga HighestBidder e recebe Seller
        player:pay(HighestBidder, Amount, HighestBidderPaid),
        player:receive(Seller, Amount, SellerPaid),
        % atualiza dono da casa para o novo proprietário (HighestBidderPaid)
        update_house_owner(HName, Board, HighestBidderPaid, NewBoard),
        % atualiza lista de jogadores substituindo os dois
        update_player_in_list(Players, HighestBidderPaid, TempPlayers),
        update_player_in_list(TempPlayers, SellerPaid, NewPlayers),
        format("Propriedade ~w vendida para ~w por R$~w~n",
               [HName, HighestBidderPaid, Amount])
    ).

bid_positive(player_bid(_, Amount)) :- Amount > 0.

% collect_bids(+Bidders, +Seller, +HName, +PlayersFull, +Board, -RawBids)
collect_bids([], _, _, _, _, []).
collect_bids([Player|Rest], Seller, HName, PlayersFull, Board, [player_bid(Player,Bid)|OtherBids]) :-
    Player = player(_, Name, _, Bal, _),
    ( sub_atom(Name, 0, 3, _, "BOT") ->
        decide_bid(Player, PlayersFull, Bid)
    ;
        format("Jogador ~w, insira seu lance para ~w (saldo R$~w, ENTER para pular): ", [Name, HName, Bal]),
        read_line_to_string(user_input, Input),
        ( Input = "" -> Bid = 0
        ; ( catch(number_string(Num, Input), _, fail), Num =< Bal -> Bid = Num
          ; writeln("Valor inválido, considerada oferta = 0"), Bid = 0
          )
        )
    ),
    collect_bids(Rest, Seller, HName, PlayersFull, Board, OtherBids).

% encontra maior lance
highest_bid([player_bid(P,A)], player_bid(P,A)).
highest_bid([player_bid(P1,A1)|T], Best) :-
    highest_bid(T, player_bid(P2,A2)),
    ( A1 >= A2 -> Best = player_bid(P1,A1) ; Best = player_bid(P2,A2) ).

% atualiza o dono da casa pelo nome
update_house_owner(HName, Board, NewOwnerPlayer, NewBoard) :-
    NewOwnerPlayer = player(NewOwnerId, _, _, _, _),
    member(house(Id,HName,HType,Price,Rent,_,Inc), Board),
    NewHouse = house(Id,HName,HType,Price,Rent,NewOwnerId,Inc),
    board:update_house_in_board(Board, NewHouse, NewBoard).

% helper local (usa no exclude)
is_player_id(Id, player(Id,_,_,_,_)).
is_player_id(_, _) :- fail.

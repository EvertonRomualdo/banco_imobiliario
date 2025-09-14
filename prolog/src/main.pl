:- encoding(utf8).
:- module(main, [start_game/0]).
:- use_module(game).
:- use_module(player).
:- use_module(ui).
:- use_module(ranking).

:- dynamic stored_players/1.
stored_players([]).

start_game :-
    writeln('=== Banco Imobiliário (Prolog) ==='),
    retractall(stored_players(_)),
    assertz(stored_players([])),
    main_menu.

main_menu :-
    nl,
    ui:print_main_menu,
    read_line_to_string(user_input, Input),
    ( Input = "1" ->
        player:register_players(Players),
        retractall(stored_players(_)),
        assertz(stored_players(Players)),
        writeln("Jogadores cadastrados com sucesso!"),
        main_menu
    ; Input = "2" ->
        stored_players(Players),
        ( Players = [] ->
            writeln("Nenhum jogador cadastrado. Cadastre jogadores primeiro!"),
            main_menu
        ;
            game:init_game_state(Players, GameState),
            game:game_loop(GameState),
            main_menu
        )
    ; Input = "3" ->
        ranking:show_ranking,
        main_menu
    ; Input = "0" ->
        writeln("Saindo do jogo...")
    ;
        writeln("Opção inválida!"),
        main_menu
    ).

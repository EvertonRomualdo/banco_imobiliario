:- encoding(utf8).
:- module(ranking, [
    update_ranking/2,
    show_ranking/0
]).

:- use_module(library(lists)).

ranking_file("ranking.txt").

% Atualiza estatísticas após fim do jogo
% +Winner (player), +AllPlayers
update_ranking(Winner, Players) :-
    Winner = player(_, WinnerName, _, _, _),
    findall(Name,
        ( member(player(_, Name, _, _, _), Players), Name \== WinnerName ),
        Losers),
    load_ranking(Current),
    update_winner(WinnerName, Current, Updated1),
    update_losers(Losers, Updated1, UpdatedFinal),
    save_ranking(UpdatedFinal).

update_winner(Name, Ranking, Updated) :-
    ( select(entry(Name, W, L), Ranking, Rest) ->
        W1 is W + 1,
        Updated = [entry(Name, W1, L) | Rest]
    ;
        Updated = [entry(Name, 1, 0) | Ranking]
    ).

update_losers([], R, R).
update_losers([Name|Rest], Ranking, Updated) :-
    ( select(entry(Name, W, L), Ranking, Temp) ->
        L1 is L + 1,
        NewR = [entry(Name, W, L1) | Temp]
    ;
        NewR = [entry(Name, 0, 1) | Ranking]
    ),
    update_losers(Rest, NewR, Updated).

% Carrega ranking de arquivo
load_ranking(Ranking) :-
    ranking_file(File),
    ( exists_file(File) ->
        open(File, read, Stream),
        read_lines(Stream, Lines),
        close(Stream),
        maplist(parse_entry, Lines, Ranking)
    ;
        Ranking = []
    ).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).
read_lines(Stream, [Line|Rest]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    read_lines(Stream, Rest).

parse_entry(Line, entry(Name, W, L)) :-
    split_string(Line, " ", "", [NameS, WS, LS]),
    atom_string(Name, NameS),
    number_string(W, WS),
    number_string(L, LS).

% Salva ranking em arquivo
save_ranking(Ranking) :-
    ranking_file(File),
    open(File, write, Stream),
    forall(member(entry(Name, W, L), Ranking),
        format(Stream, "~w ~w ~w~n", [Name, W, L])),
    close(Stream).

% Exibe ranking
show_ranking :-
    load_ranking(Ranking),
    ( Ranking = [] ->
        writeln("Nenhum histórico disponível.")
    ;
        writeln("=== Ranking de Jogadores ==="),
        forall(member(entry(Name, W, L), Ranking),
            format("~w - Vitórias: ~w | Derrotas: ~w~n", [Name, W, L]))
    ).

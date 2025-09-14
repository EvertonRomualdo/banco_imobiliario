:- encoding(utf8).
:- module(ranking, [
    show_ranking/0,
    update_stats/2
]).

:- use_module(library(readutil)).

ranking_file("prolog/data/ranking.txt").

% Mostra ranking
show_ranking :-
    ranking_file(File),
    ( exists_file(File) ->
        writeln("--- Ranking de Jogadores ---"),
        read_file_to_string(File, Content, []),
        writeln(Content)
    ; writeln("Nenhum ranking disponível ainda.")
    ).

% Atualiza estatísticas
update_stats(Name, Result) :-
    ranking_file(File),
    ( exists_file(File) ->
        read_file_to_terms(File, Terms, [])
    ; Terms = []
    ),
    update_player_stats(Terms, Name, Result, NewTerms),
    open(File, write, Out),
    forall(member(stat(N,V,D), NewTerms),
           format(Out, "~w ~w ~w~n", [N,V,D])),
    close(Out).

% Atualiza estatísticas de um jogador
update_player_stats([], Name, Result, [stat(Name,NewV,NewD)]) :-
    ( Result = win -> NewV = 1, NewD = 0
    ; Result = loss -> NewV = 0, NewD = 1 ).
update_player_stats([stat(Name,V,D)|Rest], Name, Result, [stat(Name,V1,D1)|Rest]) :-
    ( Result = win -> V1 is V+1, D1 = D
    ; Result = loss -> D1 is D+1, V1 = V ).
update_player_stats([H|Rest], Name, Result, [H|NewRest]) :-
    update_player_stats(Rest, Name, Result, NewRest).

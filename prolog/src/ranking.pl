:- encoding(utf8).
:- module(ranking, [
    show_ranking/0,
    update_stats/2
]).

:- use_module(library(readutil)).

ranking_file("../data/ranking.txt").

% ========================================
% Mostra o ranking
% ========================================
show_ranking :-
    ranking_file(File),
    ( exists_file(File) ->
        writeln('--- Ranking de Jogadores ---'),
        read_file_to_string(File, Content, []),
        split_string(Content, "\n", "\s\t\n", Lines),
        maplist(print_ranking_line, Lines)
    ; writeln("Nenhum ranking disponível ainda.")
    ).

print_ranking_line(Line) :-
    ( Line = "" -> true ; writeln(Line) ).

% ========================================
% Atualiza estatísticas do jogador
% ========================================
update_stats(Name, Result) :-
    ranking_file(File),
    ( exists_file(File) ->
        read_file_to_string(File, Content, []),
        split_string(Content, "\n", "\s\t\n", Lines),
        include(\=( ""), Lines, NonEmpty),
        maplist(parse_line, NonEmpty, Terms0)
    ; Terms0 = []
    ),
    update_player_stats(Terms0, Name, Result, NewTerms),
    open(File, write, Out),
    forall(member(stat(N,V,D), NewTerms),
           format(Out, "~w ~w ~w~n", [N,V,D])),
    close(Out).

% ========================================
% Conversão de linhas para fatos em memória
% ========================================
parse_line(Line, stat(N,V,D)) :-
    split_string(Line, " ", "", [NStr,VStr,DStr]),
    N = NStr,
    number_string(V, VStr),
    number_string(D, DStr).

% ========================================
% Atualiza ou cria o registro do jogador
% ========================================
update_player_stats([], Name, Result, [stat(Name,V,D)]) :-
    ( Result = win -> V = 1, D = 0 ; V = 0, D = 1 ).
update_player_stats([stat(Name,V,D)|Rest], Name, Result, [stat(Name,V1,D1)|Rest]) :-
    ( Result = win -> V1 is V+1, D1 = D ; V1 = V, D1 is D+1 ).
update_player_stats([H|Rest], Name, Result, [H|NewRest]) :-
    update_player_stats(Rest, Name, Result, NewRest).

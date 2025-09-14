:- encoding(utf8).
:- module(utils, [
    roll_dice/1,
    replace_nth0/4
]).

:- use_module(library(random)).

roll_dice(N) :-
    random_between(1, 6, N).

replace_nth0([_|T], 0, X, [X|T]).
replace_nth0([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace_nth0(T, I1, X, R).

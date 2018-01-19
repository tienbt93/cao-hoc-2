%:- use_rendering(sudoku).
:- use_module(library(clpfd)).
sudoku(Rows) :-
        length(Rows, 9), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        blocks(As, Bs, Cs),
        blocks(Ds, Es, Fs),
        blocks(Gs, Hs, Is),
        show(Rows).
blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
        all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
        blocks(Ns1, Ns2, Ns3).
show([H|R]):-
    write(H),nl,
    show(R).
problem(1, [[_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,3,_,8,5],
            [_,_,1,_,2,_,_,_,_],

            [_,_,_,5,_,7,_,_,_],
            [_,_,4,_,_,_,1,_,_],
            [_,9,_,_,_,_,_,_,_],

            [5,_,_,_,_,_,_,7,3],
            [_,_,2,_,1,_,_,_,_],
            [_,_,_,_,4,_,_,_,9]]).
////////////////////////////////////////////////////
use_module(library(lists)).
/*
N-queens problem solved in Prolog (SWI-Prolog).
Usage es: n_queen(4,X).
*/

n_queen(N) :-
	%create a list of N dummy variabiles
	length(Solution, N),

	queen(Solution, N),
        show(Solution). %search for a configuration of N queens


%returns a list of integer from K to N included es up2N(1,3,X) X = [1,2,3]
up2N(N,N,[N]) :-!.
up2N(K,N,[K|Tail]) :- K < N, K1 is K+1, up2N(K1, N, Tail).


queen([],_). %No queens is a solution for any N queens problem. All queens are in a safe position.

queen([Q|Qlist],N) :-

	queen(Qlist, N), %first we solve the subproblem

	%we then generate all possible positions for queen Q
	up2N(1,N,Candidate_positions_for_queenQ),

	%we pick one of such position
	member(Q, Candidate_positions_for_queenQ),

	%we check whether the queen Q is safe
	check_solution(Q,Qlist, 1).



check_solution(_,[], _).

check_solution(Q,[Q1|Qlist],Xdist) :-
	Q =\= Q1, %not on the same row
	Test is abs(Q1-Q),
	Test =\= Xdist, %diagonal distance
	Xdist1 is Xdist + 1,
	check_solution(Q,Qlist,Xdist1).
show(T):-
    write(T),nl.

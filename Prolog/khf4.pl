% Importok
:- use_module(library(lists)).
:- use_module(library(between)).

% Szabályok a sátorkoordináta származtatására a fa koordinátákból.
tent_coordinate(FY-FX, n, SY-SX) :- SY is FY - 1, SX = FX.
tent_coordinate(FY-FX, e, SY-SX) :- SY = FY, SX is FX + 1.
tent_coordinate(FY-FX, s, SY-SX) :- SY is FY + 1, SX = FX.
tent_coordinate(FY-FX, w, SY-SX) :- SY = FY, SX is FX - 1.

% Kicserél elemeket az adott indexnél.
replace_element_at_index([_|T], 1, Element, [Element|T]).
replace_element_at_index([H|T], Index, Element, [H|Rest]) :-
    Index > 1,
    NewIndex is Index - 1,
    replace_element_at_index(T, NewIndex, Element, Rest).

% Létrehoz egy mátrixot adott dimenziókkal, és értékekkel.
initialize_matrix(N, M, Value, Matrix) :-
    length(Row, M),
    maplist(=(Value), Row),
    findall(Row, between(1, N, _), Matrix).

% :- pred satrak_mx(NM::in, int-int: egy parcella helyét meghatározó egészszám-pár
%                   Fs::in, list(NM): a fák helyeit tartalmazó lista          
%                   Ss::in, list(direction): a fákhoz tartozó sátrak irányát megadó lista
%                   Mx::out, list(list(bool)): a sátrak helyét leíró 0-1 mátrix
%                   ).
satrak_mx(N-M, TreeList, TentList, Matrix) :-
    length(TreeList, L),
    initialize_matrix(N, M, 0, InitMatrix),
    create_solution(N-M, TreeList, TentList, InitMatrix, Matrix, L).

% Rekurzív segéd eljárás, a fák és a hozzájuk tartozó sátrak alapján elhelyezi a sátrakat egy mátrixban. 
% Az I változó határozza meg, hogy melyik fát és sátort kell elhelyezni, ha sikeresen elhelyezte ugrik a következőre.
create_solution(_, _, _, CurrentTents, ResultMatrix, 0) :- CurrentTents = ResultMatrix.
create_solution(N-M, Trees, TentDirections, CurrentTents, ResultMatrix, I) :-
    nth1(I, Trees, TreeY-TreeX),
    nth1(I, TentDirections, TentDirection),
    tent_coordinate(TreeY-TreeX, TentDirection, SY-SX),
    between(1, N, SY),
    between(1, M, SX),
    nth1(SY, CurrentTents, Row),
    nth1(SX, Row, 0),
    replace_element_at_index(Row, SX, 1, UpdatedRow),
    replace_element_at_index(CurrentTents, SY, UpdatedRow, NewTents),
    J is I-1,
    create_solution(N-M, Trees, TentDirections, NewTents, ResultMatrix, J).

%__________________________________________________GYURMÁZÓ__________________________________________________%
test_case(1, satrak_mx(2-3, [2-2], [n], Mx), Mx, [[0,1,0],[0,0,0]]).
test_case(2, satrak_mx(2-3, [2-2], [e], Mx), Mx, [[0,0,0],[0,0,1]]).
test_case(3, satrak_mx(2-3, [2-2], [s], Mx), Mx, _).
test_case(4, satrak_mx(2-3, [1-1,2-2], [e,n], Mx), Mx, _).
test_case(5, satrak_mx(4-5, [2-4,1-5,3-2], [s,w,w], Mx), Mx, [[0,0,0,1,0],[0,0,0,0,0],[1,0,0,1,0],[0,0,0,0,0]]).
test_case(6, satrak_mx(6-6, [1-3,1-5,3-1,4-6,5-1,5-4,6-5], [s,e,n,w,e,s,e], Mx), Mx, [[0,0,0,0,0,1],[1,0,1,0,0,0],[0,0,0,0,0,0],[0,0,0,0,1,0],[0,1,0,0,0,0],[0,0,0,1,0,1]]).
run_tests :-
    test_case(ID, Query, Result, Expected),
    ( call(Query) ->
        ( Result = Expected ->
            format("Test ~w PASSED~n", [ID])
        ;
            format("Test ~w FAILED. Expected ~w but got ~w~n", [ID, Expected, Result])
        )
    ;
        format("Test ~w gave no result.~n", [ID])
    ),
    fail.
run_tests. % Visszatér ha minden teszt lefutott.
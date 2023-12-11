% Importok
:- use_module(library(lists)).

% Szabályok a sátorkoordináta származtatására a fa koordinátákból.
tent_coordinate(TreeY-TreeX, n, TentY-TentX) :- TentY is TreeY - 1, TentX = TreeX.
tent_coordinate(TreeY-TreeX, e, TentY-TentX) :- TentY = TreeY, TentX is TreeX + 1.
tent_coordinate(TreeY-TreeX, s, TentY-TentX) :- TentY is TreeY + 1, TentX = TreeX.
tent_coordinate(TreeY-TreeX, w, TentY-TentX) :- TentY = TreeY, TentX is TreeX - 1.

% Db | TentCondition: az I. sorbeli vagy J. oszlopbeli sátrak száma Db
% B | CertainTreeSum: 
% E | UncertainTreeSum: 

% Case where InSum < TentCondition
osszeg_szukites(TreeList, SumCondition, DirListIn, []) :-
    match_sum_condition(SumCondition, TentCondition),
    be_szamol(TreeList, SumCondition, 0, 0, DirListIn, CertainTreeSum, UncertainTreeSum),
    InSum is CertainTreeSum + UncertainTreeSum,
    InSum < TentCondition.

% Case where InSum = TentCondition
osszeg_szukites(TreeList, SumCondition, DirListIn, DirListOut) :-
    match_sum_condition(SumCondition, TentCondition),
    be_szamol(TreeList, SumCondition, 0, 0, DirListIn, CertainTreeSum, UncertainTreeSum),
    InSum is CertainTreeSum + UncertainTreeSum,
    InSum = TentCondition,
    ils_szur(DirListIn, TreeList, SumCondition, [], DirListOut).

% Case where CertainTreeSum = TentCondition
osszeg_szukites(TreeList, SumCondition, DirListIn, DirListOut) :-
    match_sum_condition(SumCondition, TentCondition),
    be_szamol(TreeList, SumCondition, 0, 0, DirListIn, CertainTreeSum, _UncertainTreeSum),
    CertainTreeSum = TentCondition,
    ils_szur_iii(DirListIn, TreeList, SumCondition, [], DirListOut).

% Case where CertainTreeSum > TentCondition
osszeg_szukites(TreeList, SumCondition, DirListIn, []) :-
    match_sum_condition(SumCondition, TentCondition),
    be_szamol(TreeList, SumCondition, 0, 0, DirListIn, CertainTreeSum, _UncertainTreeSum),
    CertainTreeSum > TentCondition.

% Helper predicate to match SumCondition and get TentCondition
match_sum_condition(SumCondition, TentCondition) :-
    sor(_, TentCondition) = SumCondition;
    oszl(_, TentCondition) = SumCondition.

szomszed_b([], _).
szomszed_b([SzHY-SzHX|SzT], SumCondition) :-
    ((oszl(J,_) = SumCondition, SzHX = J);
    (sor(I,_) = SumCondition, SzHY = I)),
    szomszed_b(SzT, SumCondition).

szomszed_e([], _) :- false.
szomszed_e([SzHY-SzHX|SzT], SumCondition) :-
    ((oszl(J,_) = SumCondition, SzHX = J);
    (sor(I,_) = SumCondition, SzHY = I));
    szomszed_e(SzT, SumCondition).

be_szamol([], _, BW, EW, [], CertainTreeSum, UncertainTreeSum) :- CertainTreeSum = BW, UncertainTreeSum = EW.
be_szamol([FH|FT], SumCondition, BW, EW, [ILsH|ILsT], CertainTreeSum, UncertainTreeSum) :-
    szomszed_general(FH, ILsH, [], Szomszedok),
    (szomszed_b(Szomszedok, SumCondition) -> (Uj_B is BW + 1, Uj_E = EW);
    (Uj_B = BW, (szomszed_e(Szomszedok, SumCondition) -> Uj_E is EW + 1; Uj_E = EW))),
    be_szamol(FT, SumCondition, Uj_B, Uj_E, ILsT, CertainTreeSum, UncertainTreeSum).

il_szur(_, _, [], RegiIL, UjIL, IL_ki) :-
    (length(UjIL, 0) -> IL_ki = RegiIL; IL_ki = UjIL).
il_szur(Fa, SumCondition, [ILH|ILT], RegiIL, UjIL, IL_ki):-
    tent_coordinate(Fa, ILH, SY-SX),
    ((sor(I,_) = SumCondition, (SY = I -> append(UjIL, [ILH], UjIL2); UjIL2 = UjIL));
    (oszl(J,_) = SumCondition, (SX = J -> append(UjIL, [ILH], UjIL2); UjIL2 = UjIL))),
    il_szur(Fa, SumCondition, ILT, RegiIL, UjIL2, IL_ki).

ils_szur([], [], _, ILs1, ILs_ki) :- ILs_ki = ILs1.
ils_szur([ILs0H|ILs0T], [FakH|FakT], SumCondition, ILs1, ILs_ki) :-
    il_szur(FakH, SumCondition, ILs0H, ILs0H, [], Uj),
    append(ILs1, [Uj], ILs2),
    ils_szur(ILs0T, FakT, SumCondition, ILs2, ILs_ki).

szomszed_general(_, [], Lista, Lista_ki) :- Lista_ki = Lista.
szomszed_general(Fa, [IH|IT], Lista, Lista_ki) :-
    tent_coordinate(Fa, IH, S),
    append(Lista, [S], Lista2),
    szomszed_general(Fa, IT, Lista2, Lista_ki).

il_szur_iii(_, _, [], RegiIL, UjIL, IL_ki) :-
    (length(UjIL, 0) -> IL_ki = RegiIL; IL_ki = UjIL).
il_szur_iii(Fa, SumCondition, [ILH|ILT], RegiIL, UjIL, IL_ki):-
    tent_coordinate(Fa, ILH, SY-SX),
    ((sor(I,_) = SumCondition, (SY \= I -> append(UjIL, [ILH], UjIL2); UjIL2 = UjIL));
    (oszl(J,_) = SumCondition, (SX \= J -> append(UjIL, [ILH], UjIL2); UjIL2 = UjIL))),
    il_szur_iii(Fa, SumCondition, ILT, RegiIL, UjIL2, IL_ki).

ils_szur_iii([], [], _, ILs1, ILs_ki) :-
    ILs_ki = ILs1.
ils_szur_iii([ILs0H|ILs0T], [FakH|FakT], SumCondition, ILs1, ILs_ki) :-
    il_szur_iii(FakH, SumCondition, ILs0H, ILs0H, [], Uj),
    append(ILs1, [Uj], ILs2),
    ils_szur_iii(ILs0T, FakT, SumCondition, ILs2, ILs_ki).

%_____________________________GYURMÁZÓ_____________________________%
test_osszeg_szukites :-
    run_test_osszeg([6-6,9-7], oszl(7,2), [[n,s,w],[e,n,s,w]], []),
    run_test_osszeg([6-6,9-7], oszl(7,2), [[e,n,s,w],[e,n,s,w]], [[e],[n,s]]),
    run_test_osszeg([6-6,9-7], oszl(7,2), [[e,n,s,w],[n,s]], [[e],[n,s]]),
    run_test_osszeg([6-6,9-7], oszl(7,1), [[e,n,s,w],[n,s]], [[n,s,w],[n,s]]),
    run_test_osszeg([6-6,9-7], oszl(7,0), [[e,n,s,w],[n,s]], []),
    run_test_osszeg([6-6,9-7], oszl(7,1), [[e,n,s,w],[e,n,s,w]], no).

run_test_osszeg(A, B, C, Expected) :-
    format("Testing with parameters: ~w, ~w, ~w~n", [A, B, C]),
    ( osszeg_szukites(A, B, C, Result) -> 
        ( Result = Expected -> 
            format("PASSED. Expected: ~w, Got: ~w~n~n", [Expected, Result])
        ; 
            format("FAILED. Expected: ~w, Got: ~w~n~n", [Expected, Result])
        )
    ; 
        ( Expected = no -> 
            format("PASSED. Expected: no solution. Got: no solution.~n~n", []) 
        ; 
            format("FAILED. Expected: ~w, Got: no solution.~n~n", [Expected]) 
        )
    ).

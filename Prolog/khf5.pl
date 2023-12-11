% Importok
:- use_module(library(lists)).
:- use_module(library(between)).

% Szabályok a sátorkoordináta származtatására a fa koordinátákból.
tent_coordinate(TreeY-TreeX, n, TentY-TentX) :- TentY is TreeY - 1, TentX = TreeX.
tent_coordinate(TreeY-TreeX, e, TentY-TentX) :- TentY = TreeY, TentX is TreeX + 1.
tent_coordinate(TreeY-TreeX, s, TentY-TentX) :- TentY is TreeY + 1, TentX = TreeX.
tent_coordinate(TreeY-TreeX, w, TentY-TentX) :- TentY = TreeY, TentX is TreeX - 1.

% N-M -> int-int::mátrix sorainak ill. oszlopainak a száma.
% Fs  -> list(int-int)::a fák helyeit tartalmazó lista
% ILs -> list(list(irány)): az összes fa iránylistája
iranylistak(NM, Fs, ILs) :-
    % Begyűjtjük az összes valid irányt először.
    collect_directions(NM, Fs, Fs, [], Collected),
    % Ha üres akkor ILs is üres, amúgy meg amivel visszatértünk előbb.
    (Collected = [] -> ILs = []; ILs = Collected).

% Utolsó eset mikor már nem maradt több fa, és az accumulator-ban
% vannak a végleges irányok.
collect_directions(_, [], _, Accumulator, Accumulator).
collect_directions(NM, [CurrentTree|TailOfTrees], AllTrees, Accumulator, Out) :-
    % Kiszámoljuk a valid irányokat a jelenlegi fára.
    valid_directions(NM, CurrentTree, AllTrees, DirectionsSorted),
    % Ha a valid irányok zérus akkor nem adjuk hozzá az accumulátor-hoz.
    % Ha nem zérus akkor hozzáadjuk és ugrunk a kövi fára.
    (DirectionsSorted = [] -> 
        Out = []; 
        append(Accumulator, [DirectionsSorted], NewAccumulator),
        collect_directions(NM, TailOfTrees, AllTrees, NewAccumulator, Out)
    ).

% Visszaadja egy rendezett listában a valid irányokat adott fára (Tree).
valid_directions(NM, Tree, Trees, DirectionsSorted) :-
    % Ide lehet inkább bagof kéne?
    findall(Direction, check_direction(NM, Tree, Trees, Direction), Directions),
    sort(Directions, DirectionsSorted).

% Megnézzük hogy adott irány valid-e az adott fára (Tree).
% Kiszámoljuk a sátor koordinátáit, majd megnézzük a mátrixon belüli-e.
% Végül pedig megnézzük nem esik-e egybe egy másik fával.
check_direction(N-M, Tree, Trees, Direction) :-
    tent_coordinate(Tree, Direction, TentY-TentX),
    between(1, N, TentY),
    between(1, M, TentX),
    \+ member(TentY-TentX, Trees).

% Fs   -> list(int-int):: a fák helyeit tartalmazó lista
% I    -> int:: az i. fát jelöli
% ILs0 -> list(list(irány)):: az összes fa iránylistája
% ILs  -> list(list(irány)):: az összes fa szűkített iránylistája
sator_szukites(Fs, I, ILs0, ILs) :-
    % Megnézzük hogy biztosan egy iránya legyen a bemeneti fának,
    % ezért matcheljük rá [Ir]-re, és így visszakapjuk az irányt.
    nth1(I, ILs0, [Ir]),
    % Ezzel itt visszakapjuk az i. fa pozícióját is.
    nth1(I, Fs, F),
    tent_coordinate(F, Ir, S),
    % Kiszámoljuk melyik koordinátákat befolyásolja.
    findall(M, neighbour(S, M), AffectedCoords),
    % Az előbbi koordináták alapján kiszámoljuk a szűkített listát.
    refine_directions(Fs, ILs0, AffectedCoords, F, RefinedILs),
    % Végezetül megnézzük a refined listában van-e nem valid irány.
    (member([], RefinedILs) -> ILs = []; ILs = RefinedILs).


% Ebben a lépésben a koordináták alapján kiszámoljuk a lehetséges irányokat.
% Minden fára megnézzük (CurrentTree az éppen feldolgozandó).
refine_directions([], [], _, _, []).
refine_directions([CurrentTree|FsT], [CurrentIL|ILs0T], AffectedCoords, SingleDirTree, [RefinedIL|ILsT]) :-
    (CurrentTree \= SingleDirTree ->
        % Ha a jelenlegi fának nem egy iránya van akkor szűrjük az irányait.
        filter_directions(CurrentTree, CurrentIL, AffectedCoords, RefinedIL)
        % Ha pedig az akkor nem változtatunk rajta.
    ; RefinedIL = CurrentIL),
    % Majd megyünk a kövi fára.
    refine_directions(FsT, ILs0T, AffectedCoords, SingleDirTree, ILsT).

% Ebben a lépésben kiszedjük egy fa irányított listájából azt a koordinátát,
% amelyik benne van az AffectedCoords-ban.
filter_directions(_, [], _, []).
filter_directions(Tree, [Direction|RestDirections], AffectedCoords, FilteredDirections) :-
    tent_coordinate(Tree, Direction, Coord),
    (member(Coord, AffectedCoords) ->
        % Ha a kiszámolt koordináta benne van AffectedCoords-ban akkor nem rakjuk bele.
        filter_directions(Tree, RestDirections, AffectedCoords, FilteredDirections)
        % Ha nem befolyásolja akkor belerakjuk.
    ; FilteredDirections = [Direction|NextDirections],
      filter_directions(Tree, RestDirections, AffectedCoords, NextDirections)
    ).

% Ellenőrzi hogy 2 koordináta szomszédos-e.
neighbour(M1Y-M1X, M2Y-M2X) :-
    neighbour_helper(M1Y, M2Y),
    neighbour_helper(M1X, M2X).

neighbour_helper(Coord, NeighbourCoord) :-
    Min is Coord - 1,
    Max is Coord + 1,
    between(Min, Max, NeighbourCoord).
    
%_____________________________GYURMÁZÓ_____________________________%
test_iranylistak :- 
    run_test(2-3, [1-1,2-2], [[e,s],[e,n,w]]),
    run_test(3-3, [1-1,2-1], [[e],[e,s]]),
    run_test(6-6, [1-3,1-5,3-1,4-6,5-1,5-4,6-5], [[e,s,w],[e,s,w],[e,n,s],[n,s,w],[e,n,s],[e,n,s,w],[e,n,w]]),
    run_test(2-3, [1-1,1-2,2-1], []).

run_test(A, B, Expected) :-
    format("Testing with parameters: ~w, ~w~n", [A, B]),
    iranylistak(A, B, Result),
    ( Result = Expected -> 
        format("PASSED. Expected: ~w, Got: ~w~n~n", [Expected, Result])
    ; 
        format("FAILED. Expected: ~w, Got: ~w~n~n", [Expected, Result])
    ).

test_sator_szukites :-
    run_test_sator([1-1,2-2], 2, [[e],[n,s]], no),
    run_test_sator([1-1,2-2], 2, [[e,s],[s]], [[e],[s]]),
    run_test_sator([5-2,2-2], 1, [[n],[e,n,s,w]], [[n],[e,n,w]]),
    run_test_sator([4-2,2-2], 1, [[n],[e,n,s,w]], [[n],[n]]),
    run_test_sator([1-1,1-5,3-3,3-5], 3, [[e,s],[e,s,w],[n],[e,n,s,w]], [[s],[e,s],[n],[e,n,s]]),
    run_test_sator([1-1,2-2], 2, [[e,s],[n]], []).

run_test_sator(A, B, C, Expected) :-
    format("Testing with parameters: ~w, ~w, ~w~n", [A, B, C]),
    ( sator_szukites(A, B, C, Result) -> 
        ( Result = Expected -> 
            format("PASSED. Expected: ~w, Got: ~w~n~n", [Expected, Result])
        ; 
            format("FAILED. Expected: ~w, Got: ~w~n~n", [Expected, Result])
        )
        ; 
        ( Expected = no -> 
            format("PASSED. Expected: no solution. Got: no solution.~n~n", []) % Corrected line
        ; 
            format("FAILED. Expected: ~w, Got: no solution.~n~n", [Expected]) % Corrected line
        )
    ).

% Importok.
:- use_module(library(lists)).
:- use_module(library(between)).

% TentsPerRow    -> list(int):             Sátrak soronkénti számát tartalmazó lista.              PL: [1,1,0,3,0]
% TentsPerColumn -> list(int):             Sátrak oszloponkénti számát tartalmazó lista.           PL: [1,0,2,0,2]
% Trees          -> list(int-int):         Fák sorát és oszlopát azonosító párok lexikografikusan. PL: [1-2,3-3,3-5,5-1,5-5]
% Solution       -> list(list(direction)): A fákhoz tartozó lehetséges sátorpozíciók.              PL: [e,s,n,n,n]
satrak(satrak(TentsPerRow, TentsPerColumn, Trees), Solution) :- 
    length(TentsPerRow, RowLength),
    length(TentsPerColumn, ColumnLength),
    length(Trees, TreeLength),
    % Generálunk egy listát 1-től a falista hosszáig. TreeIndexes: [1,2,...,TreeLength]
    findall(Nummer, between(1, TreeLength, Nummer), TreeIndexes),
    %(1)% Előállítjuk az összegfeltételeket a következő formában: [1,1,0,3,0], [2,0,2,1,0] -> [col(1, 2), col(3, 2), col(4, 1), row(1, 1), row(2, 1), row(4, 3)]
    get_all_sum_conditions(TentsPerRow, TentsPerColumn, AllSumConditions),
    %(2)% Előállítjuk a kezdeti iránylistákat.
    generate_initial_dir_lists(Trees, Trees, RowLength, ColumnLength, [], PossibleDirections),
    %(3)% Sátor és összegszűkítést hajtunk végre az így létrejött fák, iránylisták, faindexek, és összegfeltételek szerint.
    choice_point_filter(Dir, 1, Trees, PossibleDirections, TreeIndexes, AllSumConditions, Solution).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%(1)%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Előállítja az összegfeltételeket.
get_all_sum_conditions(TentsPerRow, TentsPerColumn, Return) :-
    get_sum_condition(row, TentsPerRow, 1, RowResult),
    get_sum_condition(col, TentsPerColumn, 1, ColumnResult),
    append(ColumnResult, RowResult, Return).

get_sum_condition(_, [], _, []) :- !.
% Rekurzívan előállítjuk az egyes Elementeket (col(1, 2)) mikor a lista feje egy nemnegatív integer.
get_sum_condition(Type, [TentsPerHead | TentsPerTail], CurrentIndex, [Element | Result]) :-
    % Ha -1 következik akkor továbbugrunk a következő elemre.
    TentsPerHead > -1,
    !,
    % Amúgy létrehozzuk a következő Term-et.
    Element =.. [Type, CurrentIndex, TentsPerHead],
    NextIndex is CurrentIndex + 1,
    get_sum_condition(Type, TentsPerTail, NextIndex, Result).
% Ha -1 a lista feje csak átlépjük az elemet.
get_sum_condition(Type, [_ | TentsPerTail], CurrentIndex, Result) :-
    NextIndex is CurrentIndex + 1,
    get_sum_condition(Type, TentsPerTail, NextIndex, Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%(2)%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Előállítjuk a kezdeti iránylistákat, itt csak az "Out of bounds" és a másik fába ütközés hibákkal törődünk. Acc-ban tároljuk az eddigieket.
% PL: Trees = [2-2, 3-3] Matrix = 3x3 -> Result = [[n, s, w, e], [n, w]]
% Alapeset mikor a 2.Trees üres akkor a végére értünk.
generate_initial_dir_lists(Trees, [], RowLength, ColumnLength, Accumulator, Result) :- reverse(Accumulator, Result), !.
generate_initial_dir_lists(Trees, [CurrentTreeCoords | RemainingTreeCoords], RowLength, ColumnLength, Accumulator, Result) :-
    % Soron következő fára kiszámoljuk az irányokat, és megyünk tovább.
    check_dirs_of_tree(Trees, CurrentTreeCoords, RowLength, ColumnLength, Out),
    generate_initial_dir_lists(Trees, RemainingTreeCoords, RowLength, ColumnLength, [Out | Accumulator], Result).

% EGY! fára megnézi a lehetséges irányokat.
check_dirs_of_tree(Trees, TreePair, RowLength, ColumnLength, Flags) :-
    TreePair = Row-Col,
    Directions = [n-(-1,0), s-(1,0), w-(0,-1), e-(0,1)],
    % Megkeressük azokat az irányokat amik kielégítik a valid_direction predikátumot.
    include(valid_direction(Trees, Row, Col, RowLength, ColumnLength), Directions, ValidDirections),
    % Majd csinálunk belőlük egy kicsike listát.
    findall(Dir, member(Dir-_, ValidDirections), Flags),
    % Ha nincsenek lehetséges irányok bukjunk el.
    Flags \= [].

% Megnézzük egy lehetséges irányra hogy nem esik e kívül a mátrixon vagy nem ütközik-e másik fába.
valid_direction(Trees, Row, Col, RowLength, ColumnLength, Dir-(DRow, DCol)) :-
    NewRow is Row + DRow, NewCol is Col + DCol,
    % Nem esik e kívül a mátrixon.
    NewRow > 0, NewRow =< RowLength, NewCol > 0, NewCol =< ColumnLength,
    % Nem ütközik-e fába.
    \+ member(NewRow-NewCol, Trees).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%(3)%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Kiválasztunk egy pontot az iránylistából, és megpróbálkozunk vele sátor, és összegszűkítésre.
% Ha elfogytak a fa indexek, és az összegfeltételek megkaptuk a megoldást (ha van).
choice_point_filter(Dir, CurrentIndex, Trees, PossibleDirections, [], [], Result) :- flat(PossibleDirections, Result), !.
choice_point_filter(Dir, CurrentIndex, Trees, PossibleDirections, TreeIndexes, AllSumConditions, Return) :-
    % DirArr: [[n, s, w, e], [n, w]] -> [n, w]
    nth1(CurrentIndex, PossibleDirections, DirArr),
    % Prologban ez egy választási pont, kiválasztunk egy irányt, és ha nemjó akkor később ide térünk vissza.
    member(Dir, DirArr),
    replace_element_in_list(PossibleDirections, CurrentIndex, [Dir], NewPossibleDirections),
    filter(Trees, NewPossibleDirections, TreeIndexes, AllSumConditions, NewerPossibleDirections-UpdatedTreeIndexes-NewAllSumConditions),
    NewIdx is CurrentIndex + 1,
    choice_point_filter(_, NewIdx, Trees, NewerPossibleDirections, UpdatedTreeIndexes, NewAllSumConditions, Return).

% Kicserél egy adott indexű elemet egy adott elemre.
% replace_element_in_list([a, b, c, d], 3, x, Result) -> Result = [a, b, x, d]
replace_element_in_list(List, Index, Element, Result) :-
    replace_element_in_list_helper(List, Index, Element, Result).
replace_element_in_list_helper([], _, _, []).
replace_element_in_list_helper([_|Tail], 1, Element, [Element|Tail]) :- !.
replace_element_in_list_helper([Head|Tail], Index, Element, [Head|Result]) :-
    Index > 1,
    NewIndex is Index - 1,
    replace_element_in_list_helper(Tail, NewIndex, Element, Result).

% Egy nested listából flattened listát csinál. [1, [2, [3, 4], 5] -> [1, 2, 3, 4, 5]
flat(NestedList, FlattenedList) :-
    % Minden egyes elemre a listában meghívjuk.
    maplist(flat, NestedList, ListOfLists),
    % [[1, 2], [3, 4], [5, 6]] -> [1, 2, 3, 4, 5, 6]
    append(ListOfLists, FlattenedList).
flat(OneElementList, [OneElementList]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%(4)%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determinisztikus mivel először iránylista, majd összegfeltétel alapján próbálunk meg szűkíteni a lehetőségek halmazán.
filter(0, PossibleDirections, TreeIndexes, AllSumConditions, PossibleDirections-TreeIndexes-AllSumConditions) :- !.
filter(Trees, PossibleDirections, TreeIndexes, AllSumConditions, Return) :-
    (   
        % Először sátorszűkítéssel próbálkozunk.
        find_single_element_index(TreeIndexes, PossibleDirections, 1, IndexOfOne),
        tent_filter(Trees, IndexOfOne, PossibleDirections, NewPossibleDirections),
        filter_element_from_list(IndexOfOne, TreeIndexes,[], UpdatedTreeIndexes),
        filter(Trees, NewPossibleDirections, UpdatedTreeIndexes, AllSumConditions, Return), !
    ;   % Ha nem vezet megloldásra megpróbáljuk összegszűkítéssel.
        select(SumCondition, AllSumConditions, NewAllSumConditions),
        sum_filter(Trees, SumCondition, PossibleDirections, NewPossibleDirections),
        filter(Trees, NewPossibleDirections, TreeIndexes, NewAllSumConditions, Return), !
    ;   % Ha semelyik sem jön össze visszatérünk a choice_point_filter prédikátumba.
        filter(0, PossibleDirections, TreeIndexes, AllSumConditions, Return)
    ).

% Megtalálja annak a fának az indexét, amelyiknek egy lehetséges iránya van.
find_single_element_index([], _, _, _) :- fail.
find_single_element_index(_, [], _, _) :- fail. 
find_single_element_index(TreeIndexes, [DirList | RemainingDirLists], Index, Result) :-
    % Ez egy ígéret.
    length(DirList, 1),
    member(Index, TreeIndexes),
    Result = Index;
    NewIndex is Index + 1,
    find_single_element_index(TreeIndexes, RemainingDirLists, NewIndex, Result).

% Sátorszűkítést hajtunk végre, ilyen akkor fordul elő mikor egy fa iránylistájában már csak egy elem van.
tent_filter(Trees, TargetIndex, PossibleDirections, NewPossibleDirections) :- 
    place_tent_for_single_option(Trees, Trees, 1, TargetIndex, PossibleDirections, PossibleDirections, [], Result),
    (member([], Result) -> NewPossibleDirections = []; NewPossibleDirections = Result).
% Ha nincs már több fa koordináta megállunk, amúgy meg rekurzívan megnézzük az összeset.
place_tent_for_single_option(Trees, [], _, _, [], _, Return, Return) :- !.
place_tent_for_single_option(Trees, [CurrentTreeCoords | RemainingTreeCoords], CurrentIndex, TargetIndex, [CurrentDirArray | RemainingDirArrays], InitialDirArrays, Accumulator, Return) :-
    % Ha a jelenlegi index nem egyenlő a célindexel akkor továbhívunk rekurzívan.
    (CurrentIndex \= TargetIndex ->
        NewIdx is CurrentIndex + 1,
        place_tent_for_single_option(Trees, RemainingTreeCoords, NewIdx, TargetIndex, RemainingDirArrays, InitialDirArrays, Accumulator, Return)
    ;
        length(CurrentDirArray, ArrayLen),
        % Ha egyenlő akkor elvegzünk egy sátorszűkítést, ha nem akkor Acc-al térünk vissza.
        (ArrayLen = 1 ->
            % Kiszedjük az irányt majd kiszámoljuk a sátorkoordinátáját.
            [Dir] = CurrentDirArray,
            tent_coordinate(CurrentTreeCoords, Dir, Coord),
            % Az irány és falistából kiszedjük az adott indexű elemet.
            nth1(TargetIndex, InitialDirArrays, _, NewDirArrays),
            nth1(TargetIndex, Trees, _, NewTrees),
            direction_filter(NewTrees, NewDirArrays, Coord, TempResult),
            reverse(TempResult, ReverseTempResult),
            nth1(TargetIndex, FinalResult, CurrentDirArray, ReverseTempResult),
            Return = FinalResult
        ;
            Return = Accumulator)
    ).

% Szabályok sátorkoordináta származtatásra a fa koordinátákból.
tent_coordinate(TreeY-TreeX, n, TentY-TentX) :- TentY is TreeY - 1, TentX = TreeX.
tent_coordinate(TreeY-TreeX, e, TentY-TentX) :- TentY = TreeY, TentX is TreeX + 1.
tent_coordinate(TreeY-TreeX, s, TentY-TentX) :- TentY is TreeY + 1, TentX = TreeX.
tent_coordinate(TreeY-TreeX, w, TentY-TentX) :- TentY = TreeY, TentX is TreeX - 1.

% Minden fára és iránylistára lefutttatjuk a sátorszűkítést.
direction_filter(Trees, DirArrays, Coord, Return) :-
    direction_filter_helper(Trees, DirArrays, Coord, ReturnReverse, []),
    reverse(ReturnReverse, Return).
direction_filter_helper([], [], _, Accumulator, Accumulator) :- !.
direction_filter_helper([HeadTrees | TailTrees], [HeadDirArrays | TailDirArrays], Coord, Accumulator, Solution) :- 
    neighbor_filter(HeadTrees, HeadDirArrays, Coord, [], ResultArr),
    Accumulator = [ResultArr | AccumulatorTail],
    direction_filter_helper(TailTrees, TailDirArrays, Coord, AccumulatorTail, Solution). 

% Kiszűri azokat az irányokat a kapott irány alapján, amelyeknek van sátorszomszédja.
neighbor_filter(Tree, [], Coord, Return, Return) :- !.
neighbor_filter(Tree, [Head | Tail], Coord, Return, Result) :-
    tent_coordinate(Tree, Head, Coords),
    is_neighbor(Coords, Coord, Has_Neighbor),
    % Ha nem szomszédja a fej akkor hozzáadjuk az eredményhez, ha igen akkor továbbhívunk nélküle.
    (Has_Neighbor = 0 -> 
        neighbor_filter(Tree, Tail, Coord, [Head | Return], Result) 
        ;
        neighbor_filter(Tree, Tail, Coord, Return, Result)
    ).

% Leellenőrzi hogy egy sátornak van-e oldal vagy sarokszomszédja, ami másik sátor.
is_neighbor(ParamRow-ParamCol, Row-Col, 1) :-
    DeltaRow is abs(ParamRow - Row),
    DeltaCol is abs(ParamCol - Col),
    DeltaRow =< 1, DeltaCol =< 1, !.
is_neighbor(_, _, 0).

% Egy adott elem összes példányának eltávolítása a listából
filter_element_from_list(Element, List, Accumulator, FilteredList) :-
    filter_element_helper(Element, List, Accumulator, ReversedFilteredList),
    reverse(ReversedFilteredList, FilteredList).
filter_element_helper(_, [], Accumulator, Accumulator) :- !.
filter_element_helper(Element, [Element|Tail], Accumulator, FilteredList) :-
    !, filter_element_helper(Element, Tail, Accumulator, FilteredList).
filter_element_helper(Element, [Head|Tail], Accumulator, FilteredList) :-
    filter_element_helper(Element, Tail, [Head|Accumulator], FilteredList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%(5)%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Összegfeltételek alapján próbáljuk meg szűkíteni az iránylistát.
sum_filter(Trees, SumCondition, PossibleDirections, NewPossibleDirections) :-
    SumCondition =.. [IsRowOrCol, RowOrColTentNum, Db],
    get_certain_and_probable(IsRowOrCol, RowOrColTentNum, Trees, PossibleDirections, [], 0, 0, Biztos-Esetleg-TempReturnCheckedArr),
    reverse(TempReturnCheckedArr, ReturnCheckedArrs),
    B_and_E is Biztos + Esetleg,
    check_sumcondition(Biztos, Esetleg, B_and_E, Db, PossibleDirections, ReturnCheckedArrs, NewPossibleDirections).

% Megkeressük a biztos, és esetleges irányokat
get_certain_and_probable(_, _, [], [], CheckedArrs, CertainCount, ProbableCount, CertainCount-ProbableCount-CheckedArrs).
get_certain_and_probable(IsRowOrCol, RowOrColTentNum, [HeadTrees | TailTrees], [HeadDirections | TailDirections], CheckedArrs, CertainCount, ProbableCount, Return) :-
    length(HeadDirections, NumberOfDirections),
    % Ha a jelenlegi listában kevesebb mint 3 irány van, és a check_certain sikerrel tér vissza akkor inkrementáljuk a biztos esetet.
    (   NumberOfDirections =< 2,
        check_certain(IsRowOrCol, RowOrColTentNum, HeadDirections, HeadTrees, 0, 1)
    ->  NewCertainCount is CertainCount + 1,
        % Ha biztos: hozzárakunk egy 0-át, ami azt jelzi hogy nem lehet már tovább szűkíteni [[w,s]-0, [n,s,e,w]-[w,s]] 
        get_certain_and_probable(IsRowOrCol, RowOrColTentNum, TailTrees, TailDirections, [HeadDirections-0 | CheckedArrs], NewCertainCount, ProbableCount, Return)
        % Ha nem biztos nézzük meg a lehetséges esetet.
    ;   check_probable(IsRowOrCol, RowOrColTentNum, HeadDirections, HeadTrees, [], 0, IsProbable-DirEsetleg),
        (IsProbable = 1 
        % Ha esetleg akkor frissítjük mint az előbb a biztos esetet.
        ->  NewProbableCount is ProbableCount + 1,
            get_certain_and_probable(IsRowOrCol, RowOrColTentNum, TailTrees, TailDirections, [HeadDirections-DirEsetleg | CheckedArrs], CertainCount, NewProbableCount, Return)
            % Ha egyik sem akkor továbblépünk egy másikra.
        ;   get_certain_and_probable(IsRowOrCol, RowOrColTentNum, TailTrees, TailDirections, [HeadDirections-0 | CheckedArrs], CertainCount, ProbableCount, Return)
        )
    ).

% Megnézi az adott iránylistából melyek azok, amelyek adott oszlopba vagy sorba mutatnak biztosan.
check_certain(_, _, [], _, Return, Return).
check_certain(IsRowOrCol, Nummer, [HeadDirections | TailDirections], TreePair, _, IsCertain):-
    tent_coordinate(TreePair, HeadDirections, TentY-TentX),
    (   (IsRowOrCol = col, TentX = Nummer ; IsRowOrCol = row, TentY = Nummer)
    ->  check_certain(IsRowOrCol, Nummer, TailDirections, TreePair, 1, IsCertain)
    ;   IsCertain = 0
    ).

% Megnézi az adott iránylistából melyek azok, amelyek adott oszlopba vagy sorba mutatnak remélhetőleg.
check_probable(_, _, [], _, ProbableDirections, Return, Return-ProbableDirections).
check_probable(IsRowOrCol, Nummer, [HeadDirections | TailDirections], TreePair, ProbableDirections, Return, IsProbable) :-
    tent_coordinate(TreePair, HeadDirections, TentY-TentX),
    (   (IsRowOrCol = col, TentX = Nummer ; IsRowOrCol = row, TentY = Nummer)
    ->  check_probable(IsRowOrCol, Nummer, TailDirections, TreePair, [HeadDirections | ProbableDirections], 1, IsProbable),
        !
    ;   check_probable(IsRowOrCol, Nummer, TailDirections, TreePair, ProbableDirections, Return, IsProbable)
    ).

% Biztos (B) és Esetleg (E) logika a tárgyoldalról
check_sumcondition(B, _, B_and_E, Db, _, _, []) :- (B_and_E < Db ; B > Db), !.
check_sumcondition(B, _, B_and_E, Db, PossibleDirections, ReturnCheckedArrs, Return) :-
    B_and_E =:= Db, !,
    remove_directions_reverse(PossibleDirections, ReturnCheckedArrs, [], Return).
check_sumcondition(B, _, _, Db, PossibleDirections, ReturnCheckedArrs, Return) :-
    B =:= Db, !,
    remove_directions(PossibleDirections, ReturnCheckedArrs, [], Return).
check_sumcondition(_, _, _, _, _, _, _) :- false.

% Az iránylistákból eltávolítja az ElementsToKeep-ben szereplő irányokon kívüli irányokat.
remove_directions_reverse([], [], NewDirections, Return) :- reverse(NewDirections, Return).
remove_directions_reverse([HeadDirections | TailDirections], [_-0 | RemainingDirections], NewDirections, Return) :- 
    remove_directions_reverse(TailDirections, RemainingDirections, [HeadDirections | NewDirections], Return).
remove_directions_reverse([HeadDirections | TailDirections], [_-ElementsToKeep | RemainingDirections], NewDirections, Return) :-
    filter_list_by_another(HeadDirections, ElementsToKeep, NewArr),
    remove_directions_reverse(TailDirections, RemainingDirections, [NewArr | NewDirections], Return).

% Az iránylistákból eltávolítja az ElementsToRemove-ban szereplő irányokat.
remove_directions([], [], NewDirections, Return) :- reverse(NewDirections, Return).
remove_directions([HeadDirections | TailDirections], [_-0 | RemainingDirections], NewDirections, Return) :-
    remove_directions(TailDirections, RemainingDirections, [HeadDirections | NewDirections], Return).
remove_directions([HeadDirections | TailDirections], [_-ElementsToRemove | RemainingDirections], NewDirections, Return) :-
    filter_out_elements(HeadDirections, ElementsToRemove, NewArr),
    remove_directions(TailDirections, RemainingDirections, [NewArr | NewDirections], Return).

% Eltávolítja az első listából az összes 2. listában NEM szereplő elemet.
filter_list_by_another([], _, []).
filter_list_by_another([Head|Tail], SecondList, Result) :-
    % If NOT member (\+)
    \+ member(Head, SecondList), !,
    filter_list_by_another(Tail, SecondList, Result).
filter_list_by_another([Head|Tail], SecondList, [Head|Result]) :- filter_list_by_another(Tail, SecondList, Result).

% Eltávolítja az első listából az összes 2. listában szereplő elemet.
filter_out_elements([], _, []).
filter_out_elements([Head|Tail], FilterList, Result) :-
    member(Head, FilterList), !,
    filter_out_elements(Tail, FilterList, Result).
filter_out_elements([Head|Tail], FilterList, [Head|Result]) :- filter_out_elements(Tail, FilterList, Result).

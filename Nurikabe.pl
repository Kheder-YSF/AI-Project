% ! Solved Nurikabe To Build Up And Test The Rules Of The Puzzle:
% ? Fixed Cells :
%fxd_cell(1,2,3).
%fxd_cell(1,6,1).
%fxd_cell(3,1,2).
%fxd_cell(3,4,1).
%fxd_cell(5,2,1).
%fxd_cell(5,5,2).
%fxd_cell(6,3,2).
%fxd_cell(7,1,1).
%fxd_cell(7,5,1).
%fxd_cell(7,7,6).
% ? Solved Cells :
%solve_cell(1, 1, blue).
%solve_cell(1, 2, green).
%solve_cell(1, 3, green).
%solve_cell(1, 4, green).
%solve_cell(1, 5, blue).
%solve_cell(1, 6, green).
%solve_cell(1, 7, blue).
%solve_cell(2, 1, blue).
%solve_cell(2, 2, blue).
%solve_cell(2, 3, blue).
%solve_cell(2, 4, blue).
%solve_cell(2, 5, blue).
%solve_cell(2, 6, blue).
%solve_cell(2, 7, blue).
%solve_cell(3, 1, green).
%solve_cell(3, 2, green).
%solve_cell(3, 3, blue).
%solve_cell(3, 4, green).
%solve_cell(3, 5, blue).
%solve_cell(3, 6, green).
%solve_cell(3, 7, green).
%solve_cell(4, 1, blue).
%solve_cell(4, 2, blue).
%solve_cell(4, 3, blue).
%solve_cell(4, 4, blue).
%solve_cell(4, 5, blue).
%solve_cell(4, 6, blue).
%solve_cell(4, 7, green).
%solve_cell(5, 1, blue).
%solve_cell(5, 2, green).
%solve_cell(5, 3, blue).
%solve_cell(5, 4, green).
%solve_cell(5, 5, green).
%solve_cell(5, 6, blue).
%solve_cell(5, 7, green).
%solve_cell(6, 1,blue).
%solve_cell(6, 2,blue).
%solve_cell(6, 3,green).
%solve_cell(6, 4,blue).
%solve_cell(6, 5,blue).
%solve_cell(6, 6,blue).
%solve_cell(6, 7,green).
%solve_cell(7, 1,green).
%solve_cell(7, 2,blue).
%solve_cell(7, 3,green).
%solve_cell(7, 4,blue).
%solve_cell(7, 5,green).
%solve_cell(7, 6,blue).
%solve_cell(7, 7,green).
% ! --------------------------------------------------------------------------------
% ? Making The Puzzle Dynamic : 
:-dynamic solve_cell/3.
:-dynamic fxd_cell/3.
% Predicate to initialize the grid
initialize_grid(Rows, Cols, FixedCells) :-
    retractall(solve_cell(_, _, _)),
    retractall(fxd_cell(_, _, _)),
    create_grid(Rows, Cols),
    initialize_fixed_cells(FixedCells).
% Helper predicate to create the grid
create_grid(Rows, Cols) :-
    between(1, Rows, Row),
    between(1, Cols, Col),
    assertz(solve_cell(Row, Col, empty)),
    fail.
create_grid(_, _).
% Predicate to set fixed cells directly from a list
initialize_fixed_cells([]).
initialize_fixed_cells([(Row, Col, Value)|Rest]) :-
    assertz(fxd_cell(Row, Col, Value)),
    assertz(solve_cell(Row, Col, green)),
    initialize_fixed_cells(Rest).
% Predicate to print the current grid state
print_grid(Rows, Cols) :-
    between(1, Rows, Row),
    between(1, Cols, Col),
    (fxd_cell(Row, Col, Value) -> write(Value) ; (solve_cell(Row, Col, State) -> print_cell(State) ; write(' '))),
    (Col =:= Cols -> nl; true),
    fail.
print_grid(_, _).
% Helper predicates to print a single cell
print_cell(green) :- write('G').
print_cell(blue) :- write('B').
print_cell(empty) :- write('.').
% Helper predicates to color a single cell :
color_cell_blue(X,Y):-
    retract(solve_cell(X,Y,_)),
    assertz(solve_cell(X,Y,blue)).
color_cell_green(X,Y):-
    retract(solve_cell(X,Y,_)),
    assertz(solve_cell(X,Y,green)).
% * Solved Checkers :
% * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
within_the_grid(X,Y,R,C):-
    X > 0 , X =< R , Y > 0 , Y =< C.
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
neighbor((X,Y),(Xl,Y)):- Xl is X-1 , within_the_grid(Xl,Y,7,7).
neighbor((X,Y),(Xr,Y)):- Xr is X+1 , within_the_grid(Xr,Y,7,7).
neighbor((X,Y),(X,Yu)):- Yu is Y-1 , within_the_grid(X,Yu,7,7).
neighbor((X,Y),(X,Yd)):- Yd is Y+1 , within_the_grid(X,Yd,7,7).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
check_for_no_2x2_sea(X,Y):-
    Xr is X + 1 , Yd is Y + 1 , 
    within_the_grid(X,Y,7,7),
    (
        \+solve_cell(X,Y,blue);
        \+solve_cell(Xr,Y,blue);
        \+solve_cell(X,Yd,blue);
        \+solve_cell(Xr,Yd,blue)
    ).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
check_for_no_2x2_seas([]).
check_for_no_2x2_seas([(X,Y)|Rest]):-
    check_for_no_2x2_sea(X,Y) , check_for_no_2x2_seas(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
validate_no_2_by_2_sea(Rows, Cols) :-
    \+ (between(1, Rows, Row),
        between(1, Cols, Col),
        Row1 is Row + 1, Col1 is Col + 1,
        solve_cell(Row, Col, blue),
        solve_cell(Row1, Col, blue),
        solve_cell(Row, Col1, blue),
        solve_cell(Row1, Col1, blue)).
% ? This Is The Final Predicate That Validate The Mentioned Rule :
no_2x2_sea:-
    validate_no_2_by_2_sea(7,7).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
sea_helper([] , Sea , Sea).
sea_helper([(X,Y)|Rest] , Vis , Sea):-
    solve_cell(X,Y,blue),
    \+member((X,Y),Vis),
    findall((Xn,Yn),neighbor((X,Y),(Xn,Yn)),Neighbors),
    append(Rest,Neighbors,NewRest),
    sea_helper(NewRest,[(X,Y)|Vis] , Sea).
sea_helper([(X,Y)|Rest] , Vis , Sea):-
    (member((X,Y),Vis);\+solve_cell(X,Y,blue)),
    sea_helper(Rest,Vis,Sea).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
sea((X,Y),Sea):-
    sea_helper([(X,Y)],[],Sea).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
one_sea:-
    findall((X,Y),solve_cell(X,Y,blue),BlueCells),
    BlueCells \= [],
    BlueCells = [(X,Y)|_],
    sea((X,Y), Sea),
    length(BlueCells, BlueCount),
    length(Sea, SeaCount),
    BlueCount =:= SeaCount.
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
island_helper([],Island,Island).
island_helper([(X,Y)|Rest],Vis,Island):-
    solve_cell(X,Y,green),
    \+member((X,Y),Vis),
    findall((Xn,Yn),neighbor((X,Y),(Xn,Yn)),Neighbors),
    append(Rest,Neighbors,NewRest),
    island_helper(NewRest,[(X,Y)|Vis] , Island).
island_helper([(X,Y)|Rest],Vis,Island):-
    (member((X,Y),Vis);\+solve_cell(X,Y,green)),
    island_helper(Rest,Vis , Island).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
island((X,Y),Island):-island_helper([(X,Y)],[],Island).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
islands_helper([(X,Y)|Rest], Vis , Acc ,Islands):-
    \+member((X,Y),Vis),
    island((X,Y),Island),
    append(Acc, [Island] ,NewIslands),
    append(Vis, Island ,NewVis),
    islands_helper(Rest,NewVis,NewIslands,Islands).
islands_helper([(X,Y)|Rest],Vis,Acc,Islands):-
    member((X,Y),Vis),
    islands_helper(Rest,Vis,Acc,Islands).
islands_helper([],_,Islands,Islands).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
islands(LandCells,Islands):-islands_helper(LandCells,[],[],Islands).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
one_fixed_cell_in_island:-
    findall((X,Y),solve_cell(X,Y,green),LandCells),
    islands(LandCells,Islands),
    check_islands_has_each_exactly_one_fixed_cell(Islands).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_island_fixed_cells_helper([],FixedCells,FixedCells).
get_island_fixed_cells_helper([(X,Y)|Rest],Acc,FixedCells):-
    fxd_cell(X,Y,Z) , get_island_fixed_cells_helper(Rest,[(X,Y,Z)|Acc],FixedCells).
get_island_fixed_cells_helper([(X,Y)|Rest],Acc,FixedCells):-
    \+fxd_cell(X,Y,_) , get_island_fixed_cells_helper(Rest,Acc,FixedCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_island_fixed_cells(Island,IslandFixedCells):-
    get_island_fixed_cells_helper(Island,[],IslandFixedCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
check_islands_has_each_exactly_one_fixed_cell([]).
check_islands_has_each_exactly_one_fixed_cell([Island|Islands]):-
    get_island_fixed_cells(Island,FixedCells),
    length(FixedCells, L),
    L =:= 1,
    check_islands_has_each_exactly_one_fixed_cell(Islands).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
island_number_equals_size:-
    findall((X,Y),solve_cell(X,Y,green),LandCells),
    islands(LandCells,Islands),
    check_islands_each_has_size_equals_fixed_cell_number(Islands).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
check_islands_each_has_size_equals_fixed_cell_number([]).
check_islands_each_has_size_equals_fixed_cell_number([Island|Islands]):-
    get_island_fixed_cells(Island,FixedCells),
    FixedCells = [(_,_,L)|_],
    length(Island, S),
    L =:= S,
    check_islands_each_has_size_equals_fixed_cell_number(Islands).
% * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% ! The Solver :
% ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% * Logical Steps :
% ? I - Solving For Fixed Cells With Clue Equals One :
solve_for_fixed_cells_with_clue_equals_one:-
    findall((X,Y),fxd_cell(X,Y,1),FixedCellsWithClueEqualsOne),
    color_the_neighbors_of_the_fixed_cells_with_clue_equals_one_blue(FixedCellsWithClueEqualsOne).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
color_list_blue([]).
color_list_blue([(X,Y)|Rest]):-
    color_cell_blue(X,Y) , color_list_blue(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
color_the_neighbors_of_the_fixed_cells_with_clue_equals_one_blue([]).
color_the_neighbors_of_the_fixed_cells_with_clue_equals_one_blue([(X,Y)|Rest]):-
    findall((Xn,Yn),neighbor((X,Y),(Xn,Yn)),Neighbors),
    color_list_blue(Neighbors),color_the_neighbors_of_the_fixed_cells_with_clue_equals_one_blue(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ? II - Solving For Cells With All Blue Neighbors:
solve_for_cells_with_all_neighbors_blue:-
    findall((X,Y),solve_cell(X,Y,empty),CellsWithAllNeighborsBlue),
    color_the_cells_that_all_their_neighbors_are_blue(CellsWithAllNeighborsBlue).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
check_for_neighbors_being_blue([]).
check_for_neighbors_being_blue([(X,Y)|Neighbors]):-
    solve_cell(X,Y,blue) , check_for_neighbors_being_blue(Neighbors).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
color_the_cells_that_all_their_neighbors_are_blue([]).
color_the_cells_that_all_their_neighbors_are_blue([(X,Y)|Rest]):-
    findall((Xn,Yn),neighbor((X,Y),(Xn,Yn)),Neighbors),
    \+check_for_neighbors_being_blue(Neighbors) , color_the_cells_that_all_their_neighbors_are_blue(Rest).
color_the_cells_that_all_their_neighbors_are_blue([(X,Y)|Rest]):-
    findall((Xn,Yn),neighbor((X,Y),(Xn,Yn)),Neighbors),
    check_for_neighbors_being_blue(Neighbors) , color_cell_blue(X,Y) , color_the_cells_that_all_their_neighbors_are_blue(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ? III - Solving For Sea Cells With One Way Out :
seas_one_way_out:-
    findall((X,Y),solve_cell(X,Y,blue),SeaCells),
    seas_one_way_out_helper(SeaCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
seas_one_way_out_helper([]).
seas_one_way_out_helper([(X,Y)|Rest]):-
    sea_one_way_out(X,Y) ,seas_one_way_out_helper(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sea_one_way_out(X,Y):-
    Xr is X + 1 ,  Xl is X-1  , Yu is Y-1 ,  Yd is Y+1 , 
    (
        (
            Xl > 0,
            solve_cell(Xl,Y,empty),
            (solve_cell(Xr,Y,green);Xr > 7),
            (solve_cell(X,Yu,green);Yu =< 0),
            (solve_cell(X,Yd,green);Yd > 7),
            color_cell_blue(Xl,Y)
        )
        ;
        (
            Xr =< 7 ,
            (solve_cell(Xl,Y,green);Xl =< 0),
            solve_cell(Xr,Y,empty),
            (solve_cell(X,Yu,green);Yu =< 0),
            (solve_cell(X,Yd,green);Yd > 7),
            color_cell_blue(Xr,Y)
        )
        ;
        (
            Yu > 0 ,
            (solve_cell(Xl,Y,green);Xl =< 0),
            (solve_cell(Xr,Y,green);Xr > 7),
            solve_cell(X,Yu,empty),
            (solve_cell(X,Yd,green);Yd > 7),
            color_cell_blue(X,Yu)
        )
        ;
        (
            Yd =< 7 ,
            (solve_cell(Xl,Y,green);Xl =< 0),
            (solve_cell(Xr,Y,green);Xr > 7),
            (solve_cell(X,Yu,green);Yu =< 0),
            solve_cell(X,Yd,empty),
            color_cell_blue(X,Yd)
        )
    ).
sea_one_way_out(_,_).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ? IV - Solving For Complete Islands :
isolate_completed_islands:-
    findall((X,Y,N),fxd_cell(X,Y,N),FixedCells),
    isolate_completed_islands_helper(FixedCells).
isolate_completed_islands_helper([]).
isolate_completed_islands_helper([(X,Y,N)|FixedCells]):-
    island((X,Y),Island),
    length(Island,L),
    L =:= N,
    surround_the_completed_island_with_blue(Island),
    isolate_completed_islands_helper(FixedCells).
isolate_completed_islands_helper([(X,Y,N)|FixedCells]):-
    island((X,Y),Island),
    length(Island,L),
    L =\= N,
    isolate_completed_islands_helper(FixedCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
surround_the_completed_island_with_blue([]).
surround_the_completed_island_with_blue([(X,Y)|Island]):-
    findall((Xn,Yn),neighbor((X,Y),(Xn,Yn)),Neighbors),
    surround_the_completed_island_cell_with_blue_helper(Neighbors),
    surround_the_completed_island_with_blue(Island).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
surround_the_completed_island_cell_with_blue_helper([]).
surround_the_completed_island_cell_with_blue_helper([(X,Y)|Neighbors]):-
    solve_cell(X,Y,empty) , color_cell_blue(X,Y) , surround_the_completed_island_cell_with_blue_helper(Neighbors).
surround_the_completed_island_cell_with_blue_helper([(X,Y)|Neighbors]):-
    \+solve_cell(X,Y,empty) , surround_the_completed_island_cell_with_blue_helper(Neighbors).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ? V - Solving For Diagonally Adjacent Fixed Cells :
solve_for_diag_adj_fxd_cells:-
    findall((X,Y),fxd_cell(X,Y,_),FixedCells),
    solve_for_diag_adj_fxd_cells_helper(FixedCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
solve_for_diag_adj_fxd_cells_helper([]).
solve_for_diag_adj_fxd_cells_helper([(X,Y)|FixedCells]):-
    Xur is X+1,
    Yur is Y+1,
    fxd_cell(Xur,Yur,_),
    color_cell_blue(Xur,Y),
    color_cell_blue(X,Yur) , solve_for_diag_adj_fxd_cells_helper(FixedCells).
solve_for_diag_adj_fxd_cells_helper([(X,Y)|FixedCells]):-
    Xur is X+1,
    Yur is Y+1,
    \+fxd_cell(Xur,Yur,_),
    solve_for_diag_adj_fxd_cells_helper(FixedCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ? VI - Solving For Preventing 2x2 Blue Blocks :
solve_for_no_2x2_blue_blocks:-
    findall((X,Y),solve_cell(X,Y,empty),EmptyCells),
    prevent_2x2_blue_blocks(EmptyCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if_it_will_form_2x2_blue_block_color_it_green(X,Y):-
    Xr is X + 1,
    Xl is X - 1,
    Yu is Y - 1,
    Yd is Y + 1,
    (
        (
            solve_cell(Xr,Y,blue), 
            solve_cell(X,Yd,blue), 
            solve_cell(Xr,Yd,blue)
        );
        (
            solve_cell(Xl,Y,blue), 
            solve_cell(X,Yu,blue), 
            solve_cell(Xl,Yu,blue)
        )
    ),color_cell_green(X,Y).
if_it_will_form_2x2_blue_block_color_it_green(_,_).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
prevent_2x2_blue_blocks([]).
prevent_2x2_blue_blocks([(X,Y)|Rest]):-
    if_it_will_form_2x2_blue_block_color_it_green(X,Y),
    prevent_2x2_blue_blocks(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ? VII - solving for fixed cells that are vertically or horizontally separated by one cell :
solve_for_fixed_cells_that_are_one_cell_separated:-
    findall((X,Y),solve_cell(X,Y,empty),EmptyCells),
    separate_fxd_cells(EmptyCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fxd_cell_separator(X,Y):-
    Xr is X + 1,
    Xl is X - 1,
    Yu is Y - 1,
    Yd is Y + 1,
    (
        (fxd_cell(Xr,Y,_),fxd_cell(Xl,Y,_))
        ;
        (fxd_cell(X,Yd,_),fxd_cell(X,Yu,_))
    ),color_cell_blue(X,Y).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
separate_fxd_cells([]).
separate_fxd_cells([(X,Y)|Rest]):-
    fxd_cell_separator(X,Y) , separate_fxd_cells(Rest).
separate_fxd_cells([(X,Y)|Rest]):-
    \+fxd_cell_separator(X,Y) , separate_fxd_cells(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ? VIII - Solving For Unreachable Cells :
solve_for_unreachable_cells:-
    findall((X,Y),solve_cell(X,Y,empty),EmptyCells),
    check_unreachable(EmptyCells).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
check_unreachable([]).
check_unreachable([(X,Y)|Rest]):-
    findall((Xf,Yf,N),fxd_cell(Xf,Yf,N),FixedCells),
    find_reachable_cells(FixedCells,[],NestedReachableCells),
    flatten(NestedReachableCells, ReachableCells),
    \+member((X,Y),ReachableCells),
    color_cell_blue(X,Y),
    check_unreachable(Rest).
check_unreachable([(X,Y)|Rest]):-
    findall((Xf,Yf,N),fxd_cell(Xf,Yf,N),FixedCells),
    find_reachable_cells(FixedCells,[],NestedReachableCells),
    flatten(NestedReachableCells, ReachableCells),
    member((X,Y),ReachableCells),
    check_unreachable(Rest).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
find_reachable_cells([],ReachableCells,ReachableCells).
find_reachable_cells([(X,Y,N)|Rest],ReachableCells,Acc):-
    find_paths((X,Y),N,CellPaths),
    append(CellPaths,ReachableCells,NewReachableCells),
    find_reachable_cells(Rest,NewReachableCells,Acc).
% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
find_path((X,Y),C,Vis,Path):-
    C > 0,
    neighbor((X,Y),(Xn,Yn)),
    \+member((X,Y),Vis),
    C1 is C-1,
    find_path((Xn,Yn),C1,[(X,Y)|Vis],Path).
find_path((_,_),0,Path,Path).
find_paths((X,Y),L,Paths):-
    setof(Path,find_path((X,Y),L,[],Path),Paths).

% ? - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
init:-
    initialize_grid(7,7,[(1,2,3),(1,6,1),(3,1,2),(3,4,1),(5,2,1),(5,5,2),(6,3,2),(7,1,1),(7,5,1),(7,7,6)]).
    %initialize_grid(8,8,[(1,1,3),(1,7,2),(2,3,2),(3,7,6),(4,3,2),(6,6,2),(6,8,3),(7,2,3),(7,7,1),(8,4,3)]).
solve_logically:-
    solve_for_fixed_cells_with_clue_equals_one,
    solve_for_cells_with_all_neighbors_blue,
    seas_one_way_out,
    solve_for_fixed_cells_that_are_one_cell_separated,
    isolate_completed_islands,
    solve_for_diag_adj_fxd_cells,
    solve_for_no_2x2_blue_blocks,
    solve_for_unreachable_cells.
solved:-
    no_2x2_sea,
    one_sea,
    island_number_equals_size,
    one_fixed_cell_in_island.


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
    retract(solve_cell(Row, Col, empty)),
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
% Helper predicate to print a single cell
print_cell(green) :- write('G').
print_cell(blue) :- write('B').
print_cell(empty) :- write('.').
% ! -----------------------------------------------------------------------------------
% ! The Solver :



% ! -----------------------------------------------------------------------------------
% ? These Predicates Help Finding The Horizontal And The Vertical Adjacent Cells Of A Cell(X,Y) :
adjacent((X,Y),(Xl,Y),_,_):-Xl is X-1 , Xl > 0.
adjacent((X,Y),(Xr,Y),_,C):-Xr is X+1 , Xr =< C.
adjacent((X,Y),(X,Yu),_,_):-Yu is Y-1 , Yu > 0.
adjacent((X,Y),(X,Yd),R,_):-Yd is Y+1 , Yd =< R.
% * Rules Procedures : 
    % * I - one_fixed_cell_in_island & island_number_equals_size :
    % * --------------------------------------------------------------------------------
    % ? This Procedure Takes A Cell As A Starter If It's A Island Cell It Will
    % ? Find All The Cells Of This Island , If It's A Sea Cell It Will Return A Empty List
    island_helper([],Acc,Acc).
    island_helper([(X,Y)|T],Vis,Acc):-
        \+member((X,Y),Vis),
        solve_cell(X,Y,green),
        findall((Xa,Ya),adjacent((X,Y),(Xa,Ya),7,7),AdjacentCells),
        append(T,AdjacentCells,Tn),
        island_helper(Tn,[(X,Y)|Vis],Acc).
    island_helper([(X,Y)|T],Vis,Acc):-
            (member((X,Y),Vis);solve_cell(X,Y,blue);solve_cell(X,Y,empty)),island_helper(T,Vis,Acc).
    island((X,Y),Island):-
        island_helper([(X,Y)],[],Island).
    % ? This Procedure Is A Generalization For The Previous One , Where
    % ? This Procedure Will Take A List Of Cells And Apply The Previous 
    % ? Procedure On It , That Is It Will Find All The Islands In The 
    % ? Nurikabe Grid As A List Of Lists Where Each SubList Is An Island
    islands_helper([],_,Acc,Acc).
    islands_helper([(X,Y)|T],Vis,Islands,Acc):-
            member((X,Y),Vis),
            islands_helper(T,Vis,Islands,Acc).
    islands_helper([(X,Y)|T],Vis,Islands,Acc):-
        \+member((X,Y),Vis),
        island((X,Y),Island),
        append(Vis,Island,NewVis),
        append(Islands,[Island],NewIslands),
        islands_helper(T,NewVis,NewIslands,Acc).
    islands(LandCells,R):-islands_helper(LandCells,[],[],R).
    % ? This Procedure Count The Number Of The Fixed(Numeric) Cells In An Island :
    count_fxd_cells([],0).
    count_fxd_cells([(X,Y)|T],C):- fxd_cell(X,Y,_) , count_fxd_cells(T,C1) , C is C1 + 1.
    count_fxd_cells([(X,Y)|T],C):- \+fxd_cell(X,Y,_) , count_fxd_cells(T,C).
    % ? This Procedure Is More Like A Generalization For The Previous One Where
    % ? It Will Check That Each Island Has Exactly One Fixed(Numeric) Cell :
    check_islands_fxd_cells([]).
    check_islands_fxd_cells([H|T]):- count_fxd_cells(H,C) , C =:= 1 ,  check_islands_fxd_cells(T).
    % ? This Procedure Will Check If The Island Size Is Equal To Tha Number Assigned
    % ? To The Island's Fixed(Numeric) Cell :
    check_size_helper([(X,Y)|_],L):- fxd_cell(X,Y,Z) , length(L, LSize), Z =:= LSize.
    check_size_helper([(X,Y)|T],L):- \+fxd_cell(X,Y,_) , check_size_helper(T,L).
    check_size(L):-check_size_helper(L,L).
    % ? This Procedure Is Just A Generalization For The Previous One 
    % ? Where It Will Perform The Previous Procedure For All The Islands
    check_islands_size([]).
    check_islands_size([H|T]):- check_size(H) ,  check_islands_size(T).
    % ? This Is The Final Predicate That Validate The Mentioned Rule :
    validate_islands:-
        findall((X,Y),solve_cell(X,Y,green),LandCells), % ? Get All The Land Cells
        islands(LandCells,R),
        check_islands_fxd_cells(R),
        check_islands_size(R).
    % * --------------------------------------------------------------------------------
    % * II - no_2_by_2_sea :
    % ? This Procedure Will Take A List Of Cells And Then For Each Cell(X,Y) It Will
    % ? Check If The Following Cells [(X+1,Y),(X,Y+1),(X+1,Y+1)] Are Existed In The Passed
    % ? List , If They Are Existed Then A 2x2 Sea Block Is Found In The Nurikabe Grid
    % ? Which Violates The Rules Of The Puzzle (False Will Be Returned) , If Any Of
    % ? The Three Cells Mentioned Before Is Existed In The Passed List , That Means
    % ? There Is No 2x2 Block Of Water , And The Solution Pass This Rule Check
    no_2_by_2_sea(Rows, Cols) :-
        \+ (between(1, Rows, Row),
            between(1, Cols, Col),
            Row1 is Row + 1, Col1 is Col + 1,
            solve_cell(Row, Col, blue),
            solve_cell(Row1, Col, blue),
            solve_cell(Row, Col1, blue),
            solve_cell(Row1, Col1, blue)).
    % ? This Is The Final Predicate That Validate The Mentioned Rule :
    validate_no_2_by_2_sea:-
        no_2_by_2_sea(7,7).
    % * --------------------------------------------------------------------------------
    % * III - one_sea :
    % * --------------------------------------------------------------------------------
    % ? This Procedure Check If There Is Exactly One Sea , And That's By 
    % ? Checking That The Number Of The Connected Sea Cells Is Equal To 
    % ? The Number Of All Sea Cells In The Grid , The Method Here Is Similar
    % ? To Finding The Island Cells , Where We Are Going To Pass A Sea Cell As
    % ? A Starter Then We Will Get All The Sea Cells Connected (Adjacent) To This Cell
    % ? And Recursively Repeat The Process , Then Comparing The Size Of The Sea Returned
    % ? From This Procedure With The Size Of All Sea Cell In The Grid
    % ? Equal -> One Sea , Not Equal -> Not A Valid Solution
    sea_helper([],Sea,Sea). 
    sea_helper([(X,Y)|T],Vis,Sea):-
        \+member((X,Y),Vis),
        solve_cell(X,Y,blue),
        findall((Xa,Ya),adjacent((X,Y),(Xa,Ya),7,7),AdjacentCells),
        append(T,AdjacentCells,Tn),
        sea_helper(Tn,[(X,Y)|Vis],Sea).
    sea_helper([(X,Y)|T],Vis,Sea):-
        (member((X,Y),Vis);
        \+solve_cell(X,Y,blue);solve_cell(X,Y,empty)),
        sea_helper(T,Vis,Sea).
    sea((X,Y),Sea):-sea_helper([(X,Y)],[],Sea).
    validate_one_sea :-
        findall((X,Y),solve_cell(X,Y,blue),BlueCells),
        BlueCells \= [],
        BlueCells = [(X,Y)|_],
        sea((X,Y), Sea),
        length(BlueCells, BlueCount),
        length(Sea, SeaCount),
        BlueCount =:= SeaCount.
    % * --------------------------------------------------------------------------------
    solved:-
        validate_one_sea,
        validate_no_2_by_2_sea,
        validate_islands.
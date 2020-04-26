:- [input].
:- dynamic hole/1.

main :-
    % get data from input and transform to my format
    prompt(_, ''),
    read_lines(LL),
    split_lines(LL,S),
    simplifyTower(S,SS),
    towerSize(SS, ShadesCnt, ColoursCnt),

    % fix hole
    atom_string(ShadesCnt, ShadesCntStr),
    createCell(ShadesCntStr, "A", HoleCell),
    % define hole predicate
    assertz(hole(HoleCell)),
    replaceElement(SS, _,_,'**', HoleCell, Tower), % replace my hole

    % get solution for this size, backtrack from this
    generateGoal(ShadesCnt, ColoursCnt, Goal),!,

    % generate list of depths to search
    depthsGenerate(20,MaxRetriesList),
    (
        % try finding solution with this depth
        (
            nth1(_, MaxRetriesList, MaxRetries),
            solve(Tower, Goal, Moves, [], MaxRetries)
            % format('Found solution in depth ~d', [MaxRetries]),nl
        );
        % or fail miserably
        (write('Too many tries'),fail)
    ),
    !,
    printTower(Tower),nl,
    track(Tower, Moves).


%
% UTILS & HELPERS
%
depthsGenerate(Max,R) :-
    numlist(1, Max, R).
    % findall(X,(member(X,L), X mod 2=:=0),R). % get by stride of 2

towerSize(Tower, Rows, Cols) :- length(Tower, Rows), member(Row, Tower), !, length(Row, Cols).

createCell(B,A,[X,Y]) :- atom_string(A,X), atom_string(B, Y).

replaceElement(T,X,Y,E,NE,NT) :-
  nth1(Y,T,Row),
  nth1(X,Row,E),
  select(E,Row,NE,NewRow),
  select(Row,T,NewRow,NT).

simplifyTower(Tower, SimplifiedTower) :- maplist(simplifyRow, Tower, SimplifiedTower).
simplifyRow(Cells, NewRow) :- maplist(simplifyCell, Cells, NewRow).
simplifyCell([Shade, Colour], '**') :- Colour = '*', Shade = '*'. % special cell to be replaced
simplifyCell([Shade, Colour], X) :- createCell(Colour,Shade,X).

printTower([]).
printTower([Row|RestRows]) :-
    printTowerRow(Row),nl,
    printTower(RestRows).
printTowerRow([]).
printTowerRow([C|Rest]) :- hole(C),write('** '),!,printTowerRow(Rest).
printTowerRow([[CellColour,CellShade]|Rest]) :-
    write(CellColour),write(CellShade),write(" "),
    printTowerRow(Rest).

takeLetters(N, Res) :-
  length(Res, N),
  prefix(Res, ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']).

generateGoal(ShadesCnt,ColoursCnt,Goal) :-
  numlist(1, ShadesCnt, RowIndexes),
  maplist(atom_string, RowIndexes, RowIndexesStr),
  maplist(generateRow(ColoursCnt), RowIndexesStr, Goal).
generateRow(Cols,N,Row) :- takeLetters(Cols,R), maplist(createCell(N),R,Row).


%
% SOLVERS
%

% moves hole in T in direction DIR and saves the result in R
moveHole(T, R, hole:DIR) :-
    hole(H), % get hole position

    % limit search space to rows next to each other with regard to the direction DIR
    ((nextto(Row,MoveRow,T),DIR=down);(nextto(MoveRow,Row,T),DIR=up)),
    % find the row and the element
    nth1(X,Row,H),
    nth1(X,MoveRow,MoveElem),

    % replace in row
    select(MoveElem,MoveRow,H,NewMoveRow),
    select(H,Row,MoveElem,NewRow),
    % replace rows, save result
    select(Row,T,NewRow,NewT),
    select(MoveRow,NewT,NewMoveRow,R).

% rotate list
% http://stackoverflow.com/questions/10255703/how-to-rotate-lists-in-prolog
rotate([H|T],R) :- append(T,[H],R).
moveRotate(T, Res, rotateL:N) :- nth1(N,T,A),rotate(A,B),select(A,T,B,Res).
moveRotate(T, Res, rotateR:N) :- nth1(N,T,A),rotate(B,A),select(A,T,B,Res).

% use backtracking to find the solution G from starting point T
solve(T, T, [], _, _).
solve(_, _, _, _, N) :- N =< 0, !, fail.
solve(T, G, [M|S], History, N) :-
    N > 0,
    N1 is N-1,
    (
        moveRotate(T,R1,M);
        moveHole(T,R1,M)
    ),
    not(member(R1,History)),
    solve(R1,G,S,[T|History],N1).

% print steps of the found solution
track(_, []) :- !.
track(T, [M|S]) :-
    (
        moveRotate(T,R,M);
        moveHole(T,R,M)
    ),
    printTower(R),nl,!,
    track(R,S).

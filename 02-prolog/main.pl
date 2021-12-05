main :-
  get_input(Tokens),
  chunk(Tokens, ChunkedTokens),
  foldl(process1, ChunkedTokens, (0, 0), (HP1, Depth1)),
  Result1 is HP1 * Depth1,
  write('Part 1: '),write(Result1),nl,
  foldl(process2, ChunkedTokens, (0, 0, 0), (HP2, Depth2, _)),
  Result2 is HP2 * Depth2,
  write('Part 2: '),write(Result2),nl.

get_input([String|Tail]) :-
  read_string(user_input, "\n ", "", Sep, String),
  Sep \= -1,
  get_input(Tail).

get_input([]) :-
  read_string(user_input, "\n ", "", -1, _).

chunk([], []).
chunk([A,B|Tail1], [(A,B)|Tail2]) :- chunk(Tail1, Tail2).

process1(("forward", X), (HP1, Depth), (HP2, Depth)) :- atom_number(X, N), HP2 is HP1 + N.
process1(("up", X), (HP, Depth1), (HP, Depth2)) :- atom_number(X, N), Depth2 is Depth1 - N.
process1(("down", X), (HP, Depth1), (HP, Depth2)) :- atom_number(X, N), Depth2 is Depth1 + N.

process2(("forward", X), (HP1, Depth1, Aim), (HP2, Depth2, Aim)) :-
  atom_number(X, N),
  HP2 is HP1 + N,
  Depth2 is Depth1 + N * Aim.
process2(("up", X), (HP, Depth, Aim1), (HP, Depth, Aim2)) :- atom_number(X, N), Aim2 is Aim1 - N.
process2(("down", X), (HP, Depth, Aim1), (HP, Depth, Aim2)) :- atom_number(X, N), Aim2 is Aim1 + N.

?- main(), halt.

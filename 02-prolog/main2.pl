main :-
  get_input(Tokens),
  chunk(Tokens, ChunkedTokens),
  foldl(process, ChunkedTokens, (0, 0, 0), (HP, Depth, _)),
  Result is HP * Depth,
  write(Result),nl.

get_input([String|Tail]) :-
  read_string(user_input, "\n ", "", Sep, String),
  Sep \= -1,
  get_input(Tail).

get_input([]) :-
  read_string(user_input, "\n ", "", -1, _).

chunk([], []).
chunk([A,B|Tail1], [(A,B)|Tail2]) :- chunk(Tail1, Tail2).

process(("forward", X), (HP1, Depth1, Aim), (HP2, Depth2, Aim)) :-
  atom_number(X, N),
  HP2 is HP1 + N,
  Depth2 is Depth1 + N * Aim.
process(("up", X), (HP, Depth, Aim1), (HP, Depth, Aim2)) :- atom_number(X, N), Aim2 is Aim1 - N.
process(("down", X), (HP, Depth, Aim1), (HP, Depth, Aim2)) :- atom_number(X, N), Aim2 is Aim1 + N.

?- main(), halt.

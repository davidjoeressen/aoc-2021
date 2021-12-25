#!/usr/bin/env escript
-module(main).
-export([main/1]).

dice(N) ->
  F = fun(X) -> ((N+X) rem 100) + 1 end,
  lists:sum(lists:map(F,[0,1,2])).

calculate(P1,P2,S1,S2,D,P) ->
  if
    S1 >= 1000 -> S2 * D;
    S2 >= 1000 -> S1 * D;
    P == 0 ->
      PP = (P1 + dice(D)) rem 10,
      calculate(PP,P2,S1+PP+1,S2,D+3,1);
    P /= 0 ->
      PP = (P2 + dice(D)) rem 10,
      calculate(P1,PP,S1,S2+PP+1,D+3,0)
  end.

next({P1,P2,S1,S2,0,Games},{N,Dice}) ->
  P = (P1 + Dice) rem 10,
  {P,P2,S1+P+1,S2,1,Games*N};
next({P1,P2,S1,S2,1,Games},{N,Dice}) ->
  P = (P2 + Dice) rem 10,
  {P1,P,S1,S2+P+1,0,Games*N}.

getScores({_,_,S,_,_,Games}) when S >= 21 -> {Games,0};
getScores({_,_,_,S,_,Games}) when S >= 21 -> {0,Games};
getScores(State) ->
  case erlang:get({'getScores', State}) of
    {G1,G2} -> {G1,G2};
    'undefined' ->
      Dice = [{1,3},{3,4},{6,5},{7,6},{6,7},{3,8},{1,9}],
      Res = lists:foldl(fun({X,Y},{A,B}) -> {X+A,Y+B} end, {0,0}, lists:map(fun(X) -> getScores(next(State, X)) end, Dice)),
      erlang:put({'getScores', State}, Res),
      Res
  end.

getHighScore({A,B}) when A > B -> A;
getHighScore({_,B}) -> B.

part2(A,B) -> getHighScore(getScores({A-1,B-1,0,0,0,1})).
part1(A,B) -> calculate(A-1,B-1,0,0,0,0).

main([Player1, Player2]) ->
  try
    P1 = list_to_integer(Player1),
    P2 = list_to_integer(Player2),
    io:fwrite("Part 1: ~B~n", [part1(P1,P2)]),
    io:fwrite("Part 2: ~B~n", [part2(P1,P2)])
  catch
    _:_ -> usage()
  end;
main(_) -> usage().

usage() ->
  io:format("Usage: main.erl player1 player2~n"),
  halt(1).

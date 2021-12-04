512 constant max-line
25 constant board-size
100 constant max-numbers

create line-buffer max-line 2 + allot
variable board board-size cells allot
variable numbers 100 cells allot

variable number-count
variable fastest
variable score

: of-arr ( n n -- n ) cells + ;

\ logic functions

: clear-number { n -- }
\ clears the number from the board
  board-size 0 u+do
    board i of-arr @ n = if
      -1 board i of-arr !
    endif
  loop ;

: check-rows ( -- f )
  0 5 0 u+do
    -1 5 0 u+do
      board j 5 * i + of-arr @ -1 = and
    loop or
  loop ;

: check-columns ( -- f )
  0 5 0 u+do
    -1 5 0 u+do
      board j i 5 * + of-arr @ -1 = and
    loop or
  loop ;

: check-board ( -- f )
\ checks if the board is won
  check-rows check-columns or ;

: get-score ( n -- n )
\ calculate the score for the board
\ input is the last called number (multiplier)
  0 board-size 0 u+do
    board i of-arr @
    dup -1 = if drop else + endif
  loop * ;

: process-board ( -- n n )
\ return values are index of last number and score
  -1 begin
    1+
    numbers over of-arr @ clear-number
    check-board
  until
  numbers over of-arr @ get-score ;

: update-score { turn new-score part1 -- }
  turn fastest @ part1 if < else > endif
  if
    turn fastest !
    new-score score !
  endif ;

\ input functions

: clear-line-buffer ( -- )
  max-line 0 u+do
    0 line-buffer i chars + !
  loop ;

: read-number { start -- n }
  line-buffer start chars + 2 s>number? 2drop ;

: isDigit ( n -- f )
  dup 48 >= swap 57 <= and ;

: scan-behind-number ( n -- n )
  begin
    dup
    line-buffer swap chars + c@ isDigit
  while
    1+
  repeat ;

: read-numbers { file -- }
  clear-line-buffer
  0 number-count !
  line-buffer max-line file read-line throw drop ( num-chars )
  0 begin
    dup read-number
    numbers number-count @ of-arr !
    number-count @ 1+ number-count !
    scan-behind-number 1+
    2dup
  <= until
  2drop ;

: read-board { file -- }
  5 0 u+do
    line-buffer max-line file read-line throw 2drop
    5 0 u+do
      line-buffer 3 i * chars + 2 s>number? if
        drop
      else
        2drop
        line-buffer 3 i * chars + 1+ 1 s>number? 2drop
      endif
      board j 5 * i + of-arr !
    loop
  loop ;

: main { file part1 -- }
  file read-numbers
  part1 if number-count @ else 0 endif fastest !
  begin
    line-buffer max-line file read-line throw
  while
    drop
    file read-board
    process-board
    part1 update-score
  repeat
  drop
  score ? ;

: part1 ( -- ) stdin 1 main bye ;
: part2 ( -- ) stdin 0 main bye ;

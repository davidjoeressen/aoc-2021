#!/usr/bin/gst -f
Boolean extend [
  asInteger [
    ^self ifTrue: [ 1 ] ifFalse: [ 0 ]
  ]
].

Array extend [
  binaryToInt [
    ^self fold: [ :acc :n | acc * 2 + n ]
  ]
].

digits := ((stdin nextLine) asArray) collect: [ :c | (c = $1) asInteger ].
count := 1.
stdin linesDo: [ :line |
  line doWithIndex: [ :c :i |
    (c = $1) ifTrue: [ digits at: i put: (digits at: i) + 1 ].
  ].
  count := count + 1.
].
limit := (count / 2) floor.
gammaRate := digits collect: [ :n | (n > limit) asInteger ].
epsilonRate := gammaRate collect: [ :n | (n ~= 1) asInteger ].

result := gammaRate binaryToInt * epsilonRate binaryToInt.
Transcript nextPutAll: result asString; cr.

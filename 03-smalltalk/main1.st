#!/usr/bin/gst -f
digits := ((stdin nextLine) asArray) collect: [ :c | (c = $1) ifTrue: [ 1 ] ifFalse: [ 0 ]].
count := 1.
stdin linesDo: [ :line |
  line doWithIndex: [ :c :i |
    (c = $1) ifTrue: [ digits at: i put: (digits at: i) + 1 ].
  ].
  count := count + 1.
].
limit := (count / 2) floor.
gammaRateBits := digits collect: [ :n | (n > limit) ifTrue: [ 1 ] ifFalse: [ 0 ]].
epsilonRateBits := gammaRateBits collect: [ :n | (n = 1) ifTrue: [ 0 ] ifFalse: [ 1 ]].

gammaRate := gammaRateBits fold: [ :acc :n | acc * 2 + n ].
epsilonRate := epsilonRateBits fold: [ :acc :n | acc * 2 + n ].
Transcript nextPutAll: (gammaRate * epsilonRate) asString; cr.

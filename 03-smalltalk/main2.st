#!/usr/bin/gst -f
Boolean extend [
  asInteger [
    ^self ifTrue: [ 1 ] ifFalse: [ 0 ]
  ]
].

Array extend [
  binaryToInt [
    ^self fold: [ :acc :n | acc * 2 + n ].
  ]
].

OrderedCollection extend [
  selectMostCommonOn: index [
    | numOnes choose |
    numOnes := self inject: 0 into: [ :acc :x | acc + (x at: index) ].
    choose := (numOnes >= (self size - numOnes)) asInteger.
    ^self select: [ :r | (r at: index) = choose ].
  ]

  selectLeastCommonOn: index [
    | numOnes choose |
    numOnes := self inject: 0 into: [ :acc :x | acc + (x at: index) ].
    choose := (numOnes < (self size - numOnes)) asInteger.
    ^self select: [ :r | (r at: index) = choose ].
  ]
].

lines := OrderedCollection new.
stdin linesDo: [ :line |
  lines add: ((line asArray) collect: [ :c | (c = $1) ifTrue: [ 1 ] ifFalse: [ 0 ]]).
].

lineLength := (lines at: 1) size.
scrubberDigits := lines.
oxygenDigits := lines.
1 to: lineLength do: [ :n |
  (oxygenDigits size > 1) ifTrue: [ oxygenDigits := oxygenDigits selectMostCommonOn: n ].
  (scrubberDigits size > 1) ifTrue: [ scrubberDigits := scrubberDigits selectLeastCommonOn: n ].
].

oxygen := (oxygenDigits at: 1) binaryToInt.
scrubber := (scrubberDigits at: 1) binaryToInt.
Transcript nextPutAll: (oxygen * scrubber) asString; cr.

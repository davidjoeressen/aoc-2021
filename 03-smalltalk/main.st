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
  mostCommonAt: index [
    | n |
    n := (self count: [ :x | (x at: index) = 1 ]).
    ^(n >= (self size / 2) ceiling) asInteger.
  ]

  oxygenSelectBlock: index [
    | oxygenDigit |
    oxygenDigit := self mostCommonAt: index.
    ^[ :r | (r at: index) = oxygenDigit ].
  ]

  selectMostCommonOn: index [
    (self size = 1) ifTrue: [ ^self at: 1 ] ifFalse: [
      ^(self select: (self oxygenSelectBlock: index)) selectMostCommonOn: (index + 1)
    ]
  ]

  selectLeastCommonOn: index [
    (self size = 1) ifTrue: [ ^self at: 1 ] ifFalse: [
      ^(self reject: (self oxygenSelectBlock: index)) selectLeastCommonOn: (index + 1)
    ]
  ]
].

lines := OrderedCollection new.
stdin linesDo: [ :line | lines add: ((line asArray) collect: [ :c | (c = $1) asInteger ]) ].

gamma := 1 to: ((lines at: 1) size) collect: [ :n | lines mostCommonAt: n ].
epsilon := gamma collect: [ :n | (n ~= 1) asInteger ].
result1 := gamma binaryToInt * epsilon binaryToInt.

oxygen := lines selectMostCommonOn: 1.
scrubber := lines selectLeastCommonOn: 1.
result2 := oxygen binaryToInt * scrubber binaryToInt.

Transcript nextPutAll: 'Part 1: '; nextPutAll: result1 asString; cr.
Transcript nextPutAll: 'Part 2: '; nextPutAll: result2 asString; cr.

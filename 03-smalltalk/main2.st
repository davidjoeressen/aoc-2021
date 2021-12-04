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
  oxygenSelectBlock: index [
    | oxygenDigit n |
    n := (self count: [ :x | (x at: index) = 1 ]).
    oxygenDigit := (n >= (self size / 2) ceiling) asInteger.
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

oxygen := lines selectMostCommonOn: 1.
scrubber := lines selectLeastCommonOn: 1.

result := oxygen binaryToInt * scrubber binaryToInt.
Transcript nextPutAll: result asString; cr.

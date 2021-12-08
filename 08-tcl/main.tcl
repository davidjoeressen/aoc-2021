#!/bin/env tclsh
proc getdata filename {
  variable out [list]
  variable fl [open $filename]
  variable data [read $fl]
  foreach line [split $data "\n"] {
    if { [string length $line] > 0 } { lappend out [split $line] }
  }
  close $fl
  return $out
}

proc process1 line {
  variable back [lrange $line 11 14]
  variable result 0
  foreach x $back { if { [lsearch {2 3 4 7} [string length $x]] >= 0 } { incr result } }
  return $result
}

proc intersection {l1 l2} {
  variable out {}
  foreach x $l1 { if { [lsearch $l2 $x] != -1 } { lappend out $x } }
  return $out
}

proc without {l1 l2} {
  variable out {}
  foreach x $l1 { if { [lsearch $l2 $x] == -1 } { lappend out $x } }
  return $out
}

proc findallin {lst xs} {
  foreach x $lst {
    if {[intersection $x $xs] == $xs} { return $x }
  }
}

proc process2 line {
  variable front [lmap x [lrange $line 0 9] { lsort [split $x {}] }]
  variable back [lmap x [lrange $line 11 14] { lsort [split $x {}] }]
  variable digits [lrepeat 10 {}]
  variable lengths [lmap x $front {llength $x}]
  lset digits 1 [lindex $front [lsearch $lengths 2]]
  lset digits 4 [lindex $front [lsearch $lengths 4]]
  lset digits 7 [lindex $front [lsearch $lengths 3]]
  lset digits 8 {a b c d e f g}
  variable 235 [lmap i [lsearch -all $lengths 5] { lindex $front $i }]
  variable 069 [lmap i [lsearch -all $lengths 6] { lindex $front $i }]
  variable bd [without [lindex $digits 4] [lindex $digits 1]]
  lset digits 3 [findallin $235 [lindex $digits 1]]
  lset digits 5 [findallin $235 $bd]
  lset digits 2 [lindex [without $235 [list [lindex $digits 3] [lindex $digits 5]]] 0]
  lset digits 9 [findallin $069 [lindex $digits 3]]
  lset digits 6 [findallin [without $069 [list [lindex $digits 9]]] [lindex $digits 5]]
  lset digits 0 [lindex [without $069 [list [lindex $digits 6] [lindex $digits 9]]] 0]

  variable result ""
  foreach x $back {
    append result [lsearch $digits $x]
  }
  return $result
}

if { $argc == 0 } {
  puts [string cat "Usage: " $argv0 " <filename>"]
  exit 1
}
variable data [getdata [lindex $argv 0]]
variable part1 0
foreach line $data { incr part1 [process1 $line] }
puts [string cat "Part 1: " $part1]

variable part2 0
foreach line $data { incr part2 [string trimleft [process2 $line] 0] }
puts [string cat "Part 2: " $part2]

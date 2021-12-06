data =. <: #/.~ (i.9), ".;._1 ',',_1 }. fread 2 { ARGV
next =: (*&(6=i.9)@:{.)+(1&|.)
echo 'Part 1: ',":+/next^:80 data
echo 'Part 2: ',":+/next^:256 data
exit 0

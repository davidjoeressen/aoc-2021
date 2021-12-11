use strict;
use warnings;
use List::Util qw(reduce);

open(my $in, "<", $ARGV[0]) or die "Can't open $ARGV[0]: $!";
my @lines = <$in>;
close($in);

my $p1 = 0;
my $p2 = 0;
my @p2scores = ();
my %pairs = (')' => '(', ']' => '[', '}' => '{', '>' => '<');
my %scores = (')' => 3, ']' => 57, '}' => 1197, '>' => 25137);

LINE: for my $line (@lines) {
  my @stack = ();
  my @chars = split('',$line);
  for (@chars) {
    if (/[[({<]/) {
      push(@stack,$_);
    } elsif (/[])}>]/) {
      if (pop(@stack) ne $pairs{$_}) {
        $p1 += $scores{$_};
        next LINE;
      }
    }
  }
  push(@p2scores,reduce { $a * 5 + index(" ([{<",$b) } 0, (reverse(@stack))) if (@stack);
}

@p2scores = sort {$a <=> $b} @p2scores;
$p2 = @p2scores[int(@p2scores/2)] if (@p2scores);

print "Part 1: $p1\n";
print "Part 2: $p2\n";

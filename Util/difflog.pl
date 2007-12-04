#!/usr/bin/perl

use strict;
use IO::File;
use File::Temp qw(tempfile);

my @differ = qw(diff -bw);

my $newfn = pop(@ARGV);
my $oldfn = pop(@ARGV);
my (%oldhash, %newhash);

my $article = q[(?:(?:workers?|users?)/)?\d{4,5}];

read_file($newfn, \%newhash);
read_file($oldfn, \%oldhash);

my @oldentries = reverse sort keys %oldhash;
my @newentries = reverse sort keys %newhash;

my $old = 0;
my $new = 0;

while ($old < @oldentries && $new < @newentries)
{
  my $cmp = $oldentries[$old] cmp $newentries[$new];
  if ($cmp > 0)
  {
    printf("only in %s: %s\n\n", $oldfn, $oldentries[$old++]);
  }
  elsif ($cmp < 0)
  {
    printf("only in %s: %s\n\n", $newfn, $newentries[$new++]);
  }
  else
  {
    if ($oldhash{$oldentries[$old]} ne $newhash{$newentries[$new]}) {
      my($oldfh, $oldtmp) = tempfile('difflog-XXXXXXXX', SUFFIX => '.old', DIR => '/tmp');
      print $oldfh $oldhash{$oldentries[$old]};
      close($oldfh);
      my($newfh, $newtmp) = tempfile('difflog-XXXXXXXX', SUFFIX => '.new', DIR => '/tmp');
      print $newfh $newhash{$newentries[$new]};
      close($newfh);
      open(DIFF, '-|', @differ, @ARGV, $oldtmp, $newtmp) or die $!;
      my @lines = <DIFF>;
      close(DIFF);
      unlink($oldtmp, $newtmp);
      if (@lines)
      {
	print "diff for ", $oldentries[$old], ":\n";
	map {
	  s/$oldtmp/$oldfn/;
	  s/$newtmp/$newfn/;
	} @lines;
	print @lines, "\n";
      }
    }
    ++$old;
    ++$new;
  }
}

while ($old < @oldentries)    
{
  printf("only in %s: %s\n", $oldfn, $oldentries[$old++]);
}

while ($new < @newentries)
{
  printf("only in %s: %s\n", $newfn, $newentries[$new++]);
}

sub append_entry
{
  my ($hashref, $entry, $tag, $block) = @_;

  if (exists($hashref->{$entry})) {
    $hashref->{$entry} .= "$tag\n$block";
  } else {
    $hashref->{$entry} = '';
    if (defined($tag) || defined($block)) {
      $hashref->{$entry} .= "$tag\n$block";
    }
  }
}

sub make_entries
{
  my ($hashref, $entry, $tag, $block) = @_;

  if ($entry =~ s/($article)/ARTICLE/) {
    my $key = $1;
    $key =~ s:workers?/::;
    &append_entry($hashref, $key, $tag, $block);
    while ($entry =~ s/($article)/ARTICLE/) {
      $key = $1;
      $key =~ s:workers?/::;
      &append_entry($hashref, $key, $tag, $block);
    }
  } else {
    &append_entry($hashref, $entry, $tag, $block);
  }
}

sub read_file
{
  my $fn = shift;
  my $hashref = shift;
  my $fh = new IO::File($fn, 'r');
  my ($tag, $date, $entry, $block);

  my $attrib = q[(?:\w+\s+)*] . $article;
  $attrib = q[(?:[^/]*\D[:,]\s*)?] . $attrib;
  $attrib = qq[(?:unposted|$attrib)];
  $attrib = qq[(?:(?:$attrib,\\s*)*$attrib)];

  $hashref->{unattributed} = $block = '';

  while (my $line = $fh->getline())
  {
    if ($line =~ /(\d{4}-\d\d-\d\d)\s+.+\s+<.+\@.+>/i) {
      $date = $1;
      $block =~ s/\n*\Z/\n/;
      if ($entry) {
	&make_entries($hashref, $entry, $tag, $block);
      } elsif ($tag) {
	$hashref->{unattributed} .= "\n$tag$block";
      }
      $entry = $block = '';
      $tag = $line;
    } elsif ($line =~ /\* ((?:$attrib)[^:]*):/) {
      my $next = $1;
      if ($entry) {
	$block =~ s/\n*\Z/\n/;
	&make_entries($hashref, $entry, $tag, $block);
      }
      &make_entries($hashref, $next);
      $entry = $next;
      $block = $line;
    } else {
      $block .= $line;
    }
  }
  $fh->close();
}

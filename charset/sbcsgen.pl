#!/usr/bin/env perl -w

# This script generates sbcsdat.c (the data for all the SBCSes) from its
# source form sbcs.dat.

$infile = "sbcs.dat";
$outfile = undef;
$outheader = undef;

my $doing_opts = 1;
my $nargs = 0;
while (@ARGV) {
    if ($doing_opts && $ARGV[0] =~ m/^-/) {
        if ($ARGV[0] =~ m/^--source=(.*)$/) {
            $outfile = $1;
            shift;
        } elsif ($ARGV[0] =~ m/^--header=(.*)$/) {
            $outheader = $1;
            shift;
        } elsif ($ARGV[0] eq "--") {
            $doing_opts = 0;
            shift;
        } else {
            die "unrecognised option '$ARGV[0]'\n";
        }
        next;
    }
    if ($nargs++ == 0) {
        $infile = $ARGV[0];
        shift;
    } else {
        die "spurious extra argument '$ARGV[0]'\n";
    }
}

die "usage: sbcsgen.pl ( --source=SRCFILE | --header=HDRFILE ) [INFILE]\n"
    unless defined $outfile or defined $outheader;

open INFH, $infile;

my $charsetname = undef;
my @vals = ();

my @charsetnames = ();
my @sortpriority = ();

if (defined $outfile) {
    open SOURCEFH, ">", $outfile;
    select SOURCEFH;

    print "/*\n";
    print " * sbcsdat.c - data definitions for single-byte character sets.\n";
    print " *\n";
    print " * Generated by sbcsgen.pl from sbcs.dat.\n";
    print " * You should edit those files rather than editing this one.\n";
    print " */\n";
    print "\n";
    print "#ifndef ENUM_CHARSETS\n";
    print "\n";
    print "#include \"charset.h\"\n";
    print "#include \"internal.h\"\n";
    print "\n";
}

while (<INFH>) {
    chomp;
    y/\r\n//; # robustness in the face of strange line endings
    if (/^(charset|tables) (.*)$/) {
        $tables_only = ($1 eq "tables");
	$charsetname = $2;
	@vals = ();
	@sortpriority = map { 0 } 0..255;
    } elsif (/^sortpriority ([^-]*)-([^-]*) (.*)$/) {
	for ($i = hex $1; $i <= hex $2; $i++) {
	    $sortpriority[$i] += $3;
	}
    } elsif (/^[0-9a-fA-FX]/) {
	push @vals, map { $_ eq "XXXX" ? -1 : hex $_ } split / +/, $_;
	if (scalar @vals > 256) {
	    die "$infile:$.: charset $charsetname has more than 256 values\n";
	} elsif (scalar @vals == 256) {
	    &outcharset($charsetname, \@vals, \@sortpriority, $tables_only)
                if defined $outfile;
	    push @charsetnames, $charsetname unless $tables_only;
	    $charsetname = undef;
	    @vals = ();
	    @sortpriority = map { 0 } 0..255;
	}
    }
}

if (defined $outfile) {
    print "#else /* ENUM_CHARSETS */\n";
    print "\n";

    foreach $i (@charsetnames) {
        print "ENUM_CHARSET($i)\n";
    }

    print "\n";
    print "#endif /* ENUM_CHARSETS */\n";

    close SOURCEFH;
}

if (defined $outheader) {
    open HEADERFH, ">", $outheader;
    select HEADERFH;

    print "/*\n";
    print " * sbcsdat.h - header file for SBCS data structures.\n";
    print " *\n";
    print " * Generated by sbcsgen.pl from sbcs.dat.\n";
    print " * You should edit those files rather than editing this one.\n";
    print " */\n";
    print "\n";
    print "#ifndef charset_sbcsdat_h\n";
    print "#define charset_sbcsdat_h\n";
    print "\n";
    print "#include \"charset.h\"\n";
    print "#include \"internal.h\"\n";
    print "\n";
    foreach $i (@charsetnames) {
        print "extern const sbcs_data sbcsdata_$i;\n";
    }
    print "\n";
    print "#endif /* charset_sbcsdat_h */\n";

    close HEADERFH;
}

sub outcharset($$$$) {
    my ($name, $vals, $sortpriority, $tables_only) = @_;
    my ($prefix, $i, @sorted);

    print "const sbcs_data sbcsdata_$name = {\n";
    print "    {\n";
    $prefix = "    ";
    @sorted = ();
    for ($i = 0; $i < 256; $i++) {
	if ($vals->[$i] < 0) {
	    printf "%sERROR ", $prefix;
	} else {
	    printf "%s0x%04x", $prefix, $vals->[$i];
	    die "ooh? $i\n" unless defined $sortpriority->[$i];
	    push @sorted, [$i, $vals->[$i], 0+$sortpriority->[$i]];
	}
	if ($i % 8 == 7) {
	    $prefix = ",\n    ";
	} else {
	    $prefix = ", ";
	}
    }
    print "\n    },\n    {\n";
    @sorted = sort { ($a->[1] == $b->[1] ?
	              $b->[2] <=> $a->[2] :
	              $a->[1] <=> $b->[1]) ||
                     $a->[0] <=> $b->[0] } @sorted;
    $prefix = "    ";
    $uval = -1;
    for ($i = $j = 0; $i < scalar @sorted; $i++) {
	next if ($uval == $sorted[$i]->[1]); # low-priority alternative
	$uval = $sorted[$i]->[1];
	printf "%s0x%02x", $prefix, $sorted[$i]->[0];
	if ($j % 8 == 7) {
	    $prefix = ",\n    ";
	} else {
	    $prefix = ", ";
	}
	$j++;
    }
    printf "\n    },\n    %d\n", $j;
    print "};\n";
    unless ($tables_only) {
        print "const charset_spec charset_$name = {\n" .
            "    $name, read_sbcs, write_sbcs, &sbcsdata_$name\n};\n\n";
    }
}

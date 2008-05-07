#!/usr/bin/env perl

use strict;
use Getopt::Long;

main();

sub main {
    my @relevant_packages = ();

    my $result = GetOptions(
	'p|package:s' => \@relevant_packages,
	'h|help'      => \&PrintUsage,
    );

    if ( ! $result ) {
	PrintUsage(1);
    }

    if ( ! @relevant_packages ) {
	push @relevant_packages, 'main';
    }

    my $call_hash = CalculateCallingGraph( \@relevant_packages );

    PrintResults( $call_hash );
}

sub CalculateCallingGraph {
    my ( $relevant_package_list ) = @_;

    my %relevant  = ( map { $_ => 1 } @{ $relevant_package_list || [] } );
    my %call_hash = ();
    my @stack     = ( undef, -1 );

    while ( my $line = <ARGV> ) {
	chomp $line;

	my $cur_indent = 0;
	my $package = undef;
	my $symbol = undef;
	my $repeat = 1;

	if ( $line =~ m/\A
			    (\s*)		  # some leading whitespace
			    ([^\s]+)		  # package name
			    ::			  # package-symbol separator
			    ([^\s]+)		  # symbol
			    (?:\s+\(([0-9]+)x\))? # optional repeat count
			\z/x ) {
	    $cur_indent = length $1;
	    $package    = $2;
	    $symbol     = "$2::$3";
	    $repeat     = $4 if ( defined $4 );

	    $symbol =~ s/::/-/g;
	}

	    #
	    # pretty much forget this symbol if it's not in a relevant
	    # package
	    #
	if ( ! $relevant{ $package } ) {
	    $symbol = undef;
	}

	while ( $cur_indent <= $stack[-1] ) {
	    pop @stack;
	    pop @stack;
	}

	$call_hash{ $symbol }{ '' } = 1;
	$call_hash{ $stack[-2] }{ $symbol } += $repeat;

	push @stack, $symbol, $cur_indent;
    }

    return \%call_hash;
}

sub PrintResults {
    my ( $call_hash ) = @_;

    print "(\n";
    foreach my $parent ( sort keys %{ $call_hash || {} } ) {
	if ( $parent ) {
	    my $phash = $call_hash->{$parent} || {};

	    print "  ($parent";
	    foreach my $child ( sort keys %{ $phash } ) {
		if ( $child ) {
		    my $count = $phash->{ $child };
		    print " ($child . $count)";
		}
	    }
	    print ")\n";
	}
    }
    print ")\n";
}

sub PrintUsage {
    my ( $exit_code ) = @_;
    $exit_code = 0 if ( ! defined $exit_code );

    print << 'EOM';
write-dprofpp.pl [-package package1 [-package package2 ...]]
		    [dprofpp_-t_output_filename]
EOM

    exit $exit_code;
}

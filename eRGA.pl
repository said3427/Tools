#!/usr/bin/perl 

use strict;
use warnings;

my $argc=@ARGV;
my $reference;
my $denovo;
my $all;
my $output= "output.fasta";

for (my $i = 0; $i < $argc; ++$i) {
	if ($ARGV[$i] eq '-ref') {
	  if (++$i < $argc) { $reference = $ARGV[$i]; } else { fatalError(); }
	} elsif ($ARGV[$i] eq '-de-novo') {
	  if (++$i < $argc) { $denovo = $ARGV[$i]; } else { fatalError(); }
	} elsif ($ARGV[$i] eq '-out') {
	  if (++$i < $argc) { $output = $ARGV[$i]; } else { fatalError(); }
	} else {
	  fatalError();
	}
}


#memorize the reference guided assembly
open(CONSENSUS, "$reference");
my @reference;
my $header;
my $c=0;
while(<CONSENSUS>) {
	chomp $_;
	if($_ !~ /^\>/) {
		my @line=split(//, $_);
		foreach my $i (@line) {
			$reference[$c]= uc $i;
			$c++;
		}
	} else {
		$header=chomp $_;
	}
}
close(CONSENSUS);
my $reference_length=$c;
print "reference_length $reference_length \n";
print "reference $reference \n";


#memorize the de novo guided assembly
open(CONSENSUS, "$denovo");
my @denovo;
$c=0;
while(<CONSENSUS>) {
	chomp $_;
	if($_ !~ /^\>/) {
		my @line=split(//, $_);
		foreach my $i (@line) {
			$denovo[$c]=uc $i;
			$c++;
		}
	} else {
		$header=chomp $_;
	}
}
close(CONSENSUS);
my $denovo_length=$c;
print "De novo guiaded $denovo_length \n";


# memorize all the contigs of dn-RGA
my @DeNovocontigs;
my $start=0;
my $end;
my $stop=0;
my $print=0;
my $k;
my $splices=0;
my $numN=0;
print "Denovo es @denovo \n";
for($k=0; $k<@denovo; $k++) {
	if($denovo[$k] ne "N") {


		if($print == 0 ) { 
			if($start != $stop) {
				push  @DeNovocontigs, [$start,$stop, "gap"];
			}
			$start=$stop;
			$print=1;
		}
		$stop++;		
	} elsif ($denovo[$k] eq "N") {

		if($print==1) {
			push @DeNovocontigs, [$start,$stop, "contig"];
			$start=$k;
		}
		$splices++;
		$print=0;
		$stop++;

	}
}



my $i=@denovo;
if($denovo[$denovo_length-1] ne "N") {
	push @DeNovocontigs, [$start,$i, "contig"];
	$splices ++;
} else {
	push @DeNovocontigs, [$start,$i, "gap"];
}

my $DeNovocontigs = $splices;


# memorize all the contigs of s-RGA
my @Referencecontigs;
$start=0;
$stop=0;
$print=0;

$splices=0;
for($k=0; $k<@reference; $k++) {
	if($reference[$k] ne "N") {

		if($print == 0 ) { 
			if($start != $stop) {
				push  @Referencecontigs, [$start,$stop, "gap"];
			}
			$start=$stop;
			$print=1;
		}
		$stop++;		
	} elsif ($reference[$k] eq "N") {

		if($print==1) {
			push @Referencecontigs, [$start,$stop, "contig"];
			$start=$k;
		}
		$splices++;
		$print=0;
		$stop++;

	}
}

if($reference[$reference_length-1] ne "N") {
	my $i=@reference;
	push @Referencecontigs, [$start,$i];
	$splices ++;
} else {
	push @Referencecontigs, [$start,$i, "gap"];
}

my $Referencecontigs = $splices;



my $d=100;
my $read=1;
$c=0;
my $DIFF = abs($denovo_length - $reference_length);
my $position=0;

my ($dnS , $dnE , $rS, $rE) = (0,0,0,0);
my ($dnS_t , $dnE_t , $rS_t, $rE_t) = (0,0,0,0);

open(OUT, ">$output") || die("not able to open file $output: $!");

print OUT ">e-RGA\n";

open(MERGELIST, ">mergeList$output") || die("not able to open file mergeList$output : $!");

foreach my $contig (@DeNovocontigs) {
print " debivi $DeNovocontigs \n"	;

print " Contig $contig \n"	;
	#extract queries
	if($contig->[2] =~ /contig/) {
		($dnS_t , $dnE_t , $rS_t, $rE_t) = (0,0,0,0);
		$c++;
		$read=1;
		my $first =0;
		print "contig $c:\n";
		for(my $i = $contig->[0]; $i < $contig->[1]; $i+=$d) {
			my $start= $i;
			my $end= $i + $d;
			if ( $end > $contig->[1]) { # special case, i'm creating a read that exceed the contig limit
				$end = $contig->[1];
			}
			my $length = $end - $start; # length of the read --> usualy is $d
			if($length >= 50) { # if the query has length at least 50
				open(QUERY, ">Query$output\_$start.txt") || die("not able to open file Query.txt: $!"); #save the query in a file
				my $e =$end-1;
				print QUERY ">$c\_$read\_".$start."_".$e."_".$length."\n"; #print the header
				for (my $j=$start; $j < $end; $j++ ) {
					print QUERY  $denovo[$j]; #extract the read from the de novo sequence
				}
				print QUERY "\n";
				$read++;
				close QUERY;
				#Now prepare the databes from Refernce
				my $startDatabase = $start  - $DIFF; 
				if( $startDatabase < 0 ) { #special case at the beginning
					$startDatabase = 0;
				}
				my $endDatabase = $end + $DIFF;
				if( $endDatabase > $reference_length ) { #special case at the end
					$endDatabase = $reference_length-1;
				}
				open(BLAST, ">Database$output\_$start\.fasta");
				print BLAST ">database\n";
				for my $j ($startDatabase .. $endDatabase) {#save the databes on a file
					print BLAST $reference[$j];
				}
				print BLAST "\n";
				close BLAST;
			
#				print "\t\tQuery: $start .. $end\n";
#				print "\t\tDatabase: $startDatabase .. $endDatabase\n";
			
			
				system("blastn -subject Database$output\_$start\.fasta -query Query$output\_$start\.txt -outfmt 6 > blast$output\_$start\.info");
				#now extract information form blast query				
				my ($rs,$re,$dns,$dne)= (0,0,0,0);
				($rs,$re,$dns,$dne)= readAlignment("blast$output\_$start\.info", $startDatabase); #parse the alignemnt
				#we memorize the strating point and the end point on the reference and on the de novo sequence

				if($rs>0 and $first == 0) { # if an hit is found and it is the first memorize it
					($rS_t, $rE_t , $dnS_t , $dnE_t ) = ($rs,$re,$dns,$dne);
					$first = 1;
				} 
				if($rs>0 and $first == 1) {
					$dnE_t = $dne;
					$rE_t = $re;
				}
			}
		}
		#now I count the number of N contained in reference[rE..rS_t] and denovo[dnE..dnS_t]
		if($dnS_t > 0) { # if I found my de novo contig 
			
			print "[$rE , $rS_t] [$dnE, $dnS_t] ---> $position\n";
			print MERGELIST "$rE\t$rS_t\t$dnE\t$dnS_t\tcopy\n";
			
			while($reference[$rE] !~ /N/ and $rE < $rS_t) {
				print OUT $reference[$rE];
				$position++;
				if($position%70 == 0) {
					print OUT "\n";
				}
				$rE++;
				$dnE++;
			}
			my $rS_tBack=$rS_t;
			my $dn_tBack=$dnS_t;
			while($reference[$rS_tBack] !~ /N/ and $rS_t > $rE) {
				$rS_tBack--;
				$dn_tBack--;
			}
			
			my $ref_N = 0;
			my $dn_N = 0;
			for(my $j = $rE; $j < $rS_t; $j++) {
				if($reference[$j] =~ /N/) {
					$ref_N++;
				}
			}
			for(my $j = $dnE; $j < $dnS_t; $j++) {
				if($denovo[$j] =~ /N/) {
					$dn_N++;
				}
			}
			#now print the one with less N
			if($dn_N < $ref_N) {
				for(my $j = $dnE; $j < $dnS_t; $j++) {
					print OUT $denovo[$j];
					$position++;
					if($position%70 == 0) {
						print OUT "\n";
					}
				} 
			} else {
				for(my $j = $rE; $j < $rS_t; $j++) {
					print OUT $reference[$j];
					$position++;
					if($position%70 == 0) {
						print OUT "\n";
					}
				}
			}
			
#			print "[$rE , $rS_t] [$dnE, $dnS_t] ---> $position\n";
			# now print the de novo contig
			for(my $j = $dnS_t; $j < $dnE_t; $j++) {
				print OUT $denovo[$j];
				$position++;
				if($position%70 == 0) {
					print OUT "\n";
				}
			} 
			print MERGELIST "$rS_t\t$rE_t\t$dnS_t\t$dnE_t\tmerge\n";
 			print "[$rS_t , $rE_t] [$dnS_t, $dnE_t] ---> $position\n";
			print "---------------\n";
			($dnS , $dnE , $rS, $rE) = ( $dnS_t , $dnE_t, $rS_t, $rE_t )
			
			
			
			
		} else {
			print "de novo contig not found...\n";
		}
		
		
		
	}



}












exit 0;



sub synopsis {
   print "$0 -ref s-A.fasta -de-novo dn-A.fasta -out e-A.fasta\n";
   return;
}


sub fatalError {
  print synopsis();
  exit(1);
}



sub readAlignment {
	my $infoFILE=shift;
	my $start = shift;
	open(SOL, "$infoFILE") || die("not able to open file $infoFILE: $!");
	my @RES;
	my @sol=(-1,-1,-1,-1); #reference:start, reference_end, denovo_start, denovo_end
	while(<SOL>) {
		push @RES, [(split /\t/, $_)];
	}
	if(@RES == 0) {
		#print "nothing found: $h\n";
	} elsif (@RES >= 1) {
		my $hit = $RES[0];
		if($hit->[3] >= 0.8*((split /\_/, $hit->[0])[4]) ){ # if a quasi-total hit is found keep it
			$sol[0] = $start + $hit->[8];
			$sol[1] = $start + $hit->[9];
			
			$sol[2] = (split /\_/, $hit->[0])[2] + $hit->[6] -1;
			$sol[3] = $sol[2] + $hit->[3];
#			foreach my $k (@RES) {
#				print $k->[0]."\t".$k->[1]."\t".$k->[2]."\t".$k->[3]."\t".$k->[4]."\t".$k->[5]."\t".$k->[6]."\t".$k->[7]."\t".$k->[8]."\t".$k->[9]."\n";
#			}
#			print "-----------------\n";
	
		} else {
			@RES = sort{$b->[3]<=>$a->[3]} @RES;
			my $hit = $RES[0];
			if($hit->[3] >= 0.5*((split /\_/, $hit->[0])[4]) ){
				$sol[0] = $start + $hit->[8];
				$sol[1] = $start + $hit->[9];
				
				$sol[2] = (split /\_/, $hit->[0])[2] + $hit->[6] -1;
				$sol[3] = $sol[2] + $hit->[3];
			}			
#			foreach my $k (@RES) {
#				print $k->[0]."\t".$k->[1]."\t".$k->[2]."\t".$k->[3]."\t".$k->[4]."\t".$k->[5]."\t".$k->[6]."\t".$k->[7]."\t".$k->[8]."\t".$k->[9]."\n";
#			}
#			print "-----------------\n";
		}
	}	
			
	close SOL;
	system("rm Database$output\_* Query$output\_* blast$output\_*");
	return @sol;

}




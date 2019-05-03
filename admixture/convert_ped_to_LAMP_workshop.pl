#!/usr/bin/perl


#specify the working directory
$input_dir = '/home/curso_population01/andres/LAMP/';
$out_dir = '/home/curso_population01/andres/LAMP/';


#specify the range of chr numbers to work with: 
for($i=22; $i<=22; $i++)
{
	$root = 'LatinAm_chromosome_' . $i;
	$ped_file = $input_dir . $root . '.ped';
	$map_file = $input_dir . $root . '.map';
	$in_freq[0] = $input_dir . 'WAfrica_chromosome_' . $i . '.frq';
	$in_freq[1] = $input_dir . 'NatAm_chromosome_' . $i . '.frq';
	$in_freq[2] = $input_dir . 'EUR_chromosome_' . $i . '.frq';
	
	$genofile = $root . '.geno';
	$posfile = $root . '.pos';
	$config_file = $root . '.config';
	$out_freq[0] = $out_dir . 'WAfrican-lamp_chr_' . $i . '.frq';
	$out_freq[1] = $out_dir . 'NatAm-lamp_chr_' . $i . '.frq';
	$out_freq[2] = $out_dir . 'European-lamp_chr_' . $i . '.frq';
	$LAMP_outfile = $root . '-LAMPout.txt';
	
	print "$i\n";

	open(GENOFILE, '>', $out_dir . $genofile);
	
	open(PED, '<', $ped_file);
	%allele_map = ();
	
	while(<PED>)
	{
		$line = $_;
		chomp($line);
		
		@fields = split(' ', $line);
	
		shift(@fields);
		shift(@fields);
		shift(@fields);
		shift(@fields);
		shift(@fields);
		shift(@fields);
		
		$n = scalar(@fields);
		
		for($p=0; $p<$n; $p+=2)
		{
			if (($fields[$p] eq '0') | ($fields[$p+1] eq '0'))
			{
				print GENOFILE "-1\t";
			}
			elsif ($fields[$p] eq $fields[$p+1])
			{
				$a = $allele_map{$p};
				if ($a eq '')
				{
					$a = $fields[$p];
					$allele_map{$p} = $a;
				}
				
				if ($fields[$p] eq $a)
				{
					print GENOFILE "0\t";
				}
				else
				{
					print GENOFILE "2\t";				
				}
			}
			else
			{
					print GENOFILE "1\t";
			}
		}
		print GENOFILE "\n";
	}
	close(PED);
	close(GENOFILE);
	
	open(MAP, '<', $map_file);
	open(POS, '>', $out_dir . $posfile);
	
	while(<MAP>)
	{
		$line = $_;
		chomp($line);
		
		@fields = split(' ', $line);
		
		print POS "$fields[3]\n";
	}
	
	close(MAP);
	close(POS);
	
	for ($j=0; $j<3; $j++)
	{
		open(FRQ, '<', $in_freq[$j]);
		open(OUTFRQ, '>', $out_freq[$j]);
		$line = <FRQ>;
		while(<FRQ>)
		{
			$line = $_;
			chomp($line);
		
			@fields = split(' ', $line);
		
			print OUTFRQ "$fields[4]\n";
		}
		close(OUTFRQ);
		close(FRQ);
	}

	open(CONFIG, '>', $out_dir . $config_file);
	print CONFIG "# NYU 650K (LatinAmericans) chr$i\n";
	print CONFIG "populations=3\n";
	print CONFIG "genofile=$genofile\n";
	print CONFIG "posfile=$posfile\n";
	print CONFIG "outputancestryfile=$LAMP_outfile\n";
	print CONFIG "generations=20\n";
	print CONFIG "alpha=0.2,0.4,0.4\n";
	print CONFIG "recombrate=1e-8\n";
	print CONFIG "ldcutoff=0.1\n";
	print CONFIG "offset=0.2\n";
	$str = 'pfile=' . $out_freq[0] . ',' . $out_freq[1] . ',' . $out_freq[2];
	print CONFIG "$str\n";
	
}

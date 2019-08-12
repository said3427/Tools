# Export variables

`export annovar="/home/said/Downloads/Exoma/annovar/annovar/table_annovar.pl"`


`export annovarDB="/home/said/Downloads/Exoma/annovar"`

# Usage 

`sample="ddc01_s08"`

```shell
$annovar "$sample".vcf \
	-out "$sample" ${annovarDB} \
	-buildver hg38  \
	-remove \
	-protocol refGene,knownGene,ensGene,clinvar_20180603,avsnp150,cosmic89_coding,gnomad_exome,dbnsfp35c,exac03 \
	-operation g,g,g,f,f,f,f,f,f \
	-nastring . \
	-vcfinput
```

`Rscript --vanilla annovar_to_maf.r '$sample'`

# Improvements

- Arguments to allow filepath, output
- Dependencies

# Output

`sample.maf`

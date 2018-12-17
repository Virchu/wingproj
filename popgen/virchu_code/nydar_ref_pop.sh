# Run populations to create Structure file
# Sample list includes only 93 Brazil samples, all in 3,4 or 7 populations
# Biome, p=3, popmap-ny_93_popmap_bio.txt
# State, p=4, popmap-ny_93_popmap_state.txt
# Locality, p=7, popmap-ny_93_popmap_loc.txt

basedir=~
stacksdir=$basedir/adar_stacks4/stacks_rx_brazil_16
outdir=$stacksdir/new93_ref_pops_r0.75_p4

populations -P $stacksdir -M $basedir/popmaps/popmap-ny_93_popmap_state.txt -O $outdir \
        -t 20 -p 4 -r 0.75 --write_single_snp --hwe --fstats --vcf --genepop --structure

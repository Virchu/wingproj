# Run populations to create Structure file
# Sample list includes only 95 Brazil samples, all in 1 population

basedir=~
stacksdir=$basedir/adar_stacks4/stacks_rx_brazil_16
outdir=$stacksdir/populations_r0.75_try1

populations -b 1 -P $stacksdir -M $basedir/adar_popmap_bypop.txt -O $outdir \
        -t 20 -r 0.75 --write_single_snp --structure
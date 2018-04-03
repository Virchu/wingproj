# Run rxstacks genotype correction module, then re-run cstacks and sstacks
# Includes creation of MySQL database
#.tsv from after stacks?

projectdir=~/
stacksdir=$projectdir/adar_stacks4
rxdir=$stacksdir/stacks_rx_brazil_16
dbname="adar_lifehist_2016_rx"
files=$(cat $projectdir/adar_popmap_bypop.txt)

mkdir $rxdir

mysql -e 'CREATE DATABASE adar_lifehist_2016_rx'
mysql $dbname < /usr/local/share/stacks/sql/stacks.sql

rxstacks -b 1 -P $stacksdir -o $rxdir -t 20 --conf_lim 0.25 --prune_haplo --model_type bounded --bound_high 0.1 \
  --lnl_lim -200.0 --lnl_dist --verbose

samp=""
for f in $files
do
 samp+="-s $rxdir/$f ";
done

cstacks -b 1 -g -p 20 -o $rxdir &>> $rxdir/cstackslog $samp

for f in $files
do
 sstacks -g -p 20 -b 1 -c $rxdir/batch_1 \
 -s $rxdir/${f} \
 -o $rxdir/ &>> $rxdir/sstackslog
done

load_radtags.pl -D $dbname -p $rxdir -b 1 -M $projectdir/adar_popmap_bypop.txt \
	-c -B -e 'An darlingi Brazil pops/ after rxstacks lnl_lim -200'
index_radtags.pl -D $dbname -c -t

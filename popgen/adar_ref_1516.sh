#!/bin/bash
#new one
#ref_map.pl -o adar_stacks3 -O adar_popmap_bypop.txt -T 30 -b 1 --samples adar_alignments -B Adar_S2_radtags -D stacks_default --create_db 

# Run ref_map.pl, creating a MYSQL database to allow use of the Stacks web browser
# Sample list includes all 2016 Brazilian samples, all 2015 PLoS Brazilian samples

basedir=~
aligndir=~/analysis2/adar_alignments
stacksdir=~/adar_stacks4
dbname="adar_lifehist2016_plos2015"

mkdir $stacksdir

ref_map.pl -b 1 -o $stacksdir -T 20 -O $basedir/adar_popmap_bypop.txt -m 5 -B $dbname \
           --samples $aligndir -D 'An darlingi life history Brazil 2016' \
           --overw_db \

index_radtags.pl -c -t -D $dbname

#!/bin/bash
#new one
#ref_map.pl -o adar_stacks3 -O adar_popmap_bypop.txt -T 30 -b 1 --samples adar_alignments -B Adar_S2_radtags -D stacks_default --create_db 

# Run ref_map.pl, creating a MYSQL database to allow use of the Stacks web browser
# Sample list includes all 2016 Brazilian samples and PLos 2015 samples, n=

basedir=~
aligndir=~/comb2015_2016
stacksdir=~/adar_combo_stacks
dbname="adar_combo_2015_16_radtags"

mkdir $stacksdir

ref_map.pl -b 1 -o $stacksdir -T 20 -O $basedir/popmaps/combo_popmap1.txt -m 5 -B $dbname \
           --samples $aligndir -D 'An darlingi life history Brazil 2015_2016' \
           --overw_db \

index_radtags.pl -c -t -D $dbname


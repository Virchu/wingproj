#!/bin/bash

projectdir=~
inputdir=/illumina/Adarlingi_Chu_2017
dbname="Adar_scaffolds"
dbdir=/genomes/Adarlingi/Adar_scaffolds
outputdir=/home/vchu/adar_rad_alignments
mkdir $outputdir
touch $outputdir/align.log
for f in $inputdir/*.fastq.gz
do
    echo "Processing: $f" >> $outputdir/align.log
    filename=$(basename $f | sed 's/\.fastq.gz//')
    gsnap -t 60 -n 1 -m 10 -i 2 --min-coverage=0.95 --quiet-if-excessive  -A sam -d $dbname -D $dbdir --gunzip $f > $outputdir/$filename.sam 2>>$outputdir/align.log
done

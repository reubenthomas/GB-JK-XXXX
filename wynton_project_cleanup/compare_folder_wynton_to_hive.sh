#!/bin/bash
#$ -cwd
#$ -pe smp 1
#$ -o /wynton/group/gladstone/biocore/projects/logs/
#$ -e /wynton/group/gladstone/biocore/projects/logs/
#$ -l mem_free=2G
#$ -l scratch=50G
#$ -l h_rt=08:00:00


folder=$(basename $1)
dir=/wynton/group/gladstone/biocore/projects/$folder
diff -r $dir /gladstone/bioinformatics/projects/$folder > diff_$folder.txt



## End-of-job summary, if running as a job
[[ -n "$JOB_ID" ]] && qstat -j "$JOB_ID"          # This is useful for debugging and usage purposes,
                                                  # e.g. "did my job exceed its memory request?"
#find $dir -type f | wc -l
#find /gladstone/bioinformatics/projects/$dir -type f | wc -l

#diff -r $dir /gladstone/bioinformatics/projects/$dir
#
#rm -rf $dir


#dir=GB-LB-1174
#diff -r $dir /gladstone/bioinformatics/projects/$dir > diff_$dir.txt



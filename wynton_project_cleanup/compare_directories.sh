while read i
do
  echo "$i"
  jobPrefix=compare-folders-
  f=$(basename $i)
  jobName=${jobPrefix}$f
  qsub -N $jobName compare_folder_wynton_to_hive.sh $i
done < /wynton/group/gladstone/biocore/projects/check_directories.txt

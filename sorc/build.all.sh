#!/bin/sh
echo "`date`   `pwd`   $0 $*   begin"
# 20190917 RLW allow selective build by argument list
dtg=`date +%Y%m%d%H%M%S`
logfile=`basename $0`.log
logfiled=$logfile.$dtg
if [[ -f $logfile ]]; then
  rm $logfile
fi
(
echo "`date`   `pwd`   $0 $*   begin log"
pwd
dirsaved=`pwd`

module purge
module use ./
module load Module_gefs_legacy_v10.5.0

echo "`date`   build.sh $*   before"
sh build.sh $*
echo "`date`   build.sh $*   after"

echo "`date`   `pwd`   $0 $*   end of log"
) 2>&1 | tee $logfiled
ln $logfiled $logfile
echo "`date`   `pwd`   $0 $*   end"
exit

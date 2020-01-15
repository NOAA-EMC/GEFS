#!/bin/bash
#
# run and log the build.fcst.sh script
#
echo "`date`   `pwd`   $0 $*   begin"
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

echo "`date`   build.fcst.sh $*   before"
sh build.fcst.sh $*
echo "`date`   build.fcst.sh $*   after"

echo "`date`   `pwd`   $0 $*   end of log"
) 2>&1 | tee $logfiled
ln $logfiled $logfile

echo "`date`   `pwd`   $0 $*   end"

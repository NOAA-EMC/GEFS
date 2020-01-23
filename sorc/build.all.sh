#!/bin/sh
set -ex

echo "`date`   `pwd`   $0 $*   begin"
# 20190917 RLW allow selective build by argument list
dtg=`date +%Y%m%d%H%M%S`
logfile=`basename $0`.log
logfiled=$logfile.$dtg
if [[ -f $logfile ]]; then
  rm $logfile
fi


if [ ! -d "../exec" ]; then
    echo "Creating ../exec folder"
    mkdir ../exec
fi
# Check final exec folder exists in util folder
if [ ! -d "../util/exec" ]; then
  echo "Creating ../util/exec folder"
  mkdir ../util/exec
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

echo ".....Compiling esmf.3.1.0rp5 ..."
cd ${dirsaved}/esmf.3_1_0rp5/src
./makeit
cd ${dirsaved}

echo ".....Installing..."
./install.sh

echo ".....Compiling ens-tracker..."
cd ${dirsaved}/../sorc.tracker
module use .
module load Module_ens_tracker.v1.1.15_for_Dell
./build.sh
cd ${dirsaved}


echo "`date`   `pwd`   $0 $*   end of log"
) 2>&1 | tee $logfiled
ln $logfiled $logfile
echo "`date`   `pwd`   $0 $*   end"
exit

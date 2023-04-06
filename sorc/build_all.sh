#!/bin/sh
set -eux

build_dir=`pwd`
logs_dir=$build_dir/logs
if [ ! -d $logs_dir  ]; then
  echo "Creating logs folder"
  mkdir $logs_dir
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  echo "Creating ../exec folder"
  mkdir ../exec
fi

#------------------------------------
# INCLUDE PARTIAL BUILD 
#------------------------------------

. ./partial_build.sh

#------------------------------------
# build global_ensadd
#------------------------------------
$Build_global_ensadd && {
echo " .... Building global_ensadd - 01 .... "
./build_global_ensadd.sh > $logs_dir/build_global_ensadd.log 2>&1
}

#------------------------------------
# build gefs_ensstat
#------------------------------------
$Build_gefs_ensstat && {
echo " .... Building gefs_ensstat - 02 .... "
./build_gefs_ensstat.sh > $logs_dir/build_gefs_ensstat.log 2>&1
}

#------------------------------------
# build global_ensppf
#------------------------------------
$Build_global_ensppf && {
echo " .... Building global_ensppf - 03 .... "
./build_global_ensppf.sh > $logs_dir/build_global_ensppf.log 2>&1
}

#------------------------------------
# build wave_stat
#------------------------------------
$Build_wave_stat && {
echo " .... Building wave_stat - 04 .... "
./build_wave_stat.sh > $logs_dir/build_wave_stat.log 2>&1
}

#------------------------------------
# build gefs_nemsio2nc
#------------------------------------
$Build_gefs_nemsio2nc && {
echo " .... Building gefs_nemsio2nc - 05 .... "
./build_gefs_nemsio2nc.sh > $logs_dir/build_gefs_nemsio2nc.log 2>&1
}

#------------------------------------
# build global-workflow
#------------------------------------
if [[ -d global-workflow.fd ]] ; then
    if [[ -L global-workflow.fd ]] ; then
        echo " ... You don't need to build global-workflow because global-workflow.fd was linked from other directiory!"
    else
        echo " .... Building global-workflow .... "
        ./build_global-workflow.sh > $logs_dir/build_global-workflow.log 2>&1
    fi
fi

echo;echo " .... Build system finished .... "

exit 0


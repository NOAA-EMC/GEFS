#!/bin/sh
set -x

#########################################################################################
# 02/09/17 Wen.Meng@noaa.gov: Set up this script for building all executable used in GEFS
#########################################################################################

target=${1:-wcoss}    ;#wcoss

pwd=`pwd`

if [ ! -d "../exec" ]; then 
  echo "Creating ../exec folder"
  mkdir ../exec
fi
if [ ! -d "../util/exec" ]; then
  echo "Creating ../util/exec folder"
  mkdir ../util/exec
fi

cd $pwd
module purge
module use ./
if [[ -L /usrx && "$( readlink /usrx 2> /dev/null )" =~ dell ]] ; then # We are on NOAA Mars or Venus
    module load Module_gefs_v11.2.0_dell3
else
    module load Module_gefs_v11.2.0
fi

sh build.sh
sh install.sh

if [[ -d /dcom && -d /hwrf ]] ; then # Tide or Gyre
    cd ../util/sorc
    module purge
    module use ./
    ./build_nems_util.sh
fi

cd $pwd

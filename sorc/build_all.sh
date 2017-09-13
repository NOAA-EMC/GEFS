#!/bin/sh
set -x

#########################################################################################
# 02/09/17 Wen.Meng@noaa.gov: Set up this script for building all executable used in GEFS
#########################################################################################

target=${1:-wcoss}    ;#wcoss

pwd=`pwd`

cd $pwd
module purge
module use ./
module load Module_gefs_v11.2.0
sh build.sh
sh install.sh

cd ../util/sorc
module purge
module use ./
./build_nems_util.sh
cd $pwd

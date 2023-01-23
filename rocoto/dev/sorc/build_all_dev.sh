#!/bin/sh
set -ex

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

# Check final exec folder exists in util folder
if [ ! -d "../util/exec" ]; then
  echo "Creating ../util/exec folder"
  mkdir ../util/exec
fi

#------------------------------------
# INCLUDE PARTIAL BUILD 
#------------------------------------

. ./partial_build_dev.sh

#------------------------------------
# build global_enspqpf
#------------------------------------
$Build_global_enspqpf && {
echo " .... Building global_enspqpf - 01 .... "
./build_global_enspqpf.sh > $logs_dir/build_global_enspqpf.log 2>&1
}


#------------------------------------
# build global_enscvprcp
#------------------------------------
$Build_global_enscvprcp && {
echo " .... Building global_enscvprcp - 02 .... "
./build_global_enscvprcp.sh > $logs_dir/build_global_enscvprcp.log 2>&1
}

#------------------------------------
# build global_enspvrfy
#------------------------------------
$Build_global_enspvrfy && {
echo " .... Building global_enspvrfy - 03 .... "
./build_global_enspvrfy.sh > $logs_dir/build_global_enspvrfy.log 2>&1
}

#------------------------------------
# build global_enssrbias
#------------------------------------
$Build_global_enssrbias && {
echo " .... Building global_enssrbias - 04 .... "
./build_global_enssrbias.sh > $logs_dir/build_global_enssrbias.log 2>&1
}

#------------------------------------
# build global_enscqpf
#------------------------------------
$Build_global_enscqpf && {
echo " .... Building global_enscqpf - 05 .... "
./build_global_enscqpf.sh > $logs_dir/build_global_enscqpf.log 2>&1
}

#------------------------------------
# build global_enscvt24h
#------------------------------------
$Build_global_enscvt24h && {
echo " .... Building global_enscvt24h - 06 .... "
./build_global_enscvt24h.sh > $logs_dir/build_global_enscvt24h.log 2>&1
}

#------------------------------------
# build global_ensrfmat
#------------------------------------
$Build_global_ensrfmat && {
echo " .... Building global_ensrfmat - 07 .... "
./build_global_ensrfmat.sh > $logs_dir/build_global_ensrfmat.log 2>&1
}

#------------------------------------
# build overenstr_grib
#------------------------------------
$Build_overenstr_grib && {
echo " .... Building overenstr_grib - 08 .... "
./build_overenstr_grib.sh > $logs_dir/build_overenstr_grib.log 2>&1
}

#------------------------------------
# build global-workflow
#------------------------------------
#if [[ -d global-workflow.fd ]] ; then
#    if [[ -L global-workflow.fd ]] ; then
#        echo " ... You don't need to build global-workflow because global-workflow.fd was linked from other directiory!"
#    else
#        echo " .... Building global-workflow .... "
#        ./build_global-workflow.sh > $logs_dir/build_global-workflow.log 2>&1
#    fi
#fi

echo;echo " .... Build dev system finished .... "

exit 0


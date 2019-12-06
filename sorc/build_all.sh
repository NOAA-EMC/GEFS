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

. ./partial_build.sh

#------------------------------------
# build gefs_anom2_fcst - 01/02 For SST
#------------------------------------
$Build_gefs_anom2_fcst && {
echo " .... Building gefs_anom2_fcst - 01 .... "
./build_gefs_anom2_fcst.sh > $logs_dir/build_gefs_anom2_fcst.log 2>&1
}

#------------------------------------
# build gefs_nstgen     - 02/02 For SST      
#------------------------------------
$Build_gefs_nstgen && {
echo " .... Building gefs_nstgen - 02.... "
./build_gefs_nstgen.sh > $logs_dir/build_gefs_nstgen.log 2>&1
}

#------------------------------------
# build gefs_vortex_separate
#------------------------------------
$Build_gefs_vortex_separate && {
echo " .... Building gefs_vortex_separate - 03 .... "
./build_gefs_vortex_separate.sh > $logs_dir/build_gefs_vortex_separate.log 2>&1
}

#------------------------------------
# build gefs_vortex_combine
#------------------------------------
$Build_gefs_vortex_combine && {
echo " .... Building gefs_vortex_combine - 04 .... "
./build_gefs_vortex_combine.sh > $logs_dir/build_gefs_vortex_combine.log 2>&1
}

#------------------------------------
# build global_sigzvd
#------------------------------------
$Build_global_sigzvd && {
echo " .... Building global_sigzvd - 05 .... "
./build_global_sigzvd.sh > $logs_dir/build_global_sigzvd.log 2>&1
}

#------------------------------------
# build global_ensadd
#------------------------------------
$Build_global_ensadd && {
echo " .... Building global_ensadd - 06 .... "
./build_global_ensadd.sh > $logs_dir/build_global_ensadd.log 2>&1
}

#------------------------------------
# build global_enspqpf
#------------------------------------
$Build_global_enspqpf && {
echo " .... Building global_enspqpf - 07 .... "
./build_global_enspqpf.sh > $logs_dir/build_global_enspqpf.log 2>&1
}

#------------------------------------
# build gefs_ensstat
#------------------------------------
$Build_gefs_ensstat && {
echo " .... Building gefs_ensstat - 08 .... "
./build_gefs_ensstat.sh > $logs_dir/build_gefs_ensstat.log 2>&1
}

#------------------------------------
# build global_ensppf
#------------------------------------
$Build_global_ensppf && {
echo " .... Building global_ensppf - 09 .... "
./build_global_ensppf.sh > $logs_dir/build_global_ensppf.log 2>&1
}

#------------------------------------
# build global_enscvprcp
#------------------------------------
$Build_global_enscvprcp && {
echo " .... Building global_enscvprcp - 10 .... "
./build_global_enscvprcp.sh > $logs_dir/build_global_enscvprcp.log 2>&1
}

#------------------------------------
# build global_enspvrfy
#------------------------------------
$Build_global_enspvrfy && {
echo " .... Building global_enspvrfy - 11 .... "
./build_global_enspvrfy.sh > $logs_dir/build_global_enspvrfy.log 2>&1
}

#------------------------------------
# build global_enssrbias
#------------------------------------
$Build_global_enssrbias && {
echo " .... Building global_enssrbias - 12 .... "
./build_global_enssrbias.sh > $logs_dir/build_global_enssrbias.log 2>&1
}

#------------------------------------
# build global_enscqpf
#------------------------------------
$Build_global_enscqpf && {
echo " .... Building global_enscqpf - 13 .... "
./build_global_enscqpf.sh > $logs_dir/build_global_enscqpf.log 2>&1
}

#------------------------------------
# build global_enscvt24h
#------------------------------------
$Build_global_enscvt24h && {
echo " .... Building global_enscvt24h - 14 .... "
./build_global_enscvt24h.sh > $logs_dir/build_global_enscvt24h.log 2>&1
}

#------------------------------------
# build global_ensrfmat
#------------------------------------
$Build_global_ensrfmat && {
echo " .... Building global_ensrfmat - 15 .... "
./build_global_ensrfmat.sh > $logs_dir/build_global_ensrfmat.log 2>&1
}

#------------------------------------
# build gettrk 
#------------------------------------
$Build_gettrk && {
echo " .... Building gettrk - 16 .... "
./build_gettrk.sh > $logs_dir/build_gettrk.log 2>&1
}


#------------------------------------
# build overenstr_grib
#------------------------------------
$Build_overenstr_grib && {
echo " .... Building overenstr_grib - 17 .... "
./build_overenstr_grib.sh > $logs_dir/build_overenstr_grib.log 2>&1
}

#------------------------------------
# build getnsttf
#------------------------------------
$Build_getnsttf && {
echo " .... Building getnsttf - 18 .... "
./build_getnsttf.sh > $logs_dir/build_getnsttf.log 2>&1
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


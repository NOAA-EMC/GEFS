#!/bin/sh
set -ex
#------------------------------------
# USER DEFINED STUFF:
#
# USE_PREINST_LIBS: set to "true" to use preinstalled libraries.
#                   Anything other than "true"  will use libraries locally.
#------------------------------------

export USE_PREINST_LIBS="true"

#------------------------------------
# END USER DEFINED STUFF
#------------------------------------

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
#./build_global_enspqpf.sh > $logs_dir/build_global_enspqpf.log 2>&1
}

#------------------------------------
# build gefs_ensstat
#------------------------------------
$Build_gefs_ensstat && {
echo " .... Building gefs_ensstat - 08 .... "
#./build_gefs_ensstat.sh > $logs_dir/build_gefs_ensstat.log 2>&1
}

#------------------------------------
# build global_ensppf
#------------------------------------
$Build_global_ensppf && {
echo " .... Building global_ensppf - 09 .... "
#./build_global_ensppf.sh > $logs_dir/build_global_ensppf.log 2>&1
}

#------------------------------------
# build global_enscvprcp
#------------------------------------
$Build_global_enscvprcp && {
echo " .... Building global_enscvprcp - 10 .... "
#./build_global_enscvprcp.sh > $logs_dir/build_global_enscvprcp.log 2>&1
}

#------------------------------------
# build global_enspvrfy
#------------------------------------
$Build_global_enspvrfy && {
echo " .... Building global_enspvrfy - 11 .... "
#./build_global_enspvrfy.sh > $logs_dir/build_global_enspvrfy.log 2>&1
}

#------------------------------------
# build global_enssrbias
#------------------------------------
$Build_global_enssrbias && {
echo " .... Building global_enssrbias - 12 .... "
#./build_global_enssrbias.sh > $logs_dir/build_global_enssrbias.log 2>&1
}

#------------------------------------
# build global_enscqpf
#------------------------------------
$Build_global_enscqpf && {
echo " .... Building global_enscqpf - 13 .... "
#./build_global_enscqpf.sh > $logs_dir/build_global_enscqpf.log 2>&1
}

#------------------------------------
# build global_enscvt24h
#------------------------------------
$Build_global_enscvt24h && {
echo " .... Building global_enscvt24h - 14 .... "
#./build_global_enscvt24h.sh > $logs_dir/build_global_enscvt24h.log 2>&1
}

#------------------------------------
# build global_ensrfmat
#------------------------------------
$Build_global_ensrfmat && {
echo " .... Building global_ensrfmat - 15 .... "
#./build_global_ensrfmat.sh > $logs_dir/build_global_ensrfmat.log 2>&1
}

#------------------------------------
# build gettrk 
#------------------------------------
$Build_gettrk && {
echo " .... Building gettrk - 16 .... "
#./build_gettrk.sh > $logs_dir/build_gettrk.log 2>&1
}


#------------------------------------
# build overenstr_grib
#------------------------------------
$Build_overenstr_grib && {
echo " .... Building overenstr_grib - 17 .... "
#./build_overenstr_grib.sh > $logs_dir/build_overenstr_grib.log 2>&1
}

#------------------------------------
# build getnsttf
#------------------------------------
$Build_getnsttf && {
echo " .... Building getnsttf - 18 .... "
#./build_getnsttf.sh > $logs_dir/build_getnsttf.log 2>&1
}

exit

#------------------------------------
# build libraries first
#------------------------------------
$Build_libs && {
echo " .... Library build not currently supported .... "
#echo " .... Building libraries .... "
#./build_libs.sh > $logs_dir/build_libs.log 2>&1
}

#------------------------------------
# build fv3
#------------------------------------
$Build_fv3gfs && {
echo " .... Building fv3 .... "
./build_fv3.sh > $logs_dir/build_fv3.log 2>&1
}

#------------------------------------
# build gsi
#------------------------------------
$Build_gsi && {
echo " .... Building gsi .... "
./build_gsi.sh > $logs_dir/build_gsi.log 2>&1
}

#------------------------------------
# build ncep_post
#------------------------------------
$Build_ncep_post && {
echo " .... Building ncep_post .... "
./build_ncep_post.sh > $logs_dir/build_ncep_post.log 2>&1
}

#------------------------------------
# build gfs_wafs 
#------------------------------------
#$Build_gfs_wafs  && {
#echo " .... Building gfs_wafs  .... "
#./build_gfs_wafs.sh > $logs_dir/build_gfs_wafs .log 2>&1
#}

#------------------------------------
# build NEMS util
#------------------------------------
$Build_nems_util && {
echo " .... Building NEMS util .... "
./build_nems_util.sh > $logs_dir/build_NEMS.log 2>&1
}

#------------------------------------
# build chgres
#------------------------------------
$Build_chgres && {
echo " .... Building chgres .... "
./build_chgres.sh > $logs_dir/build_chgres.log 2>&1
}

#------------------------------------
# build sfcanl_nsttfchg 
#------------------------------------
$Build_sfcanl_nsttfchg && {
echo " .... Building gaussian_sfcanl and nst_tf_chg .... "
./build_sfcanl_nsttfchg.sh > $logs_dir/build_sfcanl_nsttfchg.log 2>&1
}

#------------------------------------
# build orog
#------------------------------------
$Build_orog && {
echo " .... Building orog .... "
./build_orog.sh > $logs_dir/build_orog.log 2>&1
}

#------------------------------------
# build cycle 
#------------------------------------
$Build_cycle && {
echo " .... Building cycle .... "
./build_cycle.sh > $logs_dir/build_cycle.log 2>&1
}

#------------------------------------
# build enkf_chgres_recenter
#------------------------------------
$Build_enkf_chgres_recenter && {
echo " .... Building enkf_chgres_recenter .... "
./build_enkf_chgres_recenter.sh > $logs_dir/build_enkf_chgres_recenter.log 2>&1
}

#------------------------------------
# build tropcy_NEMS
#------------------------------------
$Build_tropcy && {
echo " .... Building tropcy_NEMS .... "
./build_tropcy_NEMS.sh > $logs_dir/build_tropcy_NEMS.log 2>&1
}

#------------------------------------
# build gdas
#------------------------------------
$Build_gdas && {
echo " .... Building gdas .... "
./build_gdas.sh > $logs_dir/build_gdas.log 2>&1
}

#------------------------------------
# build gfs_fbwndgfs
#------------------------------------
$Build_gfs_fbwndgfs && {
echo " .... Building gfs_fbwndgfs .... "
./build_gfs_fbwndgfs.sh > $logs_dir/build_gfs_fbwndgfs.log 2>&1
}

#------------------------------------
# build gfs_overpdtg2
#------------------------------------
$Build_gfs_overpdtg2 && {
echo " .... Building gfs_overpdtg2 .... "
./build_gfs_overpdtg2.sh > $logs_dir/build_gfs_overpdtg2.log 2>&1
}

#------------------------------------
# build gfs_wintemv
#------------------------------------
$Build_gfs_wintemv && {
echo " .... Building gfs_wintemv .... "
./build_gfs_wintemv.sh > $logs_dir/build_gfs_wintemv.log 2>&1
}

#------------------------------------
# build gfs_bufrsnd
#------------------------------------
$Build_gfs_bufrsnd && {
echo " .... Building gfs_bufrsnd .... "
./build_gfs_bufrsnd.sh > $logs_dir/build_gfs_bufrsnd.log 2>&1
}

#------------------------------------
# build emcsfc
#------------------------------------
$Build_emcsfc && {
echo " .... Building emcsfc .... "
./build_emcsfc.sh > $logs_dir/build_emcsfc.log 2>&1
}

#------------------------------------
# build fre-nctools
#------------------------------------
$Build_nctools && {
echo " .... Building fre-nctools .... "
./build_fre-nctools.sh > $logs_dir/build_fre-nctools.log 2>&1
}

#------------------------------------
# build fv3nc2nemsio
#------------------------------------
$Build_fv3nc2nemsio && {
echo " .... Building fv3nc2nemsio .... "
./build_fv3nc2nemsio.sh > $logs_dir/build_fv3nc2nemsio.log 2>&1
}

#------------------------------------
# build regrid_nemsio
#------------------------------------
$Build_regrid_nemsio && {
echo " .... Building regrid_nemsio .... "
./build_regrid_nemsio.sh > $logs_dir/build_regrid_nemsio.log 2>&1
}

#------------------------------------
# build gfs_util       
#------------------------------------
$Build_gfs_util && {
echo " .... Building gfs_util .... "
./build_gfs_util.sh > $logs_dir/build_gfs_util.log 2>&1
}

#------------------------------------
# build prod_util
#------------------------------------
$Build_prod_util && {
echo " .... prod_util build not currently supported .... "
#echo " .... Building prod_util .... "
#./build_prod_util.sh > $logs_dir/build_prod_util.log 2>&1
}

#------------------------------------
# build grib_util
#------------------------------------
$Build_grib_util && {
echo " .... grib_util build not currently supported .... "
#echo " .... Building grib_util .... "
#./build_grib_util.sh > $logs_dir/build_grib_util.log 2>&1
}

echo;echo " .... Build system finished .... "

exit 0

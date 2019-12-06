#!/bin/ksh

#

# EXPORT list here
set -x

#export IOBUF_PARAMS=
ulimit -s unlimited
ulimit -a

#export OMP_NUM_THREADS=${GEFS_TPP:-2}
export POSTGRB2TBL=$G2TMPL_SRC/params_grib2_tbl_new

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_NCEPPOST

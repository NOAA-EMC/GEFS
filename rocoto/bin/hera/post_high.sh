#!/bin/ksh
#

# EXPORT list here
set -x
export IOBUF_PARAMS=

ulimit -s unlimited
#ulimit -s 10999888
ulimit -a

export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:2

#export NODES=3
#export total_tasks=36
#export OMP_NUM_THREADS=2
#export taskspernode=12
export POSTGRB2TBL=$G2TMPL_SRC/params_grib2_tbl_new

export ERRSCRIPT=" "

#export FORECAST_SEGMENT=hr

# export for development runs only begin

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_NCEPPOST

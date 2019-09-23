#!/bin/ksh
#

# EXPORT list here
set -x
export IOBUF_PARAMS=

ulimit -s unlimited
ulimit -a

export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:2

export NTHREADS_SIGCHGRS=2

export FORECAST_SEGMENT=hr

# export memdir_template='$ROTDIR/enkf.$CDUMP.$PDY/$cyc'

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_FORECAST_FV3


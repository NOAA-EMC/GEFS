#!/bin/ksh
#

# EXPORT list here
set -x
export IOBUF_PARAMS=

ulimit -s unlimited
ulimit -a

export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:2

export POSTGRB2TBL=$G2TMPL_SRC/params_grib2_tbl_new

export ERRSCRIPT=" "

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ATMOS_POST

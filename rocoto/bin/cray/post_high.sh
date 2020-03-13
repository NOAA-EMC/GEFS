#!/bin/ksh
#

# EXPORT list here
set -x

ulimit -s unlimited
ulimit -a

export POSTGRB2TBL=$G2TMPL_SRC/params_grib2_tbl_new

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_NCEPPOST

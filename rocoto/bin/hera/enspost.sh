#!/bin/ksh

# EXPORT list here
set -x
export IOBUF_PARAMS=*pgrb2*:size=64M:count=4:verbose,*enspost_grb2*:size=64M:count=4:verbose,*ensstat_grb2*:size=64M:count=4:verbose,*pq?f*:size=64M:count=4:verbose


# export for development runs only begin

ulimit -s unlimited

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ATMOS_ENSPOST


#!/bin/ksh
#

set -x

ulimit -s unlimited
ulimit -a

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ATMOS_POST_CLEANUP


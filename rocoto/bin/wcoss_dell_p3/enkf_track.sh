#!/bin/ksh

# EXPORT list here
set -x
ulimit -s unlimited
ulimit -a

export KMP_AFFINITY=disabled

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ENKF_TRACK

#!/bin/bash

# EXPORT list here
set -x

ulimit -s unlimited
ulimit -a

# Set NCO messaging proxies
#export jlogfile=/dev/null
#export jobid=${job}.$$

$SOURCEDIR/jobs/JWAVE_GWES_POST

#!/bin/bash

ulimit -s unlimited
ulimit -a

#export SENDDBN=NO
#export SENDCOM=YES

#export KMP_STACKSIZE=1024m
#export KMP_AFFINITY=disabled

#export ATP_ENABLED=0
#export MALLOC_MMAP_MAX_=0
#export MALLOC_TRIM_THRESHOLD_=134217728

#export FORT_BUFFERED=TRUE
#export MKL_CBWR=AVX

#export MPICH_ABORT_ON_ERROR=1
#export MPICH_ENV_DISPLAY=1
#export MPICH_VERSION_DISPLAY=1
#export MPICH_CPUMASK_DISPLAY=1

#export MP_EUIDEVICE=sn_all
#export MP_EUILIB=us
#export NTASKS=24
# Set NCO messaging proxies
#export jlogfile=/dev/null
#export jobid=${job}.$$

#export wavempexec="aprun -b -j1 -cc depth -n"
#export wave_mpmd="cfp"

$SOURCEDIR/jobs/JWAVE_GWES_PREP

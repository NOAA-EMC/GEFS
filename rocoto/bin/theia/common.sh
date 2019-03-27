#!/bin/ksh

export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}

export FORT_BUFFERED=TRUE
export MKL_CBWR=AVX

export ATP_ENABLED=0
export MALLOC_MMAP_MAX_=0
export MALLOC_TRIM_THRESHOLD_=134217728
 
export MPICH_ABORT_ON_ERROR=1
export MPICH_ENV_DISPLAY=1
export MPICH_VERSION_DISPLAY=1
export MPICH_CPUMASK_DISPLAY=1
 
export MP_EUIDEVICE=sn_all
export MP_EUILIB=us

export KMP_STACKSIZE=1024m
export KMP_AFFINITY=disabled

export gefsmpexec="srun -n $SLURM_NTASKS"
export gefsmpexec_mpmd="srun -n $SLURM_NTASKS /scratch3/NCEPDEV/nwprod/util/exec/mpiserial"
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

export NTASKS=$SLURM_NTASKS

export gefsmpexec="srun -n $NTASKS"
export gefsmpexec_mpmd="srun -n $NTASKS /scratch3/NCEPDEV/nwprod/util/exec/mpiserial"
export wavempexec="srun -n" 
export wave_mpmd="/scratch4/NCEPDEV/nems/noscrub/emc.nemspara/soft/mpiserial/3.0/exec/mpiserial -m"

export errchk="eval if [[ \$err != 0 ]]; then exit \$err; fi"

export APRUNC="srun -n 1"
export APRUN_RECENT="srun -n $NTASKS"
export APRUN_CHGRES="srun -n 1"
export aprun_gec00="srun -n 1"
export APRUN_CALCINC="srun -n 1"

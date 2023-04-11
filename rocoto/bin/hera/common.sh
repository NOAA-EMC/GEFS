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

export total_tasks=${GEFS_NTASKS}
export OMP_NUM_THREADS=${GEFS_TPP}
export taskspernode=${GEFS_PPN}

export NTASKS=$SLURM_NTASKS

#\/ Only on Hera
export pid=${pid:-${SLURM_JOBID}}
export outid="LL$job"
export jobid="${outid}.o${pid}"
export pgmout="OUTPUT.${pid}"
export PBS_JOBID=${pid}
#/\ Only for Hera

export gefsmpexec="srun -n $total_tasks"
export gefsmpexec_mpmd="srun -n $total_tasks /scratch2/NCEPDEV/ensemble/noscrub/common/soft/mpiserial.cd/mpiserial"
export wavempexec="srun -n" 
export wave_mpmd="/scratch2/NCEPDEV/ensemble/noscrub/common/soft/mpiserial.cd/mpiserial" # -m"

#export errchk="eval if [[ \$err != 0 ]]; then exit \$err; fi"

#export APRUNC="srun -n 1"
#export APRUN_RECENT="srun -n $NTASKS"
export APRUN_CHGRES="srun -n 1"
#export aprun_gec00="srun -n 1"
export APRUN_CALCINC="srun -n 1"


. $GEFS_ROCOTO/parm/setbase
. $GEFS_ROCOTO/parm/gefs_config
. $GEFS_ROCOTO/parm/gefs_dev.parm


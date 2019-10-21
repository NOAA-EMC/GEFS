#!/bin/ksh
#

# EXPORT list here
set -x

ulimit -s unlimited
ulimit -a
#export MPICH_ABORT_ON_ERROR=1
#export MPICH_ENV_DISPLAY=1
#export MPICH_VERSION_DISPLAY=1
#export MPICH_CPUMASK_DISPLAY=1

#export KMP_STACKSIZE=1024m
#export KMP_AFFINITY=disabled

#export MP_EUIDEVICE=sn_all
#export MP_EUILIB=us
#export MP_SHARED_MEMORY=yes
#export MEMORY_AFFINITY=core:2

#export NODES=1
#export total_tasks=48
#export OMP_NUM_THREADS=2
#export taskspernode=12


# export for development runs only begin
export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}

. $GEFS_ROCOTO/parm/setbase
. $GEFS_ROCOTO/parm/gefs_config
. $GEFS_ROCOTO/parm/gefs_dev.parm

# CALL executable job script here
HPSS_DIR=$HPSS_DIR
WORKDIR=$WORKDIR
PDY=$PDY
cyc=$cyc
yyyy=${PDY:0:4}
mm=${PDY:4:2}
dd=${PDY:6:2}

destination_path=${HPSS_DIR}/${yyyy}/${yyyy}${mm}/${yyyy}${mm}${dd}
output_path=${WORKDIR}/com/gens/dev/gefs.${PDY}/${cyc}

hsi mkdir -p ${destination_path}

cd ${output_path}/pgrb2bp25
htar -cvf ${destination_path}/gefs.${PDY}_${cyc}.pgrb2bp25_c00_p01-p07.tar gec00* gep01* gep02* gep03* gep04* gep05* gep06* gep07*




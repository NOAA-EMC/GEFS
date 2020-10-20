#!/bin/ksh

RocotoGen=${RocotoGen:-0}

export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}

#export NTASKS=${GEFS_NTASKS}
export total_tasks=${GEFS_NTASKS}
export OMP_NUM_THREADS=${GEFS_TPP}
export taskspernode=${GEFS_PPN}

# Calculate the number of tasks based on the task geometry
#(( NTASKS=$(echo $LSB_PJL_TASK_GEOMETRY | grep -Po "\d+" | sort -n | tail -1) + 1 ))
#export NTASKS
if [[ $RocotoGen == 0 ]]; then
    export gefsmpexec="mpirun -n $total_tasks"
    export gefsmpexec_mpmd="mpirun -n $total_tasks cfp mpmd_cmdfile"
    export wavempexec="mpirun -n"
    export wave_mpmd="cfp"

    #export APRUNC="$gefsmpexec"
    #export APRUN_RECENT="$gefsmpexec"
    export APRUN_CHGRES="mpirun -n 1"
    #export aprun_gec00="mpirun -n 1"
    export APRUN_CALCINC="mpirun -n 1"

    . $GEFS_ROCOTO/parm/setbase
    . $GEFS_ROCOTO/parm/gefs_config
    . $GEFS_ROCOTO/parm/gefs_dev.parm

elif [[ $RocotoGen == 1 ]]; then
    export HOMEtrak=/gpfs/dell2/emc/verification/noscrub/emc.enspara/common/git/ens_tracker/ens_tracker.v2.1.2
    export archsyndir=/gpfs/dell2/emc/verification/noscrub/emc.enspara/common/git/ens_tracker/TCvitalData/syndat

	export STRICT="YES"

    export HOMEgefs=$SOURCEDIR

    export COMROOT=$WORKDIR/com
    export GESROOT=$WORKDIR/nwges
    export DATAROOT=$WORKDIR/tmpnwprd

    # export DCOMROOT=${DCOMROOT:-/dcom} # CQPF
    # export PCOMROOT=$WORKDIR/pcom/$envir $For WAFS

    export COMPATH=/gpfs/dell3/ptmp/emc.glopara/ROTDIRS/v16rt2/gfs/para

    ##export COMIN_WAV_ICE=/gpfs/dell1/nco/ops/com/omb/prod

    #export WRTTASK_PER_GROUP="36"
    #export parallel_threads="1"
    #export npe_wav="112"
    export SENDCOM=YES 
    #export cplwav=".true."
    #export warm_start=".false."
    export KEEPDATA=YES     # ecflow NO
    export SENDECF=NO       # ecflow YES
    export SENDDBN=YES       # ecflow YES
    export SENDDBN_NTC=NO   # ecflow YES

    #export npert=2
    #export navg_min=1 
    #export fhmax="840"
    #export fhrestart=24
    #export restart_interval_aer=12
    ### For keep and arhive data
    #export KEEP_DIR="/gpfs/dell6/emc/modeling/noscrub/emc.enspara/Xianwu.Xue/GEFS/GEFSv12_Test02"
    #export HPSS_DIR="/NCEPDEV/emc-ensemble/2year/emc.enspara/Xianwu.Xue/GEFS_DELL35/GEFSv12_Test02"
    export DIRS_TO_KEEP="bufr,ensstat,pgrb2sp25,pgrb2ap5,pgrb22p5,pgrb2p25,pgrb2bp5,cfssst,gempak,init"
    export DIRS_TO_ARCHIVE="bufr,ensstat,pgrb2sp25,pgrb2ap5,pgrb22p5,pgrb2p25,pgrb2bp5,cfssst,gempak,init"
    export DIRS_TO_KEEP_WAVE="gridded, station, restart, gempak"
    export DIRS_TO_ARCHIVE_WAVE="gridded, station, restart, gempak"
    export DIRS_TO_KEEP_CHEM="pgrb2ap50,pgrb2ap25,init,restart"
    export DIRS_TO_ARCHIVE_CHEM="pgrb2ap50,pgrb2ap25,init,restart"

    if [[ $SENDDBN == "YES" ]]; then
        export DBNROOT=${UTILROOT}/fakedbn
        export DBNLOG=YES
    fi
fi

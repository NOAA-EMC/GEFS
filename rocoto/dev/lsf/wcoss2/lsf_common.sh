#!/bin/sh

export PS4='$SECONDS + $(basename ${0}))[$LINENO] '

export cyc=00
#export PDY=20210731
export PDY=20210824

export npert=2
export COREPERNODE=128
export fhmax=840
export fhmaxh=384
export fhmax_aer=120
export FHMAXHF=240
export FHOUTHF=3
export FHOUTLF=6
export VERBOSE=no

export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}
export NET=${NET:-gefs}
export RUN=${RUN:-gefs}

export EXPID=gefs_test_wave_11
export HOMEgefs=${HOMEgefs:-/lfs/h2/emc/ens/noscrub/Xianwu.Xue/gw/test_gw_wave/$EXPID}
#export HOMEgefs=/lfs/h2/emc/ens/noscrub/common/git/sorc/gefs/gefs_port2wcoss2_common
#export HOMEgfs=/lfs/h2/emc/ens/noscrub/common/git/sorc/gw/gw_port2wcoss2_common
export WORKDIR=/lfs/h2/emc/ptmp/Xianwu.Xue/o/$EXPID

export GEFS_ROCOTO=${HOMEgefs}/rocoto

#export HOMEdata=/lfs/h2/emc/ens/noscrub/Xianwu.Xue/gefs/HOMEdata
export HOMEdata=/lfs/h1/ops

export COMPATH=$HOMEdata/canned/com/gfs:$HOMEdata/canned/com/cfs:${WORKDIR}/$envir/com/${NET}
export DCOMROOT=${HOMEdata}/canned/dcom

#===
export job=${job:-$PBS_JOBNAME}

export COMROOT=${WORKDIR}/$envir/com
export GESROOT=${WORKDIR}/nwges
export DATAROOT=${WORKDIR}/tmp

export SENDCOM=YES
export KEEPDATA=YES     # ecflow NO
export SENDECF=NO       # ecflow YES
export SENDDBN=NO       # ecflow YES
export SENDDBN_NTC=NO   # ecflow YES

test=${test:-"test2"}
if [[ $test == "test1" ]]; then
	export layout_x=6
	export layout_y=8
	export WRITE_GROUP=1
	export WRTTASK_PER_GROUP=32
	export npe_wav=${npe_wav:-160}

	export layout_x_chem=6
	export layout_y_chem=8
	export WRITE_GROUP_chem=${WRITE_GROUP_chem:-1}
	export WRTTASK_PER_GROUP_chem=${WRTTASK_PER_GROUP_chem:-42}

	export layout_x_lr=${layout_x_lr:-8}
	export layout_y_lr=${layout_y_lr:-8}
	export WRITE_GROUP_lr=${WRITE_GROUP_lr:-1}
	export WRTTASK_PER_GROUP_lr=${WRTTASK_PER_GROUP_lr:-32} #or 42, 48

fi
echo $layout_x


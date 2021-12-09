#!/bin/sh

export PS4='$SECONDS + $(basename ${0}))[$LINENO] '

export cyc=${cyc:-00}
#export PDY=20210731
export PDY=${PDY:-20210824}

export npert=${npert:-30}
export navg_min=${navg_min:-10}
export COREPERNODE=${COREPERNODE:-128}
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

export EXPID=${EXPID:-gefs_wcoss2_canned_Nov16_2021}
#export HOMEgefs=${HOMEgefs:-/lfs/h2/emc/ens/noscrub/Xianwu.Xue/gw/test_gw_wave/$EXPID}
export HOMEgefs=/lfs/h2/emc/ens/noscrub/common/git/gefs/gefs_wcoss2_canned_Nov29_2021
#export HOMEgfs=/lfs/h2/emc/ens/noscrub/common/git/sorc/gw/gw_port2wcoss2_common
export WORKDIR=/lfs/h2/emc/ptmp/Xianwu.Xue/o/$EXPID

export GEFS_ROCOTO=${HOMEgefs}/rocoto

#export HOMEdata=/lfs/h2/emc/ens/noscrub/Xianwu.Xue/GEFS/HOMEdata
export HOMEdata=/lfs/h1/ops

export COMPATH=$HOMEdata/canned/com/gfs:$HOMEdata/canned/com/cfs:$HOMEdata/canned/com/nawips:$HOMEdata/canned/com/ecmwf:$HOMEdata/canned/com/nam:${WORKDIR}/$envir/com/${NET}
export DCOMROOT=${HOMEdata}/canned/dcom

#===
export job=${job:-$PBS_JOBNAME}

export COMROOT=${WORKDIR}/$envir/com
export GESROOT=${WORKDIR}/nwges
export DATAROOT=${WORKDIR}/tmp

export SENDCOM=YES
export KEEPDATA=NO     # ecflow NO
export SENDECF=NO       # ecflow YES
export SENDDBN=NO       # ecflow YES
export SENDDBN_NTC=NO   # ecflow YES


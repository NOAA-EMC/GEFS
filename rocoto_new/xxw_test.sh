#!/bin/bash

set -x


export EXPID='WL_Master_GIT_3'
echo $EXPID
export GEFS_ROCOTO=`pwd`
echo $GEFS_ROCOTO

export SOURCEDIR=$(readlink -f $GEFS_ROCOTO/../)
echo $SOURCEDIR

#export WORKDIR=/gpfs/hps3/ptmp/Xianwu.Xue/o/$EXPID
export WORKDIR=/scratch4/NCEPDEV/stmp4/Xianwu.Xue/o/$EXPID
export basesource=$SOURCEDIR
export baseoutput=$WORKDIR

#exit

. $GEFS_ROCOTO/bin/gefs_load_modules.inc

export PDY=20170624
export cyc=00

bin/cray/jgefs_getcfssst.sh


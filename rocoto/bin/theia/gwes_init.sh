#!/bin/bash

# Set NCO messaging proxies
export jlogfile=/dev/null
export jobid=${job}.$$

export SENDDBN=NO
export SENDCOM=YES

# Set wave-specific COM
#export CODEwave=/marine/save/wavepa/ops/wave_code.v4.15.6/st4nc

# Temp directory
export DATAROOT=${DATAROOT:-/gpfs/hps3/stmp/wavepa}

module list

export DATA=$DATAROOT/${job}.${wave_multi_1_ver}
if [ -d $DATA ]; then
  rm -rf $DATA/*
else
  mkdir -p ${DATA}
fi

export PDY=20181116
export cyc=00

$SOURCEDIR/jobs/JWAVE_GWES_INIT

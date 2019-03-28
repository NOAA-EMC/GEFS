#!/bin/bash

# Set NCO messaging proxies
export jlogfile=/dev/null
export jobid=${job}.$$

export SENDDBN=NO
export SENDCOM=YES

# Set wave-specific COM
export CODEwave=/marine/save/wavepa/ops/wave_code.v4.15.6/st4nc

# Temp directory
export DATAROOT=/gpfs/hps3/stmp/wavepa

module purge
module load modules
source $MODULESHOME/etc/modules.sh
module load PrgEnv-intel
module load cfp-intel-sandybridge/2.0.1
module load iobuf/2.0.5
module load prod_util/1.0.31
module load prod_envir/1.0.1
module load grib_util/1.0.3
module load util_shared/1.0.5
module load HDF5/HDF5-serial-intel-sandybridge/1.8.9
module load NetCDF-intel-sandybridge/4.2

module list

export COMICE=$COMROOTp2/omb/prod

export DATA=$DATAROOT/${job}.${wave_multi_1_ver}
if [ -d $DATA ]; then
  rm -rf $DATA/*
fi

export PDY=20181116
export cyc=00

$SOURCEDIR/jobs/JWAVE_GWES_PREP

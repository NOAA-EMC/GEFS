# This is a KSH include file that should be sourced at the top of
# every job.  It initializes the environment for various clusters so
# that the rest of the GEFS or Rocoto can run properly.

if [[ "$WHERE_AM_I" == wcoss ]] ; then
#load modules
. /usrx/local/Modules/default/init/ksh
module use /nwprod2/modulefiles
module purge
module load ics
module load lsf
module load prod_envir
module load prod_util
module load util_shared/v1.0.0
module load grib_util/v1.0.1
module load ibmpe
export PATH=$PATH:.
elif [[ "$WHERE_AM_I" == cray ]] ; then
#load modules
. /opt/modules/default/init/ksh
module load PrgEnv-intel ESMF-intel-haswell/3_1_0rp5 cfp-intel-sandybridge iobuf craype-hugepages2M craype-haswell
module use /gpfs/hps/nco/ops/nwprod/modulefiles
module load prod_envir
module load prod_util
module load util_shared/1.0.3
module load grib_util/1.0.3
module load NetCDF-intel-haswell/4.2
module load nco-gnu-sandybridge/4.4.4
export PATH=$PATH:.
fi

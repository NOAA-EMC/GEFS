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
module load grib_util/1.0.5
module load NetCDF-intel-haswell/4.2
module load nco-gnu-sandybridge/4.4.4
export PATH=$PATH:.
elif [[ "$WHERE_AM_I" == wcoss_dell_p3 ]] ; then
#load modules
. /usrx/local/prod/lmod/lmod/init/ksh
module load ips/18.0.1.163
module load EnvVars/1.0.2
module use /gpfs/hps/nco/ops/nwprod/modulefiles
module load prod_envir/1.0.2
module load prod_util/1.1.0
#module load util_shared/1.0.8
module unload grib_util/1.0.6
module load dev/grib_util/1.1.0
module load lsf/10.1
module load impi/18.0.1
module load NetCDF/4.5.0
module load pm5/1.0
module load mktgs/1.0
module load g2tmpl/1.5.0
module load crtm/2.2.5
module load ESMF/7_1_0r
module load NCO/4.7.0
module load HDF5-serial/1.10.1
module load NetCDF/4.5.0
module load CFP/2.0.1
export USE_CFP=YES
export PATH=$PATH:.
fi




















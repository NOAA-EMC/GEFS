# This is a KSH include file that should be sourced at the top of
# every job.  It initializes the environment for various clusters so
# that the rest of the GEFS or Rocoto can run properly.

if [[ "$WHERE_AM_I" == cray ]] ; then
    echo "Loading modules for WCOSS cray"
    . /opt/modules/default/init/ksh
    module purge
    module use /opt/cray/ari/modulefiles
    module load modules/3.2.10.3
    module load ncep/1.0
    module load PrgEnv-intel ESMF-intel-haswell/3_1_0rp5 cfp-intel-sandybridge iobuf craype-hugepages2M craype-haswell
    module use /gpfs/hps/nco/ops/nwprod/modulefiles
    module load prod_envir
    module load prod_util
    module load util_shared/1.0.3
    module load gcc
    module load grib_util/1.1.0
    module load gempak/7.3.1
    module load hpss
    module load g2tmpl-intel/1.5.0
    module load python/3.6.3
    module load xt-lsfhpc
    # wave modules
    module load HDF5-serial-intel-sandybridge/1.8.9
    module load nco-gnu-sandybridge/4.4.4
    module load NetCDF-intel-sandybridge/4.2
    # module load NetCDF-intel-haswell/4.2
    module load nco-gnu-sandybridge/4.4.4
    # theend wave modules 
	export PATH=$PATH:.
    
    . $GEFS_ROCOTO/bin/cray/common.sh

elif [[ "$WHERE_AM_I" == hera ]]; then
    echo "Loading modules for hera"
    . /apps/lmod/lmod/init/ksh
    module purge
    module load contrib
    module load hpss
    module load intel/18.0.5.274
    module load impi/2018.0.4
    module load hdf5parallel/1.10.5
    module load netcdf/4.7.0
    module load nco/4.7.0
    module load hdf5/1.10.5
    module load intelpython/3.6.8

    module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
    module load prod_util/1.1.0
    module load g2tmpl/1.5.1
    module load grib_util/1.1.1

    module load gempak/7.4.2

    export PATH=$PATH:.

    # Temporary setting to test common settings
    . $GEFS_ROCOTO/bin/hera/common.sh

elif [[ "$WHERE_AM_I" == wcoss_dell_p3 ]] ; then
    echo "Loading modules for wcoss_dell_p3"
    . /usrx/local/prod/lmod/lmod/init/ksh
    module load EnvVars/1.0.2
    module load ips/18.0.1.163
    module load prod_envir/1.0.3
    module load grib_util/1.1.0
    module load prod_util/1.1.0
    module load HPSS/5.0.2.5
    module use -a /gpfs/dell1/nco/ops/nwpara/modulefiles/compiler_prod/ips/18.0.1
    module load util_shared/1.1.0
    module load g2tmpl/1.5.0
    
    module load gempak/7.3.1

    module load NetCDF/4.5.0
    module load NCO/4.7.0
    module load HDF5-serial/1.10.1
    module load impi/18.0.1
    module load CFP/2.0.2
    module load python/3.6.3
    export USE_CFP=YES
    export PATH=$PATH:.

    # Temporary setting to test common settings
    . $GEFS_ROCOTO/bin/wcoss_dell_p3/common.sh
fi

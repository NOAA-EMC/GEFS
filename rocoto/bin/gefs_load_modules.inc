# This is a KSH include file that should be sourced at the top of
# every job.  It initializes the environment for various clusters so
# that the rest of the GEFS or Rocoto can run properly.

if [[ "$WHERE_AM_I" == hera ]]; then
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

fi

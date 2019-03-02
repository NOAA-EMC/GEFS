# This is a KSH include file that should be sourced at the top of
# every job.  It initializes the environment for various clusters so
# that the rest of the GEFS or Rocoto can run properly.

if [[ "$WHERE_AM_I" == wcoss_ibm ]] ; then
	echo "Loading modules for WCOSS IBM"
	. /usrx/local/Modules/default/init/ksh
	module use /nwprod2/modulefiles
	module purge
	module load ics/17.0.3
	module load lsf
	module load prod_envir
	module load prod_util/v1.0.24
	module load util_shared/v1.0.7
	module load grib_util/v1.0.5
	module load ibmpe/1.3.0.12
	module load hpss
    module load g2tmpl/v1.5.0
	export PATH=$PATH:.
elif [[ "$WHERE_AM_I" == cray ]] ; then
	echo "Loading modules for WCOSS cray"
	. /opt/modules/default/init/ksh
	module load PrgEnv-intel ESMF-intel-haswell/3_1_0rp5 cfp-intel-sandybridge iobuf craype-hugepages2M craype-haswell
	module use /gpfs/hps/nco/ops/nwprod/modulefiles
	module load prod_envir
	module load prod_util
	module load util_shared/1.0.3
	module load grib_util/1.0.3
	module load hpss
    module load g2tmpl-intel/1.5.0
	export PATH=$PATH:.
elif [[ "$WHERE_AM_I" == theia ]]; then
	echo "Loading modules for theia"
	. /apps/lmod/lmod/init/ksh
	module use /scratch3/NCEPDEV/nwprod/lib/modulefiles
	module load intel impi/2018.0.1
	module load hdf5parallel/1.8.14
	module load netcdf
	module load wgrib
	module load wgrib2/0.2.0.6c
	module load cnvgrib
    module load prod_util/v1.1.0
    #module load grib_util/v1.1.0
    module load g2tmpl/v1.5.0
    export PATH=$PATH:./
elif [[ "$WHERE_AM_I" == jet ]]; then
    echo "Loading modules for jet"
    . /apps/lmod/lmod/init/ksh
    module use /apps/modules/modulefiles
    module use /apps/modules/modulefamilies/intel
    module load newdefaults intel/15.0.3.187 impi/5.1.3.181 szip hdf5 netcdf4/4.2.1.1
    module use /lfs3/projects/hfv3gfs/nwprod/lib/modulefiles
    module load bacio/v2.0.2
    module load sp/v2.0.2
    module load ip/v2.0.0
    module load w3nco/v2.0.6
    module load w3emc/v2.2.0
    module load nemsio/v2.2.2
    module load g2tmpl/v1.4.0
    module use /mnt/lfs3/projects/hfv3gfs/Bing.Fu/soft/grib_util.v1.1.0/modulefiles
    module load grib_util/jet/v1.1.0
    module use /lfs3/projects/hwrf-vd/soft/modulefiles
    module load prod_util
    #module load grib_util
    module load hpss
    # mpiserial on Jet includes a cfp wrapper
    module load mpiserial
    module load gempak/7.4.2
    export PATH=$PATH:./
elif [[ "$WHERE_AM_I" == wcoss_dell_p3 ]] ; then
    echo "Loading modules for wcoss_dell_p3"
    . /usrx/local/prod/lmod/lmod/init/ksh
    module load EnvVars/1.0.2
    module load ips/18.0.1.163
    module load prod_envir/1.0.2
    module load grib_util/1.0.6
    module load prod_util/1.1.0
    module load HPSS/5.0.2.5
    #module use -a /gpfs/dell1/nco/ops/nwpara/util_shared.v1.1.0/modulefiles
    #module load util_shared/1.1.0
    #module load dev/util_shared/1.0.8
    module use -a /gpfs/dell1/nco/ops/nwpara/modulefiles/compiler_prod/ips/18.0.1
    module load util_shared/1.1.0
    module load g2tmpl/1.5.0
    
    module load NetCDF/4.5.0
    module load HDF5-serial/1.10.1
    module load impi/18.0.1
    module load CFP/2.0.1
    export USE_CFP=YES
    export PATH=$PATH:.
fi

#%Module#####################################################
## gefs_nesio2nc component - wcoss2
#############################################################
module load intel/${intel_ver}
module load PrgEnv-intel/${PrgEnv_intel_ver}

module load craype/${craype_ver}
module load cray-mpich/${cray_mpich_ver}

module load nemsio/${nemsio_ver}
module load w3nco/${w3nco_ver}
module load bacio/${bacio_ver}

module load netcdf/${netcdf_ver}

##
export FCMP=ftn
export LDFLAGSM=
export OMPFLAGM=

##
export NETCDF_LDFLAGS_C="-L${NETCDF_LIBRARIES} -lnetcdf"

#%Module#####################################################
## gefs_nesio2nc component - wcoss_dell_p3
#############################################################
module load ips/${ips_ver}
module load impi/${impi_ver}

module load nemsio/${nemsio_ver}
module load w3nco/${w3nco_ver}
module load bacio/${bacio_ver}

module load NetCDF/${NetCDF_ver}

##
export FCMP=ifort
export LDFLAGSM=
export OMPFLAGM=
export NETCDF_INCLUDES=$NETCDF_INC
export NETCDF_LIBRARIES=$NEMSIO_LIB

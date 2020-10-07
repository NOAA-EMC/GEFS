#%Module#####################################################
## gefs_nstgen component - Hera
#############################################################
#module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles

##
## load programming environment
## this typically includes compiler, MPI and job scheduler
##
module load intel/18.0.5.274
module load impi/2018.0.4

##
## NCEP libraries
##
module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles

module load w3nco/2.0.7
module load bacio/2.0.3
module load sfcio/1.1.1
module load netcdf/4.7.0

##
export NETCDF_INC=$NETCDF/include
export NETCDF_LDFLAGS="-L$NETCDF/lib -lnetcdff"

export FCMP=ifort
export LDFLAGSM=
export OMPFLAGM=


#%Module#####################################################
## global_ensppf component - Hera
#############################################################
#module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles

##
## load programming environment
## this typically includes compiler, MPI and job scheduler
##
module load intel/18.0.5.274

##
## NCEP libraries
##
module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles

module load g2/3.1.1
module load w3nco/2.0.7
module load bacio/2.0.3
module load sp/2.0.3

module load jasper/1.900.1
module load png/1.2.44
module load z/1.2.11

##
export FCMP=ifort
export LDFLAGSM=
export OMPFLAGM=


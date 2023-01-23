#%Module######################################################################
##      Roberto.Padilla@noaa.gov    IMSG@NOAA/NWS/NCEP/EMC
##      Henrique.Alves@noaa.gov     SRG @ NOAA/NWS/NCEP/EMC
##                                                
## GWES.v3.0.9
##_____________________________________________________
#
module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load EnvVars/1.0.2
module load intel/2020
module load impi/2020
module load w3nco/2.0.7
module load bacio/2.0.3
module load jasper/1.900.1
module load png/1.2.44
module load z/1.2.11
module load g2/3.1.1


##
export FCMP=ifort
export FFLAGSM="-O -xHost -list -auto"
export LDFLAGSM=
export OMPFLAGM=

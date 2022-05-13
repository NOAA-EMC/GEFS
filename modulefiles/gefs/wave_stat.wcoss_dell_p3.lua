#%Module######################################################################
##      Roberto.Padilla@noaa.gov    IMSG@NOAA/NWS/NCEP/EMC
##      Henrique.Alves@noaa.gov     SRG @ NOAA/NWS/NCEP/EMC
##                                                
## GWES.v3.0.9
##_____________________________________________________
module load EnvVars/${EnvVars_ver}
module load ips/${ips_ver}
module load w3nco/${w3nco_ver}
module load bacio/${bacio_ver}
module load jasper/${jasper_ver}
module load libpng/${libpng_ver}
module load zlib/${zlib_ver}
module load g2/${g2_ver}

##
export FCMP=ifort
export FFLAGSM="-O -xHost -list -auto"
export LDFLAGSM=
export OMPFLAGM=

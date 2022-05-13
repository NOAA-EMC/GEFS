#%Module#####################################################
## global_ensadd component - wcoss2
#############################################################
module load intel/${intel_ver}
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load craype/${craype_ver}

module load g2/${g2_ver}
module load w3nco/${w3nco_ver}
module load bacio/${bacio_ver}
module load jasper/${jasper_ver}
module load libpng/${libpng_ver}
module load zlib/${zlib_ver}

##
export FCMP=ftn
export LDFLAGSM=
export OMPFLAGM=


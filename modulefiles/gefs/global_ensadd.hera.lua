help([[
Load environment for building global_ensadd on hera
]])

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")

hpc_ver=os.getenv("hpc_ver") or "1.2.0"
load(pathJoin("hpc", hpc_ver))

intel_ver=os.getenv("intel_ver") or "18.0.5.274"
load(pathJoin("hpc-intel", intel_ver))

g2_ver=os.getenv("g2_ver") or "3.1.1"
load(pathJoin("g2", g2_ver))

w3nco_ver=os.getenv("w3nco_ver") or "2.0.7"
load(pathJoin("w3nco", w3nco_ver))

bacio_ver=os.getenv("bacio_ver") or "2.0.3"
load(pathJoin("bacio", bacio_ver))

jasper_ver=os.getenv("jasper_ver") or "1.900.1"
load(pathJoin("jasper", jasper_ver))

libpng_ver=os.getenv("libpng_ver") or "1.6.37"
load(pathJoin("libpng", libpng_ver))

zlib_ver=os.getenv("zlib_ver") or "1.2.11"
load(pathJoin("zlib", zlib_ver))


setenv("FCMP","ifort")
setenv("LDFLAGSM","")
setenv("OMPFLAGM","")

setenv("PNG_LIB","${LIBPNG_LIB}")
setenv("Z_LIB", "${ZLIB_LIB}")

whatis("Description: global_ensadd build environment")

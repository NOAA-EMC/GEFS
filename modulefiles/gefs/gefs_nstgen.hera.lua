help([[
Load environment for building gefs_nstgen on hera
]])

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")

hpc_ver=os.getenv("hpc_ver") or "1.2.0"
load(pathJoin("hpc", hpc_ver))

intel_ver=os.getenv("intel_ver") or "18.0.5.274"
load(pathJoin("hpc-intel", intel_ver))

impi_ver=os.getenv("impi_ver") or "2018.0.4"
load(pathJoin("hpc-impi", impi_ver))


w3nco_ver=os.getenv("w3nco_ver") or "2.4.1"
load(pathJoin("w3nco", w3nco_ver))

bacio_ver=os.getenv("bacio_ver") or "2.4.1"
load(pathJoin("bacio", bacio_ver))

netcdf_ver=os.getenv("netcdf_ver") or "4.7.4"
load(pathJoin("netcdf", netcdf_ver))


setenv("FCMP","ifort")
setenv("LDFLAGSM","")
setenv("OMPFLAGM","")

setenv("NETCDF_INCLUDES","${NETCDF}/include")
setenv("NETCDF_LIBRARIES","${NETCDF}/lib")
setenv("NETCDF_LDFLAGS","-L${NETCDF_LIBRARIES} -lnetcdff")

whatis("Description: gefs_nstgen build environment")

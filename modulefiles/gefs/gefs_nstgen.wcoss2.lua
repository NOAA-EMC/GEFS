help([[
Load environment for building gefs_nstgen on WCOSS2
]])

intel_ver=os.getenv("intel_ver") or "19.1.3.304"
load(pathJoin("intel", intel_ver))

PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver") or "8.1.0"
load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))

craype_ver=os.getenv("craype_ver") or "2.7.10"
load(pathJoin("craype", craype_ver))

cray_mpich_ver=os.getenv("cray_mpich_ver") or "8.1.9"
load(pathJoin("cray-mpich", cray_mpich_ver))


w3nco_ver=os.getenv("w3nco_ver") or "2.4.1"
load(pathJoin("w3nco", w3nco_ver))

bacio_ver=os.getenv("bacio_ver") or "2.4.1"
load(pathJoin("bacio", bacio_ver))

netcdf_ver=os.getenv("netcdf_ver") or "4.7.4"
load(pathJoin("netcdf", netcdf_ver))


setenv("FCMP","ftn")
setenv("LDFLAGSM","")
setenv("OMPFLAGM","")

setenv("NETCDF_LDFLAGS","-L${NETCDF_LIBRARIES}  -lnetcdff")

whatis("Description: gefs_nstgen build environment")

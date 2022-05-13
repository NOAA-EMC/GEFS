help([[
Load environment for building gefs_nesio2nc on wcoss_dell_p3
]])

ips_ver=os.getenv("ips_ver") or "18.0.1.163"
load(pathJoin("ips", ips_ver))

impi_ver=os.getenv("impi_ver") or "18.0.1"
load(pathJoin("impi", impi_ver))


nemsio_ver=os.getenv("nemsio_ver") or "2.2.3"
load(pathJoin("nemsio", nemsio_ver))

w3nco_ver=os.getenv("w3nco_ver") or "2.0.6"
load(pathJoin("w3nco", w3nco_ver))

bacio_ver=os.getenv("bacio_ver") or "2.0.2"
load(pathJoin("bacio", bacio_ver))

NetCDF_ver=os.getenv("NetCDF_ver") or "4.5.0"
load(pathJoin("NetCDF", NetCDF_ver))


setenv("FCMP","ifort")
setenv("LDFLAGSM","")
setenv("OMPFLAGM","")

setenv("NETCDF_INCLUDES","${NETCDF_INC}")
setenv("NETCDF_LIBRARIES","${NEMSIO_LIB}")

whatis("Description: gefs_nesio2nc build environment")

#!/bin/sh

#---------------------------------------------------------
if [[ -d /dcom ]] ; then
  # We are on NOAA Tide or Gyre

machine=wcoss
export INC="${G2_INCd} ${NETCDF_INCLUDE} -I${PNETCDF}/include ${HDF5_INCLUDE}"
export LIBS="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${G2_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB} ${SP_LIBd} ${IP_LIBd} ${NETCDF_LDFLAGS} -L${PNETCDF}/lib -lpnetcdf ${HDF5_LDFLAGS}"
export LIBS_SUP="${W3EMC_LIBd} ${W3NCO_LIBd}"
export LIBS_UK="${W3NCO_LIB4} ${BACIO_LIB4}"

for dir in *.fd; do
  cd $dir
  make clean
  make -f makefile_wcoss
  make install
  cd ..
done

elif [[ -d /gpfs/hps && -e /etc/SuSE-release ]] ; then
  # We are on NOAA Luna or Surge

 machine=cray
 export INC="${G2_INCd} ${NETCDF_INCLUDE} -I${PNETCDF_INC} ${HDF5_INCLUDE}"
 export LIBS="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${G2_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB} ${NETCDF_LDFLAGS_CXX} -L${PNETCDF_LIB} ${HDF5_LDFLAGS}"
 export LIBS_SUP="${W3EMC_LIBd} ${W3NCO_LIBd}"
 export LIBS_UK="${W3NCO_LIB4} ${BACIO_LIB4}"

for dir in *.fd; do
  cd $dir
  make clean
  make -f makefile_cray
  make install
  cd ..
done

elif [[ -L /usrx && "$( readlink /usrx 2> /dev/null )" =~ dell ]] ; then
  # We are on NOAA Mars or Venus

 machine=dell
 export INC="${G2_INCd} ${NETCDF_INCLUDE} ${PNetCDF_INCLUDE}"
 export LIBS="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${G2_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB} ${NETCDF_LDFLAGS} ${PNetCDF_LDFLAGS}"
 export LIBS_SUP="${W3EMC_LIBd} ${W3NCO_LIBd}"
 export LIBS_UK="${W3NCO_LIB4} ${BACIO_LIB4}"

for dir in *.fd; do
  cd $dir
  make clean
  make -f makefile
  make install
  make clean
  cd ..
done

elif [[ -d /scratch2 ]] ; then
  # We are on NOAA Hera

machine=hera
export INC="${G2_INCd} -I${NETCDF}/include -I${HDF5}/include "
export LIBS="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${G2_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB} ${SP_LIBd} ${IP_LIBd} -L${NETCDF}/lib -lnetcdff -lnetcdf -L${HDF5}/lib -lhdf5_hl -lhdf5hl_fortran -lhdf5 -lhdf5_fortran " 
export LIBS_SUP="${W3EMC_LIBd} ${W3NCO_LIBd}"
export LIBS_UK="${W3NCO_LIB4} ${BACIO_LIB4}"

for dir in *.fd; do
#for dir in leadtime.fd; do
  cd $dir
  make clean
  make -f makefile_wcoss
  make install
  cd ..
done

elif [[ -d /lfs3 ]] ; then
  # We are on NOAA Jet

machine=jet
export INC="${G2_INCd} -I${NETCDF}/include ${PNETCDF_INCLUDE} ${HDF5_INCLUDE_OPTS} "
export LIBS="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${G2_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB} ${SP_LIBd} ${IP_LIBd} -L${NETCDF}/lib -lnetcdf ${PNETCDF_LD_OPTS} ${HDF5_LINK_OPTS}"

export LIBS_SUP="${W3EMC_LIBd} ${W3NCO_LIBd}"
export LIBS_UK="${W3NCO_LIB4} ${BACIO_LIB4}"

for dir in *.fd; do
  cd $dir
  make clean
  make -f makefile_jet
  make install
  cd ..
done

else
  export machine=unknown
  echo Job failed: unknown platform 1>&2
fi

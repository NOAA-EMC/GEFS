#!/bin/sh
set -x -e

#---------------------------------------------------------
if [ -d /scratch4/NCEPDEV ]; then # For THEIA
    echo "Building for Theia"
    machine=theia
    ptmp=/scratch4/NCEPDEV/ensemble/ptmp/$LOGNAME

    # Set variables used by makefiles - same as WCOSS - WCK
    export INCG="$NEMSIO_INC"
    export INCGFS="$NEMSIOGFS_INC"

    export INCS="${SIGIO_INC4}"
    export INCSFC="${SFCIO_INC4}"
    export INC="${G2_INC4}"
    export INC_d="${G2_INCd}"
    export LIBS="${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}"
    export LIBS_d="${G2_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${IP_LIBd} ${SP_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB} ${W3NCO_LIBd}"
    export FC=ifort

    export LIBS_INIT="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${SP_LIBd} ${SIGIO_LIB4} ${W3NCO_LIBd} ${BACIO_LIB4}"
    export LIBS_GTRK="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${BACIO_LIB4} ${SIGIO_LIB4} ${IP_LIB4} ${SP_LIB4} ${SFCIO_LIB4} ${BUFR_LIB4} ${W3EMC_LIB4} ${W3NCO_LIB4} "

    export FFLAGS="-O3 -g -convert big_endian -I ${G2_INC4}"
    export FFLAGS_d="-O3 -g -r8 -convert big_endian -auto -mkl -I ${G2_INCd}"
    export OPENMPFFLAG=openmp

elif [[ -d /dcom && -d /hwrf ]] ; then # Tide or Gyre
    echo "Building for WCOSS IBM"
    machine=wcoss
    export LIBDIR=/nwprod/lib
    export INCG="$NEMSIO_INC"
    export INCGFS="$NEMSIOGFS_INC"

    # # Hui-Ya updated g2 lib for nceppost, we need to follow to use same g2 library for our apps. 
    export G2_SRC=/usrx/local/nceplibs/g2/v3.0.0/src
    export G2_INC4=/usrx/local/nceplibs/g2/v3.0.0/include/g2_v3.0.0_4
    export G2_INCd=/usrx/local/nceplibs/g2/v3.0.0/include/g2_v3.0.0_d
    export G2_LIB4=/usrx/local/nceplibs/g2/v3.0.0/libg2_v3.0.0_4.a
    export G2_LIBd=/usrx/local/nceplibs/g2/v3.0.0/libg2_v3.0.0_d.a
    export G2_VER=v3.0.0

    export INCS="${SIGIO_INC4}"
    export INCSFC="${SFCIO_INC4}"
    export INC="${G2_INC4}"
    export LIBS="${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}"
    export INC_d="${G2_INCd}"
    export LIBS_d="${G2_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${IP_LIBd} ${SP_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB} ${W3NCO_LIBd}"
    export FC=ifort
    export LIBS_INIT="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${SP_LIBd} ${SIGIO_LIB4} ${W3NCO_LIBd} ${BACIO_LIB4}"
    export LIBS_GTRK="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${BACIO_LIB4} ${SIGIO_LIB4} ${IP_LIB4} ${SP_LIB4} ${SFCIO_LIB4} ${BUFR_LIB4} ${W3EMC_LIB4} ${W3NCO_LIB4} "
    export FFLAGS="-O3 -g -convert big_endian -I ${G2_INC4}"
    export FFLAGS_d="-O3 -g -r8 -convert big_endian -auto -mkl -I ${G2_INCd}"
    export OPENMPFFLAG=qopenmp

elif [[ -d /gpfs/hps3 && -e /etc/SuSE-release ]]; then # Luna or Surge
    echo "Building for Cray"

    machine=cray
    export LIBDIR=/gpfs/hps/nco/ops/nwprod/lib
    export INCG="$NEMSIO_INC"
    export INCGFS="$NEMSIOGFS_INC"

    export INCS="${SIGIO_INC4} ${CRAY_IOBUF_INCLUDE_OPTS}"
    export INCSFC="${SFCIO_INC4}"
    export INC="${G2_INC4} ${CRAY_IOBUF_INCLUDE_OPTS}"
    export INC_d="${G2_INCd}"
    export LIBS="${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${CRAY_IOBUF_POST_LINK_OPTS}"
    export LIBS_d="${G2_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${IP_LIBd} ${SP_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB} ${W3NCO_LIBd}"
    export FC=ftn
    export LIBS_INIT="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${SP_LIBd} ${SIGIO_LIB4} ${W3NCO_LIBd} ${BACIO_LIB4}"
    export LIBS_GTRK="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${BACIO_LIB4} ${SIGIO_LIB4} ${IP_LIB4} ${SP_LIB4} ${SFCIO_LIB4} ${BUFR_LIB4} ${W3EMC_LIB4} ${W3NCO_LIB4} "
    export FFLAGS="-O3 -g -convert big_endian -I ${G2_INC4}"
    export FFLAGS_d="-O3 -g -r8 -convert big_endian -auto -mkl -I ${G2_INCd}"
    export OPENMPFFLAG=openmp

elif [[ -L /usrx && "$( readlink /usrx 2> /dev/null )" =~ dell ]] ; then # We are on NOAA Mars or Venus
    echo "Building for wcoss_dell_p3"

    machine=wcoss_dell_p3
   
    export IP_LIBd=/gpfs/dell1/nco/ops/nwtest/lib.p2/ip/v2.0.0/libip_v2.0.0_d.a
    export IP_LIB4=/gpfs/dell1/nco/ops/nwtest/lib.p2/ip/v2.0.0/libip_v2.0.0_4.a
    export LIBDIR=/gpfs/dell1/nco/ops/nwprod/lib
    export INCG="$NEMSIO_INC"
    export INCGFS="$NEMSIOGFS_INC"
    export INCS="${SIGIO_INC4}"
    export INCSFC="${SFCIO_INC4}"
    export INC="${G2_INC4}"
    export LIBS="${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}"
    export INC_d="${G2_INCd}"
    export LIBS_d="${G2_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${IP_LIBd} ${SP_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB} ${W3NCO_LIBd}"
    export FC=ifort
    export LIBS_INIT="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${SP_LIBd} ${SIGIO_LIB4} ${W3NCO_LIBd} ${BACIO_LIB4}"
    export LIBS_GTRK="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${BACIO_LIB4} ${SIGIO_LIB4} ${IP_LIB4} ${SP_LIB4} ${SFCIO_LIB4} ${BUFR_LIB4} ${W3EMC_LIB4} ${W3NCO_LIB4} "
    export FFLAGS="-O3 -g -convert big_endian -I ${G2_INC4}"
    export FFLAGS_d="-O3 -g -r8 -convert big_endian -auto -mkl -I ${G2_INCd}"
    export OPENMPFFLAG=qopenmp
fi


for dir in global_sigzvd.fd  global_ensadd.fd  global_enspqpf.fd  gefs_ensstat.fd  global_ensppf.fd ; do
    cd $dir
    make clean
    make -f makefile
    cd ..
done

export LIBS="${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}"
for dir in global_enscvprcp.fd global_enspvrfy.fd global_enssrbias.fd global_enscqpf.fd  global_enscvt24h.fd  global_ensrfmat.fd ; do
    cd $dir
    make clean
    make -f makefile
    cd ..
done

for dir in ../util/sorc/gettrk.fd ../util/sorc/overenstr.grib.fd ../util/sorc/getnsttf.fd; do
    cd $dir
    make clean
    make -f Makefile
    cd ../../../sorc
done

# For SST
./build_gefs_anom2_fcst.sh
./build_gefs_nstgen.sh

./build_gefs_vortex_separate.sh
./build_gefs_vortex_combine.sh

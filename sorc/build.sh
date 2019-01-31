#!/bin/sh
set -x -e

mac=$(hostname | cut -c1-1)

#---------------------------------------------------------
if [ $mac = t -o $mac = e -o $mac = g ] ; then # For WCOSS
                                                 # --------
 machine=wcoss
#export LIBDIR=/nwprod2/lib
 export INCS="${SIGIO_INC4}"
 export INCSFC="${SFCIO_INC4}"
 export INC="${G2_INC4}"
 export LIBS="${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}"
 export INC_d="${G2_INCd}"
 export LIBS_d="${G2_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${IP_LIBd} ${SP_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB} ${W3NCO_LIBd}"
 export FC=ifort

 export LIBS_INIT="${SP_LIBd} ${SIGIO_LIB4} ${W3NCO_LIBd}"
 export LIBS_GTRK="${BACIO_LIB4} ${SIGIO_LIB4} ${IP_LIB4} ${SP_LIB4} ${SFCIO_LIB4} ${BUFR_LIB4} ${W3EMC_LIB4} ${W3NCO_LIB4} "

 export FFLAGS="-O3 -g -convert big_endian -I ${G2_INC4}"
 export FFLAGS_d="-O3 -g -r8 -convert big_endian -auto -mkl -I ${G2_INCd}"
 export OPENMPFFLAG="openmp"

#---------------------------------------------------------

elif [ $mac = z -o $mac = h -o $mac = f ] ; then # For ZEUS
                                                 # --------
 machine=zeus
# ptmp=/scratch2/portfolios/NCEPDEV/ptmp/$LOGNAME
 export LIBDIR=/contrib/nceplibs/nwprod/lib
 export NCEPLIB=/contrib/nceplibs/dev/lib
 export INC="/contrib/nceplibs/nwprod/lib/incmod/g2_d"
 export INCS="/contrib/nceplibs/nwprod/lib/incmod/sigio_v1.0.1"
 export INCSFC="/contrib/nceplibs/nwprod/lib/incmod/sfcio_v1.1.0"
 export LIBS="-L/contrib/nceplibs/nwprod/lib -lg2_4  -lw3nco_4  -lbacio_4 -ljasper -lpng -lz"
 export LIBS_d="-L/contrib/nceplibs/nwprod/lib -lg2_d -lw3emc_d -lw3nco_d  -lbacio_4 -lip_d -lsp_d -ljasper -lpng -lz"
 export LIBS_INIT="-L/contrib/nceplibs/nwprod/lib -lsp_d -lsigio_4 -lw3nco_d "
 export LIT_GTRK="-L/contrib/nceplibs/nwprod/lib -lip_4 -lsp_4 -lbacio_4 -lsigio_4 -lsfcio_4 -lbufr_4_64 -lw3emc_4 -lw3nco_4"
 export FC=ifort 
 export FFLAGS="-openmp -O3 -g -traceback -r8 -I "
 export OPENMPFFLAG="openmp"
else

  if [[ -L /usrx && "$( readlink /usrx 2> /dev/null )" =~ dell ]] ; then # We are on NOAA Mars or Venus
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


fi

#---------------------------------------------------------
for dir in gefs_vortex_separate.fd gefs_vortex_combine.fd global_sigzvd.fd  global_ensadd.fd  global_enspqpf.fd  gefs_ensstat.fd  global_ensppf.fd global_enscvprcp.fd  global_enspvrfy.fd  global_enssrbias.fd global_enscqpf.fd  global_enscvt24h.fd  global_ensrfmat.fd ; do
#for dir in global_enspvrfy.fd ;do
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
if [[ -L /usrx && "$( readlink /usrx 2> /dev/null )" =~ dell ]] ; then # We are on NOAA Mars or Venus
    for dir in ../util/sorc/nemsio_get.fd; do
        export FCMP=ifort
        cd $dir
        #make clean
        make -f makefile
        cd ../../../sorc
    done
fi


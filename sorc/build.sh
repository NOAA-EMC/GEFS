#!/bin/sh
set -x -e

mac=$(hostname | cut -c1-1)

#---------------------------------------------------------
if [ $mac = t -o $mac = e -o $mac = g ] ; then # For WCOSS
 # --------
 machine=wcoss
 export LIBDIR=/nwprod/lib
#export NEMSIOGFS_LIB=/global/save/Fanglin.Yang/svn/gfs/tags/nemsiogfs/intel/libnemsiogfs_v1.1.0.a
#export NEMSIOGFS_INC=/global/save/Fanglin.Yang/svn/gfs/tags/nemsiogfs/intel/include/nemsiogfs_v1.1.0
# export NEMSIOGFS_LIB=/global/save/emc.glopara/svn/gfs/q3fy17/nemsiogfsv2.0.1/libnemsiogfs.a
# export NEMSIOGFS_INC=/global/save/emc.glopara/svn/gfs/q3fy17/nemsiogfsv2.0.1/include/nemsiogfs
 export NEMSIO_LIB=/global/save/emc.glopara/svn/nceplibs/nemsio/trunk/libnemsio.a
 export NEMSIO_INC=/global/save/emc.glopara/svn/nceplibs/nemsio/trunk/incmod/nemsio
 export INCG="$NEMSIO_INC"
 export INCGFS="$NEMSIOGFS_INC"

# Hui-Ya updated g2 lib for nceppost, we need to follow to use same g2 library for our apps. 
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

#---------------------------------------------------------

elif [ $mac = l -o $mac = s ] ; then # For CRAY
                                                 # --------
 machine=cray
 export LIBDIR=/gpfs/hps/nco/ops/nwprod/lib
 export LIBDIR=/gpfs/hps/nco/ops/nwprod/lib
 export NEMSIOGFS_LIB=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/q3fy17/nemsiogfsv2.0.1/intel/libnemsiogfs.a
 export NEMSIOGFS_INC=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/q3fy17/nemsiogfsv2.0.1/intel/include/nemsiogfs
 export NEMSIO_LIB=/gpfs/hps/nco/ops/nwprod/lib/nemsio/v2.2.2/intel/libnemsio_v2.2.2.a
 export NEMSIO_INC=/gpfs/hps/nco/ops/nwprod/lib/nemsio/v2.2.2/intel/include/nemsio_v2.2.2
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

elif [ $mac2 = tf ]; then # For THEIA
        machine=theia
        ptmp=/scratch4/NCEPDEV/ensemble/ptmp/$LOGNAME

        # Set variables used by makefiles - same as WCOSS - WCK
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

fi
#---------------------------------------------------------
for dir in gefs_vortex_separate.fd gefs_vortex_combine.fd global_sigzvd.fd  global_ensadd.fd  global_enspqpf.fd  gefs_ensstat.fd  global_ensppf.fd ; do
 cd $dir
 make clean
 make -f makefile
 cd ..
done
export LIBS="${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}"

export LIBS="${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}"
for dir in global_enscvprcp.fd  global_enspvrfy.fd  global_enssrbias.fd global_enscqpf.fd  global_enscvt24h.fd  global_ensrfmat.fd ; do
 cd $dir
 make clean
 make -f makefile
 cd ..
done

for dir in ../util/sorc/gettrk.fd ../util/sorc/overenstr.grib.fd; do
        cd $dir
        make clean
        make -f Makefile
        cd ../../../sorc
done


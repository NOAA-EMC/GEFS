#!/bin/sh
set -x -e

mac=$(hostname | cut -c1-1)
mac2=$(hostname | cut -c1-2)

#---------------------------------------------------------
if [ $mac2 = tf ]; then                        # For THEIA
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

elif [ $mac = t -o $mac = e -o $mac = g ] ; then # For WCOSS
	# --------
	machine=wcoss
	export LIBDIR=/nwprod/lib
#        export NEMSIOGFS_LIB=/global/save/Fanglin.Yang/svn/gfs/tags/nemsiogfs/intel/libnemsiogfs_v1.1.0.a
#        export NEMSIOGFS_INC=/global/save/Fanglin.Yang/svn/gfs/tags/nemsiogfs/intel/include/nemsiogfs_v1.1.0
        export NEMSIOGFS_LIB=/global/save/Fanglin.Yang/svn/gfs/tags/nemsiogfs_test/intel/libnemsiogfs_v1.1.0.a
        export NEMSIOGFS_INC=/global/save/Fanglin.Yang/svn/gfs/tags/nemsiogfs_test/intel/include/nemsiogfs_v1.1.0
        export NEMSIO_LIB=/global/save/emc.glopara/svn/nceplibs/nemsio/trunk/libnemsio.a
        export NEMSIO_INC=/global/save/emc.glopara/svn/nceplibs/nemsio/trunk/incmod/nemsio

	export INCS="${SIGIO_INC4}"
        export INCG="$NEMSIO_INC" 
        export INCGFS="$NEMSIOGFS_INC"
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
fi
#---------------------------------------------------------
for dir in gefs_vortex_separate.fd gefs_vortex_combine.fd global_sigzvd.fd global_ensadd.fd global_enspqpf.fd gefs_ensstat.fd global_ensppf.fd global_enscvprcp.fd global_enspvrfy.fd global_enssrbias.fd global_enscqpf.fd global_enscvt24h.fd global_ensrfmat.fd ; do
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

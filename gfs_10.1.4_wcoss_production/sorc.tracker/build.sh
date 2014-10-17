#!/bin/sh
set -x -e

mac=$(hostname | cut -c1-1)

#---------------------------------------------------------
if [ $mac = z -o $mac = h -o $mac = f ] ; then # For ZEUS
                                                 # --------
 machine=zeus
# ptmp=/scratch2/portfolios/NCEPDEV/ptmp/$LOGNAME
 export LIBDIR=/contrib/nceplibs/nwprod/lib
 export NCEPLIB=/contrib/nceplibs/dev/lib
 export INC="/contrib/nceplibs/nwprod/lib/incmod/g2_d"
 export LIBS="-L/contrib/nceplibs/nwprod/lib -lw3emc_d -lw3nco_d -lg2_d -lbacio_4 -ljasper -lpng -lz"
 export LIBS_SUP="-L/contrib/nceplibs/nwprod/lib -lw3emc_d -lw3nco_d -lg2_d -lbacio_4 -ljasper -lpng -lz"
 export LIBS_REL="-L/contrib/nceplibs/nwprod/lib -lw3nco_v2.0.6_4 -lsigio_v1.0.1_4 -lbacio_v2.0.1_4 ${NCEPLIB}/libsp_v2.0.2_d-12.0.a"
 export LIBS_SIG="/contrib/nceplibs/nwprod/lib/incmod/sigio_v1.0.1"
 export LIBS_SYN_GET="-L/contrib/nceplibs/nwprod/lib -lw3nco_v2.0.5_4"
 export LIBS_SYN_MAK="-L/contrib/nceplibs/nwprod/lib -lw3nco_v2.0.5_4 -lbacio_v2.0.1_4"
 export LIBS_SYN_QCT="-L/contrib/nceplibs/nwprod/lib -lw3nco_v2.0.5_8"
 export FC=ifort 
 export FFLAGS="-openmp -O3 -g -traceback -r8 -I ${LIBS_SIG}"

#---------------------------------------------------------
elif [ $mac = t -o $mac = e -o $mac = g ] ; then # For WCOSS
                                                 # --------
 machine=wcoss
# ptmp="/ptmpp1/$LOGNAME"
 export LIBDIR=/nwprod/lib
 export NCEPLIB=/usrx/local/nceplibs
 export INC="/nwprod/lib/incmod/g2_d"
 export LIBS="-L/nwprod/lib -lw3emc_d -lw3nco_d -lg2_d -lbacio_4 -ljasper -lpng -lz"
 export LIBS_SUP="-L/nwprod/lib -lw3emc_d -lw3nco_d"
 export LIBS_REL="-L/nwprod/lib -lw3nco_v2.0.6_4 -lsigio_v1.0.1_4 -lbacio_v2.0.1_4 /usrx/local/nceplibs/libsp_v2.0.2p_d.a"
 export LIBS_SIG="/nwprod/lib/incmod/sigio_v1.0.1_4"
 export LIBS_SYN_GET="/nwprod/lib/libw3nco_v2.0.6_4.a"
 export LIBS_SYN_MAK="/nwprod/lib/libw3nco_v2.0.6_4.a /nwprod/lib/libbacio_v2.0.1_4.a"
 export LIBS_SYN_QCT="/nwprod/lib/libw3nco_v2.0.6_8.a"
 export FC=mpiifort 
 export FFLAGS="-openmp -O3 -g -traceback -r8 -I ${LIBS_SIG}"

fi
#---------------------------------------------------------

#source ./setlibs.rc
#source ./load_libs.rc  # use this instead of setlibs.rc when ready to used library modules.
for dir in *.fd; do
 cd $dir
 make clean
 make -f makefile
 cd ..
done



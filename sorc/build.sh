#!/bin/sh
echo "`date`   `pwd`   $0 $*   begin"
# 20190916 RLW allow compilations to be specified by argument list
# 20190916 RLW which can be passed in from build.all.sh
# 20190909 RLW logging replaced by logging in build.all.sh
#dtg=`date +%Y%m%d%H%M%S`
#logfile=`basename $0`.log
#logfiled=$logfile.$dtg
#if [[ -f $logfile ]]; then
#  rm $logfile
#fi
#(
#echo "`date`   `pwd`   $0 $*   begin log"
pwd
dirsaved=`pwd`

#set -x -e

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

    echo
    echo before env
    echo
    env | grep _LIB
    echo
    echo after env
    echo
    echo before env mkl
    echo
    env | grep _LIB | grep -i mkl
    echo
    echo after env mkl
    echo

    export IP_LIBd=/gpfs/dell1/nco/ops/nwtest/lib.p2/ip/v2.0.0/libip_v2.0.0_d.a
    export IP_LIB4=/gpfs/dell1/nco/ops/nwtest/lib.p2/ip/v2.0.0/libip_v2.0.0_4.a
    export LIBDIR=/gpfs/dell1/nco/ops/nwprod/lib
    export INCG="$NEMSIO_INC"
    export INCGFS="$NEMSIOGFS_INC"
    export INCS="${SIGIO_INC4}"
    export INCSFC="${SFCIO_INC4}"
    export INC="${G2_INC4}"
    export LIBS="${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}"
    export LIBS_V="${IP_LIB4} ${SP_LIB4} ${BACIO_LIB4} ${W3NCO_LIB4} ${PNG_LIB} ${Z_LIB}"
    export INC_d="${G2_INCd}"
    export LIBS_d="${G2_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${IP_LIBd} ${SP_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB} ${W3NCO_LIBd}"
    export FC=ifort
    export LIBS_INIT="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${SP_LIBd} ${SIGIO_LIB4} ${W3NCO_LIBd} ${BACIO_LIB4}"
    export LIBS_INIT_ET="${SP_LIB4} ${SIGIO_LIB4} ${W3NCO_LIBd}"
    export LIBS_ET="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${SP_LIBd} ${SIGIO_LIB4} ${W3NCO_LIBd} ${BACIO_LIB4} "
    export LIBS_GTRK="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${BACIO_LIB4} ${SIGIO_LIB4} ${IP_LIB4} ${SP_LIB4} ${SFCIO_LIB4} ${BUFR_LIB4} ${W3EMC_LIB4} ${W3NCO_LIB4} "
    export FFLAGS="-O3 -g -convert big_endian -I ${G2_INC4}"
    export FFLAGS_d="-O3 -g -r8 -convert big_endian -auto -mkl -I ${G2_INCd}"
    export OPENMPFFLAG=qopenmp
  fi


fi

#---------------------------------------------------------

set +x

#sorclist="gefs_ensstat      gefs_vortex_combine gefs_vortex_separate     global_ensadd global_ensadd_g2     global_sigzvd"
#sorclist="gefs_init_et_p"

#sorclist="gefs_vortex_separate gefs_vortex_combine global_sigzvd  global_ensadd  global_ensadd_g2 global_enspqpf  gefs_ensstat  global_ensppf global_enscvprcp  global_enspvrfy  global_enssrbias global_enscqpf  global_enscvt24h  global_ensrfmat" 


#sorclist="\
#  gefs_ensstat\
#  gefs_init_et_p\
#  gefs_vortex_combine\
#  gefs_vortex_separate\
#  global_ensadd\
#  global_ensadd_g2\
#  global_enscqpf\
#  global_enscvprcp\
#  global_enscvt24h\
#  global_ensppf\
#  global_enspqpf\
#  global_enspvrfy\
#  global_ensrfmat\
#  global_enssrbias\
#  global_sigzvd\
#"

sorclist="\
  global_ensadd\
  global_enscqpf\
  global_enscvprcp\
  global_enscvt24h\
  global_ensppf\
  global_enspqpf\
  global_enspvrfy\
  global_ensrfmat\
  global_enssrbias\
  global_sigzvd\
  global_enssrbias\
  gefs_vortex_combine\
  gefs_vortex_separate\
  gefs_init_et_p\
  gefs_ensstat\
  overparm_grib\
"

#utillist="gettrk overenstr.grib getnsttf"
utillist="overenstr.grib getnsttf"

nemslist="nemsio_get"
nemslist=

if (( $# > 0 )); then
  echo "only compile codes specified in argument list"
  sorctest=$sorclist
  utiltest=$utillist
  nemstest=$nemslist
  sorclist=
  utillist=
  nemslist=
  ifound=0
  for arg in $* ; do
    for sorct in $sorctest ; do
      if [[ $arg = $sorct ]]; then
        sorclist="$sorclist $sorct"
	ifound=1
      fi
    done
    for utilt in $utiltest ; do
      if [[ $arg = $utilt ]]; then
        utillist="$utillist $utilt"
	ifound=1
      fi
    done
    for nemst in $nemstest ; do
      if [[ $arg = $nemst ]]; then
        nemslist="$nemslist $nemst"
	ifound=1
      fi
    done
    if (( ifound == 0 )); then
      echo "arg=$arg IS NOT FOUND in any default compilation list"
    fi
  done
fi

echo sorclist=$sorclist
echo utillist=$utillist
echo nemslist=$nemslist

#set -x -e

msgstring="
"
#for dir in gefs_vortex_separate.fd gefs_vortex_combine.fd global_sigzvd.fd  global_ensadd.fd  global_enspqpf.fd  gefs_ensstat.fd  global_ensppf.fd global_enscvprcp.fd  global_enspvrfy.fd  global_enssrbias.fd global_enscqpf.fd  global_enscvt24h.fd  global_ensrfmat.fd ; do
for bdir in $sorclist ; do
  dir=$bdir.fd
  echo bdir=$bdir dir=$dir
  cd $dir
  rc=$?
  if (( rc == 0 )); then
    make clean
    (set -e -x; make -f makefile)
    rc=$?
    msgstring="$msgstring
      $rc   $dir"
    ls -Alrt --time-style="+%Y %b %d %T" `find . -type f -mtime -1 -print`
    cd ..
  else
    rc=x
    msgstring="$msgstring
      $rc   $dir"
  fi
done
msgstring="$msgstring
"

#for dir in ../util/sorc/gettrk.fd ../util/sorc/overenstr.grib.fd ../util/sorc/getnsttf.fd; do
for bdir in $utillist ; do
 dir=../util/sorc/$bdir.fd
 echo bdir=$bdir dir=$dir
 cd $dir
 make clean 
 make -f Makefile
 make install
 rc=$?
 msgstring="$msgstring
  $rc   $dir"
 cd ../../../sorc
done
msgstring="$msgstring
"
#for dir in ../util/sorc/gettrk.fd ../util/sorc/overenstr.grib.fd ../util/sorc/getnsttf.fd; do
if [[ -L /usrx && "$( readlink /usrx 2> /dev/null )" =~ dell ]] ; then # We are on NOAA Mars or Venus
    #for dir in ../util/sorc/nemsio_get.fd; do
    for bdir in $nemslist ; do
	dir=../util/sorc/$nemslist.fd
	echo bdir=$bdir dir=$dir
        export FCMP=ifort
        cd $dir
        #make clean
        make -f makefile
 rc=$?
 msgstring="$msgstring
  $rc   $dir"
        cd ../../../sorc
    done
fi
msgstring="$msgstring
"
echo "$msgstring"
echo "`date`   `pwd`   $0 $*   end"


#!/bin/sh
set -x

mac=$(hostname | cut -c1-1)

#---------------------------------------------
if [ $mac = t -o $mac = g ] ; then  #For WCOSS
#---------------------------------------------

    export NWPRODLIB=/nwprod/lib
    export FCMP=ifort

    export W3NCO_VER=v2.0.6
    export W3NCO_DIR=$NWPRODLIB/w3nco/$W3NCO_VER
    export W3NCO_LIB4=w3nco_${W3NCO_VER}_4    

    export SIGIO_VER=v1.0.1
    export SIGIO_DIR=$NWPRODLIB/sigio/$SIGIO_VER
    export SIGIO_LIB4=sigio_${SIGIO_VER}_4    
    export SIGIO_INC4=${SIGIO_DIR}/incmod/$SIGIO_LIB4

    export BACIO_VER=v2.0.1
    export BACIO_DIR=$NWPRODLIB/bacio/$BACIO_VER
    export BACIO_LIB4=bacio_${BACIO_VER}_4    

#---------------------------------------------
elif [ $mac = f ]; then #For Zeus
#---------------------------------------------
    export NWPRODLIB=/contrib/nceplibs/nwprod/lib
    export FCMP=ifort

    export W3NCO_VER=v2.0.6
    export W3NCO_DIR=$NWPRODLIB
    export W3NCO_LIB4=w3nco_${W3NCO_VER}_4    

    export SIGIO_VER=v2.0.1_beta
    export SIGIO_DIR=/contrib/nceplibs/dev/lib
    export SIGIO_LIB4=sigio_${SIGIO_VER}   
    export SIGIO_INC4=${SIGIO_DIR}/incmod/$SIGIO_LIB4

    export BACIO_VER=v2.0.1
    export BACIO_DIR=$NWPRODLIB
    export BACIO_LIB4=bacio_${BACIO_VER}_4   
    
#---------------------------------------------
elif [ $mac = v -o $mac = m ]; then #For Venus or Mars
#---------------------------------------------
    echo "--Running on Venus or Mars"
    export NWPRODLIB=/gpfs/dell1/nco/ops/nwprod/lib
    
    . /usrx/local/prod/lmod/lmod/init/profile
    module purge
    module load EnvVars/1.0.2
    module load ips/18.0.1.163
    #module load w3nco/2.0.6
    #module load sp/2.0.2
    #module load ip/3.0.1
    #module load sfcio/1.0.0
    #module load sigio/2.0.1
    #module load gfsio/1.1.0
    #module load landsfcutil/2.1.0
    #module load bacio/2.0.2
    module list

 
    export FCMP=ifort
    
    export W3NCO_VER=v2.0.6
    export W3NCO_DIR=${NWPRODLIB}/w3nco/${W3NCO_VER}/ips/18.0.1
    export W3NCO_LIB4=w3nco_${W3NCO_VER}_4

    export SIGIO_VER=v2.1.0
    export SIGIO_DIR=$NWPRODLIB/sigio/$SIGIO_VER/ips/18.0.1
    export SIGIO_LIB4=sigio_${SIGIO_VER}_4
    export SIGIO_INC4=${SIGIO_DIR}/include/sigio_${SIGIO_VER}_4

    export BACIO_VER=v2.0.2
    export BACIO_DIR=${NWPRODLIB}/bacio/${BACIO_VER}/ips/18.0.1
    export BACIO_LIB4=bacio_${BACIO_VER}_4 

#---------------------------------------------
else
 echo "Machine Option Not Found, exit"
 exit
fi
#---------------------------------------------
if [ ! -d "../../exec" ]; then
  echo "Creating ../../exec folder"
  mkdir ../../exec
fi

export FFLAGS=" -O2 -xHOST -convert big_endian -traceback -g -FR"

make -f Makefile clean
make -f Makefile
make -f Makefile install
make -f Makefile clean

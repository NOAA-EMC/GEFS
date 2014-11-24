#!/bin/sh
###############################################################
# This script generates the 6-hourly pqpf pqif pqrf pqff pqsc
###############################################################
set -x

export CDATE=$CDATE
ICYC=`echo $CDATE | cut -c9-10`
YYYYMMDD=`echo $CDATE | cut -c1-8`

#export enspqpf=$EXECGLOBAL/global_enspqpf_grb1
#DHOU 03/27/2012 For ZEUS
export enspqpf=$EXECgefs/global_enspqpf_grb1
export ext_h=${ext_h:-""}

cd $DATA

# Specify the input/output file names:
export CPGB=$COMIN/${ICYC}/ensstat/enspost${ext_h}.t${ICYC}z.prcp
export CPGI=$COMIN/${ICYC}/ensstat/enspost${ext_h}.t${ICYC}z.prcpi
export CPGO=$DATA/pqpf   
export CRAIN=$COMIN/${ICYC}/ensstat/enspost${ext_h}.t${ICYC}z.rain
export CRAINI=$COMIN/${ICYC}/ensstat/enspost${ext_h}.t${ICYC}z.raini
export CRAINO=$DATA/pqrf
export CFRZR=$COMIN/${ICYC}/ensstat/enspost${ext_h}.t${ICYC}z.frzr
export CFRZRI=$COMIN/${ICYC}/ensstat/enspost${ext_h}.t${ICYC}z.frzri
export CFRZRO=$DATA/pqff
export CICEP=$COMIN/${ICYC}/ensstat/enspost${ext_h}.t${ICYC}z.icep
export CICEPI=$COMIN/${ICYC}/ensstat/enspost${ext_h}.t${ICYC}z.icepi
export CICEPO=$DATA/pqif 
export CSNOW=$COMIN/${ICYC}/ensstat/enspost${ext_h}.t${ICYC}z.snow
export CSNOWI=$COMIN/${ICYC}/ensstat/enspost${ext_h}.t${ICYC}z.snowi
export CSNOWO=$DATA/pqsf  

echo "&namin"                                            >inputpqpf
echo "icyc=$ICYC"                                       >>inputpqpf
echo "cpgb='$CPGB',cpgi='$CPGI',cpge='$CPGO'"           >>inputpqpf
echo "crain='$CRAIN',craini='$CRAINI',craino='$CRAINO'" >>inputpqpf
echo "cfrzr='$CFRZR',cfrzri='$CFRZRI',cfrzro='$CFRZRO'" >>inputpqpf
echo "cicep='$CICEP',cicepi='$CICEPI',cicepo='$CICEPO'" >>inputpqpf
echo "csnow='$CSNOW',csnowi='$CSNOWI',csnowo='$CSNOWO'" >>inputpqpf
echo "/"                                                >>inputpqpf

cat inputpqpf

rm $CPGO $CRAINO $CFRZRO $CICEPO $CSNOWO

export pgm=global_enspqpf_grb1
. prep_step

startmsg
$enspqpf_grb1  <inputpqpf 
export err=$?;err_chk


########################################################################
# Script: ensadd_g2.sh
# ABSTRACT:  This scripts inserts ensemble PDS extensions for grib2 data 
########################################################################
#cd $DATA

set +x
echo " "
echo "Entering sub script ensadd_g2.sh"
echo "File 1 =$1"
echo "File 2 =$2"
echo "File 3 =$3"
echo "File 4 =$4"
echo " "
set -x

if [[ $# != 4 ]];then
  echo "Usage: $0 ienst iensi gribin gribout"
  echo " inserts ensemble PDS extensions in GRIB2 file"
  exit 1
fi

export pgm=global_ensadd
. prep_step

startmsg

eval $EXECgefs/$pgm <<EOF >> $pgmout 2> errfile
 &namin
 ienst=$1,iensi=$2,cpgb='$3',cpge='$4' /
EOF
export err=$?; err_chk

set +x
echo " "
echo "Leaving sub script ensadd_g2.sh"
echo " "
set -x

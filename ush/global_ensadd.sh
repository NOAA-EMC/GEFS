#######################################
# Script: ensadd.sh
# ABSTRACT:  This scripts produces a gribindex of 
#            files.  Theses files are then feed 
#            into the executable ensaddx 
#######################################
cd $DATA

set +x
echo " "
echo "Entering sub script  ensadd.sh"
echo "File 1 =$1"
echo "File 2 =$2"
echo "File 3 =$3"
echo "File 4 =$4"
echo "File 5 =$5"
echo " "
set -x

if [[ $# != 5 ]];then
  echo "Usage: $0 ienst iensi gribin indexin gribout"
  echo "       inserts ensemble PDS extensions in GRIB file"
  exit 1
fi

$GRBIDX $3 $4

export pgm=global_ensadd
. prep_step

startmsg
#DHOU, 01/21/2013 , For prod, EXECGLOBAL=EXECgefs, should be no problem
#eval $EXECGLOBAL/global_ensadd <<EOF >> $pgmout 2> errfile
eval $EXECgefs/global_ensadd <<EOF >> $pgmout 2> errfile
 &namin
 ienst=$1,iensi=$2,cpgb='$3',cpgi='$4',cpge='$5' /
EOF
export err=$?; err_chk

set +x
echo " "
echo "Leaving sub script  ensadd.sh"
echo " "
set -x

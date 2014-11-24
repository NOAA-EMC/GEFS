#######################################
# Child script: ensppf_grb1.sh
# ABSTRACT:  This script produces a gribindex of
#            files
#            Theses files are then feed into the
#            executable ensaddx.$xc
#
#######################################
set +x
echo " "
echo "Entering sub script  ensppf_grb1.sh"
echo " "
set -x

cd $DATA

if [[ $# != 3 ]]
then
   echo "Usage: $0 gribin indexin gribout"
   echo "       inserts ensemble PDS extensions in GRIB file"
   exit 1
fi

$GRBIDX $1 $2

export pgm=global_ensppf_grb1
. prep_step

startmsg
#eval $EXECGLOBAL/global_ensppf_grb1 <<EOF 2>/dev/null
#DHOU 03/26/2012 for Zeus
eval $EXECgefs/global_ensppf_grb1 <<EOF 2>/dev/null
 &namin
 cpgb='$1',cpgi='$2',cpge='$3' /
EOF
export err=$?; err_chk

set +x
echo "         ======= END OF ENSPPF PROCESS  ========== "
echo " "
echo "Leaving sub script  ensppf_grb1.sh"
echo " "
set -x

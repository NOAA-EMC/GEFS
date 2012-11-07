#######################################
# Script:    ensprcpcv.sh
# ABSTRACT:  This script produces a gribindex of
#            files.  Theses files are then feed 
#            into the executable ensprcpcv
#######################################

set +x
echo " "
echo "Entering sub script  ensprcpcv.sh"
echo " "
echo "     ==== START TO PRCPCV PROCESS ====="
echo " "
set -x

if [ $# -lt 3 ]; then
   echo "Usage: $0 gribin indexin gribout"
   echo "       convert precipitation to every 12 hrs accumulation"

   exit 1
fi

cd $DATA

$GRBIDX $1 $2

export pgm=global_prcpcv
. prep_step

startmsg
eval $EXECGLOBAL/global_ensprcpcv <<EOF 2>/dev/null                
 &namin
 cpgb='$1',cpgi='$2',cpge='$3' /
EOF
export err=$?;err_chk

set +x
echo " "
echo "Leaving sub script  ensprcpcv.sh"
echo " "
set -x

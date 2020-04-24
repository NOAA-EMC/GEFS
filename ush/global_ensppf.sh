#######################################
# Child script: ensppf.sh
# ABSTRACT:  This script produces a gribindex of
#            files
#            Theses files are then feed into the
#            executable ensaddx.$xc
#
#######################################
set +x
echo " "
echo "Entering sub script  ensppf.sh"
echo " "
set -x

cd $DATA

if [[ $# != 3 ]]; then
	echo "Usage: $0 gribin gribout npert"
	exit 1
fi

#$GRBIDX $1 $2

export pgm=global_ensppf
#$USHutil/prep_step

startmsg
#eval $EXECGLOBAL/global_ensppf <<EOF 2>/dev/null
#DHOU 03/26/2012 for Zeus
eval $EXECgefs/global_ensppf <<-EOF 2>/dev/null
	&namin
	cpgb='$1',cpge='$2',npert=$3 /
	EOF
export err=$?; err_chk

set +x
echo "         ======= END OF ENSPPF PROCESS  ========== "
echo " "
echo "Leaving sub script  ensppf.sh"
echo " "
set -x

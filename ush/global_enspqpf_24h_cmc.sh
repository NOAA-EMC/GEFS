#######################################
# Child script: enspqpf_24h_cmc.sh
# ABSTRACT:  This script produces CMC's ensemble based
#            Probabilistic quantitative precipitation forecast (PQPF)
#######################################
set +x
echo " "
echo "Entering sub script enspqpf_24h_cmc.sh"
echo " "
set -x

if [[ $# != 2 ]]
then
   echo "Usage: $0 gribin gribout"
   exit 1
fi

export pgm=$EXECGLOBAL/global_enspqpf_24h_cmc
. prep_step

startmsg
eval $pgm <<EOF >> $pgmout 2>errfile
 &namin
 cpgb='$1',cpge='$2' /
EOF
export err=$?; err_chk

set +x
echo "         ======= END OF ENSPPF PROCESS  ========== "
echo " "
echo "Leaving sub script enspqpf_24h_cmc.sh"
echo " "
set -x

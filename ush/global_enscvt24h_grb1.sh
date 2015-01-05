######################## CALLED BY EXENSCQPF ##########################
echo "------------------------------------------------"
echo "Ensemble CQPF -> globa_enspvrfy.sh -> global_enscvt24h.sh  "
echo "------------------------------------------------"
echo "History: Feb 2004 - First implementation of this new script."
echo "History: Feb 2006 - 2nd   implementation of this new script."
echo "AUTHOR: Yuejian Zhu (wx20yz)"

IYMDH=$1             

IYMD=`echo $IYMDH | cut -c1-8`
IYMDHM12=`$NDATE -12 $IYMDH `
IYMDM1=`$NDATE -24 $IYMDH | cut -c1-8`

if [ -s $COMIN/gefs.$IYMD/00/ensstat/enspost.t00z.prcp ]; then
 cat $COMIN/gefs.$IYMD/00/ensstat/enspost.t00z.prcp    >precip.$IYMDH
 $USHGLOBAL/global_enscvprcp.sh precip.$IYMDH precipi.$IYMDH precipt.$IYMDH 
 mv precipt.$IYMDH precip.$IYMDH
else
 echo " No either $COMIN/gefs.$IYMD/00/ensstat/enspost.t00z.prcp"
 echo " Missing preciptation data detected, quit "
 #export err=8;err_chk
fi

echo "         ######################################### "
echo "         **** We start to make 24 hour acc prcip** "
echo "         ****        Please  wait !!!!!!!      *** "
echo "         ######################################### "

$GBINDX precip.$IYMDH precipi.$IYMDH 

cat <<namEOF >input_c24h
&namin
cpgb='precip.$IYMDH',
cpgi='precipi.$IYMDH',
pgb71='qpf_gfs.$IYMDH',
pgb72='qpf_ctl.$IYMDH',
/
namEOF

export pgm=global_enscqpf
. prep_step

startmsg

$EXECGLOBAL/global_enscvt24h <input_c24h >> $pgmout 2>errfile
#export err=$?;err_chk


######################## CALLED BY EXENSCQPF ##########################
echo "------------------------------------------------"
echo "Ensemble CQPF -> globa_enspvrfy.sh -> global_enscvt24h.sh  "
echo "------------------------------------------------"
echo "History: Feb 2004 - First implementation of this new script."
echo "History: Feb 2006 - 2nd   implementation of this new script."
echo "AUTHOR: Yuejian Zhu (wx20yz)"
echo "History: Nov 2014 - Grib2 code conversion"
echo "AUTHOR: Yan Luo (wx22lu)"

IYMDH=$1             

IYMD=$(echo $IYMDH | cut -c1-8)
IYMDHM12=$($NDATE -12 $IYMDH )
IYMDM1=$($NDATE -24 $IYMDH | cut -c1-8)

if [ -s $COMIN/gefs.$IYMD/00/atmos/ensstat/enspost_grb2.t00z.prcp ]; then
 cat $COMIN/gefs.$IYMD/00/atmos/ensstat/enspost_grb2.t00z.prcp    >precip.$IYMDH
 $USHgefs/global_enscvprcp.sh precip.$IYMDH precipt.$IYMDH $IYMDH
 mv precipt.$IYMDH precip.$IYMDH
else
 echo " No either $COMIN/gefs.$IYMD/00/atmos/ensstat/enspost.t00z.prcp"
 echo " Missing preciptation data detected, quit "
 #export err=8;err_chk
fi

echo "         ######################################### "
echo "         **** We start to make 24 hour acc prcip** "
echo "         ****        Please  wait !!!!!!!      *** "
echo "         ######################################### "

cat <<namEOF >input_c24h_$IYMDH
&namin
cpgb='precip.$IYMDH',
pgb71='qpf_gfs.$IYMDH',
pgb72='qpf_ctl.$IYMDH',
/
namEOF

export pgm=global_enscvt24h
. prep_step

startmsg

$EXECgefs/global_enscvt24h <input_c24h_$IYMDH >> $pgmout 2>errfile
#export err=$?;err_chk


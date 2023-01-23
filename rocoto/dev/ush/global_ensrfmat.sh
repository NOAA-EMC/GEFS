######################### CALLED BY EXENSCQPF ##########################
echo "------------------------------------------------"
echo "Ensemble CQPF - global_ensrfmat.sh              "
echo "------------------------------------------------"
echo "History: Feb 2004 - First implementation of this new script."
echo "AUTHOR: Yuejian Zhu (wx20yz)"

echo "         ######################################### "
echo "         ####  CONVERT RFC DATA FORMAT !!!!!  #### "
echo "         ####  CONVERT RFC DATA FORMAT !!!!!  #### "
echo "         ####  CONVERT RFC DATA FORMAT !!!!!  #### "
echo "         ######################################### "

cp $FIXgefs/ingest_nwsli.uniq .
cat <<namEOF >input_reformat 
&namin
iymd=$OBSYMD,
fname1='ingest_nwsli.uniq',
fname2='rfc24-uniq-early',
fname3='usa-dlyprcp-$OBSYMD',
/
namEOF

if [ -s usa-dlyprcp-$OBSYMD ]; then
 rm usa-dlyprcp-$OBSYMD
fi

export pgm=global_ensrfmat
. prep_step

startmsg
$EXECgefs/global_ensrfmat <input_reformat >> $pgmout 2>errfile
export err=$?;err_chk


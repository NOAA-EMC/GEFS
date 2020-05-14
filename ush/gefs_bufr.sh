#!/bin/ksh
#
#  UTILITY SCRIPT NAME :  gefsbufr.sh from gfs_bufr.sh
#               AUTHOR :  Hua-Lu Pan
#         DATE WRITTEN :  02/03/97
#
#  Abstract:  This utility script produces BUFR file of
#             station forecasts from the GEFS suite.
#
#     Input:  none
# Script History Log:
# 2016-10-30  H Chuang: Tranistion to read nems output.
#             Change to read flux file fields in gfs_bufr
#             so remove excution of gfs_flux
# 2018-03-22 Guang Ping Lou: Making it works for either 1 hourly or 3 hourly output
# 2018-05-22 Guang Ping Lou: Making it work for both GFS and FV3GFS 
# 2018-05-30  Guang Ping Lou: Make sure all files are available.

echo "$(date -u) begin ${.sh.file}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

if [ "$F00FLAG" = "YES" ]; then
	f00flag=".true."
else
	f00flag=".false."
fi

export pgm=gfs_bufr
#. prep_step

if [ "$MAKEBUFR" = "YES" ]; then
	bufrflag=".true."
else
	bufrflag=".false."
fi

if [ -s ${COMIN}/$COMPONENT/sfcsig/${RUNMEM}.${cycle}.sfcf000.nemsio ]; then
	SFCF="sfc"
	CLASS="class1fv3"
else
	SFCF="flx"
	CLASS="class1"
fi 
cat <<- EOF > gfsparm
	&NAMMET
		iromb=0,maxwv=$JCAP,levs=$LEVS,makebufr=$bufrflag,
		dird="$COMOUT/$COMPONENT/bufr/$mem/bufr",
		nstart=$FSTART,nend=$FEND,nint=$FINT,
		nend1=$NEND1,nint1=$NINT1,nint3=$NINT3,
		nsfc=80,f00=$f00flag,
	/
	EOF

hh=$(printf %02i $FSTART)
# hh=$FSTART
# if [ $hh -lt 100 ]; then
#     hh1=$(echo "${hh#"${hh%??}"}")
#     hh=$hh1
# fi

SLEEP_LOOP_MAX=$(($SLEEP_TIME / $SLEEP_INT))

while [ $hh -le $FEND ]; do
	hh3=$(printf %03i $hh)

	#---------------------------------------------------------
	# Make sure all files are available:
	ic=0
	while [ $ic -lt $SLEEP_LOOP_MAX ]; do
		fcstchk=$COMIN/$COMPONENT/sfcsig/${RUNMEM}.${cycle}.logf${hh3}.nemsio
		if [ ! -f $fcstchk ]; then
			sleep $SLEEP_INT
			ic=$(($ic + 1))
		else
			break
		fi        

		if [ $ic -ge SLEEP_LOOP_MAX ]; then
			echo <<- EOF
				FATAL ERROR in ${.sh.file}: Unable to find forecast output $fcstchk at $(date -u) after waiting ${SLEEP_TIME}s!
				EOF
			export err=6
			err_chk
			exit $err
		fi
	done
	#------------------------------------------------------------------
	ln -sf $COMIN/$COMPONENT/sfcsig/${RUNMEM}.${cycle}.atmf${hh3}.nemsio sigf${hh}
	ln -sf $COMIN/$COMPONENT/sfcsig/${RUNMEM}.${cycle}.${SFCF}f${hh3}.nemsio flxf${hh}

	hh=$(printf %02i $((10#$hh + $FINT)))
done

#  define input BUFR table file.
# prep_step
ln -sf $PARMbufrsnd/bufr_gfs_${CLASS}.tbl fort.1
ln -sf ${STNLIST:-$PARMbufrsnd/bufr_stalist.meteo.gfs} fort.8

$APRUN $EXECbufrsnd/gfs_bufr < gfsparm > out_gfs_bufr_$FEND
export err=$?

if [[ $err != 0 ]]; then
	echo "FATAL ERROR in ${.sh.file}: gfs_bufr failed!"
	err_chk
	exit $err
fi

exit $err


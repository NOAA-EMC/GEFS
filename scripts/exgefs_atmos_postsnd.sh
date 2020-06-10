#!/bin/ksh
################################################################
# Script Name:      exgfs_postsnd.sh.sms
# Script Description:   Generate GFS BUFR sounding files
# Script History Log:
#   1) 2003-03-25       Hualu Pan       First Implementation
#   2) 2010-05-25       V. Krishna Kumar Modified for the GFS 
#                                  resolution upgrade
#   3) 2014-08-01       D. Carlis Updated to vertical structure 
#                                 and T1534 Resolution 
#   4) 2016-11-01       H. Chuang Update to read new model nems output
#   5) 2017-02-21       Guang Ping Lou setup mpmd to speedup the run
#                                 and 1 & 3 hourly output
#   6) 2018-03-22       Guang Ping Lou  Take FV3GFS configuration
#                          parameters as input; make it work for 
#                          both FV3GFS and GFS
#   7) 2018-07-18       Guang Ping Lou Generalize this version to other platforms
#   8) 2019-05-27       Bo Cui modify to generate GEFS BUFR sounding files
#   9) 2019-06-05       Xianwu Xue reformatted it and add checked the errors        
################################################################
echo "$(date -u) begin ${.sh.file}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

cd $DATA

###################################################
## Run meteogram generator for T574
###################################################
export FHOUT=$FHOUTLF

export JCAP=${JCAP:-766}
export LEVS=${LEVS:-64}
export LATB=${LATB:-768}
export LONB=${LONB:-1536}
export NEND1=${NEND1:-180} #$FHMAXHF ##first period length with time interval = NINT1
export NINT1=$FHOUTHF        ##first period time interval
export NINT3=$FHOUTLF        ##second period time interval

export STARTHOUR=${STARTHOUR:-00}
export ENDHOUR=${ENDHOUR:-180}
if (( ENDHOUR > fhmaxh )); then
	export ENDHOUR=$fhmaxh
fi
if (( NEND1 >= ENDHOUR )); then
	export NEND1=$ENDHOUR
fi
if (( NEND1 >= FHMAXHF )); then
	export NEND1=$FHMAXHF
fi

export NZERO=6
export INCREMENT=${INCREMENT:-12}
export OUTFILE=meteomrf
export MAKEBUFR=NO
export MODEL_OUT_FORM=binarynemsiompiio

export mem=$(echo $RUNMEM|cut -c3-5)

if [[ $SENDCOM == "YES" ]]; then
	mkdir -p $COMOUT/$COMPONENT
	mkdir -p $COMOUT/$COMPONENT/wmo
	mkdir -p $COMOUT/$COMPONENT/gempak

	mkdir -m 775 -p $COMOUT/$COMPONENT/bufr/$mem
fi # [[ $SENDCOM == "YES" ]]

### Loop for the hour and wait for the sigma and surface flux file:
export FSTART=$STARTHOUR

SLEEP_LOOP_MAX=$(($SLEEP_TIME / $SLEEP_INT))

while [ $FSTART -lt $ENDHOUR ]; do
	export FINT=$NINT1
	# Define the end hour for the input
	export FEND=$(printf %03i $((10#$FSTART + $INCREMENT)))
	if [ $FSTART -eq 00 ]; then 
		export F00FLAG=YES
	else
		export F00FLAG=NO
	fi
   
	if [ $FEND -eq $ENDHOUR ]; then
		export MAKEBUFR=YES
	fi

	ic=0
	while [ $ic -lt $SLEEP_LOOP_MAX ]; do
		fcstchk=$COMIN/$COMPONENT/sfcsig/${RUNMEM}.${cycle}.logf$FEND.nemsio
		if [ ! -f $fcstchk ]; then
			ic=$(($ic + 1))
			sleep $SLEEP_INT
		else
			break
		fi

		if [ $ic -ge $SLEEP_LOOP_MAX ]; then
			echo <<- EOF
				FATAL ERROR in ${.sh.file}: Unable to find forecast output $fcstchk at $(date -u) after waiting ${SLEEP_TIME}s!
				EOF
			export err=5
			err_chk
			exit $err
		fi
	done

	## 1-hourly output before $NEND1, 3-hourly output after
	if [ $FEND -gt $NEND1 ]; then
		export FINT=$NINT3
	fi
	$USHgefs/gefs_bufr.sh
	export err=$?
	if [[ $err != 0 ]]; then
		echo "FATAL ERROR in ${.sh.file}: gefs_bufr failed for f$FSTART!"
		err_chk
		exit $err
	fi

	export FSTART=$FEND
done

##############################################################
# Tar and gzip the individual bufr files and send them to /com
##############################################################
if [[ $SENDCOM == "YES" ]]; then
	cd ${COMOUT}/$COMPONENT/bufr/${mem}
	tar -cf - bufr.* | /usr/bin/gzip > ../${RUNMEM}.${cycle}.bufrsnd.tar.gz
fi
cd $DATA

########################################
# Send the single tar file to OSO
########################################
if [ "$SENDDBN" = 'YES' ]; then
	$DBNROOT/bin/dbn_alert MODEL GEFS_${RUNMEM}_BUFRSND_TAR $job \
	$COMOUT/$COMPONENT/bufr/${RUNMEM}.${cycle}.bufrsnd.tar.gz
fi

########################################
# Create Regional Collectives of BUFR data and 
# add appropriate WMO Headers.
########################################
collect=' 1 2 3 4 5 6 7 8 9'

rm -rf mpmd_cmdfile
echo "$USHgefs/gefs_bfr2gpk.sh " >> mpmd_cmdfile
for m in ${collect}; do
	echo "$USHgefs/gefs_sndp.sh $m " >> mpmd_cmdfile
done

cat mpmd_cmdfile
chmod 775 mpmd_cmdfile
export MP_CMDFILE=${DATA}/mpmd_cmdfile
export SCR_CMDFILE=$MP_CMDFILE  # Used by mpiserial on Theia
export MP_PGMMODEL=mpmd

#############################################################
# Execute the script
$APRUN_MPMD
export err=$?

if [[ $err != 0 ]]; then
	echo "FATAL ERROR in ${.sh.file}: One or more BUFR regions in $MP_CMDFILE failed!"
	exit $err
fi
#############################################################

echo "$(date -u) end ${.sh.file}"

exit $err



#! /bin/ksh

echo "$(date -u) begin ${0}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

export FORECAST_SEGMENT=${FORECAST_SEGMENT:-hr}

if [[ $FORECAST_SEGMENT = hr ]] ; then
    LEVS=$LEVSHR 
elif [[ $FORECAST_SEGMENT = lr ]]; then
    LEVS=$LEVSLR 
else
	echo "FATAL ERROR in ${0}: FORECAST_SEGMENT ${FORECAST_SEGMENT} is not supported!"
	export err=9
	exit $err
fi

export SHOUR=${SHOUR:-00}
export FHOUR=${FHOUR:-180}
if (( FHOUR > fhmaxh )); then
	export FHOUR=$fhmaxh
fi
export FHOUT_HF=${FHOUTHF:-3}
export FHOUT_LF=${FHOUTLF:-6}
export FHMAX_HF=${FHMAXHF:-240}

export GRIBVERSION=${GRIBVERSION:-grib2}
export nemsioget=$EXECgfs/nemsio_get
export ensavg_nemsio_log=$DATA/ensavg_nemsio.$FORECAST_SEGMENT.log

#############################################################
# Execute the script
$HOMEgefs/ush/gefs_ensavg_nemsio.sh $DATA $SHOUR $FHOUT_HF $FHOUT_LF $FHMAXHF $FHOUR $ensavg_nemsio_log
export err=$?
if [[ $err != 0 ]]; then
	echo "FATAL ERROR in ${0}: gefs_ensavg_nemsio.sh returned a non-zero value!"
	exit $err
fi

#############################################################
# SENDCOM
if [[ $SENDCOM == "YES" ]]; then
	if [ ! -d $COMOUT/$COMPONENT/misc ]; then
		mkdir -m 775 -p $COMOUT/$COMPONENT/misc
	fi
	mv $ensavg_nemsio_log $COMOUT/$COMPONENT/misc/ensavg_nemsio
fi
#############################################################

echo "$(date -u) end ${0}"

exit $err

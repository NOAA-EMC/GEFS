#! /usr/bin/env bash

echo "$(date -u) begin ${BASH_SOURCE}"

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
  echo "FATAL ERROR in ${BASH_SOURCE}: FORECAST_SEGMENT ${FORECAST_SEGMENT} is not supported!"
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

export ensavg_netcdf_log=$DATA/ensavg_netcdf.$FORECAST_SEGMENT.log

#export mem_ens="avg"
#mkdir -p ${COMOUT}/${mem_ens}/atmos
export mem_ens="avg"
export COMOUT=${COMOUT}/${mem_ens}

mkdir -p ${COMOUT}/${COMPONENT}
#############################################################
# Execute the script
$HOMEgefs/ush/gefs_ensavg_netcdf.sh $DATA $SHOUR $FHOUT_HF $FHOUT_LF $FHMAXHF $FHOUR $ensavg_netcdf_log
export err=$?
if [[ $err != 0 ]]; then
  echo "FATAL ERROR in ${BASH_SOURCE}: gefs_ensavg_netcdf.sh returned a non-zero value!"
  exit $err
fi

#############################################################
# SENDCOM
if [[ $SENDCOM == "YES" ]]; then
  if [ ! -d ${COMOUT}/${COMPONENT}/misc ]; then
    mkdir -m 775 -p ${COMOUT}/${COMPONENT}/misc
  fi
  mv $ensavg_netcdf_log ${COMOUT}/${COMPONENT}/misc/ensavg_netcdf
fi
#############################################################

echo "$(date -u) end ${BASH_SOURCE}"

exit $err

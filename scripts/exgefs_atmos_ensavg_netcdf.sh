#! /usr/bin/env bash

source "${HOMEgfs:-${HOMEgefs}}/ush/preamble.sh"

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

export mem_ens="avg"
export COMOUT=${COMOUT}/${mem_ens}

MEMDIR="ensstat" YMD=${PDY} HH=${cyc} generate_com -rx COM_ATMOS_HISTORY_ENSAVG:COM_ATMOS_HISTORY_TMPL

mkdir -p ${COM_ATMOS_HISTORY_ENSAVG}

#############################################################
# Execute the script
${HOMEgefs}/ush/gefs_ensavg_netcdf.sh ${DATA} ${SHOUR} ${FHOUT_HF} ${FHOUT_LF} ${FHMAXHF} ${FHOUR}
export err=$?
if [[ $err != 0 ]]; then
  echo "FATAL ERROR in ${BASH_SOURCE[0]}: gefs_ensavg_netcdf.sh returned a non-zero value!"
  exit $err
fi

exit ${err}

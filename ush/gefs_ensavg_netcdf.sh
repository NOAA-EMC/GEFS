#! /usr/bin/env bash

source "${HOMEgfs:-${HOMEgefs}}/ush/preamble.sh"

echo DATA=$DATA

############################################################
#  Define Variables:
#  -----------------
#  SHOUR        is the starting forecast hour. normally 0 except for restarts.
#  FHOUR        is the ending forecast hour.
#  FHINC        is the increment hour for each forecast steps.
#  fhr          is the current forecast hour.
#  SLEEP_TIME   is the number of seconds to sleep before exiting with error.
#  SLEEP_INT    is the number of seconds to sleep between restrt file checks.
############################################################

export jobdir="${1}"
export SHOUR="${2}"
export FHOUTHF="${3}"
export FHOUTLF="${4}"
export FHMAXFH="${5}"
export FHOUR="${6}"
cd ${jobdir}

export CASE=${CASE:-384}
ntiles=${ntiles:-6}

# Utilities
GETATMENSMEANEXEC=${GETATMENSMEANEXEC:-$HOMEgsi/exec/getsigensmeanp_smooth.x}
GETSFCENSMEANEXEC=${GETSFCENSMEANEXEC:-$HOMEgsi/exec/getsfcensmeanp.x}

SLEEP_LOOP_MAX=$(($SLEEP_TIME / $SLEEP_INT))

# Compute ensemble mean 

# Remove control member from list
memberlist=$(echo ${memberlist} | sed -e 's/000//g')
echo "memberlist=${memberlist}"

${NCP} ${GETATMENSMEANEXEC} ${DATA}
${NCP} ${GETSFCENSMEANEXEC} ${DATA}

FHINC=${FHOUTHF}
fhr=${SHOUR}
while [[ $fhr -le ${FHOUR} ]]; do
  fhr=$(printf %03i ${fhr})
  #fhr0=$(printf %i 10#${fhr})
  logfile="${COM_ATMOS_HISTORY_ENSAVG}/gefs.${cycle}.logf${fhr}.txt"
  if [[ -f ${logfile} ]]; then
    echo "netcdf average file ${logfile} exists, skipping."
    if [ ${fhr} -ge ${FHMAXFH} ]; then
      FHINC=${FHOUTLF}
    fi
    fhr=$( expr ${fhr} + ${FHINC} )
    continue
  fi

  nfile=${npert}
  for mem in $memberlist; do
    MEMDIR="mem${mem}" YMD=${PDY} HH=${cyc} generate_com INDIR:COM_ATMOS_HISTORY_TMPL
    ic=0
    while [ $ic -le $SLEEP_LOOP_MAX ]; do
      if [ -f  ${INDIR}/gefs.${cycle}.logf${fhr}.txt ]; then
        $NLN ${INDIR}/gefs.${cycle}.atmf${fhr}.nc ./atm_mem${mem}
        $NLN ${INDIR}/gefs.${cycle}.sfcf${fhr}.nc ./sfc_mem${mem}
        break
      else
        ic=$(($ic + 1))
        sleep $SLEEP_INT
      fi
      if [ $ic -eq $SLEEP_LOOP_MAX ]; then
        (( nfile = nfile - 1 ))
        echo <<- EOF
					WARNING: ${job} could not find forecast $mem at $(date -u) after waiting ${SLEEP_TIME}s
						Looked for the following files:
							Log file: ${INDIR}/${CDUMP}.${cycle}.logf${fhr}.txt
							Atm file: ${INDIR}/${CDUMP}.${cycle}.atmf${fhr}.nc
							Sfc file: ${INDIR}/${CDUMP}.${cycle}.sfcf${fhr}.nc
					EOF
				msg="WARNING: ${job} was unable to find $mem; will continue but mean may be degraded!"
        echo "$msg" | mail.py -c $MAIL_LIST
      fi # [ $ic -eq $SLEEP_LOOP_MAX ]
    done
  done
  if [ $nfile -le 1 ]; then
    echo <<- EOF
			FATAL ERROR in ${BASH_SOURCE}: Not enough forecast files available to create average at hour $fhr!
			EOF
    export err=1
    ${ERRSCRIPT}
    exit $err
  fi # [ $ic


  if [[ $SENDCOM == "YES" ]]; then
    $NLN ${COM_ATMOS_HISTORY_ENSAVG}/${CDUMP_ENS}.${cycle}.atmf${fhr}.nc ./atm_ensmean
    $NLN ${COM_ATMOS_HISTORY_ENSAVG}/${CDUMP_ENS}.${cycle}.sfcf${fhr}.nc ./sfc_ensmean
  fi
  $APRUN ${DATA}/$(basename ${GETATMENSMEANEXEC}) ./ atm_ensmean atm ${nfile}
  export err=$?

  if [[ $err != 0 ]]; then
    echo "FATAL ERROR in ${BASH_SOURCE}: $(basename $GETATMENSMEANEXEC) failed for f${fhr}!"
    $ERRSCRIPT
    exit $err
  fi

  $APRUN ${DATA}/$(basename ${GETSFCENSMEANEXEC}) ./ sfc_ensmean sfc ${nfile}
  export err=$?

  if [[ $err != 0 ]]; then
    echo "FATAL ERROR in ${BASH_SOURCE}: $(basename $GETSFCENSMEANEXEC) failed for f${fhr}!"
    $ERRSCRIPT
    exit $err
  fi

  export err=$?
  ${ERRSCRIPT} || exit ${err}
  if [[ ${SENDCOM} == "YES" ]]; then
    echo "completed fv3gfs average fhour= ${fhr}" > ${logfile}
  fi
  if [ ${fhr} -ge ${FHMAXFH} ]; then
    FHINC=${FHOUTLF}
  fi
  fhr=$( expr ${fhr} + ${FHINC} )
done

exit $err

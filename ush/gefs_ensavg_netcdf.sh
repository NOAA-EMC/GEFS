#! /usr/bin/env bash

echo "$(date -u) begin ${BASH_SOURCE}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
  # Turn on strict bash error checking
  set -eu
fi

echo DATA=$DATA

VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = "YES" ]; then
  echo $(date) EXECUTING ${BASH_SOURCE} $* >&2
  set -x
fi

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
export ensavg_netcdf_log="${7}"
cd $jobdir

export CASE=${CASE:-384}
ntiles=${ntiles:-6}

# Utilities
NCP=${NCP:-"/bin/cp -p"}
NLN=${NLN:-"/bin/ln -sf"}
NMV=${NMV:-"/bin/mv -uv"}

GETATMENSMEANEXEC=${GETATMENSMEANEXEC:-$HOMEgsi/exec/getsigensmeanp_smooth.x}
GETSFCENSMEANEXEC=${GETSFCENSMEANEXEC:-$HOMEgsi/exec/getsfcensmeanp.x}

SLEEP_LOOP_MAX=$(($SLEEP_TIME / $SLEEP_INT))

# Compute ensemble mean 

# Remove control member from list
memberlist=$(echo $memberlist | sed -e 's/c00//g')
echo "memberlist=$memberlist"

$NCP $GETATMENSMEANEXEC $DATA
$NCP $GETSFCENSMEANEXEC $DATA

CDUMP_ENS=gefs
OUTDIR=${COMOUT}/${COMPONENT}

FHINC=$FHOUTHF
fhr=$SHOUR
while [[ $fhr -le $FHOUR ]]; do
  fhr=$(printf %03i ${fhr})
  #fhr0=$(printf %i 10#${fhr})
  logfile="${OUTDIR}/${CDUMP_ENS}.${cycle}.logf${fhr}.txt"
  if [[ -f $logfile ]]; then
    echo "netcdf average file $logfile exists, skipping."
    if [ $fhr -ge $FHMAXFH ]; then
      FHINC=$FHOUTLF
    fi
    fhr=$( expr ${fhr} + ${FHINC} )
    continue
  fi

  nfile=$npert
  for mem in $memberlist; do
    mem2=$(echo $mem | cut -c2-)
    mem3=$(printf %03i ${mem2#0})
    CDUMP="gefs"
    INDIR=${COMIN}/${mem}/${COMPONENT}
    ic=0
    while [ $ic -le $SLEEP_LOOP_MAX ]; do
      if [ -f  ${INDIR}/${CDUMP}.${cycle}.logf${fhr}.txt ]; then
        $NLN ${INDIR}/${CDUMP}.${cycle}.atmf${fhr}.nc ./atm_mem${mem3}
        $NLN ${INDIR}/${CDUMP}.${cycle}.sfcf${fhr}.nc ./sfc_mem${mem3}
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
    $ERRSCRIPT
    exit $err
  fi # [ $ic


  if [[ $SENDCOM == "YES" ]]; then
    $NLN ${OUTDIR}/${CDUMP_ENS}.${cycle}.atmf${fhr}.nc ./atm_ensmean
    $NLN ${OUTDIR}/${CDUMP_ENS}.${cycle}.sfcf${fhr}.nc ./sfc_ensmean
  fi
  $APRUN ${DATA}/$(basename $GETATMENSMEANEXEC) ./ atm_ensmean atm $nfile
  export err=$?

  if [[ $err != 0 ]]; then
    echo "FATAL ERROR in ${BASH_SOURCE}: $(basename $GETATMENSMEANEXEC) failed for f${fhr}!"
    $ERRSCRIPT
    exit $err
  fi

  $APRUN ${DATA}/$(basename $GETSFCENSMEANEXEC) ./ sfc_ensmean sfc $nfile
  export err=$?

  if [[ $err != 0 ]]; then
    echo "FATAL ERROR in ${BASH_SOURCE}: $(basename $GETSFCENSMEANEXEC) failed for f${fhr}!"
    $ERRSCRIPT
    exit $err
  fi

  echo "f${fhr}_done --- $(date -u)" >> $ensavg_netcdf_log

  export err=$?
  $ERRSCRIPT || exit $err
  if [[ $SENDCOM == "YES" ]]; then
    echo "completed fv3gfs average fhour= $fhr" > $logfile
  fi
  if [ $fhr -ge $FHMAXFH ]; then
    FHINC=$FHOUTLF
  fi
  fhr=$( expr ${fhr} + ${FHINC} )
done

echo "$(date -u) end ${BASH_SOURCE}"

exit $err

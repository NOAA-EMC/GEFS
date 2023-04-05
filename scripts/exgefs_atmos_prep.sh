#! /usr/bin/env bash

echo "$(date -u) begin $(basename $BASH_SOURCE)"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
  # Turn on strict bash error checking
  set -eu
fi

export HOMEgfs=${HOMEgfs:-${HOMEgefs}}
export HOMEufs=${HOMEufs:-${HOMEgfs}}
export USHgfs=$HOMEgfs/ush
export FIXgfs=$HOMEgfs/fix

export CDUMP=$RUNMEM
mem=$(echo $RUNMEM|cut -c3-5)

cd $DATA

# Run scripts
#############################################################
$USHgefs/gefs_atmos_prep.sh $mem
export err=$?
if [[ $err != 0 ]]; then
	echo "FATAL ERROR in $(basename $BASH_SOURCE): atmos_prep failed for $RUNMEM!"
	exit $err
fi
#############################################################

echo "$(date -u) end $(basename $BASH_SOURCE)"

exit $err

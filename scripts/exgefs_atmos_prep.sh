#! /usr/bin/env bash

source "${HOMEgfs:-${HOMEgefs}}/ush/preamble.sh"

export HOMEgfs=${HOMEgfs:-${HOMEgefs}}
export HOMEufs=${HOMEufs:-${HOMEgfs}}
export USHgfs=$HOMEgfs/ush
export FIXgfs=$HOMEgfs/fix

export CDUMP=${RUNMEM}

cd $DATA

# Run scripts
#############################################################
$USHgefs/gefs_atmos_prep.sh ${RUNMEM}
export err=$?
if [[ $err != 0 ]]; then
	echo "FATAL ERROR in $(basename $BASH_SOURCE): atmos_prep failed for $RUNMEM!"
	exit $err
fi
#############################################################

echo "$(date -u) end $(basename $BASH_SOURCE)"

exit $err

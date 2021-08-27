#! /bin/bash

echo "$(date -u) begin $(basename ${0})"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

export HOMEgfs=${HOMEgfs:-${HOMEgefs}}
export HOMEufs=${HOMEufs:-${HOMEgfs}}
export USHgfs=$HOMEgfs/ush
export FIXgfs=$HOMEgfs/fix
export FIXfv3=${FIXfv3:-$FIXgfs/fix_fv3_gmted2010}
export FIXam=${FIXam:-$FIXgfs/fix_am}
export VCOORD_FILE=${VCOORD_FILE:-$FIXam/global_hyblev.l${LEVS}.txt}

mem=$(echo $RUNMEM|cut -c3-5)
sfc_mem=${sfc_mem:-"c00"}

cd $DATA

# Run scripts
#############################################################
$USHgefs/gefs_atmos_prep.sh $mem
export err=$?
if [[ $err != 0 ]]; then
	echo "FATAL ERROR in $(basename ${0}): atmos_prep failed for $RUNMEM!"
	exit $err
fi
#############################################################

echo "$(date -u) end $(basename ${0})"

exit $err

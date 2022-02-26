#!/bin/bash

echo "$(date -u) begin $(basename $BASH_SOURCE)"

if [[ ${STRICT:-NO} == "YES" ]]; then
    # Turn on strict bash error checking
    set -eu
fi

cd $DATA

MODCOM=$(echo ${NET}_${COMPONENT} | tr '[a-z]' '[A-Z]')
export DBN_ALERT_TYPE=${MODCOM}_GEMPAK
export fstart=0
grids='global.0p25'
grdID=$grids
DATAgrid=${DATA}
echo "$HOMEwave/scripts/exwave_nawips.sh $COMPONENTwave $DATAout >gempak.out 2>&1" >> poescript
env

cat poescript
chmod 775 poescript
export MP_CMDFILE=${DATA}/poescript
export SCR_CMDFILE=$MP_CMDFILE  # Used by mpiserial on Theia
export MP_PGMMODEL=mpmd
rm -f mpmd_cmdfile
ln -s $MP_CMDFILE mpmd_cmdfile

#############################################################
# Execute the script
$APRUN_MPMD
export err=$?

if [[ $err != 0 ]]; then
	echo "FATAL ERROR in $(basename $BASH_SOURCE): One or more gempak jobs in $MP_CMDFILE failed!"
	exit $err
fi
#############################################################

echo "$(date -u) end $(basename $BASH_SOURCE)"

exit $err

#!/bin/bash

echo "$(date -u) begin $(basename $BASH_SOURCE)"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
    # Turn on strict bash error checking
    set -eu
fi

cd $DATA
if [[ -s poescript ]]; then rm poescript; fi

################################################################
# Create a script to be poe'd
#
#  Note:  The number of scripts to be run MUST match the number
#  of total_tasks set in the ecf script, or the job will fail.
#
################################################################

for script in $(cat $FIXgempak/gefs_meta_${cyc}); do
    echo "$script" >> $DATA/poescript
done

num=$(cat $DATA/poescript |wc -l) 

# Add dummy lines to execute if there are fewer scripts than tasks
while [ $num -lt $total_tasks ] ; do
   echo "hostname" >> poescript
   num=$(($num + 1))
done

cat poescript

chmod 775 $DATA/poescript
export MP_PGMMODEL=mpmd
export MP_CMDFILE=$DATA/poescript
export SCR_CMDFILE=$MP_CMDFILE  # Used by mpiserial on Theia
rm -f mpmd_cmdfile
ln -s $MP_CMDFILE mpmd_cmdfile
# Execute the script.

#############################################################
# Execute the script
$APRUN_MPMD

export err=$?

if [[ $err != 0 ]]; then
    echo "FATAL ERROR in $(basename $BASH_SOURCE): One or more gempak scripts in $MP_CMDFILE failed!"
    exit $err
fi
#############################################################

echo "$(date -u) end $(basename $BASH_SOURCE)"

exit $err

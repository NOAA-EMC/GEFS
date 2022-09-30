#!/bin/ksh
#####################################################################
# -----------------------------------------------------
#  exgefs_post_cleanup.sh.sms
#  based on exglobal_post.sh.sms
#  Apr 99 - Michaud - Generated to post global forecast
#  Mar 03 - Zhu - Add post for 0.5x0.5 degree
#  Jul 05 - Wobus - 6-hour breeding, ensemble only
#  Jul 07 - Wobus - interpolate master post file
#  Jul 07 - Wobus - separate cleanup from rest of job
#  Jul 11 - Wobus - unified cleanup job
# -----------------------------------------------------
#####################################################################

echo "$(date -u) begin ${.sh.file}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

if [[ $member == "c00" ]]; then
    if [[ $cycle == "t06z"]]; then
        fhsave="${fhsave} f012"
    fi
fi

export MP_LABELIO=YES

fhr=$SHOUR
export fhr

############################################################
# Loop Through the Post Forecast Files
############################################################
while [[ $fhr -le $FHOUR ]]; do
	ffhr="f$(printf %03i $fhr)"

	echo "$(date) $ffhr begin"

	if [[ "$SENDCOM" = "YES" ]]; then
		####################################
		# Remove nemsio fcst and sflux files
		####################################
                echo $fhsave |grep $ffhr
                err=$?
		if [[ $err -ne 0 ]]; then
			rm $COMOUT/$COMPONENT/sfcsig/ge$member.$cycle.atm${ffhr}.nemsio
			rm $COMOUT/$COMPONENT/sfcsig/ge$member.$cycle.sfc${ffhr}.nemsio
		fi # [[ $fhr -ne $hrsave ]]
	fi # [[ "$SENDCOM" = "YES" ]]

	if (( fhr < FHMAXHF )); then
		(( fhr = fhr + FHOUTHF ))
	else
		(( fhr = fhr + FHOUTLF ))
	fi
done # [[ $fhr -le $FHOUR ]]

echo "$(date -u) end ${.sh.file}"

exit 0

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

set -x

export MP_LABELIO=YES

############################################################
#  Define Variables:
#  -----------------
#  SHOUR        is the starting forecast hour. normally 0 except for restarts.
#  FHOUR        is the ending forecast hour.
#  FHINC        is the increment hour for each forecast steps.
#  FH           is the current forecast hour.
#  SLEEP_TIME   is the number of seconds to sleep before exiting with error.
#  SLEEP_INT    is the number of seconds to sleep between restrt file checks.
############################################################

fhr=$SHOUR
export fhr

############################################################
# Loop Through the Post Forecast Files
############################################################
while [[ $fhr <= $FHOUR ]]; do
	ffhr="f$(printf %03i $fhr)"

	echo "$(date) $ffhr begin"

	if [[ "$SENDCOM" = "YES" ]]; then
		####################################
		# Remove nemsio fcst and sflux files
		####################################
		if [[ $fhr > $fhsave ]]; then
			rm $COMOUT/$COMPONENT/sfcsig/ge$member.$cycle.atm${ffhr}.nemsio
			rm $COMOUT/$COMPONENT/sfcsig/ge$member.$cycle.sfc${ffhr}.nemsio
		fi # [[ $fhr > $fhsave ]]
	fi # [[ "$SENDCOM" = "YES" ]]

	if (( fhr < FHMAXHF )); then
		(( fhr = fhr + FHOUTHF ))
	else
		(( fhr = fhr + FHOUTLF ))
	fi
done # [[ $fhr -le $FHOUR ]]

echo "$(date -u) end ${.sh.file}"

exit 0

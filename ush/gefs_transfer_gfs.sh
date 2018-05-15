#!/bin/ksh

#####################################################################
echo "-----------------------------------------------------"
echo " Script: global_transfer.sh" 
echo " "
echo " Purpose - Copy Global Posts to /nwges and /com"
echo "           Alert posted files to DBNet"
echo " "
echo " History - "
echo "    Facey   - 9/10/96 - Implemented on C90"
echo "    Michaud - 6/03/99 - Converted to IBM SP"
echo "    Wobus   - 8/25/03 - Changes for Ens 4cyc x 180hr "
echo "    Wobus   - 6/08/05 - Changes for 6-hour breeding "
echo "    Wobus   - 2/06/06 - version for gfs in ensemble "
echo "    Hou   - X/XX/15 - processing both grib2 and grib1 files, plus 0p5 grid "
echo "    Meng    - 03/14/17 - Remove saving and alerting pgrb files "
echo "-----------------------------------------------------"
#####################################################################
set -xa

GRBDIR2=$1
GRBFIL2=$2

if test "$SENDCOM" = "YES"; then
	#
	# Save Pressure GRIB/GRIB Index files
	#
	#cp pgb2afile $COMOUT/$cyc/$GRBDIR2/ge${RUN}.${cycle}.$GRBFIL2\f$pgfhr
	cp pgb2afile $COMOUT/$cyc/$GRBDIR2/ge${RUNMEM}.${cycle}.$GRBFIL2\f${pgfhr}${filetail}
	if [[ "$makegrb2i" = "yes" ]]; then
		#cp pgb2aifile $COMOUT/$cyc/$GRBDIR2/ge${RUN}.${cycle}.$GRBFIL2\f${pgfhr}.idx
		cp pgb2aifile $COMOUT/$cyc/$GRBDIR2/ge${RUNMEM}.${cycle}.$GRBFIL2\f${pgfhr}${filetail}.idx
	fi
fi # test "$SENDCOM" = "YES"

#
# DBNet Alerts for gfs ensemble
#

############## CHECK DBN ALERTS BEFORE USING ########## especially time limits
############## CHECK DBN ALERTS BEFORE USING ########## especially time limits
############## CHECK DBN ALERTS BEFORE USING ########## especially time limits

GRID=
FHINCDBN=6
if [[ $jobgrid == 0p5 ]]; then
	FHINCDBN=3
fi

if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 12 ` -eq 0; then
	#   MEMBER=$RUN | tr '[a-z]' '[A-Z]'
	MEMBER=`echo $RUNMEM |tr '[a-z]' '[A-Z]'`

	if [[ $fhr -ge 0 && $fhr -le 84 && ` expr $fhr % $FHINCDBN ` -eq 0 ]]; then
		#$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A${GRID}_$MEMBER $job $COMOUT/$cyc/$GRBDIR2/ge${RUN}.${cycle}.$GRBFIL2\f$pgfhr
		$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_${GRID}\_$MEMBER $job $COMOUT/$cyc/$GRBDIR2/ge${RUNMEM}.${cycle}.$GRBFIL2\f${pgfhr}${filetail}
	fi # [[ $fhr -ge 0 && $fhr -le 84 && ` expr $fhr % $FHINCDBN ` -eq 0 ]]

	if test "$jobgrid" = '2p5' -a ` expr $fhr % 12 ` -eq 0 -a $fhr -ge 96
		#$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A2_$MEMBER $job $COMOUT/$cyc/pgrb2alr/ge${RUN}.${cycle}.pgrb2af$pgfhr.2
		$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_${GRID}\_$MEMBER $job $COMOUT/$cyc/pgrb2alr/ge${RUNMEM}.${cycle}.$GRBFIL2\f${pgfhr}${filetail}
	fi # test "$jobgrid" = '2p5' -a ` expr $fhr % 12 ` -eq 0 -a $fhr -ge 96
	if [[ $fhr -ge 240 && ` expr $fhr % 12 ` -eq 0 ]]; then
		#$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A${GRID}_$MEMBER $job $COMOUT/$cyc/$GRBDIR2/ge${RUNMEM}.${cycle}.$GRBFIL2\f$pgfhr
		$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_${GRID}\_$MEMBER $job $COMOUT/$cyc/$GRBDIR2/ge${RUNMEM}.${cycle}.$GRBFIL2\f${pgfhr}${filetail}
	fi # [[ $fhr -ge 240 && ` expr $fhr % 12 ` -eq 0 ]]
fi # test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 12 ` -eq 0

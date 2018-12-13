#!/bin/ksh
#####################################################################
echo "-----------------------------------------------------"
echo " Script: gefs_prdgen.sh" 
echo " "
echo " Purpose - Perform interpolation and GRIB2 conversion"
echo "           on master GRIB files"
echo "           for one member and one time step."
echo "           Move posted files to /com"
echo "           Alert posted files to DBNet"
echo " "
echo " History - "
echo "    Wobus   - 8/28/07 - New "
echo "    Wobus   - 7/30/10 - move 180-192hr products to pgrbd"
echo "    Hou     - 7/31/14 - adopted for grib2 based processing "
echo "    Meng    - 11/17/16 - Use neighbor interpolation for ICSEV "
echo "    Meng    - 03/09/17 - Remove grib1, PGRBC and PGRBD generation, "
echo "                         and use the same ush script to generate all grids"
echo "    B. Fu   - XX/XX/17 - Replace COPYGB2 with WGRIB2"
echo "-----------------------------------------------------"
#####################################################################
set -xa

export option1=' -set_grib_type same -new_grid_winds earth '
export option21=' -new_grid_interpolation bilinear  -if '
export option22=":(LAND|CSNOW|CRAIN|CFRZR|CICEP|ICSEV):"
export option23=' -new_grid_interpolation neighbor -fi '
export grid0p25="latlon 0:1440:0.25 90:721:-0.25"
export grid0p5="latlon 0:720:0.5 90:361:-0.5"
export grid1p0="latlon 0:360:1.0 90:181:-1.0"
export grid2p5="latlon 0:144:2.5 90:73:-2.5"
#NB: the gridXXX defined in gefs.parm is for copygb2. 
#Now we are changing to wgrib2 and these new settings are needed.

ffhr=$ffhr
fhr=$fhr

case $jobgrid in
   1p0) grid=$grid1p0;;
   2p5) grid=$grid2p5;;
   0p5) grid=$grid0p5;;
   0p25) grid=$grid0p25;;
esac

#export WGRIB=${WGRIB:-$EXECgrib/wgrib}
#export GRBINDEX=${GRBINDEX:-$EXECgrib/grbindex}
#export COPYGB=${COPYGB:-$EXECgrib/copygb}
#export WGRIB2=${WGRIB2:-$EXECgrib/wgrib2}
#export GRB2INDEX=${GRB2INDEX:-$EXECgrib/grb2index}
#export COPYGB2=${COPYGB2:-$EXECgrib/copygb2}
#export CNVGRIB=${CNVGRIB:-$EXECgrib/cnvgrib21_gfs}

echo "settings in $0 gefsmachine=$gefsmachine"
echo "settings in $0 WGRIB=$WGRIB"
echo "settings in $0 WGRIB2=$WGRIB2"
echo "settings in $0 GRBINDEX=$GRBINDEX"
echo "settings in $0 GRB2INDEX=$GRB2INDEX"
echo "settings in $0 COPYGB=$COPYGB"
echo "settings in $0 COPYGB2=$COPYGB2"
echo "settings in $0 CNVGRIB=$CNVGRIB"

R1=`echo $RUNMEM|cut -c1-3`
R2=`echo $RUNMEM|cut -c4-5`
mem=`echo $RUNMEM|cut -c3-5`
case $R1 in
	(gec)
		if (( R2 == 0 )); then 
			(( e1 = 1 ))
			(( e2 = 2 ))
		fi
		;;
	(gen)
		(( e1 = 2 ))
		(( e2 = R2 ))
		;;
	(gep)
		(( e1 = 3 ))
		(( e2 = R2 ))
		;;
	(*)
		(( e1 = 0 ))
		(( e2 = 0 ))
		echo "unrecognized RUNMEM=$RUNMEM R1=$R1 R2=$R2"
		;;
esac # $R1 in

msg="Starting post for member=$member ffhr=$ffhr"
postmsg "$jlogfile" "$msg"

if (( fhr == 0 )); then
	hsuffix="00"
else
	hsuffix="hh"
fi

####################################
# Step I: Create pgrb2 files 
####################################
if [[ -s $DATA/pgrb2$ffhr$cfsuffix ]] && \ 
   [[ $overwrite = no ]]; then
	echo `date` $jobgrid  pgrb2 processing skipped for $RUNMEM $ffhr
else
	$WGRIB2 $mafile $option1 $option21 $option22 $option23 -new_grid $grid pgb2file.$ffhr$cfsuffix
	rc=$?
	if [[ $rc -ne 0 ]]; then
		msg="FATAL ERROR: wgrib2 for $mafile failed!"
		echo "`date`    $msg"
		postmsg "$jlogfile" "$msg"
		export err=1
		err_chk
	fi
	echo `date` pgrb2 $jobgrid grbfile $ffhr completed

	######################################################
	# Split the pgb2 file into pgrb2a, pgrb2b and pgrb2d parts
	######################################################
	#  set +x

	excludestring='180-192hr'

	parmlist=$PARMgefs/gefs_pgrb2a_f${hsuffix}.parm
	$WGRIB2 -s pgb2file.$ffhr$cfsuffix | \
		grep -F -f $parmlist | \
		grep -v -F $excludestring | \
		$WGRIB2 pgb2file.$ffhr$cfsuffix -s -i -grib pgb2afile.$ffhr$cfsuffix
	if [[ x$fhoroglist != x ]]; then
		for fhorog in $fhoroglist; do
			if (( fhr == fhorog )); then
				$WGRIB2 -s pgb2file.$ffhr$cfsuffix | grep 'HGT:surface' | $WGRIB2 pgb2file.$ffhr$cfsuffix -i -append -grib pgb2afile.$ffhr$cfsuffix
			fi
		done # for fhorog in $fhoroglist
	fi # [[ x$fhoroglist != x ]]
	$WGRIB2 -s pgb2afile.$ffhr$cfsuffix > pgb2afile.${ffhr}${cfsuffix}.idx

	parmlist2=$PARMgefs/gefs_pgrb2ab_f${hsuffix}.parm
	$WGRIB2 -s pgb2file.$ffhr$cfsuffix | \
		grep -F -f $parmlist2 | \
		grep -v -F -f $parmlist | \
		grep -v -F $excludestring | \
		$WGRIB2 pgb2file.$ffhr$cfsuffix -s -i -grib pgb2bfile.$ffhr$cfsuffix
	$WGRIB2 -s pgb2bfile.$ffhr$cfsuffix > pgb2bfile.${ffhr}${cfsuffix}.idx

	##############################################
	# Save the master files at 0p5 grid for fcst beyond day 10
	##############################################
	if test "$save_master_p5" = 'YES' -a "$jobgrid" = '0p5'; then
		if (( fhr > FHMAXHF )); then
			$WGRIB2 -s pgb2file.$ffhr$cfsuffix > pgb2file.${ffhr}${cfsuffix}.idx
			mv pgb2file.${ffhr}${cfsuffix} $mafile_p5 
			mv pgb2file.${ffhr}${cfsuffix}.idx $mifile_p5
		fi
	fi

	##############################################
	# Save Data
	##############################################
	if test "$SENDCOM" = 'YES'; then
		#
		# Save Pressure GRIB/Index files
		#
		#mv pgb2afile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2a/${RUNMEM}.${cycle}.pgrb2a$ffhr$cfsuffix
		#testfile=$COMOUT/$cyc/pgrb2a/${RUNMEM}.${cycle}.pgrb2a$ffhr$cfsuffix
		mv pgb2afile.$ffhr$cfsuffix $fileaout
		testfile=$fileaout
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "`date`    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]
		#mv pgb2bfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2b/${RUNMEM}.${cycle}.pgrb2b$ffhr$cfsuffix
		#testfile=$COMOUT/$cyc/pgrb2b/${RUNMEM}.${cycle}.pgrb2b$ffhr$cfsuffix
		mv pgb2bfile.$ffhr$cfsuffix $filebout
		testfile=$filebout
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "`date`    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]
		if [[ "$makegrb2i" = "yes" ]]; then
			#mv pgb2afile.$ffhr$cfsuffix.idx $COMOUT/$cyc/pgrb2a/${RUNMEM}.${cycle}.pgrb2a$ffhr$cfsuffix.idx
			#testfile=$COMOUT/$cyc/pgrb2a/${RUNMEM}.${cycle}.pgrb2a$ffhr${cfsuffix}.idx
			mv pgb2afile.$ffhr$cfsuffix.idx $fileaouti
			testfile=$fileaouti
			if [[ ! -s $testfile ]]; then
				msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
				echo "`date`    $msg"
				postmsg "$jlogfile" "$msg"
				export err=1
				err_chk
			fi # [[ ! -s $testfile ]]
			#mv pgb2bfile.$ffhr$cfsuffix.idx $COMOUT/$cyc/pgrb2b/${RUNMEM}.${cycle}.pgrb2b$ffhr$cfsuffix.idx
			#testfile=$COMOUT/$cyc/pgrb2b/${RUNMEM}.${cycle}.pgrb2b$ffhr$cfsuffix.idx
			mv pgb2bfile.$ffhr$cfsuffix.idx $filebouti
			testfile=$filebouti
			if [[ ! -s $testfile ]]; then
				msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
				echo "`date`    $msg"
				postmsg "$jlogfile" "$msg"
				export err=1
				err_chk
			fi # [[ ! -s $testfile ]]
		fi # [[ "$makegrb2i" = "yes" ]]

		###############################################################################
		# Send DBNet alerts for PGB2A at 6 hour increments for all forecast hours
		# Do for 00, 06, 12, and 18Z cycles.
		###############################################################################
		if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 6 ` -eq 0; then
			if test `echo $RUNMEM | cut -c1-2` = "ge"; then
				MEMBER=`echo $RUNMEM | cut -c3-5 | tr '[a-z]' '[A-Z]'`
				if [[ $fhr -ge 0 && $fhr -le $fhmax && ` expr $fhr % 6 ` -eq 0 && ! -n "$cfsuffix" ]]; then
					#$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_$MEMBER $job $COMOUT/$cyc/pgrb2a/${RUNMEM}.${cycle}.pgrb2a$ffhr$cfsuffix
					$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_$GRID\_$MEMBER $job $fileaout
					#$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_${MEMBER}_WIDX $job $COMOUT/$cyc/pgrb2a/${RUNMEM}.${cycle}.pgrb2a$ffhr$cfsuffix.idx
					$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_$GRID\_${MEMBER}_WIDX $job $fileaouti
				fi
			fi # test `echo $RUNMEM | cut -c1-2` = "ge"
		fi # test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 6 ` -eq 0

		###############################################################################
		# Send DBNet alerts for PGB2B at 6 hour increments for up to 84 hours
		# Do for 00Z and 12Z only
		###############################################################################
		if test "$SENDDBN" = 'YES' -a "$NET" = 'gens'; then
			if test `echo $RUNMEM | cut -c1-2` = "ge" -a ! -n "$cfsuffix"; then
				MEMBER=`echo $RUNMEM | cut -c3-5 | tr '[a-z]' '[A-Z]'`
				#$DBNROOT/bin/dbn_alert MODEL ENS_PGB2B_$MEMBER $job $COMOUT/$cyc/pgrb2b/${RUNMEM}.${cycle}.pgrb2b$ffhr$cfsuffix
				$DBNROOT/bin/dbn_alert MODEL ENS_PGB2B_$GRID\_$MEMBER $job $filebout
				#$DBNROOT/bin/dbn_alert MODEL ENS_PGB2B_${MEMBER}_WIDX $job $COMOUT/$cyc/pgrb2b/${RUNMEM}.${cycle}.pgrb2b$ffhr$cfsuffix.idx
				$DBNROOT/bin/dbn_alert MODEL ENS_PGB2B_$GRID\_${MEMBER}_WIDX $job $filebouti
			fi # test `echo $RUNMEM | cut -c1-2` = "ge" -a ! -n "$cfsuffix"
		fi # test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a "$NET" = 'gens'
	fi # test "$SENDCOM" = 'YES'
	echo `date` pgrb2a 1x1 sendcom $ffhr completed
fi # [[ -s $DATA/pgrb2$ffhr$cfsuffix ]] && [[ $overwrite = no ]]

########################################################
echo `date` $sname $member $partltr $cfsuffix $fsuffix 1x1 GRIB end on machine=`uname -n`
msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"
################## END OF SCRIPT #######################

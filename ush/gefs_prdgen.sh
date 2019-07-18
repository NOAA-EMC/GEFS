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

# These are already set in gefs_prdgen.parm
# export option1=${option1:-' -set_grib_type same -new_grid_winds earth '}
# export option21=${option21:-' -new_grid_interpolation bilinear  -if '}
# export option22=${option22:-":(LAND|CSNOW|CRAIN|CFRZR|CICEP|ICSEV):"}
# export option23=${option23:-' -new_grid_interpolation neighbor -fi '}
export ENSADD=${ENSADD:-$USHgefs/global_ensadd.sh}

msg="Starting post for member=$RUNMEM ffhr=$ffhr"
postmsg "$jlogfile" "$msg"

cat <<-EOF
	Settings for $0:
	  RUNMEM: $RUNMEM
	  DATA: $DATA

	  mafile: $mafile
	  mifile: $mifile
	  mcfile: $mcfile
	  pcfile: $pcfile

	  fileaout: $fileaout
	  fileaouti: $fileaouti
	  filebout: $filebout
	  filebouti: $filebouti

	  WGRIB2: $WGRIB2
	  COPYGB2: $COPYGB2
	  makepgrb2b: $makepgrb2b

	  option1: $option1
	  option21: $option21
	  option22: $option22
	  option23: $option23
EOF

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
	echo "$(date) $jobgrid  pgrb2 processing skipped for $RUNMEM $ffhr"
else
#	$WGRIB2 $mafile $option1 $option21 $option22 $option23 -set_bitmap 1 -new_grid $grid_spec pgb2file.$ffhr$cfsuffix
	$WGRIB2 $mafile $option1 $option21 $option22 $option23 $option24 \
			$option25 $option26 $option27 $option28 \
			-new_grid $grid_spec pgb2file.$ffhr
	rc=$?
	if [[ $rc -ne 0 ]]; then
		msg="FATAL ERROR: wgrib2 for $mafile failed!"
		echo "$(date)    $msg"
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
	if [[ $RUNMEM = "gegfs" ]]; then
		# Add ensemble PDS header to GFS file
		$ENSADD 0 0 pgb2afile.$ffhr$cfsuffix epgbafile
		mv epgbafile pgb2afile.$ffhr$cfsuffix
	fi # [[ $RUNMEM = "gegfs" ]]

	if [[ $makepgrb2b = "yes" ]]; then
		parmlist2=$PARMgefs/gefs_pgrb2ab_f${hsuffix}.parm
		$WGRIB2 -s pgb2file.$ffhr$cfsuffix | \
			grep -F -f $parmlist2 | \
			grep -v -F -f $parmlist | \
			grep -v -F $excludestring | \
			$WGRIB2 pgb2file.$ffhr$cfsuffix -s -i -grib pgb2bfile.$ffhr$cfsuffix
		$WGRIB2 -s pgb2bfile.$ffhr$cfsuffix > pgb2bfile.${ffhr}${cfsuffix}.idx
	fi # [[ $makepgrb2b = "yes" ]]

	##############################################
	# Save the master files at 0p5 grid for fcst beyond day 10
	##############################################
#	if test "$save_master_p5" = 'YES' -a "$jobgrid" = '0p5'; then
#		if (( fhr > FHMAXHF )); then
#			$WGRIB2 -s pgb2file.$ffhr$cfsuffix > pgb2file.${ffhr}${cfsuffix}.idx
#			mv pgb2file.${ffhr}${cfsuffix} $mafile_p5 
#			mv pgb2file.${ffhr}${cfsuffix}.idx $mifile_p5
#		fi
#	fi
	if test "$save_pgrb2_p5" = 'YES' -a "$jobgrid" = '0p5'; then
		if (( fhr > FHMAXHF )); then
			$WGRIB2 -s pgb2file.$ffhr$cfsuffix > pgb2file.${ffhr}${cfsuffix}.idx
			mv pgb2file.${ffhr}${cfsuffix} $mafile_p5 
			mv pgb2file.${ffhr}${cfsuffix}.idx $mifile_p5
		fi
	fi
	if test "$save_pgrb2_p25" = 'YES' -a "$jobgrid" = '0p25'; then
		if (( fhr <= FHMAXHF )); then
			$WGRIB2 -s pgb2file.$ffhr$cfsuffix > pgb2file.${ffhr}${cfsuffix}.idx
			mv pgb2file.${ffhr}${cfsuffix} $mafile_p25 
			mv pgb2file.${ffhr}${cfsuffix}.idx $mifile_p25
		fi
	fi

	##############################################
	# Save Data
	##############################################
	if [[ "$SENDCOM" = 'YES' ]]; then
		#
		# Save Pressure GRIB/Index files
		#
		mv pgb2afile.$ffhr$cfsuffix $fileaout
		testfile=$fileaout
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "$(date)    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]

		if [[ "$makegrb2i" = "yes" ]]; then
			mv pgb2afile.$ffhr$cfsuffix.idx $fileaouti
			testfile=$fileaouti
			if [[ ! -s $testfile ]]; then
				msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
				echo "$(date)    $msg"
				postmsg "$jlogfile" "$msg"
				export err=1
				err_chk
			fi # [[ ! -s $testfile ]]
		fi

		if [[ $makepgrb2b = "yes" ]]; then
			mv pgb2bfile.$ffhr$cfsuffix $filebout
			testfile=$filebout
			if [[ ! -s $testfile ]]; then
				msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
				echo "$(date)    $msg"
				postmsg "$jlogfile" "$msg"
				export err=1
				err_chk
			fi # [[ ! -s $testfile ]]

			if [[ "$makegrb2i" = "yes" ]]; then
				mv pgb2bfile.$ffhr$cfsuffix.idx $filebouti
				testfile=$filebouti
				if [[ ! -s $testfile ]]; then
					msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
					echo "$(date)    $msg"
					postmsg "$jlogfile" "$msg"
					export err=1
					err_chk
				fi # [[ ! -s $testfile ]]
			fi # [[ "$makegrb2i" = "yes" ]]
		fi # [[ $makepgrb2b = "yes" ]]

		###############################################################################
		# Send DBNet alerts for PGB2A at 6 hour increments for all forecast hours
		# Do for 00, 06, 12, and 18Z cycles.
		###############################################################################
		if [[ "$SENDDBN" = 'YES' && "$NET" = 'gens' && ` expr $cyc % 6 ` -eq 0 ]]; then
			if [[ `echo $RUNMEM | cut -c1-2` = "ge" ]]; then
				MEMBER=`echo $RUNMEM | cut -c3-5 | tr '[a-z]' '[A-Z]'`
				if [[ $fhr -ge 0 && $fhr -le $fhmax && ` expr $fhr % 6 ` -eq 0 && ! -n "$cfsuffix" ]]; then
					$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_$GRID\_$MEMBER $job $fileaout
					$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_$GRID\_${MEMBER}_WIDX $job $fileaouti
				fi
			fi # [[ `echo $RUNMEM | cut -c1-2` = "ge" ]]
		fi # [[ "$SENDDBN" = 'YES' && "$NET" = 'gens' && ` expr $cyc % 6 ` -eq 0 ]]

		###############################################################################
		# Send DBNet alerts for PGB2B at 6 hour increments for up to 84 hours
		# Do for 00Z and 12Z only
		###############################################################################
		if [[ "$SENDDBN" = 'YES' && "$NET" = 'gens' ]]; then
			if [[ `echo $RUNMEM | cut -c1-2` = "ge" && ! -n "$cfsuffix" ]]; then
				MEMBER=`echo $RUNMEM | cut -c3-5 | tr '[a-z]' '[A-Z]'`
				$DBNROOT/bin/dbn_alert MODEL ENS_PGB2B_$GRID\_$MEMBER $job $filebout
				$DBNROOT/bin/dbn_alert MODEL ENS_PGB2B_$GRID\_${MEMBER}_WIDX $job $filebouti
			fi # [[ `echo $RUNMEM | cut -c1-2` = "ge" && ! -n "$cfsuffix" ]]
		fi # [[ "$SENDDBN" = 'YES' && "$NET" = 'gens' ]]
	fi # [[ "$SENDCOM" = 'YES' ]]
	echo `date` pgrb2a 1x1 sendcom $ffhr completed
fi # [[ -s $DATA/pgrb2$ffhr$cfsuffix ]] && [[ $overwrite = no ]]

########################################################
echo `date` $sname $member $partltr $cfsuffix $fsuffix 1x1 GRIB end on machine=`uname -n`
msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"
################## END OF SCRIPT #######################

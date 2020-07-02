#!/bin/ksh
#####################################################################
# -----------------------------------------------------
#  Script: gefs_prdgen.sh
#  
#  Purpose - Perform interpolation and GRIB2 conversion
#            on master GRIB files
#            for one member and one time step.
#            Move posted files to /com
#            Alert posted files to DBNet
#  
#  History - 
#     Wobus   - 8/28/07 - New 
#     Wobus   - 7/30/10 - move 180-192hr products to pgrbd
#     Hou     - 7/31/14 - adopted for grib2 based processing 
#     Meng    - 11/17/16 - Use neighbor interpolation for ICSEV 
#     Meng    - 03/09/17 - Remove grib1, PGRBC and PGRBD generation, 
#                          and use the same ush script to generate all grids
#     B. Fu   - XX/XX/17 - Replace COPYGB2 with WGRIB2
# -----------------------------------------------------
#####################################################################

echo "$(date -u) begin ${.sh.file}"

set -xa

export ENSADD=${ENSADD:-$USHgefs/global_ensadd.sh}

cat <<-EOF
	Settings for ${.sh.file}:
	  RUNMEM: $RUNMEM
	  DATA: $DATA

	  mafile: $mafile
	  mifile: $mifile
	  mcfile: $mcfile
	  pcfile: $pcfile

	  parmlist_a: $parmlist_a
	  parmlist_b: $parmlist_b

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

if [ "$jobgrid" = '2p5' ]; then
	SENDDBN=NO
fi

####################################
# Step I: Create pgrb2 files 
####################################
if [[ -s $DATA/pgrb2$ffhr && $overwrite = no ]]; then
	echo "$jobgrid pgrb2 processing skipped for $RUNMEM $ffhr"
else
	$WGRIB2 $mafile $option1 $option21 $option22 $option23 $option24 \
			$option25 $option26 $option27 $option28 \
			-new_grid $grid_spec pgb2file.$ffhr
	export err=$?
	if [[ $err -ne 0 ]]; then
		echo "FATAL ERROR in ${.sh.file} ($stream): wgrib2 for $mafile failed!"
		export err=1
		err_chk || exit $err
	fi
	echo "$(date) pgrb2 $jobgrid grbfile $ffhr completed"

	######################################################
	# Split the pgb2 file into pgrb2a, pgrb2b and pgrb2d parts
	######################################################
	#  set +x

	excludestring=${excludestring:-'372-384hr'}

	$WGRIB2 -s pgb2file.$ffhr | \
		grep -F -f $parmlist_a | \
		grep -v -F $excludestring | \
		$WGRIB2 -s pgb2file.$ffhr -i -grib pgb2afile.$ffhr
	if [[ $RUNMEM = "gegfs" ]]; then
	    if (( fhr >= 3 )); then 
		    rm exlist
		    $WGRIB2 -s pgb2afile.$ffhr | grep -e CSN -e CIC -e CFR -e CRA | grep "hour fcst" > exlist 
		    if (( fhr > 6 )); then 
			    $WGRIB2 -s pgb2afile.$ffhr | grep "APCP" | grep ":0-" >> exlist
		    fi
		    $WGRIB2 -s pgb2afile.$ffhr | grep -v -f exlist | $WGRIB2 -i pgb2afile.$ffhr -grib tmpfile 
		    mv tmpfile pgb2afile.$ffhr
	    fi
	fi
	if [[ x$fhoroglist != x ]]; then
		for fhorog in $fhoroglist; do
			if (( fhr == fhorog )); then
				$WGRIB2 -s pgb2file.$ffhr | grep 'HGT:surface' | $WGRIB2 pgb2file.$ffhr -i -append -grib pgb2afile.$ffhr
			fi
		done # for fhorog in $fhoroglist
	fi # [[ x$fhoroglist != x ]]
	$WGRIB2 -s pgb2afile.$ffhr > pgb2afile.${ffhr}.idx
	if [[ $RUNMEM = "gegfs" ]]; then
		# Add ensemble PDS header to GFS file
		$ENSADD 0 0 pgb2afile.$ffhr epgbafile
		mv epgbafile pgb2afile.$ffhr
	fi # [[ $RUNMEM = "gegfs" ]]

	if [[ $makepgrb2b = "yes" ]]; then
		$WGRIB2 -s pgb2file.$ffhr | \
			grep -F -f $parmlist_b | \
			grep -v -F -f $parmlist_a | \
			grep -v -F $excludestring | \
			$WGRIB2 pgb2file.$ffhr -s -i -grib pgb2bfile.$ffhr
		$WGRIB2 -s pgb2bfile.$ffhr > pgb2bfile.${ffhr}.idx
	fi # [[ $makepgrb2b = "yes" ]]

	##############################################
	# Save the master files at 0p5 grid for fcst beyond day 10
	##############################################
	if [ "$save_pgrb2_p5" = 'YES' -a "$jobgrid" = '0p5' ]; then
		if (( fhr > FHMAXHF )); then
			$WGRIB2 -s pgb2file.$ffhr > pgb2file.${ffhr}.idx
			mv pgb2file.${ffhr} $mafile_p5 
			mv pgb2file.${ffhr}.idx $mifile_p5
		fi
	fi
	if [ "$save_pgrb2_p25" = 'YES' -a "$jobgrid" = '0p25' ]; then
		if (( fhr <= FHMAXHF )); then
			$WGRIB2 -s pgb2file.$ffhr > pgb2file.${ffhr}.idx
			mv pgb2file.${ffhr} $mafile_p25 
			mv pgb2file.${ffhr}.idx $mifile_p25
		fi
	fi

	##############################################
	# Save Data
	##############################################
	if [[ "$SENDCOM" = 'YES' ]]; then
		#
		# Save Pressure GRIB/Index files
		#
		mv pgb2afile.$ffhr $fileaout
		testfile=$fileaout
		if [[ ! -s $testfile ]]; then
			echo "FATAL ERROR in ${.sh.file} ($stream): $testfile WAS NOT WRITTEN"
			export err=1
			err_chk || exit $err
		fi # [[ ! -s $testfile ]]

		if [[ "$makegrb2i" = "yes" ]]; then
			mv pgb2afile.$ffhr.idx $fileaouti
			testfile=$fileaouti
			if [[ ! -s $testfile ]]; then
				echo "FATAL ERROR in ${.sh.file} ($stream): $testfile WAS NOT WRITTEN"
				export err=1
				err_chk || exit $err
			fi # [[ ! -s $testfile ]]
		fi

		if [[ $makepgrb2b = "yes" ]]; then
			mv pgb2bfile.$ffhr $filebout
			testfile=$filebout
			if [[ ! -s $testfile ]]; then
				echo "FATAL ERROR in ${.sh.file} ($stream): $testfile WAS NOT WRITTEN"
				export err=1
				err_chk || exit $err
			fi # [[ ! -s $testfile ]]

			if [[ "$makegrb2i" = "yes" ]]; then
				mv pgb2bfile.$ffhr.idx $filebouti
				testfile=$filebouti
				if [[ ! -s $testfile ]]; then
					echo "FATAL ERROR in ${.sh.file} ($stream): $testfile WAS NOT WRITTEN"
					export err=1
					err_chk || exit $err
				fi # [[ ! -s $testfile ]]
			fi # [[ "$makegrb2i" = "yes" ]]
		fi # [[ $makepgrb2b = "yes" ]]

		###############################################################################
		# Send DBNet alerts for PGB2A all forecast hours for 00, 06, 12, and 18Z cycles.
		###############################################################################
		MODCOM=$(echo ${NET}_${COMPONENT} | tr '[a-z]' '[A-Z]')
		if [[ "$SENDDBN" = 'YES' ]]; then
			DBNTYP=${MODCOM}_PGB2A
			if [[ $(echo $RUNMEM | cut -c3-5) = "aer" ]]; then
				if [[ "$jobgrid" = '0p50' ]]; then
					DBNTYP=${MODCOM}_A3D_GB2
				else
					DBNTYP=${MODCOM}_A2D_GB2
				fi
			fi
			if [[ $(echo $RUNMEM | cut -c1-2) = "ge" ]]; then
				$DBNROOT/bin/dbn_alert MODEL ${DBNTYP}_$GRID $job $fileaout
				$DBNROOT/bin/dbn_alert MODEL ${DBNTYP}_$GRID\_WIDX $job $fileaouti
			fi # [[ $(echo $RUNMEM | cut -c1-2) = "ge" ]]
		fi # [[ "$SENDDBN" = 'YES' ]]

		###############################################################################
		# Send DBNet alerts for PGB2B 
		###############################################################################
		if [[ "$SENDDBN" = 'YES' && "$jobgrid" = '0p50' && "$makepgrb2b" = "yes" ]]; then
			DBNTYP=${MODCOM}_PGB2B
			if [[ $(echo $RUNMEM | cut -c1-2) = "ge" ]]; then
				$DBNROOT/bin/dbn_alert MODEL ${DBNTYP}_$GRID $job $filebout
				$DBNROOT/bin/dbn_alert MODEL ${DBNTYP}_$GRID\_WIDX $job $filebouti
			fi # [[ $(echo $RUNMEM | cut -c1-2) = "ge" ]]
		fi # [[ "$SENDDBN" = 'YES' ]]
	fi # [[ "$SENDCOM" = 'YES' ]]
	echo $(date) pgrb2a $jobgrid sendcom $ffhr completed
fi # [[ -s $DATA/pgrb2$ffhr ]] && [[ $overwrite = no ]]

echo "$(date -u) end ${.sh.file}"

exit 0


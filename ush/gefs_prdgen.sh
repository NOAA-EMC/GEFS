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
echo "-----------------------------------------------------"
#####################################################################
set -xa

anlflag=$anlflag
ffhr=$ffhr
fhr=$fhr
grid=$grid1p0

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

####################################
# Step I: Create 1x1 pgrb2 files 
####################################
if [[ -s $DATA/pgrb2$ffhr$cfsuffix ]] && [[ -s $DATA/pgrb2i$ffhr$cfsuffix ]] && [[ $overwrite = no ]]; then
	echo `date` 1x1 pgrb2 processing skipped for $RUNMEM $ffhr
else
	$COPYGB2 -g "${grid}" -i0 -x $COMIN/$cyc/master/$RUNMEM.$cycle.master.grb2$ffhr$cfsuffix pgb2file.$ffhr$cfsuffix
	echo `date` pgrb2a 1x1 grbfile $ffhr completed

	######################################################
	# Split the pgb2file into pgrb2a, pgrb2b and pgrb2d parts
	######################################################
	if (( fhr == 0 )); then
		hsuffix="00"
	else
		hsuffix="hh"
	fi

	#  set +x

	excludestring='180-192hr'

	# begin block removed from background
	#  parmlist=$PARMgefs/gefs_pgrba_f${hsuffix}.parm
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
	#  $GRB2INDEX pgb2afile.$ffhr$cfsuffix pgb2afile.$ffhr$cfsuffix.idx
	# end block removed from background

	# begin block removed from background
	#  parmlist=$PARMgefs/gefs_pgrba_f${hsuffix}.parm
	parmlist2=$PARMgefs/gefs_pgrb2ab_f${hsuffix}.parm
	$WGRIB2 -s pgb2file.$ffhr$cfsuffix | \
	grep -F -f $parmlist2 | \
	grep -v -F -f $parmlist | \
	grep -v -F $excludestring | \
	$WGRIB2 pgb2file.$ffhr$cfsuffix -s -i -grib pgb2bfile.$ffhr$cfsuffix
	$WGRIB2 -s pgb2bfile.$ffhr$cfsuffix > pgb2bfile.${ffhr}${cfsuffix}.idx
	#  $GRB2INDEX pgb2bfile.$ffhr$cfsuffix pgb2bfile.$ffhr$cfsuffix.idx
	# end block removed from background

	if test "$CREATE_TIGGE" = 'YES'; then
		if (( fhr == 0 )); then
			#       parmlist=${PARMgefs}/gefs_pgrbc_f00.parm
			parmlist=${PARMgefs}/gefs_pgrb2c_f00.parm
		else
			#       parmlist=${PARMgefs}/gefs_pgrbc_fhh.parm
			parmlist=${PARMgefs}/gefs_pgrb2c_fhh.parm
		fi # (( fhr == 0 ))
		#     set +x
		$WGRIB2 pgb2bfile.$ffhr$cfsuffix | \
		grep -F -f $parmlist | \
		$WGRIB2 pgb2bfile.$ffhr$cfsuffix -i -grib pgb2cfile.$ffhr$cfsuffix 
		if test "$SENDCOM" = 'YES'; then
			mv pgb2cfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2c/${RUNMEM}.${cycle}.pgrb2c$ffhr$cfsuffix
			testfile=$COMOUT/$cyc/pgrb2c/${RUNMEM}.${cycle}.pgrb2c$ffhr$cfsuffix
			if [[ ! -s $testfile ]]; then
				msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
				echo "`date`    $msg"
				postmsg "$jlogfile" "$msg"
				export err=1
				err_chk
			fi # [[ ! -s $testfile ]]
		fi # test "$SENDCOM" = 'YES'
		#     set -x
	fi # test "$CREATE_TIGGE" = 'YES'

	if test "$makepgrb2d" = 'yes'; then
		$WGRIB2 -s pgb2file.$ffhr$cfsuffix | \
		grep -v -F -f $parmlist2 | \
		$WGRIB2 pgb2file.$ffhr$cfsuffix -s -i -grib pgb2dfile.$ffhr$cfsuffix
		$WGRIB2 -s pgb2file.$ffhr$cfsuffix | \
		grep -F -f $parmlist2 | \
		grep -F $excludestring | \
		$WGRIB2 pgb2file.$ffhr$cfsuffix -s -i -append -grib pgb2dfile.$ffhr$cfsuffix
	fi # test "$makepgrb2d" = 'yes'
	#  set -x

	if test "$SENDCOM" = 'YES'; then
		#
		# Save Pressure GRIB/Index files
		#
		mv pgb2afile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2a/${RUNMEM}.${cycle}.pgrb2a$ffhr$cfsuffix
		testfile=$COMOUT/$cyc/pgrb2a/${RUNMEM}.${cycle}.pgrb2a$ffhr$cfsuffix
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "`date`    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]
		mv pgb2bfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2b/${RUNMEM}.${cycle}.pgrb2b$ffhr$cfsuffix
		testfile=$COMOUT/$cyc/pgrb2b/${RUNMEM}.${cycle}.pgrb2b$ffhr$cfsuffix
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "`date`    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]
		#     mv pgb2cfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2c/${RUNMEM}.${cycle}.pgrb2c$ffhr$cfsuffix
		if test "$makepgrb2d" = 'yes'; then
			mv pgb2dfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2d/${RUNMEM}.${cycle}.pgrb2d$ffhr$cfsuffix
			testfile=$COMOUT/$cyc/pgrb2d/${RUNMEM}.${cycle}.pgrb2d$ffhr$cfsuffix
			if [[ ! -s $testfile ]]; then
				msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
				echo "`date`    $msg"
				postmsg "$jlogfile" "$msg"
				export err=1
				err_chk
			fi # [[ ! -s $testfile ]]
		fi # test "$makepgrb2d" = 'yes'
		if [[ "$makegrb2i" = "yes" ]]; then
			mv pgb2afile.$ffhr$cfsuffix.idx $COMOUT/$cyc/pgrb2a/${RUNMEM}.${cycle}.pgrb2a$ffhr$cfsuffix.idx
			testfile=$COMOUT/$cyc/pgrb2a/${RUNMEM}.${cycle}.pgrb2a$ffhr${cfsuffix}.idx
			if [[ ! -s $testfile ]]; then
				msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
				echo "`date`    $msg"
				postmsg "$jlogfile" "$msg"
				export err=1
				err_chk
			fi # [[ ! -s $testfile ]]
			mv pgb2bfile.$ffhr$cfsuffix.idx $COMOUT/$cyc/pgrb2b/${RUNMEM}.${cycle}.pgrb2b$ffhr$cfsuffix.idx
			testfile=$COMOUT/$cyc/pgrb2b/${RUNMEM}.${cycle}.pgrb2b$ffhr$cfsuffix.idx
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
					$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_$MEMBER $job $COMOUT/$cyc/pgrb2a/${RUNMEM}.${cycle}.pgrb2a$ffhr$cfsuffix
					$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_${MEMBER}_WIDX $job $COMOUT/$cyc/pgrb2a/${RUNMEM}.${cycle}.pgrb2a$ffhr$cfsuffix.idx
				fi
			fi # test `echo $RUNMEM | cut -c1-2` = "ge"
		fi # test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 6 ` -eq 0

		###############################################################################
		# Send DBNet alerts for PGB2B at 6 hour increments for up to 84 hours
		# Do for 00Z and 12Z only
		###############################################################################
		if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a "$NET" = 'gens'; then
			if test `echo $RUNMEM | cut -c1-2` = "ge" -a ! -n "$cfsuffix"; then
				MEMBER=`echo $RUNMEM | cut -c3-5 | tr '[a-z]' '[A-Z]'`
				# if [[ $fhr -ge 0 && $fhr -le 84 && ` expr $fhr % 6 ` -eq 0 && ! -n "$cfsuffix" ]]
				# then
				$DBNROOT/bin/dbn_alert MODEL ENS_PGB2B_$MEMBER $job $COMOUT/$cyc/pgrb2b/${RUNMEM}.${cycle}.pgrb2b$ffhr$cfsuffix
				$DBNROOT/bin/dbn_alert MODEL ENS_PGB2B_${MEMBER}_WIDX $job $COMOUT/$cyc/pgrb2b/${RUNMEM}.${cycle}.pgrb2b$ffhr$cfsuffix.idx
				# fi
			fi # test `echo $RUNMEM | cut -c1-2` = "ge" -a ! -n "$cfsuffix"

			###############################################################################
			# Send DBNet alerts for PGB2C at 6 hour increments 
			###############################################################################
			if test "$CREATE_TIGGE" = 'YES'; then
				MEMBER=`echo $RUNMEM | cut -c3-5 | tr '[a-z]' '[A-Z]'`
				$DBNROOT/bin/dbn_alert MODEL ENS_PGB2C_$MEMBER $job $COMOUT/$cyc/pgrb2c/${RUNMEM}.${cycle}.pgrb2c$ffhr$cfsuffix
			fi # test "$CREATE_TIGGE" = 'YES'

			###############################################################################
			# Do Not send DBNet alerts for the PGBD files at this time
			###############################################################################
		fi # test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a "$NET" = 'gens'
	fi # test "$SENDCOM" = 'YES'
	echo `date` pgrb2a 1x1 sendcom $ffhr completed
fi # [[ -s $DATA/pgrb2$ffhr$cfsuffix ]] && [[ -s $DATA/pgrb2i$ffhr$cfsuffix ]] && [[ $overwrite = no ]]

if [[ "$makepgrb1" = "yes" ]]; then
	######################################
	# Step II: Create GRIBA files
	#####################################
	if [[ -s $COMOUT/$cyc/pgrba/${RUNMEM}.${cycle}.pgrba$ffhr$cfsuffix ]] && \
	[[ -s $COMOUT/$cyc/pgrba/${RUNMEM}.${cycle}.pgrbai$ffhr$cfsuffix ]] && \
	[[ $overwrite = no ]]; then
		echo `date` 1x1 pgrba processing skipped for $RUNMEM $ffhr
	else
		FILEA=$COMIN/$cyc/pgrb2a/${RUNMEM}.${cycle}.pgrb2a$ffhr$cfsuffix
		$CNVGRIB -g21 $FILEA pgbafile.$ffhr$cfsuffix
		$GRBINDEX pgbafile.$ffhr$cfsuffix pgbaifile.$ffhr$cfsuffix
		#  $WGRIB -s pgbafile.$ffhr$cfsuffix > pgbaifile.${ffhr}${cfsuffix}.idx
		#  $ENSADD $e1 $e2 pgbafile.$ffhr$cfsuffix pgbaifile.$ffhr$cfsuffix epgbafile.$ffhr$cfsuffix
		#  echo after ADDING 1p0
		#  ls -lt pgbafile.$ffhr$cfsuffix pgbaifile.$ffhr$cfsuffix epgbafile.$ffhr$cfsuffix
		if [[ "$addgrb1id" = "yes" ]]; then
			# mv epgbafile.$ffhr$cfsuffix pgbafile.$ffhr$cfsuffix
			#  echo after MVING 1p0
			#  ls -lt pgbafile.$ffhr$cfsuffix pgbaifile.$ffhr$cfsuffix epgbafile.$ffhr$cfsuffix
			if [[ "$makegrb1i" = "yes" ]]; then
				$GRBINDEX pgbafile.$ffhr$cfsuffix pgbaifile.$ffhr$cfsuffix
			fi

			if test "$SENDCOM" = 'YES'; then
				#
				# Save Pressure GRIB/Index files
				#
				mv pgbafile.$ffhr$cfsuffix $COMOUT/$cyc/pgrba/${RUNMEM}.${cycle}.pgrba$ffhr$cfsuffix
				testfile=$COMOUT/$cyc/pgrba/${RUNMEM}.${cycle}.pgrba$ffhr$cfsuffix
				if [[ ! -s $testfile ]]; then
					msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
					echo "`date`    $msg"
					postmsg "$jlogfile" "$msg"
					export err=1
					err_chk
				fi # [[ ! -s $testfile ]]
				mv pgbaifile.${ffhr}${cfsuffix} $COMOUT/$cyc/pgrba/${RUNMEM}.${cycle}.pgrbai${ffhr}${cfsuffix}
				testfile=$COMOUT/$cyc/pgrba/${RUNMEM}.${cycle}.pgrbai${ffhr}${cfsuffix}
				if [[ ! -s $testfile ]]; then
					msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
					echo "`date`    $msg"
					postmsg "$jlogfile" "$msg"
					export err=1
					err_chk
				fi # [[ ! -s $testfile ]]
				if test "$SENDDBN" = 'YES'; then
					if test "$NET" = 'gens'; then
						if test `echo $RUNMEM | cut -c1-2` = "ge" -a ! -n "$cfsuffix"; then
							MEMBER=`echo $RUNMEM | cut -c3-5 | tr '[a-z]' '[A-Z]'`
							$DBNROOT/bin/dbn_alert MODEL ENS_PGBA_$MEMBER $job $COMOUT/$cyc/pgrba/${RUNMEM}.${cycle}.pgrba$ffhr$cfsuffix
							$DBNROOT/bin/dbn_alert MODEL ENS_PGBA_$MEMBER_WIDX $job $COMOUT/$cyc/pgrba/${RUNMEM}.${cycle}.pgrbai${ffhr}${cfsuffix}
						fi # test `echo $RUNMEM | cut -c1-2` = "ge" -a ! -n "$cfsuffix"
					fi # test "$NET" = 'gens'
				fi # test "$SENDDBN" = 'YES'
			fi # test "$SENDCOM" = 'YES'
		fi # [[ "$addgrb1id" = "yes" ]]
	fi # [[ -s $COMOUT/$cyc/pgrba/${RUNMEM}.${cycle}.pgrba$ffhr$cfsuffix ]] && [[ -s $COMOUT/$cyc/pgrba/${RUNMEM}.${cycle}.pgrbai$ffhr$cfsuffix ]] && [[ $overwrite = no ]]

	###########################################
	# STEP III: Create GRIBB files
	###########################################
	if [[ -s $COMOUT/$cyc/pgrbb/${RUNMEM}.${cycle}.pgrbb$ffhr$cfsuffix ]] && \
	[[ -s $COMOUT/$cyc/pgrbb/${RUNMEM}.${cycle}.pgrbbi$ffhr$cfsuffix ]] && \
	[[ $overwrite = no ]]; then
		echo `date` 1x1 pgrbb processing skipped for $RUNMEM $ffhr
	else
		FILEB=$COMIN/$cyc/pgrb2b/${RUNMEM}.${cycle}.pgrb2b$ffhr$cfsuffix
		$CNVGRIB -g21 $FILEB pgbbfile.$ffhr$cfsuffix
		$GRBINDEX pgbbfile.$ffhr$cfsuffix pgbbifile.$ffhr$cfsuffix
		#  $WGRIB -s pgbbfile.$ffhr$cfsuffix > pgbbfile.${ffhr}${cfsuffix}.idx
		#  $ENSADD $e1 $e2 pgbbfile.$ffhr$cfsuffix pgbbifile.$ffhr$cfsuffix epgbbfile.$ffhr$cfsuffix
		if [[ "$addgrb1id" = "yes" ]]; then
			# mv epgbbfile.$ffhr$cfsuffix pgbbfile.$ffhr$cfsuffix
			if [[ "$makegrb1i" = "yes" ]]; then
				$GRBINDEX pgbbfile.$ffhr$cfsuffix pgbbifile.$ffhr$cfsuffix
			fi

			if test "$SENDCOM" = 'YES'; then
				#
				# Save Pressure GRIB/Index files
				#
				mv pgbbfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrbb/${RUNMEM}.${cycle}.pgrbb$ffhr$cfsuffix
				testfile=$COMOUT/$cyc/pgrbb/${RUNMEM}.${cycle}.pgrbb$ffhr$cfsuffix
				if [[ ! -s $testfile ]]; then
					msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
					echo "`date`    $msg"
					postmsg "$jlogfile" "$msg"
					export err=1
					err_chk
				fi # [[ ! -s $testfile ]]
				mv pgbbifile.${ffhr}${cfsuffix} $COMOUT/$cyc/pgrbb/${RUNMEM}.${cycle}.pgrbbi${ffhr}${cfsuffix}
				testfile=$COMOUT/$cyc/pgrbb/${RUNMEM}.${cycle}.pgrbbi${ffhr}${cfsuffix}
				if [[ ! -s $testfile ]]; then
					msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
					echo "`date`    $msg"
					postmsg "$jlogfile" "$msg"
					export err=1
					err_chk
				fi # [[ ! -s $testfile ]]
				if test "$SENDDBN" = 'YES'; then
					if test "$NET" = 'gens'; then
						if test `echo $RUNMEM | cut -c1-2` = "ge" -a ! -n "$cfsuffix"; then
							MEMBER=`echo $RUNMEM | cut -c3-5 | tr '[a-z]' '[A-Z]'`
							$DBNROOT/bin/dbn_alert MODEL ENS_PGBB_$MEMBER $job $COMOUT/$cyc/pgrbb/${RUNMEM}.${cycle}.pgrbb$ffhr$cfsuffix
							$DBNROOT/bin/dbn_alert MODEL ENS_PGBB_${MEMBER}_WIDX $job \
							$COMOUT/$cyc/pgrbb/${RUNMEM}.${cycle}.pgrbbi$ffhr${cfsuffix}
						fi # test `echo $RUNMEM | cut -c1-2` = "ge" -a ! -n "$cfsuffix"
					fi # test "$NET" = 'gens'
				fi # test "$SENDDBN" = 'YES'
			fi # test "$SENDCOM" = 'YES'
		fi # [[ "$addgrb1id" = "yes" ]]
	fi # [[ -s $COMOUT/$cyc/pgrbb/${RUNMEM}.${cycle}.pgrbb$ffhr$cfsuffix ]] && [[ -s $COMOUT/$cyc/pgrbb/${RUNMEM}.${cycle}.pgrbbi$ffhr$cfsuffix ]] && [[ $overwrite = no ]]

	###############################
	# STEP IV: Create GRIBD files
	###############################
	if test "$makepgrb2d" = 'yes'; then
		if [[ -f $COMOUT/$cyc/pgrb2d/${RUNMEM}.${cycle}.pgrb2d$ffhr$cfsuffix ]] && \
		[[ -f $COMOUT/$cyc/pgrb2d/${RUNMEM}.${cycle}.pgrb2di$ffhr$cfsuffix ]] && \
		[[ $overwrite = no ]]; then
			echo `date` 1x1 pgrb2d processing skipped for $RUNMEM $ffhr
		else
			FILED=$COMIN/$cyc/pgrb2d/${RUNMEM}.${cycle}.pgrb2d$ffhr$cfsuffix

			$CNVGRIB -g21 $FILED pgbdfile.$ffhr$cfsuffix

			if test "$SENDCOM" = 'YES'; then
				#
				# Save Pressure GRIB/Index files
				#
				mv pgbdfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2d/${RUNMEM}.${cycle}.pgrb2d$ffhr$cfsuffix
				testfile=$COMOUT/$cyc/pgrb2d/${RUNMEM}.${cycle}.pgrb2d$ffhr$cfsuffix
				if [[ ! -s $testfile ]]; then
					msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
					echo "`date`    $msg"
					postmsg "$jlogfile" "$msg"
					export err=1
					err_chk
				fi # [[ ! -s $testfile ]]
			fi # test "$SENDCOM" = 'YES'
		fi # [[ -f $COMOUT/$cyc/pgrb2d/${RUNMEM}.${cycle}.pgrb2d$ffhr$cfsuffix ]] && [[ -f $COMOUT/$cyc/pgrb2d/${RUNMEM}.${cycle}.pgrb2di$ffhr$cfsuffix ]] && [[ $overwrite = no ]]
	fi # test "$makepgrb2d" = 'yes
fi # [[ "$makepgrb1" = "yes" ]]

########################################################
echo `date` $sname $member $partltr $cfsuffix $fsuffix 1x1 GRIB end on machine=`uname -n`
msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################

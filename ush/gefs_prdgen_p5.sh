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
export option1=' -set_grib_type same -new_grid_winds earth '
export option21=' -new_grid_interpolation bilinear  -if '
export option22=":(LAND|CSNOW|CRAIN|CFRZR|CICEP|ICSEV):"
export option23=' -new_grid_interpolation neighbor -fi '
export grid0p25="latlon 0:1440:0.25 90:721:-0.25"
export grid0p5="latlon 0:720:0.5 90:361:-0.5"
export grid1p0="latlon 0:360:1.0 90:181:-1.0"
export grid2p5="latlon 0:144:2.5 90:73:-2.5"

anlflag=$anlflag
ffhr=$ffhr
fhr=$fhr
grid=$grid0p5
export fhr1=$fhr
#typeset -RZ3 fhr1
#export WGRIB=${WGRIB:-$EXECgrib/wgrib}
#export GRBINDEX=${GRBINDEX:-$EXECgrib/grbindex}
#export COPYGB=${COPYGB:-$EXECgrib/copygb}
#export WGRIB2=${WGRIB2:-$EXECgrib/wgrib2}
#export GRB2INDEX=${GRB2INDEX:-$EXECgrib/grb2index}
#export COPYGB2=${COPYGB2:-$EXECgrib/copygb2}
#export CNVGRIB=${CNVGRIB:-$EXECgrib/cnvgrib21_gfs}

echo settings in $0 gefsmachine=$gefsmachine
echo settings in $0 WGRIB=$WGRIB
echo settings in $0 WGRIB2=$WGRIB2
echo settings in $0 GRBINDEX=$GRBINDEX
echo settings in $0 GRB2INDEX=$GRB2INDEX
echo settings in $0 COPYGB=$COPYGB
echo settings in $0 COPYGB2=$COPYGB2
echo settings in $0 CNVGRIB=$CNVGRIB

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
		echo unrecognized RUNMEM=$RUNMEM R1=$R1 R2=$R2
		;;
esac # $R1

msg="Starting post for member=$member ffhr=$ffhr"
postmsg "$jlogfile" "$msg"

####################################
# Step I: Create 1x1 pgrb2 files 
####################################
if [[ -s $DATA/pgrb2$ffhr$cfsuffix ]] && \
	[[ -s $DATA/pgrb2i$ffhr$cfsuffix ]] && \
	[[ $overwrite = no ]]; then
	echo `date` 1x1 pgrb2 processing skipped for $RUNMEM $ffhr
else

$WGRIB2 $COMIN/$cyc/master/$RUNMEM.$cycle.master.grb2f$fhr1$cfsuffix $option1 $option21 $option22 $option23 -new_grid $grid pgb2file.$ffhr$cfsuffix
#	$COPYGB2 -g "${grid}" -i0 -x $COMIN/$cyc/$mem/pgrbm$fhr1.gfs.$PDY$cyc.grib2 pgb2file.$ffhr$cfsuffix
#	$COPYGB2 -g "${grid}" -i0 -x $COMIN/$cyc/master/$RUNMEM.$cycle.master.grb2f$fhr1$cfsuffix pgb2file.$ffhr$cfsuffix
	echo `date` pgrb2ap5 1x1 grbfile $ffhr completed

	######################################################
	# Split the pgb2file into pgrb2ap5, pgrb2bp5 and pgrb2dp5 parts
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
	#  parmlist=$PARMgefs/gefs_pgrb2a_f${hsuffix}.parm
	parmlist=$PARMgefs/gefs_pgrb2a_0p50_f${hsuffix}.parm
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

	#  parmlist=$PARMgefs/gefs_pgrbb_f${hsuffix}.parm
	parmlist2=$PARMgefs/gefs_pgrb2ab_f${hsuffix}.parm
	$WGRIB2 -s pgb2file.$ffhr$cfsuffix | \
	grep -F -f $parmlist2 | \
	grep -v -F -f $parmlist | \
	grep -v -F $excludestring | \
	$WGRIB2 pgb2file.$ffhr$cfsuffix -s -i -grib pgb2bfile.$ffhr$cfsuffix
	$WGRIB2 -s pgb2bfile.$ffhr$cfsuffix > pgb2bfile.${ffhr}${cfsuffix}.idx
	#  $GRB2INDEX pgb2bfile.$ffhr$cfsuffix pgb2bfile.$ffhr$cfsuffix.idx

	if test "$makepgrb2d" = 'yes'; then
		$WGRIB2 -s pgb2file.$ffhr$cfsuffix | \
		grep -v -F -f $parmlist2 | \
		$WGRIB2 pgb2file.$ffhr$cfsuffix -s -i -grib pgb2dfile.$ffhr$cfsuffix
		$WGRIB2 -s pgb2file.$ffhr$cfsuffix | \
		grep -F -f $parmlist2 | \
		grep -F $excludestring | \
		$WGRIB2 pgb2file.$ffhr$cfsuffix -s -i -append -grib pgb2dfile.$ffhr$cfsuffix
		# end block removed from background
	fi # test "$makepgrb2d" = 'yes'
	#  set -x

	if test "$ffhr" = 'anl'; then
		pgffhr=$ffhr
	elif test $fhr -lt 100; then
		ahr=`echo $ffhr | cut -c2-3`
		pgffhr=f0$ahr
	else
		pgffhr=$ffhr
	fi # test "$ffhr" = 'anl'

	if test "$SENDCOM" = 'YES'; then
		#
		# Save Pressure GRIB/Index files
		#
		mv pgb2afile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2ap5/${RUNMEM}.${cycle}.pgrb2a.0p50.$pgffhr$cfsuffix
		testfile=$COMOUT/$cyc/pgrb2ap5/${RUNMEM}.${cycle}.pgrb2a.0p50.$pgffhr$cfsuffix
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "`date`    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]

		mv pgb2bfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2bp5/${RUNMEM}.${cycle}.pgrb2b.0p50.$pgffhr$cfsuffix
		testfile=$COMOUT/$cyc/pgrb2bp5/${RUNMEM}.${cycle}.pgrb2b.0p50.$pgffhr$cfsuffix
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "`date`    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]
		if test "$makepgrb2d" = 'yes'; then
			mv pgb2dfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2dp5/${RUNMEM}.${cycle}.pgrb2d.0p50.$pgffhr$cfsuffix
			testfile=$COMOUT/$cyc/pgrb2dp5/${RUNMEM}.${cycle}.pgrb2d.0p50.$pgffhr$cfsuffix
			if [[ ! -s $testfile ]]; then
				msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
				echo "`date`    $msg"
				postmsg "$jlogfile" "$msg"
				export err=1
				err_chk
			fi # [[ ! -s $testfile ]]
		fi # test "$makepgrb2d" = 'yes'
		if [[ "$makegrb2i" = "yes" ]]; then
			mv pgb2afile.$ffhr$cfsuffix.idx $COMOUT/$cyc/pgrb2ap5/${RUNMEM}.${cycle}.pgrb2a.0p50.$pgffhr$cfsuffix.idx
			testfile=$COMOUT/$cyc/pgrb2ap5/${RUNMEM}.${cycle}.pgrb2a.0p50.$pgffhr$cfsuffix.idx
			if [[ ! -s $testfile ]]; then
				msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
				echo "`date`    $msg"
				postmsg "$jlogfile" "$msg"
				export err=1
				err_chk
			fi # [[ ! -s $testfile ]]
			mv pgb2bfile.$ffhr$cfsuffix.idx $COMOUT/$cyc/pgrb2bp5/${RUNMEM}.${cycle}.pgrb2b.0p50.$pgffhr$cfsuffix.idx
			testfile=$COMOUT/$cyc/pgrb2bp5/${RUNMEM}.${cycle}.pgrb2b.0p50.$pgffhr$cfsuffix.idx
			if [[ ! -s $testfile ]]; then
				msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
				echo "`date`    $msg"
				postmsg "$jlogfile" "$msg"
				export err=1
				err_chk
			fi # [[ ! -s $testfile ]]
		fi # [[ "$makegrb2i" = "yes" ]]

		###############################################################################
		# Send DBNet alerts for  PGB2A at 3 hour increments for all forecast hours
		# Do for 00, 06, 12, and 18Z cycles.
		###############################################################################
		if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 6 ` -eq 0; then
			if test `echo $RUNMEM | cut -c1-2` = "ge"; then
				MEMBER=`echo $RUNMEM | cut -c3-5 | tr '[a-z]' '[A-Z]'`
				if [[ $fhr -ge 0 && $fhr -le $fhmax && ` expr $fhr % $FHINCP5 ` -eq 0 && ! -n "$cfsuffix" ]]; then
					$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_0P5_$MEMBER $job $COMOUT/$cyc/pgrb2ap5/${RUNMEM}.${cycle}.pgrb2a.0p50.$pgffhr$cfsuffix
					$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_0P5_${MEMBER}_WIDX $job $COMOUT/$cyc/pgrb2ap5/${RUNMEM}.${cycle}.pgrb2a.0p50.$pgffhr$cfsuffix.idx
				fi
			fi # test `echo $RUNMEM | cut -c1-2` = "ge"
		fi # test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 6 ` -eq 0

		###############################################################################
		# Send DBNet alerts for PGB2B at 3 hour increments for up to 84 hours
		# Do for 00Z and 12Z only
		###############################################################################
		if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a "$NET" = 'gens'; then
			if test `echo $RUNMEM | cut -c1-2` = "ge" -a ! -n "$cfsuffix"; then
				MEMBER=`echo $RUNMEM | cut -c3-5 | tr '[a-z]' '[A-Z]'`
				# if [[ $fhr -ge 0 && $fhr -le 84 && ` expr $fhr % $FHINCP5 ` -eq 0 && ! -n "$cfsuffix" ]]
				# then
				$DBNROOT/bin/dbn_alert MODEL ENS_PGB2B_0P5_$MEMBER $job $COMOUT/$cyc/pgrb2bp5/${RUNMEM}.${cycle}.pgrb2b.0p50.$pgffhr$cfsuffix
				$DBNROOT/bin/dbn_alert MODEL ENS_PGB2B_0P5_${MEMBER}_WIDX $job $COMOUT/$cyc/pgrb2bp5/${RUNMEM}.${cycle}.pgrb2b.0p50.$pgffhr$cfsuffix.idx
				# fi
			fi # test `echo $RUNMEM | cut -c1-2` = "ge" -a ! -n "$cfsuffix"

			###############################################################################
			# Do Not send DBNet alerts for the PGBD files at this time
			###############################################################################
		fi # test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a "$NET" = 'gens'
	fi # test "$SENDCOM" = 'YES'

	echo `date` pgrb2ap5 0.5x0.5 sendcom $ffhr completed
fi # [[ -s $DATA/pgrb2$ffhr$cfsuffix ]] && [[ -s $DATA/pgrb2i$ffhr$cfsuffix ]] && [[ $overwrite = no ]]

if [[ "$makepgrb1" = "yes" ]]; then
	######################################
	# Step II: Create GRIBA files
	#####################################
	if [[ -s $COMOUT/$cyc/pgrbap5/${RUNMEM}.${cycle}.pgrbap5$pgffhr$cfsuffix ]] && \
		[[ -s $COMOUT/$cyc/pgrbap5/${RUNMEM}.${cycle}.pgrbap5i$pgffhr$cfsuffix ]] && \
		[[ $overwrite = no ]]; then
		echo `date` 1x1 pgrbap5 processing skipped for $RUNMEM $pgffhr
	else
		FILEA=$COMIN/$cyc/pgrb2ap5/${RUNMEM}.${cycle}.pgrb2a.0p50.$pgffhr$cfsuffix
		$CNVGRIB -g21 $FILEA pgbafile.$ffhr$cfsuffix
		#  ls -lt  pgbafile.$ffhr$cfsuffix pgbaifile.$ffhr$cfsuffix 
		if [[ "$addgrb1id" = "yes" ]]; then
			if [[ "$makegrb1i" = "yes" ]]; then
				$GRBINDEX pgbafile.$ffhr$cfsuffix pgbaifile.$ffhr$cfsuffix
			fi

			if test "$SENDCOM" = 'YES'; then
				#
				# Save Pressure GRIB/Index files
				#
				mv pgbafile.$ffhr$cfsuffix $COMOUT/$cyc/pgrbap5/${RUNMEM}.${cycle}.pgrbap5$pgffhr$cfsuffix
				testfile=$COMOUT/$cyc/pgrbap5/${RUNMEM}.${cycle}.pgrbap5$pgffhr$cfsuffix
				if [[ ! -s $testfile ]]; then
					msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
					echo "`date`    $msg"
					postmsg "$jlogfile" "$msg"
					export err=1
					err_chk
				fi # [[ ! -s $testfile ]]
				mv pgbaifile.${ffhr}${cfsuffix} $COMOUT/$cyc/pgrbap5/${RUNMEM}.${cycle}.pgrbap5i${pgffhr}${cfsuffix}
				testfile=$COMOUT/$cyc/pgrbap5/${RUNMEM}.${cycle}.pgrbap5i${pgffhr}${cfsuffix}
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
							$DBNROOT/bin/dbn_alert MODEL ENS_PGBA_0P5_$MEMBER $job $COMOUT/$cyc/pgrbap5/${RUNMEM}.${cycle}.pgrbap5$pgffhr$cfsuffix
							$COMOUT/$cyc/pgrbap5/${RUNMEM}.${cycle}.pgrbap5i${pgffhr}${cfsuffix}
						fi # test `echo $RUNMEM | cut -c1-2` = "ge" -a ! -n "$cfsuffix"
					fi # test "$NET" = 'gens'
				fi # test "$SENDDBN" = 'YES'
			fi # test "$SENDCOM" = 'YES'
		fi # [[ -s $COMOUT/$cyc/pgrbap5/${RUNMEM}.${cycle}.pgrbap5$pgffhr$cfsuffix ]] && [[ -s $COMOUT/$cyc/pgrbap5/${RUNMEM}.${cycle}.pgrbap5i$pgffhr$cfsuffix ]] && [[ $overwrite = no ]]
	fi

	###########################################
	# STEP III: Create GRIBB files
	###########################################
	if [[ -s $COMOUT/$cyc/pgrbbp5/${RUNMEM}.${cycle}.pgrbbp5$pgffhr$cfsuffix ]] && \
		[[ -s $COMOUT/$cyc/pgrbbp5/${RUNMEM}.${cycle}.pgrbbp5i$pgffhr$cfsuffix ]] && \
		[[ $overwrite = no ]]; then
		echo `date` 1x1 pgrbbp5 processing skipped for $RUNMEM $ffhr
	else
		FILEB=$COMIN/$cyc/pgrb2bp5/${RUNMEM}.${cycle}.pgrb2b.0p50.$pgffhr$cfsuffix
		$CNVGRIB -g21 $FILEB pgbbfile.$ffhr$cfsuffix
		if [[ "$addgrb1id" = "yes" ]]; then
			if [[ "$makegrb1i" = "yes" ]]; then
				$GRBINDEX pgbbfile.$ffhr$cfsuffix pgbbifile.$ffhr$cfsuffix
			fi

			if test "$SENDCOM" = 'YES'; then
				#
				# Save Pressure GRIB/Index files
				#
				mv pgbbfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrbbp5/${RUNMEM}.${cycle}.pgrbbp5$pgffhr$cfsuffix
				testfile=$COMOUT/$cyc/pgrbbp5/${RUNMEM}.${cycle}.pgrbbp5$pgffhr$cfsuffix
				if [[ ! -s $testfile ]]; then
					msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
					echo "`date`    $msg"
					postmsg "$jlogfile" "$msg"
					export err=1
					err_chk
				fi # [[ ! -s $testfile ]]
				mv pgbbifile.${ffhr}${cfsuffix} $COMOUT/$cyc/pgrbbp5/${RUNMEM}.${cycle}.pgrbbp5i${pgffhr}${cfsuffix}
				testfile=$COMOUT/$cyc/pgrbbp5/${RUNMEM}.${cycle}.pgrbbp5i${pgffhr}${cfsuffix}
				if [[ ! -s $testfile ]]; then
					msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
					echo "`date`    $msg"
					postmsg "$jlogfile" "$msg"
					export err=1
					err_chk
				fi # [[ ! -s $testfile ]]

				#if test "$SENDDBN" = 'YES'
				#then
				#if test "$NET" = 'gens'
				#then
				#if test `echo $RUNMEM | cut -c1-2` = "ge" -a ! -n "$cfsuffix"
				#then
				#MEMBER=`echo $RUNMEM | cut -c3-5 | tr '[a-z]' '[A-Z]'`
				#$DBNROOT/bin/dbn_alert MODEL ENS_PGBB_0P5_$MEMBER $job $COMOUT/$cyc/pgrbbp5/${RUNMEM}.${cycle}.pgrbbp5$pgffhr$cfsuffix
				#$DBNROOT/bin/dbn_alert MODEL ENS_PGBB_0P5_${MEMBER}_WIDX $job \
				#       $COMOUT/$cyc/pgrbbp5/${RUNMEM}.${cycle}.pgrbbp5i$.pgffhr${cfsuffix}

				#if test "$CREATE_TIGGE" = 'YES'
				#then
				#  $DBNROOT/bin/dbn_alert MODEL ENS_PGB2C_0P5_$MEMBER $job $COMOUT/$cyc/pgrb2cp5/${RUNMEM}.${cycle}.pgrb2cp5$pgffhr$cfsuffix
				#fi
				#fi
				#fi
				#fi
			fi # test "$SENDCOM" = 'YES'
		fi # [[ "$addgrb1id" = "yes" ]]
	fi # [[ -s $COMOUT/$cyc/pgrbbp5/${RUNMEM}.${cycle}.pgrbbp5$pgffhr$cfsuffix ]] && [[ -s $COMOUT/$cyc/pgrbbp5/${RUNMEM}.${cycle}.pgrbbp5i$pgffhr$cfsuffix ]] && [[ $overwrite = no ]]

	###############################
	# STEP IV: Create GRIBD files
	###############################
	if test "$makepgrb2d" = 'yes'; then
		if [[ -f $COMOUT/$cyc/pgrb2dp5/${RUNMEM}.${cycle}.pgrb2dp5$pgffhr$cfsuffix ]] && \
			[[ -f $COMOUT/$cyc/pgrb2dp5/${RUNMEM}.${cycle}.pgrb2dp5i$pgffhr$cfsuffix ]] && \
			[[ $overwrite = no ]]; then
			echo `date` 1x1 pgrb2dp5 processing skipped for $RUNMEM $ffhr
		else
			FILED=$COMIN/$cyc/pgrb2dp5/${RUNMEM}.${cycle}.pgrb2d.0p50.$pgffhr$cfsuffix
			$CNVGRIB -g21 $FILED pgbdfile.$ffhr$cfsuffix

			if test "$SENDCOM" = 'YES'; then
				#
				# Save Pressure GRIB/Index files
				#
				mv pgbdfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2dp5/${RUNMEM}.${cycle}.pgrb2dp5$pgffhr$cfsuffix
				testfile=$COMOUT/$cyc/pgrb2dp5/${RUNMEM}.${cycle}.pgrb2dp5$pgffhr$cfsuffix
				if [[ ! -s $testfile ]]; then
					msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
					echo "`date`    $msg"
					postmsg "$jlogfile" "$msg"
					export err=1
					err_chk
				fi # [[ ! -s $testfile ]]
			fi # test "$SENDCOM" = 'YES'
		fi # [[ -f $COMOUT/$cyc/pgrb2dp5/${RUNMEM}.${cycle}.pgrb2dp5$pgffhr$cfsuffix ]] && [[ -f $COMOUT/$cyc/pgrb2dp5/${RUNMEM}.${cycle}.pgrb2dp5i$pgffhr$cfsuffix ]] && [[ $overwrite = no ]]
	fi # test "$makepgrb2d" = 'yes'

fi # [[ "$makepgrb1" = "yes" ]]
########################################################
echo `date` $sname $member $partltr $cfsuffix $fsuffix 1x1 GRIB end on machine=`uname -n`
msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################

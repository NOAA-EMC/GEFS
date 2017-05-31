#!/bin/ksh
#####################################################################
echo "-----------------------------------------------------"
echo " Script: gefs_prdgen_lowres.sh" 
echo " "
echo " Purpose - Perform interpolation and GRIB2 conversion"
echo "           on master GRIB files to 2.5x2.5 degree"
echo "           for one member and one time step."
echo "           Move posted files to /com"
echo "           Alert posted files to DBNet"
echo " "
echo " History - "
echo "    Wobus   - 8/28/07 - New "
echo "    Wobus   - 7/30/10 - exclude 180-192hr products"
echo "    Hou     - 7/31/14 - adopted for grib2 based processing"
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
grid=$grid2p5
export fhr1=$fhr
typeset -RZ3 fhr1
#export WGRIB=${WGRIB:-$EXECgrib/wgrib}
#export GRBINDEX=${GRBINDEX:-$EXECgrib/grbindex}
#export COPYGB=${COPYGB:-$EXECgrib/copygb}
#export WGRIB2=${WGRIB2:-$EXECgrib/wgrib2}
#export GRB2INDEX=${GRB2INDEX:-$EXECgrib/grb2index}
#export COPYGB2=${COPYGB2:-$EXECgrib/copygb2}
#export CNVGRIB=${CNVGRIB:-$EXECgrib/cnvgrib21}

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
# Step I: create 2.5 x 2.5 pgrb2
####################################
$WGRIB2 $COMIN/$cyc/master/$RUNMEM.$cycle.master.grb2f$fhr1$cfsuffix $option1 $option21 $option22 $option23 -new_grid $grid pgb2file.$ffhr.2$cfsuffix

#$COPYGB2 -g "${grid}" -i0 -x $COMIN/$cyc/$mem/pgrbm$fhr1.gfs.$PDY$cyc.grib2 pgb2file.$ffhr.2$cfsuffix
#$COPYGB2 -g "${grid}" -i0 -x $COMIN/$cyc/master/$RUNMEM.$cycle.master.grb2f$fhr1$cfsuffix pgb2file.$ffhr.2$cfsuffix
echo `date` pgrba 2.5x2.5 grbindex $ffhr completed

if (( fhr == 0 )); then
	hsuffix="00"
else
	hsuffix="hh"
fi

excludestring='180-192hr'

#######################################
# Step II: Create 2.5x2.5 PGRB2A files
#######################################
if [[ -s $COMOUT/$cyc/pgrb2alr/${RUNMEM}.${cycle}.pgrb2a$ffhr.2$cfsuffix ]] && \
	[[ -s $COMOUT/$cyc/pgrb2alr/${RUNMEM}.${cycle}.pgrb2a$ffhr.2$cfsuffix.idx ]] && \
	[[ $overwrite = no ]]; then
	echo `date` "2.5x2.5 pgrb2a processing skipped for $RUNMEM $ffhr"
else
	parmlist=$PARMgefs/gefs_pgrb2a_f${hsuffix}.parm
	set +x
	$WGRIB2 -s pgb2file.$ffhr.2$cfsuffix | \
	grep -F -f $parmlist | \
	grep -v -F $excludestring | \
	$WGRIB2 pgb2file.$ffhr.2$cfsuffix -s -i -grib pgb2afile.$ffhr.2$cfsuffix
	if [[ x$fhoroglist != x ]]; then
		for fhorog in $fhoroglist; do
			if (( fhr == fhorog )); then
				$WGRIB2 -s pgb2file.$ffhr.2$cfsuffix | grep 'HGT:surface' | $WGRIB2 pgb2file.$ffhr.2$cfsuffix -i -append -grib pgb2afile.$ffhr.2$cfsuffix
			fi
		done # for fhorog in $fhoroglist
	fi # [[ x$fhoroglist != x ]]
	set -x
	$WGRIB2 -s pgb2afile.$ffhr.2$cfsuffix > pgb2afile.$ffhr.2${cfsuffix}.idx
fi # [[ -s $COMOUT/$cyc/pgrb2alr/${RUNMEM}.${cycle}.pgrb2a$ffhr.2$cfsuffix ]] && [[ -s $COMOUT/$cyc/pgrb2alr/${RUNMEM}.${cycle}.pgrb2a$ffhr.2$cfsuffix.idx ]] && [[ $overwrite = no ]]

#######################################
# Step III: Create 2.5x2.5 PGRB2B files
#######################################
if [[ -s $COMOUT/$cyc/pgrb2blr/${RUNMEM}.${cycle}.pgrb2b$ffhr.2$cfsuffix ]] && \
	[[ -s $COMOUT/$cyc/pgrb2blr/${RUNMEM}.${cycle}.pgrb2b$ffhr.2$cfsuffix.idx ]] && \
	[[ $overwrite = no ]]; then
	echo `date` 2.5x2.5 pgrb2b processing skipped for $RUNMEM $ffhr
else
	parmlist2=$PARMgefs/gefs_pgrb2ab_f${hsuffix}.parm
	set +x
	$WGRIB2 -s pgb2file.$ffhr.2$cfsuffix | \
	grep -F -f $parmlist2 | \
	grep -v -F -f $parmlist | \
	grep -v -F $excludestring | \
	$WGRIB2 pgb2file.$ffhr.2$cfsuffix -s -i -grib pgb2bfile.$ffhr.2$cfsuffix
	$WGRIB2 -s pgb2bfile.$ffhr.2$cfsuffix > pgb2bfile.$ffhr.2${cfsuffix}.idx
	set -x
fi # [[ -s $COMOUT/$cyc/pgrb2blr/${RUNMEM}.${cycle}.pgrb2b$ffhr.2$cfsuffix ]] && [[ -s $COMOUT/$cyc/pgrb2blr/${RUNMEM}.${cycle}.pgrb2b$ffhr.2$cfsuffix.idx ]] && [[ $overwrite = no ]]

if test "$SENDCOM" = 'YES'; then
	#
	# Save Pressure GRIB/Index files
	#
	mv pgb2afile.$ffhr.2$cfsuffix $COMOUT/$cyc/pgrb2alr/${RUNMEM}.${cycle}.pgrb2a$ffhr.2$cfsuffix
	testfile=$COMOUT/$cyc/pgrb2alr/${RUNMEM}.${cycle}.pgrb2a$ffhr.2$cfsuffix
	if [[ ! -s $testfile ]]; then
		msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
		echo "`date`    $msg"
		postmsg "$jlogfile" "$msg"
		export err=1
		err_chk
	fi # [[ ! -s $testfile ]]
	mv pgb2bfile.$ffhr.2$cfsuffix $COMOUT/$cyc/pgrb2blr/${RUNMEM}.${cycle}.pgrb2b$ffhr.2$cfsuffix
	testfile=$COMOUT/$cyc/pgrb2blr/${RUNMEM}.${cycle}.pgrb2b$ffhr.2$cfsuffix
	if [[ ! -s $testfile ]]; then
		msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
		echo "`date`    $msg"
		postmsg "$jlogfile" "$msg"
		export err=1
		err_chk
	fi # [[ ! -s $testfile ]]
	if [[ "$makegrb2i" = "yes" ]]; then
		mv pgb2afile.$ffhr.2$cfsuffix.idx $COMOUT/$cyc/pgrb2alr/${RUNMEM}.${cycle}.pgrb2a$ffhr.2$cfsuffix.idx
		testfile=$COMOUT/$cyc/pgrb2alr/${RUNMEM}.${cycle}.pgrb2a$ffhr.2$cfsuffix.idx
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "`date`    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]
		mv pgb2bfile.$ffhr.2$cfsuffix.idx $COMOUT/$cyc/pgrb2blr/${RUNMEM}.${cycle}.pgrb2b$ffhr.2$cfsuffix.idx
		testfile=$COMOUT/$cyc/pgrb2blr/${RUNMEM}.${cycle}.pgrb2b$ffhr.2$cfsuffix.idx
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "`date`    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]
	fi # [[ "$makegrb2i" = "yes" ]]

	######################################################################################
	# Send DBNet alerts for PGBA2 at 6 hour increments for all forecast hours
	# Do for 00, 06, 12, and 18Z cycles.
	######################################################################################
	if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 6 ` -eq 0; then
		if test `echo $RUNMEM | cut -c1-2` = "ge"; then
			MEMBER=`echo $RUNMEM | cut -c3-5 | tr '[a-z]' '[A-Z]'`
			if test "$DO_LOW_RES" = 'YES' -a ` expr $fhr % 6 ` -eq 0 -a ! -n "$cfsuffix"; then
				$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A2_$MEMBER $job $COMOUT/$cyc/pgrb2alr/${RUNMEM}.${cycle}.pgrb2a$ffhr.2$cfsuffix
				if [[ "$makegrb2i" = "yes" ]]; then
					$DBNROOT/bin/dbn_alert MODEL ENS_PGB2A2_${MEMBER}_WIDX $job $COMOUT/$cyc/pgrb2alr/${RUNMEM}.${cycle}.pgrb2a$ffhr.2${cfsuffix}.idx
				fi
			fi # test "$DO_LOW_RES" = 'YES' -a ` expr $fhr % 6 ` -eq 0 -a ! -n "$cfsuffix"
		fi # test `echo $RUNMEM | cut -c1-2` = "ge"
	fi # test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 6 ` -eq 0

	######################################################################################
	# Send DBNet alerts for PGB2B at 6 hour increments for forecast hours 0 - 84 and
	# for PGBB2 at 6 hour increments for forecast hours 90 through 384.
	# Do for 00 and 12Z cycles.
	######################################################################################

	if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 12 ` -eq 0; then
		if test `echo $RUNMEM | cut -c1-2` = "ge"; then
			MEMBER=`echo $RUNMEM | cut -c3-5 | tr '[a-z]' '[A-Z]'`
			if test "$DO_LOW_RES" = 'YES' -a ` expr $fhr % 6 ` -eq 0 -a $fhr -ge 90 -a ! -n "$cfsuffix"; then
				$DBNROOT/bin/dbn_alert MODEL ENS_PGB2B2_$MEMBER $job $COMOUT/$cyc/pgrb2blr/${RUNMEM}.${cycle}.pgrb2b$ffhr.2$cfsuffix
				if [[ "$makegrb2i" = "yes" ]]; then
					$DBNROOT/bin/dbn_alert MODEL ENS_PGB2B2_${MEMBER}_WIDX $job $COMOUT/$cyc/pgrb2blr/${RUNMEM}.${cycle}.pgrb2b$ffhr.2$cfsuffix.idx
				fi
			fi # test "$DO_LOW_RES" = 'YES' -a ` expr $fhr % 6 ` -eq 0 -a $fhr -ge 90 -a ! -n "$cfsuffix"
		fi # test `echo $RUNMEM | cut -c1-2` = "ge"
	fi # test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 12 ` -eq 0
fi # test "$SENDCOM" = 'YES'

if [[ "$makepgrb1" = "yes" ]]; then
	#DHOU 03/23/2012, skip grib2 files for ZEUS
	#########################################
	# Step IV: Create the 2.5x2.5 GRIB files
	########################################
	if [[ -s $COMOUT/$cyc/pgrbalr/${RUNMEM}.${cycle}.pgrba$ffhr.2$cfsuffix ]] && \
		[[ -s $COMOUT/$cyc/pgrbalr/${RUNMEM}.${cycle}.pgrbai$ffhr.2$cfsuffix ]] && \
		[[ $overwrite = no ]]; then
		echo `date` 2.5x2.5 pgrba processing skipped for $RUNMEM $ffhr
	else
		FILEALR=$COMIN/$cyc/pgrb2alr/${RUNMEM}.${cycle}.pgrb2a$ffhr.2$cfsuffix

		$CNVGRIB -g21 $FILEALR pgbafile.$ffhr.2$cfsuffix
		$GRBINDEX pgbafile.$ffhr.2$cfsuffix pgbai.$ffhr.2$cfsuffix
		# $WGRIB -s pgba.$ffhr.2$cfsuffix >pgba.$ffhr.2${cfsuffix}.idx
		# $ENSADD $e1 $e2 pgbafile.$ffhr.2$cfsuffix pgbai.$ffhr.2$cfsuffix epgba.$ffhr.2$cfsuffix
		if [[ "$addgrb1id" = "yes" ]]; then
			# mv epgba.$ffhr.2$cfsuffix pgbafile.$ffhr.2$cfsuffix
			if [[ "$makegrb1i" = "yes" ]]; then
				$GRBINDEX pgbafile.$ffhr.2$cfsuffix pgbai.$ffhr.2$cfsuffix
			fi
		fi # [[ "$addgrb1id" = "yes" ]]

		if test "$SENDCOM" = 'YES'; then
			#
			# Save Pressure GRIB/Index files
			#
			mv pgbafile.$ffhr.2$cfsuffix $COMOUT/$cyc/pgrbalr/${RUNMEM}.${cycle}.pgrba$ffhr.2$cfsuffix
			testfile=$COMOUT/$cyc/pgrbalr/${RUNMEM}.${cycle}.pgrba$ffhr.2$cfsuffix
			if [[ ! -s $testfile ]]; then
				msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
				echo "`date`    $msg"
				postmsg "$jlogfile" "$msg"
				export err=1
				err_chk
			fi # [[ ! -s $testfile ]]
			mv pgbai.$ffhr.2${cfsuffix} $COMOUT/$cyc/pgrbalr/${RUNMEM}.${cycle}.pgrbai$ffhr.2${cfsuffix}
			testfile=$COMOUT/$cyc/pgrbalr/${RUNMEM}.${cycle}.pgrbai$ffhr.2${cfsuffix}
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
						$DBNROOT/bin/dbn_alert MODEL ENS_PGBA2_$MEMBER $job $COMOUT/$cyc/pgrbalr/${RUNMEM}.${cycle}.pgrba$ffhr.2$cfsuffix
						$DBNROOT/bin/dbn_alert MODEL ENS_PGBA2_${MEMBER}_WIDX $job \
						$COMOUT/$cyc/pgrbalr/${RUNMEM}.${cycle}.pgrbai$ffhr.2${cfsuffix}
					fi # test `echo $RUNMEM | cut -c1-2` = "ge" -a ! -n "$cfsuffix"
				fi # test "$NET" = 'gens'
			fi # test "$SENDDBN" = 'YES'
		fi # test "$SENDCOM" = 'YES'
	fi


	if [[ -s $COMOUT/$cyc/pgrbblr/${RUNMEM}.${cycle}.pgrbb$ffhr.2$cfsuffix ]] && \
		[[ -s $COMOUT/$cyc/pgrbblr/${RUNMEM}.${cycle}.pgrbbi$ffhr.2$cfsuffix ]] && \
		[[ $overwrite = no ]]; then
		echo `date` 2.5x2.5 pgrbb processing skipped for $RUNMEM $ffhr
	else
		FILEBLR=$COMIN/$cyc/pgrb2blr/${RUNMEM}.${cycle}.pgrb2b$ffhr.2$cfsuffix

		$CNVGRIB -g21 $FILEBLR pgbbfile.$ffhr.2$cfsuffix
		$GRBINDEX pgbbfile.$ffhr.2$cfsuffix pgbbifile.$ffhr.2$cfsuffix

		if test "$SENDCOM" = 'YES'; then
			#
			# Save Pressure GRIB/Index files
			#
			mv pgbbfile.$ffhr.2$cfsuffix $COMOUT/$cyc/pgrbblr/${RUNMEM}.${cycle}.pgrbb$ffhr.2$cfsuffix
			testfile=$COMOUT/$cyc/pgrbblr/${RUNMEM}.${cycle}.pgrbb$ffhr.2$cfsuffix
			if [[ ! -s $testfile ]]; then
				msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
				echo "`date`    $msg"
				postmsg "$jlogfile" "$msg"
				export err=1
				err_chk
			fi # [[ ! -s $testfile ]]
			mv pgbbifile.$ffhr.2${cfsuffix} $COMOUT/$cyc/pgrbblr/${RUNMEM}.${cycle}.pgrbbi$ffhr.2${cfsuffix}
			testfile=$COMOUT/$cyc/pgrbblr/${RUNMEM}.${cycle}.pgrbbi$ffhr.2${cfsuffix}
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
						$DBNROOT/bin/dbn_alert MODEL ENS_PGBB2_$MEMBER $job $COMOUT/$cyc/pgrbblr/${RUNMEM}.${cycle}.pgrbb$ffhr.2$cfsuffix
						$DBNROOT/bin/dbn_alert MODEL ENS_PGBB2_${MEMBER}_WIDX $job \
						$COMOUT/$cyc/pgrbblr/${RUNMEM}.${cycle}.pgrbbi$ffhr.2${cfsuffix}
					fi # test `echo $RUNMEM | cut -c1-2` = "ge" -a ! -n "$cfsuffix"
				fi # test "$NET" = 'gens'
			fi # test "$SENDDBN" = 'YES'
		fi # test "$SENDCOM" = 'YES'
	fi # [[ -s $COMOUT/$cyc/pgrbblr/${RUNMEM}.${cycle}.pgrbb$ffhr.2$cfsuffix ]] && [[ -s $COMOUT/$cyc/pgrbblr/${RUNMEM}.${cycle}.pgrbbi$ffhr.2$cfsuffix ]] && [[ $overwrite = no ]]
fi # [[ "$makepgrb1" = "yes" ]]

####################################
# Step V: Save forecasts to /nwges
####################################
if test "$SAVEGES" = "YES" -a $fhr -le 15; then
	cp $COMOUT/$cyc/sfcsig/${RUNMEM}.t${cyc}z.s$ffhr$cfsuffix $GESdir/${RUNMEM}.${cycle}.s$ffhr$cfsuffix
	testfile=$GESdir/${RUNMEM}.${cycle}.s$ffhr$cfsuffix
	if [[ ! -s $testfile ]]; then
		msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
		echo "`date`    $msg"
		postmsg "$jlogfile" "$msg"
		export err=1
		err_chk
	fi # [[ ! -s $testfile ]]
	cp $COMOUT/$cyc/sfcsig/${RUNMEM}.t${cyc}z.b$ffhr$cfsuffix $GESdir/${RUNMEM}.${cycle}.b$ffhr$cfsuffix
	testfile=$GESdir/${RUNMEM}.${cycle}.b$ffhr$cfsuffix
	if [[ ! -s $testfile ]]; then
		msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
		echo "`date`    $msg"
		postmsg "$jlogfile" "$msg"
		export err=1
		err_chk
	fi # [[ ! -s $testfile ]]
	msg="Guess files for fcst hour $fhr copied to $GESdir"
	postmsg "$jlogfile" "$msg"
fi # test "$SAVEGES" = "YES" -a $fhr -le 15
echo `date` sf and bf copied to nwges $ffhr completed

##########################################
# Step VI: Create sigstats in non-nco run
##########################################
#DHOU 04/14/2012 temporly commented out this bloc as sigstat is not found
#if [[ $envir != prod ]]; then
#  if [[ $envir != para ]]; then
#    if [[ $envir != test ]]; then
#      sigfilename=${RUNMEM}.t${cyc}z.s$ffhr$cfsuffix
#      $EXECgefs/sigstat $COMIN/$cyc/sfcsig/$sigfilename >$COMOUT/$cyc/stats/sigstat.$sigfilename
#    fi
#  fi
#fi

########################################################
echo `date` $sname $member $partltr $cfsuffix $fsuffix 2.5x2.5 GRIB end on machine=`uname -n`
msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################

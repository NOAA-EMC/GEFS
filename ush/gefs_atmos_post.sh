#! /bin/ksh

#   Imported Shell Variables:
#     SIGINP        Input sigma file

#####################################################################
# -----------------------------------------------------
#  exgefs_nceppost_master.sh.sms"
#  based on exglobal_post.sh.sms"
#  Apr 99 - Michaud - Generated to post global forecast
#  Mar 03 - Zhu - Add post for 0.5x0.5 degree
#  Jul 05 - Wobus - 6-hour breeding, ensemble only
#  Jul 07 - Wobus - nceppost creates master files
#  Jul 10 - Wobus - create additional anl log file
#  Jun 11 - Wobus - move master post files into /com
#  Dec 14 - Hou - updated to grib2 format and add grib2 ensemble messages
#  Feb 18 - Hou - updated to process FV3 based nemsio files
#  Jun 18 - Hou - replaced hard-wired output frequency with FHOUT_HF and FHOUT
#  Jul 18 - Hou - removed variables related to cyc_fcst (Breeding) 
#  Jul 26 - Xue - Add RUN_HOURS to avoid modifying two loop and make it fore flexiable
#                 Add list of variables for future reference
# -----------------------------------------------------
#####################################################################

echo "$(date -u) begin ${.sh.file}"

set -x

cd $DATA

export SHOUR=${1:-${SHOUR}}
export FHOUR=${2:-${FHOUR}}
export FHOUT_HF=${3:-${FHOUT_HF}}
export FHOUT_LF=${4:-${FHOUT_LF}}
export FHMAX_HF=${5:-${FHMAX_HF}}
export SHOUR_LF=${6:-${SHOUR_LF}}

export err=0

# Get RUN_HOURS
typeset -a RUN_HOURS
iHour=0
(( fhr=SHOUR ))
while ((fhr <= FHOUR)); do
	fhr=$(printf %03i $fhr)
	echo $fhr
	RUN_HOURS[iHour]=$fhr

	if (( FHMAX_HF > 0 && FHOUT_HF > 0 && fhr < FHMAX_HF )); then
		FHINC=$FHOUT_HF
	else
		FHINC=$FHOUT_LF
	fi

	export fhrtest=$(( fhr + FHINC ))
	if (( FHMAX_HF > 0 && FHOUT_HF > 0 && fhr < $FHMAX_HF && fhrtest >= FHMAX_HF )); then
		export FHINC=$FHOUT_LF
		export fhr=$(( SHOUR_LF ))
	else
		export fhr=$(( fhr + FHINC ))
	fi
	iHour=$(( iHour + 1 ))
done # while (( fhr <= FHOUR ))

echo "RUN_HOURS = ${RUN_HOURS[@]}"

export MP_LABELIO=YES

export POSTGPSH=${POSTGPSH:-$USHpost/global_nceppost.sh}
export SIGHDR=${SIGHDR:-$EXECgfs/global_sighdr}
export nemsioget=$nemsioget

R2=$(echo $RUNMEM|cut -c4-5)
mem=$(echo $RUNMEM|cut -c3-5)
case $RUNMEM in
	(gec00 | geaer)
		ens_pert_type='unpert_lo_res_ctrl_fcst'
		(( ens_pert_numb = 0 ))
		(( e1 = 1 ))
		;;
	(gep[0-9][0-9])
		ens_pert_type='pos_pert_fcst'
		(( ens_pert_numb = $R2 ))
		(( e1 = 3 ))
		;;
	(*)
		echo "FATAL: Unrecognized RUNMEM $RUNMEM, unable to determine pert type"
		export err=200
		err_chk; exit $err
		;;
esac # $RUNMEM
export ens_pert_type
export ens_pert_numb
export ens=YES
# e1,e2,e3 are used to set the grib ensemble information
export e1=$e1
export e2=$R2
export e3=$npert
echo <<- EOF
	ens_pert_type: $ens_pert_type
	ens_pert_numb: $ens_pert_type
	ens: $ens
	EOF

############################################################
#  Define Variables:
#  -----------------
#  SHOUR        is the starting forecast hour. normally 0 except for restarts.
#  FHOUR        is the ending forecast hour.
#  FHINC        is the increment hour for each forecast steps.
#  FH           is the current forecast hour.
#  SLEEP_TIME   is the number of seconds to sleep before exiting with error.
#  SLEEP_INT    is the number of seconds to sleep between restrt file checks.
#  restart_file is the name of the file to key off of to kick off post.
############################################################

############################################################
# remove control files as needed
############################################################
function check_existing_post {
	fhr=$1
	if [[ $fhr == anl ]]; then
		ffhr="anl"
		source_file="$COMIN/init/${RUNMEM}.t${cyc}z.sanl"
	else
		ffhr="f${fhr}"
		source_file="$restart_file${fhr}.nemsio"
	fi

	mcfile=$COMOUT/$COMPONENT/misc/post/${RUNMEM}.t${cyc}z.master.control.${ffhr}

	if [[ ! -f $source_file || $RERUN == YES ]]; then
		if [[ -f $mcfile ]]; then rm $mcfile; fi
	fi

	if [ $GRIBVERSION = grib1 ]; then
		fileout=$COMOUT/$COMPONENT/master/$RUNMEM.$cycle.master.grb${ffhr}
		fileouti=$COMOUT/$COMPONENT/master/$RUNMEM.$cycle.master.grbi${ffhr}
	elif [ $GRIBVERSION = grib2 ]; then
		fileout=$COMOUT/$COMPONENT/master/$RUNMEM.$cycle.master.grb2${ffhr}
		fileouti=$COMOUT/$COMPONENT/master/$RUNMEM.$cycle.master.grb2i${ffhr}
	fi # [ $GRIBVERSION = grib1 ]

	if [[ -s $mcfile ]]; then
		if [[ -s $fileout ]]; then
			if [[ -s $fileouti ]]; then
				# cat $mcfile
				echo "All post files exist for ${ffhr}"
			else
				echo <<- EOF
					Post output $fileout exists but index $fileouti is missing!
					  Deleting control file $mcfile so post is regenerated
					EOF
				if [[ -f $mcfile ]]; then rm $mcfile; fi
			fi # [[ -s $fileouti ]]
		else
			echo <<- EOF
				Control file $mcfile exists but post output $fileout is missing!
				  Deleting control file $mcfile so post is regenerated
			EOF
			if [[ -f $mcfile ]]; then rm $mcfile; fi
		fi # [[ -s $fileout ]]
	fi # [[ -s $mcfile ]]

	# Remove associated post files for any missing control files
	if [[ ! -s $mcfile ]]; then
		# 20150622 RLW disable this temporarily to test the new post code
		if [[ -f $fileout ]]; then rm $fileout; fi
		if [[ -f $fileouti ]]; then rm $fileouti; fi
		for submc in prd0p25 prd0p5 prd2p5; do
			pcfile=$COMOUT/$COMPONENT/misc/$submc/${RUNMEM}.t${cyc}z.prdgen.control.${ffhr}
			if [[ -f $pcfile ]]; then rm $pcfile; fi
		done # for submc in prd0p25 prd0p5 prd2p5
	fi # [[ ! -s $mcfile ]]
}

if [[ $SENDCOM == "YES" ]]; then
	if [ $DOANALYSIS = YES ]; then
		check_existing_post anl
	fi

	for fhr in ${RUN_HOURS[@]}; do
		check_existing_post $fhr
	done # for fhr in RUN_HOURS
fi

SLEEP_LOOP_MAX=$(( $SLEEP_TIME / $SLEEP_INT ))

if [ $DOANALYSIS = YES ]; then
	if [[ $SENDCOM == "YES" ]]; then
		mcfile=$COMOUT/$COMPONENT/misc/post/${RUNMEM}.t${cyc}z.master.control.anl
	else
		mcfile=$DATA/${RUNMEM}.t${cyc}z.master.control.anl
	fi

	if [[ ! -s $mcfile ]]; then
		############################################################
		# Post Analysis Files before starting the Forecast Post
		############################################################
		ic=1
		while [ $ic -le $SLEEP_LOOP_MAX ]; do
			if [ -f $COMIN/$COMPONENT/init/${RUNMEM}.t${cyc}z.sanl ]; then
				break
			else
				ic=$(( $ic + 1 ))
				sleep $SLEEP_INT
			fi # test -f $COMIN/$COMPONENT/init/${RUNMEM}.t${cyc}z.sanl
			###############################
			# If we reach this point assume
			# fcst job never reached restart 
			# period and error exit
			###############################
			if [ $ic -eq $SLEEP_LOOP_MAX ]; then
				echo <<- EOF
					FATAL ERROR in ${.sh.file}: Forecast missing for anl of $RUNMEM
					  File $mcfile still missing at $(date -u) after waiting ${SLEEP_TIME}s
				EOF
				export err=9
				err_chk || exit $err
			fi
		done  #while [ $ic -le $SLEEP_LOOP_MAX ]

		if [ -f $COMIN/$COMPONENT/init/${RUNMEM}.t${cyc}z.sanl ]; then
			# add new environmental variables for running new ncep post
			# Validation date

			export VDATE=${PDY}${cyc}

			# specify output file name from chgres which is input file name to nceppost
			# if model already runs gfs io, make sure GFSOUT is linked to the gfsio file
			# new imported variable for global_nceppost.sh

			export GFSOUT=${RUNMEM}.${cycle}.gfsioanl

			# specify smaller control file for GDAS because GDAS does not
			# produce flux file, the default will be /nwprod/parm/gfs_master_fhh.parm

			if [ $GRIBVERSION = grib1 ]; then
				export CTLFILE=$PARMgefs/gefs_master_f00.parm
			elif [ $GRIBVERSION = grib2 ]; then
				export PostFlatFile=${FLTFILEGFSANL:-$PARMPOST/postxconfig-NT-GEFS-ANL.txt}
			fi # [ $GRIBVERSION = grib1 ]

			if [[ -f sigfile.anl ]]; then rm -rf sigfile.anl; fi
			ln -s $COMIN/$COMPONENT/init/${RUNMEM}.t${cyc}z.sanl sigfile.anl

			####################################
			# Create Master Post File 
			####################################
			export SIGINP=sigfile.anl
			export NEMSINP=sigfile.anl
			export SFCINPUT=
			export FLXINP=
			export FLXIOUT=
			if [[ -f pgbfout ]]; then rm -rf pgbfout; fi
			if [[ -f pgbifout ]]; then rm -rf pgbifout; fi
			if [[ $SENDCOM == "YES" ]]; then
				if [ $GRIBVERSION = grib1 ]; then
					ln -s $COMOUT/$COMPONENT/master/$RUNMEM.$cycle.master.grbanl pgbfout
					ln -s $COMOUT/$COMPONENT/master/$RUNMEM.$cycle.master.grbianl pgbifout
				elif [ $GRIBVERSION = grib2 ]; then
					ln -s $COMOUT/$COMPONENT/master/$RUNMEM.$cycle.master.grb2anl pgbfout
					ln -s $COMOUT/$COMPONENT/master/$RUNMEM.$cycle.master.grb2ianl pgbifout
				fi # [ $GRIBVERSION = grib1 ]
			else
				if [ $GRIBVERSION = grib1 ]; then
					ln -s $DATA/$RUNMEM.$cycle.master.grbanl pgbfout
					ln -s $DATA/$RUNMEM.$cycle.master.grbianl pgbifout
				elif [ $GRIBVERSION = grib2 ]; then
					ln -s $DATA/$RUNMEM.$cycle.master.grb2anl pgbfout
					ln -s $DATA/$RUNMEM.$cycle.master.grb2ianl pgbifout
				fi # [ $GRIBVERSION = grib1 ]
			fi
			export PGBOUT=pgbfout
			export PGIOUT=pgbifout
			export IGEN=$IGEN_ANL

			# specify fhr even for analysis because postgp uses it
			export fhr=000

			# run nceppost
			export VERBOSE=YES
			
			$POSTGPSH
			export err=$?

			if (( err == 0 )); then
				echo "$POSTGPSH completed successfully for anl"
			else
				echo "FATAL ERROR in ${.sh.file}: $POSTGPSH failed for member $RUNMEM hour anl"
				err_chk || exit $err
			fi # (( err == 0 ))

			pgbfoutd=$(readlink -nf pgbfout)
			if [[ ! -s $pgbfoutd ]]; then
				echo "FATAL ERROR in ${.sh.file}: $pgbfoutd WAS NOT WRITTEN"
				err_chk || exit $err
			fi # [[ ! -s $pgbfoutd ]];

			pgbifoutd=$(readlink -nf pgbifout)
			if [[ ! -s $pgbifoutd ]]; then
				echo "FATAL ERROR in ${.sh.file}: $pgbifoutd WAS NOT WRITTEN"
				err_chk || exit $err
			fi # [[ ! -s $pgbifoutd ]]

			# 20100730 create a separate log file for the analysis for use by prdgen job
			if [ $SENDCOM = "YES" ]; then
				ls -al $PGBOUT $PGBIOUT
				echo "$PDY$cyc$fhr" > $mcfile
				ls -al  $mcfile
				cat  $mcfile
				echo "anl_done" >> $post_log
				echo
			fi # test $SENDCOM = "YES"
		fi # test -f $COMIN/$COMPONENT/init/${RUNMEM}.t${cyc}z.sanl
	fi # [[ ! -s $mcfile ]]
fi # [ $DOANALYSIS = YES ]

############################################################
# Loop Through the Post Forecast Files 
############################################################
for fhr in ${RUN_HOURS[@]}; do
	if [[ $SENDCOM == "YES" ]]; then
		mcfile=$COMOUT/$COMPONENT/misc/post/${RUNMEM}.t${cyc}z.master.control.f$fhr
	else
		mcfile=$DATA/${RUNMEM}.t${cyc}z.master.control.f$fhr
	fi

	if [[ ! -s $mcfile ]]; then
		###############################
		# Start Looping for the 
		# existence of the restart files
		###############################
		export pgm="postcheck"
		ic=1

		while [ $ic -le $SLEEP_LOOP_MAX ]; do
			if [ -f $restart_file${fhr}.nemsio ]; then
				break
			else
				ic=$(( $ic + 1 ))
				sleep $SLEEP_INT
			fi # test -f $restart_file$fhr
			###############################
			# If we reach this point assume
			# fcst job never reached restart 
			# period and error exit
			###############################
			if [ $ic -eq $SLEEP_LOOP_MAX ]; then
				echo <<- EOF
					FATAL ERROR in ${.sh.file}: Forecast missing for f${fhr} of $RUNMEM
					  File $mcfile still missing as of $(date -u) after waiting ${SLEEP_TIME}s
				EOF
				export err=9
				err_chk
			fi # [ $ic -eq $SLEEP_LOOP_MAX ]
		done # [ $ic -le $SLEEP_LOOP_MAX ]

		echo "Starting post for fhr=$fhr"

		###############################
		# link sigma and flux files
		###############################
		if [[ -f sigfile ]]; then rm -rf sigfile; fi
		if [[ -f flxfile ]]; then rm -rf flxfile; fi
		ln -s $COMIN/$COMPONENT/sfcsig/${RUNMEM}.t${cyc}z.atmf$fhr.nemsio sigfile.f$fhr
		ln -s $COMIN/$COMPONENT/sfcsig/${RUNMEM}.t${cyc}z.sfcf$fhr.nemsio flxfile.f$fhr

		###################################3
		# Create Master Post File
		###################################3
		# add new environmental variables for running new ncep post
		# Validation date

		export VDATE=$($NDATE +${fhr} ${PDY}${cyc})

		# specify output file name from chgres which is input file name to nceppost
		# if model already runs gfs io, make sure GFSOUT is linked to the gfsio file
		# new imported variable for global_nceppost.sh

		if [ $fhr -gt 0 ]; then
			export IGEN=$IGEN_FCST
		else
			export IGEN=$IGEN_ANL
		fi

		if [ $GRIBVERSION = grib1 ]; then
			export CTLFILE=$PARMgefs/gefs_master_fhh.parm
		else
			if [ $fhr -eq 0 ]; then
				export PostFlatFile=${FLTFILEGFSF00:-$PARMPOST/postxconfig-NT-GEFS-F00.txt}
			else
				if [ $fhr -le 96 ]; then
					export PostFlatFile=${FLTFILEGFS:-$PARMPOST/postxconfig-NT-GEFS.txt}
				else
					export PostFlatFile=${FLTFILEGFS1:-$PARMPOST/postxconfig-NT-GEFS.txt}
				fi
			fi # test $fhr -eq 0
		fi # [ $GRIBVERSION = grib1 ]

		export SIGINP=sigfile.f$fhr
		export NEMSINP=sigfile.f$fhr
		export SFCINPUT=sfcfile.f$fhr

		export FLXINP=flxfile.f$fhr
		export FLXIOUT=flxifile.f$fhr
		if [[ -f pgbfout ]]; then rm -rf pgbfout; fi
		if [[ -f pgbifout ]]; then rm -rf pgbifout; fi
		if [[ $SENDCOM == "YES" ]]; then
			ln -s $COMOUT/$COMPONENT/master/$RUNMEM.$cycle.master.grb2f$fhr pgbfout
			ln -s $COMOUT/$COMPONENT/master/$RUNMEM.$cycle.master.grb2if$fhr pgbifout
		else
			ln -s $DATA/$RUNMEM.$cycle.master.grb2f$fhr pgbfout
			ln -s $DATA/$RUNMEM.$cycle.master.grb2if$fhr pgbifout
		fi
		export PGBOUT=pgbfout
		export PGIOUT=pgbifout
		export FILTER=1

		# RLW 20100910 add cleanup to prevent problems with overparm
		echo
		for file in fort.11 fort.51 h5wav prmsl tfile; do
			# ls -al $file
			if [[ -L $file || -f $file ]]; then
				rm -f $file
				echo "$file removed from working directory"
			else
				echo "$file not removed from working directory"
			fi # [[ -L $file || -f $file ]]
		done # for file in fort.11 fort.51 h5wav prmsl tfile
		echo

		$POSTGPSH
		export err=$?

		if (( err == 0 )); then
			echo "$POSTGPSH completed successfully"
		else
			echo "FATAL ERROR in ${.sh.file}: $POSTGPSH failed for member $RUNMEM hour $fhr"
			export err
			err_chk || exit $err
		fi # (( err == 0 ))

		pgbfoutd=$(readlink -nf pgbfout)
		if [[ ! -s $pgbfoutd ]]; then
			msg="FATAL ERROR in ${.sh.file}: $pgbfoutd WAS NOT WRITTEN"
			export err=1
			err_chk || exit $err
		fi # [[ ! -s $pgbfoutd ]]
		pgbifoutd=$(readlink -nf pgbifout)
		if [[ ! -s $pgbifoutd ]]; then
			msg="FATAL ERROR in ${.sh.file}: $pgbifoutd WAS NOT WRITTEN"
			export err=1
			err_chk || exit $err
		fi # [[ ! -s $pgbifoutd ]]

		if [[ $SENDCOM == "YES" ]]; then
			echo
			ls -al $PGBOUT $PGIOUT
			echo "$PDY$cyc$fhr" > $mcfile
			ls -al $mcfile
			cat  $mcfile
			echo "f${fhr}_done" >> $post_log
			echo
		fi # [[ $SENDCOM == "YES" ]];
	fi # [[ ! -s $mcfile ]]
done # for fhr in ${RUN_HOURS[@]}

echo "$(date -u) end ${.sh.file}"

exit 0

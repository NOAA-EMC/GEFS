#!/bin/ksh

#####################################################################
# ----------------------------------------------------
# exgefs_prdgen.sh.sms
# based on exglobal_post.sh.sms and exglobal_post_pgrb2.sh.sms
# interpolate master post files (GRIB2) and convert to GRIB1
# Sep 07 - Wobus - reorganized script
# Aug 10 - Wobus - added test for control file from post
# Jun 11 - Wobus - moved master post files to /com
# Jun 14 - Hou   - adopted to grb2 version master files as input
# Feb 17 - Meng  - Unify version for creating all grids(1.0, 2.5
#                  and 0.5) files. 
# Sep 18 - Cui   - add option for 0.25d grids 
# ----------------------------------------------------
#####################################################################

echo "$(date -u) begin ${.sh.file}"
export PS4="${PS4}${1}: "

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

export VERBOSE=yes
sname=$(basename ${.sh.file})

export stream="${1}"
export jobdir="${2}"                   # ${DATA}/${stream}
export infile="${3}"                   # ${DATA}/${stream}/${stream}.in

. ${infile}

# Input file:
# jobgrid=                  # PRDGEN_GRID[$stream]
# grid_spec=                # PRDGEN_GRID_SPEC[$stream]
# hours=                    # PRDGEN_HOURS[$stream]
# submc=                    # PRDGEN_SUBMC[$stream]
# pgad=                     # PRDGEN_A_DIR[$stream]
# pgapre=                   # PRDGEN_A_PREFIX[$stream]
# parmlist_a00=             # PRDGEN_A_LIST_F00[$stream]
# parmlist_ahh=             # PRDGEN_A_LIST_FHH[$stream]
# pgbd=                     # PRDGEN_B_DIR[$stream]
# pgbpre=                   # PRDGEN_B_PREFIX[$stream]
# parmlist_b00=             # PRDGEN_B_LIST_F00[$stream]
# parmlist_bhh=             # PRDGEN_B_LIST_FHH[$stream]
# do_analysis=              # PRDGEN_DO_ANALYSIS[$stream]

cat <<-EOF
	Settings for $(basename ${.sh.file}) stream $stream:
	  RUNMEM: $RUNMEM
	  cyc: $cyc
	  DATA: $DATA
  
	  jobdir: $jobdir
	  jobgrid: $jobgrid
	  grid_spec: $grid_spec
	  hours: ($hours)
	  submc: $submc
	  pgad: $pgad
	  pgapre: $pgapre
	  parmlist_a00: $parmlist_a00
	  parmlist_ahh: $parmlist_ahh
	  pgbd: $pgbd
	  pgbpre: $pgbpre
	  parmlist_b00: $parmlist_b00
	  parmlist_bhh: $parmlist_bhh
	  do_analysis: $do_analysis

EOF

export MP_LABELIO=YES
export GRID=$jobgrid   # GRID is part of the DBN message

############################################################
# clean up missing markers from previous run
############################################################
if [[ $SENDCOM == "YES" ]]; then
	mkdir -m 775 -p $COMOUT/$COMPONENT/misc/$submc
	cd $COMOUT/$COMPONENT/misc/$submc

	rc=$?
	if (( rc == 0 )); then
		for file in $RUNMEM.*.missing; do
			if [[ -f $file ]]; then
				echo "Removing $COMOUT/$COMPONENT/misc/$submc/$file"
				rm -f $COMOUT/$COMPONENT/misc/$submc/$file
			fi
		done # for file in $RUNMEM.*.missing
	fi # (( rc == 0 ))
fi

cd $jobdir

SLEEP_LOOP_MAX=$(($SLEEP_TIME / $SLEEP_INT))

for hour in $hours; do
	if [[ $do_analysis = YES ]] && (( hour == 0 )); then
		export ffhr=anl
		export fhr=000

		if [[ $RUNMEM = "gegfs" ]]; then
			export mafile=$COMINgfs/gfs.$cycle.master.grb2anl
			export mifile=$COMINgfs/gfs.$cycle.master.grb2ianl
			export mcfile=""
			export makepgrb2b="no"
		else
			export mafile=$COMIN/$COMPONENT/master/$RUNMEM.$cycle.master.grb2anl
			export mifile=$COMIN/$COMPONENT/master/$RUNMEM.$cycle.master.grb2ianl
			export mcfile=$COMIN/$COMPONENT/misc/post/$RUNMEM.$cycle.master.control.anl
			if [[ -z "$pgbd" ]]; then
				export makepgrb2b="no"
			else
				export makepgrb2b="yes"
			fi
		fi # [[ $RUNMEM = "gegfs" ]]

		if [[ $SENDCOM == "YES" ]]; then
			export pcfile=$COMOUT/$COMPONENT/misc/$submc/${RUNMEM}.$cycle.prdgen.control.anl
			export fileaout=$COMOUT/$COMPONENT/$pgad/$RUNMEM.$cycle.${pgapre}anl
			export fileaouti=$COMOUT/$COMPONENT/$pgad/$RUNMEM.$cycle.${pgapre}anl.idx
			export filebout=$COMOUT/$COMPONENT/$pgbd/$RUNMEM.$cycle.${pgbpre}anl
			export filebouti=$COMOUT/$COMPONENT/$pgbd/$RUNMEM.$cycle.${pgbpre}anl.idx
		else
			export pcfile=$DATA/$submc/${RUNMEM}.$cycle.prdgen.control.anl
			export fileaout=$DATA/$RUNMEM.$cycle.${pgapre}anl
			export fileaouti=$DATA/$RUNMEM.$cycle.${pgapre}anl.idx
			export filebout=$DATA/$RUNMEM.$cycle.${pgbpre}anl
			export filebouti=$DATA/$RUNMEM.$cycle.${pgbpre}anl.idx
		fi

		ic=1
		while [ $ic -le $SLEEP_LOOP_MAX ]; do
			if [[ $RUNMEM = "gegfs" ]]; then
				# Assume GFS is complete
				if [[ -f $mifile ]]; then
					break
				fi
			else
				# Check if control file has been created, to make sure file is complete before using
				testfhr=-1
				if [[ -f $mcfile ]]; then
					teststring=$(cat $mcfile|head -1)
					if [[ $teststring != '' ]]; then
						if [[ -f $mifile ]]; then
							testfhr=$(echo $teststring | cut -c11-13)
						fi
					fi # [[ $teststring != '' ]]
				fi # [[ -f $mcfile ]]
				echo "testfhr=$testfhr fhr=$fhr"

				if (( testfhr >= fhr )); then
					break
				fi # (( testfhr >= fhr ))
			fi

			ic=$(expr $ic + 1)
			sleep $SLEEP_INT

			###############################
			# If we reach this point assume
			# fcst job never reached restart 
			# period and write file to
			# indicate missing data
			###############################
			if [ $ic -eq $SLEEP_LOOP_MAX ]; then
				if [[ $SENDCOM == "YES" ]]; then
					date >$COMOUT/$COMPONENT/misc/$submc/${RUNMEM}.t${cyc}z.anl.missing
				fi
				cat <<-EOF
					FATAL ERROR in ${.sh.file} ($stream): Post data still missing for analysis at $(date) after waiting ${SLEEP_TIME}s.
						Looked for the following files:
							$(set +x; if [[ $RUNMEM != "gegfs" ]]; then Control file: $mcfile $(if [[ -f $mcfile ]]; then echo "exists"; else; echo "doesn't exist"; fi); fi)
							Grib file:    $mafile $(set +x; if [[ -f $mafile ]]; then echo "exists"; else; echo "doesn't exist"; fi)
							Index file:   $mifile $(set +x; if [[ -f $mifile ]]; then echo "exists"; else; echo "doesn't exist"; fi)
					EOF
				export err=1;
				err_chk
				exit $err
			fi # [ $ic -eq $SLEEP_LOOP_MAX ]

		done # [ $ic -le $SLEEP_LOOP_MAX ]

		#
		# If control file already exists and skip if all output also present, otherwise
		#   delete control file to rerun. 
		#
		if [[ -s $pcfile ]]; then
			nmissing=0
			check_files="$fileaout $fileaouti"
			if [[ makepgrb2b == "yes" ]]; then
				check_files="$check_files $filebout $filebouti"
			fi
			for file in $check_files ; do
				if [[ ! -s $file ]]; then
					echo "file=$file IS MISSING"
					(( nmissing = nmissing + 1 ))
				fi
			done # for file in $fileaout $fileaouti $filebout $filebouti
			if (( nmissing > 0 )) || [[ $RERUN == "YES" ]]; then
				rm $pcfile
			fi
		else
			nmissing=1
		fi # [[ -s $pcfile ]]

		if [[ ! -s $pcfile ]]; then
			export parmlist_a=$parmlist_a00
			export parmlist_b=$parmlist_b00

			$USHgefs/gefs_prdgen.sh

			# Check for error
			export err=$?
			if [[ $err != 0 ]]; then
				echo "FATAL ERROR in ${.sh.file} ($stream): Creation of product failed for analysis!"
				err_chk
				exit $err
			fi

			####################################
			# send control files to misc
			####################################
			if [[ $SENDCOM == "YES" ]]; then
				echo "$PDY$cyc$fhr" > $pcfile
			fi # [[ $SENDCOM == "YES" ]]
		fi # [[ ! -s $pcfile ]]
	fi # [[ $do_analysis = YES ]] && (( hour == 0 ))

	export fhr=$(printf "%03.0f" $hour)        # Zero-pad to three places
	export ffhr="f${fhr}"
	# GFS output is only every 12 hours after 240, skip the unwanted hours
	if [[ $RUNMEM = "gegfs" ]] && (( fhr > gfsfhmaxh )) && (( fhr%12 == 6 )); then
		echo "fhr=$fhr not expected for GFS"
		continue
	fi

	###############################
	# Start Looping for the 
	# existence of the restart files
	###############################
	export pgm="postcheck"

	if [[ $RUNMEM = "gegfs" ]]; then
		export mafile=$COMINgfs/gfs.$cycle.master.grb2f$fhr
		export mifile=$COMINgfs/gfs.$cycle.master.grb2if$fhr
		export mcfile=""
		export makepgrb2b="no"
	else 
		export mafile=$COMIN/$COMPONENT/master/$RUNMEM.$cycle.master.grb2f$fhr
		export mifile=$COMIN/$COMPONENT/master/$RUNMEM.$cycle.master.grb2if$fhr
		export mcfile=$COMIN/$COMPONENT/misc/post/$RUNMEM.$cycle.master.control.f$fhr
		if [[ -z "$pgbd" ]]; then
			export makepgrb2b="no"
		else
			export makepgrb2b="yes"
		fi
	fi # [[ $RUNMEM = "gegfs" ]]

	if [[ $SENDCOM == "YES" ]]; then
		export pcfile=$COMOUT/$COMPONENT/misc/$submc/${RUNMEM}.t${cyc}z.prdgen.control.f$fhr
		export fileaout=$COMOUT/$COMPONENT/$pgad/$RUNMEM.$cycle.${pgapre}f${fhr}
		export fileaouti=$COMOUT/$COMPONENT/$pgad/$RUNMEM.$cycle.${pgapre}f${fhr}.idx
		if [[ $RUNMEM = "geaer" ]]; then
			export fileaout=$COMOUT/$COMPONENT/$pgad/${NET}.${COMPONENT}.$cycle.${pgapre}.f${fhr}.grib2
			export fileaouti=${fileaout}.idx
		fi
		export filebout=$COMOUT/$COMPONENT/$pgbd/$RUNMEM.$cycle.${pgbpre}f${fhr}
		export filebouti=$COMOUT/$COMPONENT/$pgbd/$RUNMEM.$cycle.${pgbpre}f${fhr}.idx
	else
		export pcfile=$DATA/$submc/${RUNMEM}.t${cyc}z.prdgen.control.f$fhr
		export fileaout=$DATA/$RUNMEM.$cycle.${pgapre}f${fhr}
		export fileaouti=$DATA/$RUNMEM.$cycle.${pgapre}f${fhr}.idx
		export filebout=$DATA/$RUNMEM.$cycle.${pgbpre}f${fhr}
		export filebouti=$DATA/$RUNMEM.$cycle.${pgbpre}f${fhr}.idx
	fi

	if [[ $SENDCOM == "YES" && $save_pgrb2_p5 = YES ]] && (( FHOUR > FHMAXHF )); then
		export mafile_p5=$COMOUT/$COMPONENT/pgrb2p5/$RUNMEM.$cycle.pgrb2.0p50.f${fhr}
		export mifile_p5=$COMOUT/$COMPONENT/pgrb2p5/$RUNMEM.$cycle.pgrb2.0p50.f${fhr}.idx
	fi
	if [[ $SENDCOM == "YES" && $save_pgrb2_p25 = YES ]] && (( FHOUR <= FHMAXHF )); then
		export mafile_p25=$COMOUT/$COMPONENT/pgrb2p25/$RUNMEM.$cycle.pgrb2.0p25.f${fhr}
		export mifile_p25=$COMOUT/$COMPONENT/pgrb2p25/$RUNMEM.$cycle.pgrb2.0p25.f${fhr}.idx
	fi

	ic=1
	found="no"
	while [ $ic -le $SLEEP_LOOP_MAX ]; do
		if [[ $RUNMEM = "gegfs" ]]; then
			# Check if index file has been created, to make sure file is complete before using
			if [[ -f $mifile ]]; then
				found="yes"
				break
			fi # [[ -f $mafile ]]
		else # [[ $RUNMEM = "gegfs" ]]
			# Check if control file has been created, to make sure file is complete before using
			testfhr=-1
			if [[ -f $mcfile ]]; then
				teststring=$(cat $mcfile|head -1)
				if [[ $teststring != '' ]]; then
					if [[ -f $mifile ]]; then
						testfhr=$(echo $teststring | cut -c11-13)
					fi
				fi # [[ $teststring != '' ]]
			fi # [[ -f $mcfile ]]
			echo "testfhr=$testfhr fhr=$fhr"

			if (( testfhr >= fhr )); then
				found="yes"
				break
			fi # (( testfhr >= fhr ))
		fi # [[ $RUNMEM = "gegfs" ]]

		ic=$((ic + 1))
		sleep $SLEEP_INT

		###############################
		# If we reach this point assume
		# fcst job never reached restart 
		# period and error exit
		###############################
		if [ $ic -eq $SLEEP_LOOP_MAX ]; then
			if [[ $SENDCOM == "YES" ]]; then
				date >$COMOUT/$COMPONENT/misc/$submc/${RUNMEM}.t${cyc}z.f$fhr.missing
			fi
			cat <<-EOF
				FATAL ERROR in ${.sh.file} ($stream): Post data still missing for f$fhr at $(date) after waiting ${SLEEP_TIME}s.
					Looked for the following files:
						Control file: $mcfile $(set -x; if [[ -f $mcfile ]]; then echo "exists"; else; echo "doesn't exist"; fi)
						Grib file:    $mafile $(set -x; if [[ -f $mafile ]]; then echo "exists"; else; echo "doesn't exist"; fi)
						Index file:   $mifile $(set -x; if [[ -f $mifile ]]; then echo "exists"; else; echo "doesn't exist"; fi)
			EOF
			export err=1;
			err_chk
			exit $err
		fi # [ $ic -eq $SLEEP_LOOP_MAX ]
	done # while [ $ic -le $SLEEP_LOOP_MAX ]

	if [[ $found = "yes" ]]; then
		echo "Starting post for fhr=$fhr"

		#################################### 
		# control the inclusion of perturbation identifiers
		# in the GRIB1 ensemble PDS header extension
		####################################

		if [[ -s $pcfile ]]; then
			nmissing=0
			check_files="$fileaout $fileaouti"
			if [[ makepgrb2b == "yes" ]]; then
				check_files="$check_files $filebout $filebouti"
			fi
			for file in $check_files ; do
				if [[ ! -s $file ]]; then
					echo "file=$file IS MISSING"
					(( nmissing = nmissing + 1 ))
				fi
			done # for file in $fileaout $fileaouti $filebout $filebouti
			if (( nmissing > 0 )) || [[ $RERUN == "YES" ]]; then
				rm $pcfile
			fi
		else
			nmissing=1
		fi # [[ -s $pcfile ]]

		if [[ ! -s $pcfile ]]; then
			if (( hour == 0 )); then
				parmlist_a=$parmlist_a00
				parmlist_b=$parmlist_b00
			else
				parmlist_a=$parmlist_ahh
				parmlist_b=$parmlist_bhh
			fi
			export parmlist_a
			export parmlist_b
			$USHgefs/gefs_prdgen.sh

			# Check for error
			export err=$?
			if [[ $err -ne 0 ]]; then
				echo "FATAL ERROR in ${.sh.file} ($stream): Creation of product failed at f${fhr}!"
				err_chk
				exit $err
			fi

			####################################
			# send control files to misc
			####################################
			if [[ $SENDCOM = "YES" ]]; then
				echo "$PDY$cyc$fhr" > $pcfile
			fi
		fi # [[ ! -s $pcfile ]]

	fi # [[ $found = "yes" ]]
done # for hour in $hours

echo "$(date -u) end ${.sh.file}"

exit 0

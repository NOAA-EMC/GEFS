#!/bin/ksh
########################### EXENSSTAT ################################
# ------------------------------------------------
# Ensemble Postprocessing
# create mean and spread members
# ------------------------------------------------
# History: MAY 1997 - First implementation of this new script.
# AUTHOR: Yuejian Zhu (wd20yz)
# Modified by: Mary Jacobs
# Modified by: Yuejian Zhu ---- October 1997
# Modified by: Maxine Brown for Yuejian Zhu ---- November 1997
# Modified by: David Michaud to port to IBM SP ---- September 1999
# Modified by: Larry Sager to add spaghetti ensembles -- Feb 2000
# Modified by: Yuejian Zhu to add high resolution archive---- June 2000
# Modified by: Richard Wobus to add ensstat statistics---- September 2001
# Modified by: Yuejian Zhu to add more vars, 6-hr interval fcst output,
#              apply to T00Z, T06Z, T12Z and T18Z cycles,               
#              no more ensppf production                                
#              move pqpf to ensstat process                ---- May 2003
# Modified by: Richard Wobus to add more variables ---- MARCH 2004
# Modified by: Richard Wobus to add 192hr zsfc  ---- August 2004
# Modified by: Richard Wobus reorganize by hour  ---- March 2006
# Modified by: Richard Wobus separate avg/spr from ensstat job  ---- June 2011
# Modified by: Bo Cui and Dingchen Hou change to grib2 operations  ---- Nov. 2014

### need pass the values of CYC, YMD, DATA, COMIN and COMOUT

echo "$(date -u) begin ${.sh.file}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

export subdata="${1}"                  # ${DATA}/${stream}
export stream="${2}"                   # PRDGEN_STREAMS
export jobgrid="${3}"                  # PRDGEN_GRID[$stream]
typeset -a hours="${4}"                # PRDGEN_HOURS[$stream]
export pgad="${5}"                     # PRDGEN_A_DIR[$stream]
export pgapre="${6}"                   # PRDGEN_A_PREFIX[$stream]

if [ "$jobgrid" = '2p5' ]; then
        SENDDBN=NO
fi

cat <<-EOF
	Settings for ensstat stream $stream:
		Subdata: $subdata
		Grid: $stream
		Grid Spec: $jobgrid
		Hours: ($grid_spec)
		A Dir: $pgad
		A Prefix: $pgbd
EOF

if [[ ! -d $DATA ]]; then
	mkdir -p $DATA
fi

set -x

cd $subdata

#####################################
# Define ensemble message switch
#   iens_msg=0 will add ensemble extension message
#   iens_msg=1 do not need to add ensemble extension message
#              only if evry members (include GFS) have
#              extension message
#####################################
export iens_msg=1

#####################################
# Define Script/Exec Variables
#####################################
export ENSPPF=$USHgefs/global_ensppf.sh
export ENSSTAT=$EXECgefs/gefs_ensstat
export ENSPQPF=$USHgefs/global_enspqpf.sh

echo settings in ${.sh.file} WGRIB2=$WGRIB2
parmlist=$PARMgefs/gefs_pgrb2a_fhh.parm

echo
echo "$(date)  ==== START OF HOURLY PROCESSING OF PGB FORECAST FILES ====="
echo

SLEEP_LOOP_MAX=$(($SLEEP_TIME / $SLEEP_INT))

############################################################
# Loop Through the Post Forecast Files
############################################################
(( nfilesprev = 9999 ))
foundgfs=no

for hour in $hours; do
	export fhr=$(printf "%02.0f" $hour)        # Zero-pad to two places
	export pfhr=$(printf "%03.0f" $hour)       # Zero-pad to three places
	export ffhr="f${pfhr}"

	if [[ -f $COMOUT/$COMPONENT/$pgad/geavg.${cycle}.$pgapre${ffhr}.idx ]]  && [[ -f $COMOUT/$COMPONENT/$pgad/gespr.${cycle}.$pgapre${ffhr}.idx ]]; then
		echo "Skip geavg.${cycle}.$pgapre${ffhr} & gespr.${cycle}.$pgapre${ffhr}"
		continue
	fi

	nenspost=0

	# set +x
	ic=1
	while [ $ic -le $SLEEP_LOOP_MAX ]; do
		nfiles=0
		nmem=0
		foundall=yes
		previncr=no
		for mem in $memberlist; do
			(( nmem = nmem + 1 ))
			testfile=$COMIN/$COMPONENT/$pgad/ge${mem}.${cycle}.$pgapre${ffhr}.idx

			if [[ -f $testfile ]]; then
				echo "testfile=$testfile found"
				(( nfiles = nfiles + 1 ))
				if [[ $mem = gfs ]]; then
					foundgfs=yes
				fi
				echo "mem=$mem nfiles=$nfiles foundgfs=$foundgfs foundall=$foundall previncr=$previncr ic=$ic fhr=$fhr found"
			else # [[ -f $testfile ]]
				echo "testfile=$testfile not found"
				if [[ $mem = gfs ]] && [[ $foundgfs = yes ]] && (( fhr > 180 )) && (( fhr % 12 > 0 )); then
					previncr=yes
				else
					foundall=no
				fi
				echo "mem=$mem nfiles=$nfiles foundgfs=$foundgfs foundall=$foundall previncr=$previncr ic=$ic fhr=$fhr not found"
			fi # [[ -f $testfile ]]
		done # for mem in $memberlist

		if [[ $foundall = yes ]]; then
			if [[ $previncr = yes ]]; then
				(( nfilesprev = nfiles + 1 ))
			else
				(( nfilesprev = nfiles ))
			fi
			echo "Search process ends nfilesprev=$nfilesprev foundgfs=$foundgfs foundall=$foundall previncr=$previncr ic=$ic fhr=$fhr"
			echo "Process all $nfiles members"
			break
		else # [[ $foundall = yes ]]
			if (( nfiles < nfilesprev )); then
				ic=$(($ic + 1))
				sleep $SLEEP_INT
			else
				if [[ $previncr = yes ]]; then
					(( nfilesprev = nfiles + 1 ))
				else
					(( nfilesprev = nfiles ))
				fi
				echo "Search process ends nfilesprev=$nfilesprev foundgfs=$foundgfs foundall=$foundall previncr=$previncr ic=$ic fhr=$fhr"
				echo "Continue processing with $nfiles members"
				break
			fi # (( nfiles < nfilesprev ))
		fi # [[ $foundall = yes ]]

		###############################
		# If we reach this point assume
		# fcst job never reached restart
		# period and error exit
		###############################
		echo "$nfiles out of $nmem members were found"
		if [ $ic -eq $SLEEP_LOOP_MAX ]; then

			###############################
			# MODIFY THIS STATEMENT TO
			# ALLOW A DIFFERENT NUMBER OF
			# MEMBERS TO BE MISSING
			#
			# CURRENTLY ALLOWS ONE MISSING
			###############################
			(( nfilesmin = nmem - 1 ))

			if (( nfiles < nfilesmin )); then
				echo <<- EOF
					FATAL ERROR in ${.sh.file} ($stream): Insufficient members found for f${fhr} to calculate stats at $(date) after ${SLEEP_TIME}s!
						Total members:         $nmem
						Min members for stats: $nfilesmin
						Members found:         $nfiles
					EOF
				export err=9
				err_chk
			else
				if (( nfiles < nmem )); then
					echo <<- EOF
						WARNING in ${.sh.file} ($stream): Some members still missing for f${fhr} at $(date) after ${SLEEP_TIME}s
							Will continue with $nfiles members, but products may be degraded.
						EOF
						msg="WARNING: ${job}, stream ${stream} did not find all ensemble member for f${fhr}! Will continue with fewer members, but products may be degraded."
					echo "$msg" | mail.py -c $MAIL_LIST
				fi
				(( nfilesprev = nfiles ))
				break
			fi # (( nfiles < nfilesmin ))
		fi # [ $ic -eq $SLEEP_LOOP_MAX ]
	done # [ $ic -le $SLEEP_LOOP_MAX ]
	# set -x

	echo "Starting ensstat generation for fhr=$fhr"

	#
	#  Make namelist file
	#
	cat <<- EOF >namin
		&namdim
			lfdim=${lfm:-''}
		/
		&namens
		EOF
	ifile=0
	for mem in $memberlist; do
		(( ifile = ifile + 1 ))
		iskip=0
		for nskip in $statskiplist; do
			if [[ $mem = $nskip ]]; then
				iskip=1
			fi
		done # for nskip in $statskiplist

		if [[ $iskip = 0 ]]; then
			if [[ -a cfipg$ifile.$jobgrid ]]; then rm cfipg$ifile.$jobgrid; fi
			ln -s $COMIN/$COMPONENT/$pgad/ge${mem}.${cycle}.$pgapre${ffhr} cfipg$ifile.$jobgrid
		fi # [[ $iskip = 0 ]]

		echo "	cfipg($ifile)"=\"cfipg$ifile.$jobgrid\", >>namin
		echo "	iskip($ifile)"=$iskip, >>namin

	done # for mem in $memberlist

	cat <<- EOF >>namin

			nfiles=$ifile
			nenspost=$nenspost

			cfopg1="geavg.${cycle}.$pgapre${ffhr}"
			cfopg2="gespr.${cycle}.$pgapre${ffhr}"

			navg_min=${navg_min}
		/
		EOF
	echo
	cat namin
	echo

	echo "####################### $(date) $fhr $jobgrid ensstat begin" >$pgmout.$jobgrid.$pfhr
	$ENSSTAT <namin >$pgmout.$jobgrid.$pfhr.temp
	export err=$?
	if [[ $err != 0 ]]; then
		echo <<- EOF
			FATAL ERROR in ${.sh.file} ($stream): $ENSSTAT returned a non-zero error code for f${fhr}!
				Namelist namin was used and had the following contents:
					$(cat namin)
			EOF
		err_chk
		exit $err
	fi
	cat $pgmout.$jobgrid.$pfhr.temp | fold -w 2000 >$pgmout.$jobgrid.$pfhr
	rm $pgmout.$jobgrid.$pfhr.temp
	echo "####################### $(date) $fhr $jobgrid ensstat end">>$pgmout.$jobgrid.$pfhr
	for fhout in $statoutfhlist; do
		if (( fhr == fhout )); then
			cat $pgmout.$jobgrid.$pfhr >> $pgmout
		else
			lines=$(cat $pgmout.$jobgrid.$pfhr| wc -l)
			lobeg=5
			loend=40
			echo lines=$lines lobeg=$lobeg loend=$loend
			(( lskip = lines - lobeg - loend ))
			if (( lskip > 100 )); then
				head -$lobeg $pgmout.$jobgrid.$pfhr >>$pgmout
				echo "####################### $lskip Lines Skipped">>$pgmout
				tail -$loend $pgmout.$jobgrid.$pfhr >>$pgmout
			else
				cat $$pgmout.$jobgrid.$pfhr >> $pgmout
			fi # (( lskip > 100 ))
		fi # (( fhr == fhout ))
	done # for fhout in $statoutfhlist
	echo "$(date) check for missing or zero-length output files"

	for run in geavg gespr; do
		if [[ -s ${run}.${cycle}.$pgapre${ffhr} ]]; then
			ls -al ${run}.${cycle}.$pgapre${ffhr}
		else
			echo output file ${run}.${cycle}.$pgapre${ffhr} IS MISSING
			export err=9
			err_chk
		fi # [[ -s ${run}.${cycle}.$pgapre${ffhr} ]]
	done # for run in geavg gespr

	echo "$(date) send $pgapre output begin"

	if [ $SENDCOM = "YES" ]; then
		MODCOM=$(echo ${NET}_${COMPONENT} | tr '[a-z]' '[A-Z]')
		GRID=$(echo ${jobgrid} | tr '[a-z]' '[A-Z]')
		for run in geavg gespr; do
			if [[ "$makegrb2i" = "yes" ]]; then
				$WGRIB2 -s ${run}.${cycle}.$pgapre${ffhr} >${run}.${cycle}.$pgapre${ffhr}.idx
			fi
			if [[ -s ${run}.${cycle}.$pgapre${ffhr} ]]; then
				mv ${run}.${cycle}.$pgapre${ffhr} $COMOUT/$COMPONENT/$pgad
				mv ${run}.${cycle}.$pgapre${ffhr}.idx $COMOUT/$COMPONENT/$pgad
			fi # [[ -s ${run}.${cycle}.$pgapre${ffhr} ]]
			if [[ "$SENDDBN" = 'YES' ]]; then
				$DBNROOT/bin/dbn_alert MODEL ${MODCOM}_PGB2A_${GRID} $job $COMOUT/$COMPONENT/$pgad/${run}.${cycle}.$pgapre${ffhr}
				$DBNROOT/bin/dbn_alert MODEL ${MODCOM}_PGB2A_${GRID}_IDX $job $COMOUT/$COMPONENT/$pgad/${run}.${cycle}.$pgapre${ffhr}.idx
			fi
		done

	fi
	echo "$(date) send pgrb2a output end"
	
done #hour in $hours

if [[ -s $pgmout ]]; then
	echo ###############################$(date) cat $pgmout begin
	cat $pgmout
	echo ###############################$(date) cat $pgmout end
fi
echo "$(date -u) end ${.sh.file}"

exit 0

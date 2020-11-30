#!/bin/ksh

########################### EXENSPOST ################################
# ------------------------------------------------
# Ensemble Postprocessing
# Create enspost ensstat ensppf files
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
# Modified by: Richard Wobus separate enspost from avg/spr ---- June 2011
# Modified by: Bo Cui and Dingchen Hou change to grib2 operations  ---- Nov. 2014

### need pass the values of CYC, YMD, DATA, COMIN and COMOUT

echo "$(date -u) begin ${.sh.file}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

cd $DATA

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

case $FORECAST_SEGMENT in
    hr)
		export ext_h=""
		export SHOUR=00
		export FHOUR=$fhmaxh
        ;;
    lr)
		export ext_h=ss
		(( SHOUR = fhmaxh + FHOUTLF ))
		export SHOUR=$SHOUR 
		export FHOUR=$fhmax
        ;;
    *)
		echo "FATAL ERROR in ${.sh.file}: Unknown FORECAST_SEGMENT $FORECAST_SEGMENT!"
		exit 100
        ;;
esac # $FORECAST_SEGMENT in

export FHINC=${FHOUTLF:-6}

#####################################
# START TO DUMP DATA FOR $cycle CYCLE
# START TO DUMP DATA FROM PGB FORECAST FILES
#####################################

varlout=" \
ensppf pqpf  pqsf  pqff  pqrf  pqif \
"

if [[ "$SENDCOM" == 'YES' ]]; then
	for file in enspost${ext_h} ensstat${ext_h}; do
		#   for FIELD in $varlboth $varlnostat $varlout
		for FIELD in $epnamhr $epnamlr $varlout; do
			if [ -s $COMOUT/$COMPONENT/ensstat/$file.${cycle}.${FIELD} ]; then
				rm $COMOUT/$COMPONENT/ensstat/$file.${cycle}.${FIELD}
				rm $COMOUT/$COMPONENT/ensstat/$file.${cycle}.${FIELD}i
			fi
			if [ -s $COMOUT/$COMPONENT/ensstat/$file.${cycle}.${FIELD}hr ]; then
				rm $COMOUT/$COMPONENT/ensstat/$file.${cycle}.${FIELD}hr
				rm $COMOUT/$COMPONENT/ensstat/$file.${cycle}.${FIELD}hri
			fi
		done # for FIELD in $epnamhr $epnamlr $varlout
	done # for file in enspost${ext_h} ensstat${ext_h}
fi # [[ "$SENDCOM" == 'YES' ]]

if [[ ${FORECAST_SEGMENT} == hr ]]; then
    memberlist="gfs $memberlist"
fi

echo
echo "$(date)  ==== START OF HOURLY PROCESSING OF PGB FORECAST FILES ====="
echo

SLEEP_LOOP_MAX=$((SLEEP_TIME / SLEEP_INT))

export fh=$SHOUR

############################################################
# Loop Through the Post Forecast Files
############################################################
(( nfilesprev = 9999 ))
foundgfs=no
while [[ $fh -le $FHOUR ]]; do
	#  the order here should correspond to the order in
	#  which the pgrb files are produced by the post
	# for res in lr hr p5

	export fh=$(printf %03i $fh)

	for res in lr; do
		if [[ $res = lr ]]; then
			EXT=
			FXT=.2p50.
			lr=2p5
			ext_h=$ext_h
			nenspost=$nenspostlr
			lfm=$lfmlr
			set -A enspostnam x $epnamlr
			set -A enspostvar x $epvarlr
			set -A enspostlvt x $eplvtlr
			set -A enspostlev x $eplevlr
		elif [[ $res = hr ]]; then
			EXT=
			FXT=
			lr=
			ext_h=1p
			nenspost=$nensposthr
			lfm=$lfmhr
			set -A enspostnam x $epnamhr
			set -A enspostvar x $epvarhr
			set -A enspostlvt x $eplvthr
			set -A enspostlev x $eplevhr
		elif [[ $res = p5 ]]; then  
			EXT=
			lr=p5
			FXT=p5
			ext_h=p5
			nenspost=$nenspostp5
			lfm=$lfmp5
			set -A enspostnam x $epnamp5
			set -A enspostvar x $epvarp5
			set -A enspostlvt x $eplvtp5
			set -A enspostlev x $eplevp5
		else
			echo "FATAL ERROR in ${.sh.file}: Unknown res $res!"
			exit 100
		fi # [[ $res = lr ]]

		###############################
		# Start Looping for the
		# existence of the pgrba files
		###############################
		set +x
		ic=1
		while [ $ic -le $SLEEP_LOOP_MAX ]; do
			nfiles=0
			nmem=0
			foundall=yes
			previncr=no
			for mem in $memberlist; do
				(( nmem = nmem + 1 ))
				testfile=$COMIN/$COMPONENT/pgrb2$lr/ge${mem}.${cycle}.pgrb2${FXT}f$fh$EXT.idx
				if [ -f $testfile ]; then
					echo testfile=$testfile found
					(( nfiles = nfiles + 1 ))
					if [[ $mem = gfs ]]; then
						foundgfs=yes
					fi
					echo "mem=$mem nfiles=$nfiles foundgfs=$foundgfs foundall=$foundall previncr=$previncr ic=$ic fh=$fh found"
				else # test -f $testfile
					echo testfile=$testfile not found
					if [[ $mem = gfs ]] && [[ $foundgfs = yes ]] && (( fh > gfsfhmaxh )) && (( fh % 12 > 0 )); then
						previncr=yes
					else
						foundall=no
					fi
					echo "mem=$mem nfiles=$nfiles foundgfs=$foundgfs foundall=$foundall previncr=$previncr ic=$ic fh=$fh not found"
				fi # test -f $testfile
			done # for mem in $memberlist

			if [[ $foundall = yes ]]; then
				if [[ $previncr = yes ]]; then
					(( nfilesprev = nfiles + 1 ))
				else
					(( nfilesprev = nfiles ))
				fi
				echo "Search process ends nfilesprev=$nfilesprev foundgfs=$foundgfs foundall=$foundall previncr=$previncr ic=$ic fh=$fh"
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
					echo "Search process ends nfilesprev=$nfilesprev foundgfs=$foundgfs foundall=$foundall previncr=$previncr ic=$ic fh=$fh"
					echo "Continue processing with $nfiles members"
					break
				fi # (( nfiles < nfilesprev ))
			fi # [[ $foundall = yes ]]
			###############################
			# If we reach this point assume
			# fcst job never reached restart
			# period and error exit
			###############################
			echo $nfiles out of $nmem members were found
			if [ $ic -eq $SLEEP_LOOP_MAX ]; then

				###############################
				# MODIFY THIS STATEMENT TO
				# ALLOW A DIFFERENT NUMBER OF
				# MEMBERS TO BE MISSING
				#
				# CURRENTLY ALLOWS ONE MISSING,`
				# but TWO at 246, 258, ..378h
				###############################
				(( nfilesmin = nmem - 1 ))
				if (( fh > gfsfhmaxh )) && (( fh % 12 > 0 )); then
					(( nfilesmin = nfilesmin - 1 ))
				fi

				if (( nfiles < nfilesmin )); then
					echo <<- EOF
						FATAL ERROR in ${.sh.file}: Insufficient members found for f${fh} to calculate stats at $(date) after ${SLEEP_TIME}s!
							Total members:         $nmem
							Min members for stats: $nfilesmin
							Members found:         $nfiles
						EOF
					export err=9
					err_chk
				else
					if (( nfiles < nmem )); then
						echo <<- EOF
							WARNING in ${.sh.file}: Some members still missing for f${fh} at $(date) after ${SLEEP_TIME}s
								Will continue with $nfiles members, but products may be degraded.
							EOF
						msg="WARNING: ${job} did not find all ensemble member for f${fh}! Will continue with fewer members, but products may be degraded."
						echo "$msg" | mail.py -c $MAIL_LIST
					fi
					(( nfilesprev = nfiles ))
					break
				fi # (( nfiles < nfilesmin ))
			fi # [ $ic -eq $SLEEP_LOOP_MAX ]
		done # while [ $ic -le $SLEEP_LOOP_MAX ]
		set -x

		echo "Starting ensstat generation for fhr=$fh"

		#
		#  Make namelist file
		#
		cat <<- EOF >namin
			&namdim
				lfdim=${lfm:-''}
			/
			&namens
			EOF

		parmlist=$PARMgefs/gefs_enspost_grb2.parm
		ifile=0
		for mem in $memberlist; do
			#DHOU, 20141028, select required variables from pgrb2a files
			if [[ "$mem" != "gfs" || $fh -le $gfsfhmaxh || $(($fh % 12)) -eq 0 ]]; then
				pgtem=$COMIN/$COMPONENT/pgrb2$lr/ge${mem}.${cycle}.pgrb2${FXT}f$fh$EXT
				if [[ -s $pgtem ]]; then
					$WGRIB2 -s $pgtem | grep -F -f $parmlist | $WGRIB2 $pgtem -s -i -grib pgrb2a_$mem 
					#DHOU 20141028
					(( ifile = ifile + 1 ))
					iskip=0
					echo " cfipg($ifile)"="pgrb2a_$mem", >>namin
					echo " iskip($ifile)"=$iskip, >>namin
				fi
			fi # [[ "$mem" != "gfs" || $fh -le $gfsfhmaxh || $(($fh % 12)) -eq 0 ]]
		done # for mem in $memberlist

		echo " nfiles=$ifile", >>namin
		echo " nenspost=${nenspost}", >>namin

		(( ienspost = 0 ))
		(( iunitouts = 52 ))
		#   while (( ienspost < nenspost ))
		while (( ienspost < 0 )); do
			(( ienspost = ienspost + 1 ))
			(( iunitouta = iunitouts + 1 ))
			(( iunitouts = iunitouts + 2 ))
			cat <<- EOF >>namin

					ivar($ienspost)=${enspostvar[$ienspost]},
					ilev($ienspost)=${enspostlev[$ienspost]},
					ilvt($ienspost)=${enspostlvt[$ienspost]},
					cfopg($ienspost)="enspost${ext_h}.$cycle.${enspostnam[$ienspost]}",
					cfotg($ienspost)="ensstat${ext_h}.$cycle.${enspostnam[$ienspost]}",
				EOF
		done # while (( ienspost < 0 ))

		cat <<- EOF >>namin

				cfopg1="ensstat"
				cfopg2="ensstat"

				navg_min=${navg_min}
			/
			EOF

		echo
		cat namin
		echo

		echo "####################### $(date) $fh $res ensstat begin" >$pgmout.$res$fh
		$ENSSTAT <namin >$pgmout.$res$fh.temp
		export err=$?
		if [[ $err != 0 ]]; then
			echo <<- EOF
				FATAL ERROR in ${.sh.file}: $ENSSTAT returned a non-zero error code for f${fh}!
					Namelist namin was used and had the following contents:
						$(cat namin)
				EOF
			err_chk
			exit $err
		fi
		cat $pgmout.$res$fh.temp | fold -w 2000 >$pgmout.$res$fh
		rm $pgmout.$res$fh.temp
		echo "####################### $(date) $fh $res ensstat end">>$pgmout.$res$fh
		#DHOU 20141028 Separate the enspost variables in to individual files
		for var in $postvarlist; do
			case $var in
				z500) TEXT='HGT:500 mb';;
				z1000) TEXT='HGT:1000 mb';;
				t850) TEXT='TMP:850 mb';;
				u250) TEXT='UGRD:250 mb';;
				v250) TEXT='VGRD:250 mb';;
				u850) TEXT='UGRD:850 mb';;
				v850) TEXT='VGRD:850 mb';;
				t2m) TEXT='TMP:2 m a';;
				u10m) TEXT='UGRD:10 m a';;
				v10m) TEXT='VGRD:10 m a';;
				rh700) TEXT='RH:700 mb' ;;
				prmsl) TEXT='PRMSL:mean';;
				prcp) TEXT='APCP:surface';;
				snow) TEXT='CSNOW:surface';;
				rain) TEXT='CRAIN:surface';;
				icep) TEXT='CICEP:surface';;
				frzr) TEXT='CFRZR:surface';;
				u200) TEXT='UGRD:200 mb';;
				v200) TEXT='VGRD:200 mb';;
				olr) TEXT='ULWRF:top of atmosphere';;
			esac # $var
			echo "$var  $TEXT"
			for mem in $memberlist; do
				if [ -s pgrb2a_$mem ]; then
					$WGRIB2 -s pgrb2a_$mem | grep "$TEXT" | $WGRIB2 pgrb2a_$mem -i -append -grib enspost_grb2${ext_h}.$cycle.$var 
				fi
			done # for mem in $memberlist
			$WGRIB2 -s ensstat | grep "$TEXT" | $WGRIB2 ensstat -i -append -grib ensstat_grb2${ext_h}.$cycle.$var 
		done # for var in $postvarlist 
		rm pgrb2a* ensstat
		#DHOU 20141028

		for fhout in $statoutfhlist; do
			if (( fh == fhout )); then
				cat $pgmout.$res$fh >> $pgmout
			else
				lines=$(cat $pgmout.$res$fh| wc -l)
				lobeg=5
				loend=40
				echo "lines=$lines lobeg=$lobeg loend=$loend"
				(( lskip = lines - lobeg - loend ))
				if (( lskip > 100 )); then
					head -$lobeg $pgmout.$res$fh >>$pgmout
					echo "####################### $lskip Lines Skipped">>$pgmout
					tail -$loend $pgmout.$res$fh >>$pgmout
				else
					cat $pgmout$res.$fh >> $pgmout
				fi
			fi # (( fh == fhout ))
		done # for fhout in $statoutfhlist
	done # for res in lr

	export fh=$(( fh + FHINC ))

done # while test $fh -le $FHOUR

if [[ -s $pgmout ]]; then
	echo ###############################$(date) cat $pgmout begin
	cat $pgmout
	echo ###############################$(date) cat $pgmout end
fi

echo
echo "$(date)  ==== END  OF  HOURLY PROCESSING OF PGB FORECAST FILES ====="
echo

# check for missing or zero-length output files
for file in $postvarlist; do
	if [[ -s enspost_grb2${ext_h}.$cycle.${file} ]]; then
		ls -al enspost_grb2${ext_h}.$cycle.${file}
	else
		echo "FATAL ERROR in ${.sh.file}: Output file enspost_grb2${ext_h}.$cycle.${file} is missing!"
		export err=9
		err_chk
		exit $err
	fi
done # for file in $postvarlist

# Moving  output files to /com directory
if [ "$SENDCOM" = "YES" ]; then
	for file in $postvarlist; do
		mv enspost_grb2${ext_h}.$cycle.${file} $COMOUT/$COMPONENT/ensstat/enspost_grb2${ext_h}.${cycle}.${file}
		mv ensstat_grb2${ext_h}.$cycle.${file} $COMOUT/$COMPONENT/ensstat/ensstat_grb2${ext_h}.${cycle}.${file}
	done # for file in $postvarlist
fi # [ "$SENDCOM" = "YES" ]

#
# create pqpf 24h and probabilistic precip forecast files
## write out with ensemble extended messages
#

## part (1a): calculate PQPF (named ensppf ) for each 24 hours period,
#

if [[ "$cycle" == "t00z" ]] && [[ -z $ext_h ]]; then
	$ENSPPF $COMOUT/$COMPONENT/ensstat/enspost_grb2${ext_h}.$cycle.prcp ensppf${ext_h}.$PDY$cyc.grib2 $npert
	$WGRIB2 ensppf${ext_h}.$PDY$cyc.grib2 -s >ensppf${ext_h}.$PDY$cyc.grib2.idx 

	###########################
	# Convert to grib1 format:
	###########################
	$CNVGRIB -g21 ensppf${ext_h}.$PDY$cyc.grib2 ensppf${ext_h}.$PDY$cyc
	ls -lt ensppf${ext_h}.$PDY$cyc.grib2 ensppf${ext_h}.$PDY$cyc
	$WGRIB2 -s ensppf${ext_h}.$PDY$cyc.grib2
	$GRBINDEX ensppf${ext_h}.$PDY$cyc ensppf${ext_h}i.$PDY$cyc

	# check for missing or zero-length output files

	if [[ -s ensppf${ext_h}.$PDY$cyc.grib2 ]]; then
		ls -al ensppf${ext_h}.$PDY$cyc.grib2
	else
		echo output file ensppf${ext_h}.$PDY$cyc.grb2 IS MISSING
		export err=9
		err_chk
	fi # [[ -s ensppf${ext_h}.$PDY$cyc.grib2 ]]

	if [ $SENDCOM = "YES" ]; then
		cp ensppf${ext_h}.$PDY$cyc $COMOUT/$COMPONENT/ensstat/ensstat${ext_h}.$cycle.pqpf_24h
		cp ensppf${ext_h}i.$PDY$cyc $COMOUT/$COMPONENT/ensstat/ensstat${ext_h}.$cycle.pqpfi_24h

		cp ensppf${ext_h}.$PDY$cyc.grib2 $COMOUT/$COMPONENT/ensstat/ensstat${ext_h}.$cycle.pqpf_24h.grib2
		cp ensppf${ext_h}.$PDY$cyc.grib2.idx $COMOUT/$COMPONENT/ensstat/ensstat${ext_h}.$cycle.pqpf_24h.grib2.idx
	fi  #[ $SENDCOM = "YES" ]
fi # test "$cycle" = "t00z"

# part (1b): probabilistic forecasts ( PQPF, PQRF, PQFF, PQSF and PQIF )
if [[ -z $ext_h ]]; then

	export CDATE=$PDY$cyc; 
	$ENSPQPF

	for file in pqpf pqrf pqff pqsf pqif; do
		$CNVGRIB -g21 $DATA/$file $DATA/${file}_grb1
		$WGRIB2 $DATA/$file -s >$DATA/${file}.idx
		$GRBINDEX $DATA/${file}_grb1 $DATA/${file}i_grb1
		if [ $SENDCOM = "YES" ];        then
			mv $DATA/${file} $COMOUT/$COMPONENT/ensstat/ensstat_grb2${ext_h}.$cycle.$file
			mv $DATA/${file}.idx $COMOUT/$COMPONENT/ensstat/ensstat_grb2${ext_h}.$cycle.${file}.idx
			mv $DATA/${file}_grb1 $COMOUT/$COMPONENT/ensstat/ensstat${ext_h}.$cycle.$file
			mv $DATA/${file}i_grb1 $COMOUT/$COMPONENT/ensstat/ensstat${ext_h}.$cycle.${file}i
		fi # [ $SENDCOM = "YES" ]
	done # for file in pqpf pqrf pqff pqsf pqif

fi

############################################################################
###########  ADD DBN ALERTS FOR PPF AND PQPF FILES IF NEEDED  ##############
############################################################################

############################################################################
#   The following block is added in 2015 upgrade (when grib2 version is adopted) 
#   Generate (extra) grib1 version of output files and move them to /com 
############################################################################

for file in $postvarlist; do
	ln -s $COMOUT/$COMPONENT/ensstat/enspost_grb2${ext_h}.${cycle}.${file} enspost_grb2${ext_h}.$cycle.${file}
	ln -s $COMOUT/$COMPONENT/ensstat/ensstat_grb2${ext_h}.${cycle}.${file} ensstat_grb2${ext_h}.$cycle.${file}

	$WGRIB2 enspost_grb2${ext_h}.$cycle.${file} -s > enspost_grb2${ext_h}.$cycle.${file}.idx
	$WGRIB2 ensstat_grb2${ext_h}.$cycle.${file} -s > ensstat_grb2${ext_h}.$cycle.${file}.idx
	$CNVGRIB -g21 enspost_grb2${ext_h}.$cycle.${file} enspost${ext_h}.$cycle.${file}
	$CNVGRIB -g21 ensstat_grb2${ext_h}.$cycle.${file} ensstat${ext_h}.$cycle.${file}
	$GRBINDEX enspost${ext_h}.$cycle.${file}   enspost${ext_h}.$cycle.${file}i
	$GRBINDEX ensstat${ext_h}.$cycle.${file}   ensstat${ext_h}.$cycle.${file}i

	if [ "$SENDCOM" = "YES" ]; then
		mv enspost_grb2${ext_h}.$cycle.${file}.idx $COMOUT/$COMPONENT/ensstat/enspost_grb2${ext_h}.${cycle}.${file}.idx
		mv ensstat_grb2${ext_h}.$cycle.${file}.idx $COMOUT/$COMPONENT/ensstat/ensstat_grb2${ext_h}.${cycle}.${file}.idx
		mv enspost${ext_h}.$cycle.${file}  $COMOUT/$COMPONENT/ensstat/enspost${ext_h}.${cycle}.${file}
		mv enspost${ext_h}.$cycle.${file}i $COMOUT/$COMPONENT/ensstat/enspost${ext_h}.${cycle}.${file}i
		mv ensstat${ext_h}.$cycle.${file}  $COMOUT/$COMPONENT/ensstat/ensstat${ext_h}.${cycle}.${file}
		mv ensstat${ext_h}.$cycle.${file}i $COMOUT/$COMPONENT/ensstat/ensstat${ext_h}.${cycle}.${file}i
	fi # [ "$SENDCOM" = "YES" ]
done # for file in $postvarlist

echo "$(date -u) end ${.sh.file}"

exit 0


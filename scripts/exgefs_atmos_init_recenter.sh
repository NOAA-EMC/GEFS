#!/bin/ksh

echo "$(date -u) begin ${.sh.file}"

if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

################################################################################
#   Script:	exgefs_init_recenter.sh.sms
#
#   Author:	Xiaqiong Zhou
#   Date:	2018 March 08
#        April 2018, Dingchen Hou: Slight modification for consistency of the cold start part
#        May 8 2018, Dingchen Hou: Added function for enkf membership shifting with the 4 cucles
#        May 8 2018, Dingchen Hou: Added function to allow fleximble ensemble size
#        April 2018, Bing Fu: Added the warm start part
#        May 11 2018: Dingchen Hou: Added enkf membership shifting and flexible gefs membership 
#        April, 2019: Xiaqiong Zhou: Added new recenter method with NETCDF files
#        May 2, 2019: Dingchen Hou: Dropped off the old recenter methods with NEMSIO files
#        May 3, 2019: Dingchen Hou: Cleaning the script by remove commented lines
#
#    Abstract:	Creates initial conditions for the global ensemble (GEFS) from FV3 based enkf
#		6-h forecast nemsio files 
#		Cold start: Recenter the 6 hour forecast to gfs analysis (nemsio) file
#			The resulted nemsio files need to changed to netCDF in init_fv3chgres jobs
#		Warm start: Generate  increment file (netCDF format) for the ensemble members 
#			from the ensemble mean forecast and gfs analysis, for the encemble contro;
#                        from enkf member npert+1 forecast and gfs analysis
#                	The netCDF format increment files will be used in the forecast job, together
#			with RESTAT files  
#
################################################################################

echo " ------------------------------------------------------------"
echo "  "
echo "            GLOBAL ENSEMBLE INITIALIZATION "
echo "  "
echo "                $(date)     "
echo "  "
echo "                   JOB  $job  "
echo "  "
echo "  "
echo "               FORECAST cycle TIME is $cycle"
echo "  "
echo " ------------------------------------------------------------"
echo "          processing info for this execution"
echo " Home directory is ............................ $HOMEgefs"
echo " Processing directory for files.. ............. $DATA"
echo "  "
echo " Network id is ................................ $NET"
echo " Run id for com processing is ................. $RUN"
echo "  "
echo " standard output in file ...................... $pgmout"
echo " YES SENDCOM means save com files ............. $SENDCOM"
echo " ------------------------------------------------------------"

export warm_start=${warm_start:-"false"}

echo "DATA=$DATA"

# Set environment.
VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = "YES" ]; then
   echo "$(date) EXECUTING ${.sh.file} $*" >&2
   set -x
fi

export CASE=${CASE:-384}
ntiles=${ntiles:-6}

# Utilities
ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
NCP=${NCP:-"/bin/cp -p"}
NLN=${NLN:-"/bin/ln -sf"}
NMV=${NMV:-"/bin/mv -uv"}

# Scripts
RECENATMPY_PREP=${RECENATMPY:-$HOMEgefs/util/ush/recentensemble_prep.py}
RECENATMPY_POST=${RECENATMPY:-$HOMEgefs/util/ush/recentensemble_post.py}

err=0
SLEEP_LOOP_MAX=$((SLEEP_TIME / SLEEP_INT))
if [ $warm_start = ".false." ]; then
	export FILENAME='gfs_data.tile'
	export FILEINPATH=$GESIN/enkf
	export FILEOUTPATH=$GESOUT/init

	if [ $npert -gt 0 ]; then
		# To run recenter-prep
		imem=1
		while [[ imem -le $npert ]]; do
			sMem=p$(printf %02i $imem)

			ic=1
			while [ $ic -le $SLEEP_LOOP_MAX ]; do
				sInputFile=$FILEINPATH/${sMem}/chgres_atm.log
				echo $sInputFile
				if [ -f ${sInputFile} ]; then
					break
				else
					ic=$(( $ic + 1 ))
					echo "---" $ic $sMem
					sleep $SLEEP_INT
				fi # test -f $sInputFile
				###############################
				# If we reach this point assume
				# atmos_prep job for pxx is working
				###############################
				if [ $ic -eq $SLEEP_LOOP_MAX ]; then
					echo <<- EOF
							FATAL ERROR in ${.sh.file}: Forecast missing for one tile of ${sMem}
							File $sInputFile still missing at $(date -u) after waiting ${SLEEP_TIME}s
						EOF
					export err=9
					err_chk || exit $err
				fi
			done  # while [ $ic -le $SLEEP_LOOP_MAX ]
            (( imem++ ))
		done # while [[ imem -le $npert ]]; do

		# To copy p01 data to init/
		mkdir -p $FILEOUTPATH
		$NCP $FILEINPATH/p01/${FILENAME}*  $FILEOUTPATH/.

		rm -rf poescript*

		(( itile = 1 ))
		while (( itile <= ntiles  )); do
			echo "$RECENATMPY_PREP $npert $ntiles $FILENAME $FILEINPATH $FILEOUTPATH $itile" >>poescript
			(( itile = itile + 1 ))
		done # while (( itask < npert ))

		chmod 755 poescript
		ls -al poescript
		cat poescript
		export MP_HOLDTIME=1000

		export MP_CMDFILE=poescript
		export SCR_CMDFILE=$MP_CMDFILE  # Used by mpiserial on Theia
		export MP_LABELIO=yes
		export MP_INFOLEVEL=3
		export MP_STDOUTMODE=unordered
		export MP_PGMMODEL=mpmd

		if [ -f mpmd_cmdfile ]; then
			rm mpmd_cmdfile
		fi
		ln -s $MP_CMDFILE mpmd_cmdfile
		$APRUN_MPMD

		export err=$?
		if [[ $err != 0 ]]; then
			echo "FATAL ERROR in ${.sh.file}: One or more recenter jobs in $MP_CMDFILE failed!"
			exit $err
		fi
	
		# Ro run recenter-post
		ic=1
		while [ $ic -le $SLEEP_LOOP_MAX ]; do
			sInputFile=$FILEINPATH/c00/chgres_atm.log
			echo $sInputFile
			if [ -f ${sInputFile} ]; then
				break
			else
				ic=$(( $ic + 1 ))
				echo "---" $ic
				sleep $SLEEP_INT
			fi # test -f $sInputFile
			###############################
			# If we reach this point assume
			# atmos_prep job for c00 is working
			###############################
			if [ $ic -eq $SLEEP_LOOP_MAX ]; then
			echo <<- EOF
				FATAL ERROR in ${.sh.file}: Forecast missing for one tile of c00
				File $sInputFile still missing at $(date -u) after waiting ${SLEEP_TIME}s
			EOF
				export err=9
				err_chk || exit $err
			fi
		done  # while [ $ic -le $SLEEP_LOOP_MAX ]

		mkdir -p $FILEOUTPATH/c00
		$NCP $FILEINPATH/c00/${FILENAME}*  $FILEOUTPATH/c00/.

		rm -rf poescript*

		(( itile = 1 ))
		while (( itile <= ntiles  )); do
			echo "$RECENATMPY_POST $npert $ntiles $FILENAME $FILEINPATH $FILEOUTPATH $itile $pert_scaling" >>poescript
			(( itile = itile + 1 ))
		done # while (( itask < npert ))

		chmod 755 poescript
		ls -al poescript
		cat poescript
		export MP_HOLDTIME=1000

		export MP_CMDFILE=poescript
		export SCR_CMDFILE=$MP_CMDFILE  # Used by mpiserial on Theia
		export MP_LABELIO=yes
		export MP_INFOLEVEL=3
		export MP_STDOUTMODE=unordered
		export MP_PGMMODEL=mpmd

		if [ -f mpmd_cmdfile ]; then
			rm mpmd_cmdfile
		fi
		ln -s $MP_CMDFILE mpmd_cmdfile
		$APRUN_MPMD

		export err=$?
		if [[ $err != 0 ]]; then
			echo "FATAL ERROR in ${.sh.file}: One or more recenter jobs in $MP_CMDFILE failed!"
			exit $err
		fi
	else # npert=0
		mkdir -p $FILEOUTPATH/c00
		$NCP $FILEINPATH/c00/${FILENAME}*  $FILEOUTPATH/c00/.
	fi
else
	echo "FATAL ERROR in ${.sh.file}: init_recenter only works for cold start"
	exit 1
fi # $warm_start = ".false."

if [[ $SENDCOM == YES ]]; then
    MODCOM=$(echo ${NET}_${COMPONENT} | tr '[a-z]' '[A-Z]')
    DBNTYP=${MODCOM}_INIT
    mem=01
    while [ $mem -le $npert ]; do
        smem=p$(printf %02i $mem)
        mkdir -p $COMOUT/init/$smem
        $NCP $GESOUT/init/$smem/gfs* $COMOUT/init/$smem
        export err=$?
        if [[ $err != 0 ]]; then
            echo "FATAL ERROR in ${.sh.file}: failed to copy data from GESOUT to COMOUT"
            err_chk || exit $err
        fi
	    if [[ $SENDDBN = YES ]];then
            for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
                $DBNROOT/bin/dbn_alert MODEL $DBNTYP $job $COMOUT/init/$smem/gfs_data.${tile}.nc
            done
	    fi		
        (( mem = mem +1 ))
    done
fi

rm -rf $GESOUT/enkf
echo "$(date -u) end ${.sh.file}"

exit $err



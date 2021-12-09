#! /bin/ksh

echo "$(date -u) begin ${.sh.file}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

export RERUN=${RERUN:-RESTART}

export HOMEgfs=${HOMEgfs:-$HOMEgefs}
export FIXgfs=${FIXgfs:-$HOMEgfs/fix/fix_am}
export PARMgfs=${PARMgfs:-$HOMEgfs/parm}

echo "PRDGEN_STREAMS = $PRDGEN_STREAMS"

# 20150622 RLW change to "yes" to remake prdgen when post is remade
export overwrite=yes

if [[ $SENDCOM == "YES" && $save_pgrb2_p5 == YES ]]; then
	mkdir -m 775 -p $COMOUT/$COMPONENT/pgrb2p5
fi
if [[ $SENDCOM == "YES" && $save_pgrb2_p25 == YES ]]; then
	mkdir -m 775 -p $COMOUT/$COMPONENT/pgrb2p25
fi

case ${FORECAST_SEGMENT:-none} in
	hr)
		start_hour=0
		end_hour=${fhmaxh}
		;;
	lr)
		start_hour=$((${fhmaxh}+1))
		end_hour=${fhmax}
		PRDGEN_STREAMS=$PRDGEN_STREAMS_LR
		;;
	*)
		start_hour=0
		end_hour=${fhmaxh}
		;;
esac # $FORECAST_SEGMENT in

for stream in ${PRDGEN_STREAMS[@]}; do
	# Filter out hours based on forecast segment
	typeset -a hours=($(echo ${PRDGEN_HOURS[$stream]}))
	echo "hours = $hours"
	for i in "${!hours[@]}"; do
		hour=${hours[i]}
		echo "i = $i  hour = $hour"
		if [[ $hour -lt $start_hour || $hour -gt $end_hour ]]; then
			unset 'hours[i]'
		fi
	done
	PRDGEN_HOURS[$stream]="${hours[@]}"
	unset hours

	# Ensure required variables are defined
	for var in PRDGEN_GRID PRDGEN_GRID_SPEC PRDGEN_HOURS PRDGEN_SUBMC PRDGEN_A_DIR PRDGEN_A_PREFIX PRDGEN_A_LIST_F00 PRDGEN_A_LIST_FHH; do
		pointer="$var[$stream]"
		if [[ -z ${!pointer} ]]; then
			echo "FATAL ERROR in ${.sh.file}: $var not defined for $stream"
			exit -1
		fi
	done

	# Print out settings for this stream
	cat <<-EOF
		Settings for prgden stream $stream:
			Grid: ${PRDGEN_GRID[$stream]}
			Grid Spec: ${PRDGEN_GRID_SPEC[$stream]}
			Hours: (${PRDGEN_HOURS[$stream]})
			submc: ${PRDGEN_SUBMC[$stream]}
			A Dir: ${PRDGEN_A_DIR[$stream]}
			A Prefix: ${PRDGEN_A_PREFIX[$stream]}
			A Parmlist f00: ${PRDGEN_A_LIST_F00[$stream]}
			A Parmlist fhh: ${PRDGEN_A_LIST_FHH[$stream]}
			B Dir: ${PRDGEN_B_DIR[$stream]:-""}
			B Prefix: ${PRDGEN_B_PREFIX[$stream]:-""}			
			B Parmlist f00: ${PRDGEN_B_LIST_F00[$stream]:-""}
			B Parmlist fhh: ${PRDGEN_B_LIST_FHH[$stream]:-""}
			Do Anaylsis: ${PRDGEN_DO_ANALYSIS[$stream]:-"NO (default)"}

	EOF
done

export NTHREADS=1

####################################
# Specify Process ID
####################################
export IGEN_ANL=107
export IGEN_FCST=107

export DO_HD_PGRB=NO
export HDMAX=00

#################################
# Run Post if Needed
#################################
rm -f prdgen.cmdfile
for stream in ${PRDGEN_STREAMS[@]}; do
	if [[ $SENDCOM == "YES" ]]; then
		mkdir -m 775 -p $COMOUT/$COMPONENT/${PRDGEN_A_DIR[${stream}]}
		if [[ ! -z ${PRDGEN_B_DIR[$stream]} ]]; then
			mkdir -m 775 -p $COMOUT/$COMPONENT/${PRDGEN_B_DIR[${stream}]}
		fi
	fi

	subdata=${DATA}/${stream}
	if [ ! -d ${subdata} ]; then
		mkdir -p ${subdata};
	fi
	infile=${subdata}/${stream}.in
	outfile=${subdata}/${stream}.out

	cat > ${infile} <<-EOF
		jobgrid="${PRDGEN_GRID[$stream]}"
		grid_spec="${PRDGEN_GRID_SPEC[$stream]}"
		hours="${PRDGEN_HOURS[$stream]}"
		submc="${PRDGEN_SUBMC[$stream]}"
		pgad="${PRDGEN_A_DIR[$stream]}"
		pgapre="${PRDGEN_A_PREFIX[$stream]}"
		parmlist_a00=$PARMgefs/${PRDGEN_A_LIST_F00[$stream]}
		parmlist_ahh=$PARMgefs/${PRDGEN_A_LIST_FHH[$stream]}
		pgbd="${PRDGEN_B_DIR[$stream]}"
		pgbpre="${PRDGEN_B_PREFIX[$stream]}"
		parmlist_b00=$PARMgefs/${PRDGEN_B_LIST_F00[$stream]}
		parmlist_bhh=$PARMgefs/${PRDGEN_B_LIST_FHH[$stream]}
		do_analysis="${PRDGEN_DO_ANALYSIS[$stream]:-NO}"
	EOF

	echo "$HOMEgefs/ush/gefs_prdgen_driver.sh $stream $subdata \"$infile\" 2>&1 >${outfile}" >> prdgen.cmdfile

done

cat prdgen.cmdfile
chmod 775 prdgen.cmdfile
export MP_CMDFILE=${DATA}/prdgen.cmdfile
export SCR_CMDFILE=$MP_CMDFILE  # Used by mpiserial on Theia
export MP_PGMMODEL=mpmd
rm -f mpmd_cmdfile
ln -s $MP_CMDFILE mpmd_cmdfile

#############################################################
# Execute the script
$APRUN_MPMD
export err=$?

if [[ $err != 0 ]]; then
	echo "FATAL ERROR in ${.sh.file}: One or more prdgen streams in $MP_CMDFILE failed!"
	exit $err
fi
#############################################################

echo "$(date -u) end ${.sh.file}"

exit $err

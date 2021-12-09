#!/bin/ksh

echo "$(date -u) begin ${.sh.file}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

case $FORECAST_SEGMENT in
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
		end_hour=${fhmax}
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
			B Dir: ${PRDGEN_B_DIR[$stream]}
			B Prefix: ${PRDGEN_B_PREFIX[$stream]}
			Do Anaylsis: ${PRDGEN_DO_ANALYSIS[$stream]:-"NO (default)"}
	EOF
done

for stream in ${PRDGEN_STREAMS[@]}; do
	subdata=${DATA}/${stream}
	if [ ! -d ${subdata} ]; then mkdir -p ${subdata}; fi
	outfile=${subdata}/${stream}.out

	jobgrid="${PRDGEN_GRID[$stream]}"
	grid_spec="${PRDGEN_GRID_SPEC[$stream]}"
	hours="${PRDGEN_HOURS[$stream]}"
	submc="${PRDGEN_SUBMC[$stream]}"
	pgad="${PRDGEN_A_DIR[$stream]}"
	pgbd="${PRDGEN_B_DIR[$stream]}"
	pgapre="${PRDGEN_A_PREFIX[$stream]}"
	pgbpre="${PRDGEN_B_PREFIX[$stream]}"
	do_analysis="${PRDGEN_DO_ANALYSIS[$stream]:-NO}"
	if [[ $SENDCOM == "YES" && ! -d ${pgad} ]]; then mkdir -m 775 -p $COMOUT/$COMPONENT/${pgad}; fi
	
	echo "$HOMEgefs/ush/gefs_ensstat.sh $subdata \"$stream\" \"$jobgrid\" \"$hours\" \"$pgad\" \"$pgapre\" 2>&1 >${outfile}" >> ensstat.cmdfile

done

cat ensstat.cmdfile
chmod 775 ensstat.cmdfile
export MP_CMDFILE=${DATA}/ensstat.cmdfile
export SCR_CMDFILE=$MP_CMDFILE  # Used by mpiserial on Theia
export MP_PGMMODEL=mpmd

rm -f mpmd_cmdfile
ln -s $MP_CMDFILE mpmd_cmdfile

#############################################################
# Execute the script
$APRUN_MPMD
export err=$?

if [[ $err != 0 ]]; then
    echo "FATAL ERROR in ${.sh.file}: One or more streams in $MP_CMDFILE failed!"
    export err=100
fi
#############################################################

echo "$(date -u) end ${.sh.file}"

exit $err

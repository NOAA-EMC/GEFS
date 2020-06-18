#! /bin/ksh

echo "$(date -u) begin ${.sh.file}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

export RUNMEM=$RUNMEM
export mem=$(echo $RUNMEM|cut -c3-5)
export cplwav=${cplwav:-.true.}

export ERRSCRIPT=err_chk
export LOGSCRIPT=startmsg

case $FORECAST_SEGMENT in
	hr) 
		echo "Integrate the model for the Half-Month range segment"
        fhr_atmos_fcst=0
        fhr_atmos_post=0
        fhr_wave_post=0
		;;
	lr)
		echo "Integrate the model for the Longer Range segment"
        fhr_atmos_fcst=390
        fhr_atmos_post=390
        fhr_wave_post=0
		;;
	*)
		echo "FATAL ERROR in ${.sh.file}: Incorrect value of FORECAST_SEGMENT=$FORECAST_SEGMENT"
		export err=100
		exit $err
		;;
esac

########################################################
## Execute the script.
done_fcst=0
done_atmos_post=0
done_wave_post=0
ic=1
icDone=0
if [[ $cplwav = ".true." ]]; then
    icDone_Max=3
else
    icDone_Max=2
fi
export SLEEP_TIME=1800
export SLEEP_INT=5
SLEEP_LOOP_MAX=$(( $SLEEP_TIME / $SLEEP_INT ))
#while [ $icnt -lt 1000 ]
while [ $ic -le $SLEEP_LOOP_MAX ]; do
    echo $ic, $icDone

    if [ $done_fcst == 0 ]; then
        fhr3=$(printf %03i $fhr_atmos_fcst)
        if [ -s ${COMIN}/atmos/sfcsig/${RUNMEM}.t${cyc}z.logf${fhr3}.nemsio ]; then
            if [[ $SENDECF == "YES" ]]; then
                ecflow_client --event release_fcst
            else
                echo "release_fcst"
            fi
            (( icDone = icDone + 1 ))
            done_fcst=1
        fi
    fi

    if [ $done_atmos_post == 0 ]; then
        fhr3=$(printf %03i $fhr_atmos_post)
        if [ -s ${COMIN}/atmos/misc/post/${RUNMEM}.t${cyc}z.master.control.f${fhr3} ]; then
            if [[ $SENDECF == "YES" ]]; then
                ecflow_client --event release_atmos_post
            else
                echo "release_atmos_post"
            fi
            (( icDone = icDone + 1 ))
            done_atmos_post=1
        fi
    fi

    if [[ $done_wave_post == 0 && $cplwav = ".true." ]]; then
        fhr3=$(printf %03i $fhr_wave_post)
        if [ -s ${COMIN}/wave/gridded/gefswave.t${cyc}z.${mem}.global.0p25.f${fhr3}.grib2 ]; then
            if [[ $SENDECF == "YES" ]]; then
                ecflow_client --event release_wave_post
            else
                echo "release_wave_post"
            fi
            (( icDone = icDone + 1 ))
            done_wave_post=1
        fi
    fi

    if [ $icDone == $icDone_Max ]; then
        export err=0
        break
    fi

    sleep $SLEEP_INT
    ic=$(( ic + 1 ))
    ###############################
    # If we reach this point assume
    # fcst and post job never reached 
    # period and error exit
    ###############################
    if [ $ic -eq $SLEEP_LOOP_MAX ]; then
        echo "FATAL ERROR in ${.sh.file}: Forecast and post missing of $RUNMEM File still missing at $(date -u) after waiting ${SLEEP_TIME}s"
        export err=9
        err_chk || exit $err
     fi

done

if [[ $err != 0 ]]; then
	echo "FATAL ERROR in ${.sh.file}: received a non-zero return code from jgefs_fcst_post_manager"
	exit $err
fi

echo "$(date -u) end ${.sh.file}"

exit $err

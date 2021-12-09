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
                ecflow_client --event release_post
            else
                echo "release_post"
            fi
            (( icDone = icDone + 1 ))
            done_fcst=1
        fi
    fi

    if [ $done_atmos_post == 0 ]; then
        fhr3=$(printf %03i $fhr_atmos_post)
        if [ -s ${COMIN}/atmos/misc/post/${RUNMEM}.t${cyc}z.master.control.f${fhr3} ]; then
            if [[ $SENDECF == "YES" ]]; then
                ecflow_client --event release_atmos_prdgen
            else
                echo "release_atmos_prdgen"
            fi
            (( icDone = icDone + 1 ))
            done_atmos_post=1
        fi
    fi

    if [[ $done_wave_post == 0 && $cplwav = ".true." ]]; then
        fhr3=$(printf %03i $fhr_wave_post)
        if [ -s ${COMIN}/wave/gridded/gefs.wave.t${cyc}z.${mem}.global.0p25.f${fhr3}.grib2.idx ]; then
            if [[ $SENDECF == "YES" ]]; then
                ecflow_client --event release_wave_gempak
            else
                echo "release_wave_gempak"
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
        err_chk
     fi

done

#
#Check prdgen process
#
hour=00
TEND=240
TCP=243

if [ -e prdgenhours ]; then
   rm -f prdgenhours
fi

while [ $hour -lt $TCP ];
do
    hour=$(printf %03i $hour)
    echo $hour >>prdgenhours
    let "hour=hour+3"
done
prdgenhr=`cat prdgenhours`

icnt=1
while [ $icnt -lt 1000 ]
do
    for fhr in $prdgenhr
    do
        fhr3=`printf "%03d" $fhr`
        if [ -s ${COMIN}/atmos/pgrb2ap5/${RUNMEM}.t${cyc}z.pgrb2a.0p50.f${fhr3}.idx -a -s ${COMIN}/atmos/pgrb2bp5/${RUNMEM}.t${cyc}z.pgrb2b.0p50.f${fhr3}.idx -a -s ${COMIN}/wave/gridded/gefs.wave.t${cyc}z.${mem}.global.0p25.f${fhr3}.grib2.idx ]
        then
            # Remove current fhr from list
            prdgenhr=`echo $prdgenhr | sed "s/${fhr}//"`
            if [ $fhr -eq 192 ]; then
                if [[ $SENDECF == "YES" ]]; then
                    ecflow_client --event pgrb2abp5_f192_ready
                else
                    echo "pgrb2abp5_f192_ready"
                fi
            fi
            if [ $fhr -eq 240 ]; then
                if [[ $SENDECF == "YES" ]]; then
                    ecflow_client --event gefswave_f240_ready
                else
                    echo "gefswave_f240_ready"
                fi
            fi
        fi
    done

    result_check=`echo $prdgenhr | wc -w`
    if [ $result_check -eq 0 ]
    then
        break
    fi

    sleep 10
    icnt=$((icnt + 1))
    if [ $icnt -ge 1080 ]
    then
        msg="FATAL ERROR: ABORTING after 3 hours of waiting for ${RUNMEM} PRDGEN hours $prdgenhr."
        err_exit $msg
    fi

done


if [[ $err != 0 ]]; then
	echo "FATAL ERROR in ${.sh.file}: received a non-zero return code from jgefs_fcst_post_manager"
	err_chk
fi

echo "$(date -u) end ${.sh.file}"

exit $err

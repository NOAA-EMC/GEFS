#!/bin/ksh

echo "$(date -u) begin ${.sh.file}"
export PS4="${PS4}${1}: "

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

export mem=$1
export nmem=$(echo $mem|cut -c 2-)
nmem=${nmem#0}

export DATA=$DATA/$mem
export INIDIR=$DATA
export OUTDIR=$GESOUT/enkf/$mem
mkdir -p $INIDIR
mkdir -p $OUTDIR

cd $INIDIR

# Surface file must always be from current GFS cycle
SFCFILE=$COMINgfs/gfs.t${cyc}z.sfcanl.nemsio
if [[ -f $SFCFILE ]]; then
    $NCP $SFCFILE $INIDIR/sfnanl.gfs.$PDY$cyc
else
    msg="FATAL ERROR in ${.sh.file}: GFS surfce analysis $SFCFILE not found!"
    echo $msg
    err=100
    err_chk || exit $err
fi

if [[ $mem = c00 ]] ;then
    # Control intial conditions must be from current GFS cycle
    ATMFILE=$COMINgfs/gfs.t${cyc}z.atmanl.nemsio
    if [[ -f $ATMFILE ]]; then
        $NCP $ATMFILE $INIDIR/gfnanl.gfs.$PDY$cyc
    else
        msg="FATAL ERROR in ${.sh.file}: GFS atmospheric analysis file $ATMFILE not found!"
        echo "$msg"
        err=101
        err_chk || exit $err
    fi
else
    i=0
    success="NO"
	(( cmem = nmem + memshift ))
    while [[ $success == "NO" && i < MAX_ENKF_SEARCHES ]]; do
        if (( cmem > 80 )); then
            (( cmem = cmem - 80 ))
        fi

        memchar="mem"$(printf %03i $cmem)
        ATMFILE="$COMINenkf$pdyp/$cycp/$memchar/gdas.t${cycp}z.atmf006.nemsio"
    
        if [[ -f $ATMFILE ]]; then
            $NCP $ATMFILE $INIDIR/gfnanl.gfs.$PDY$cyc
            success="YES"
        else
            i=i+1
            if [[ i < MAX_ENKF_SEARCHES ]]; then
                echo "EnKF atmospheric file $ATMFILE not found, trying different member"
                (( cmem = cmem + ENKF_SEARCH_LEAP ))
            else
                msg="FATAL ERROR in ${.sh.file}: Unable to find EnKF atmospheric file after $MAX_ENKF_SEARCHES attempts"
                echo $msg
                err=102
                err_chk || exit $err
            fi
        fi # [[ -f $ATMFILE ]]
    done # [[ success == "NO" && i < MAX_ENKF_SEARCHES ]]
fi

#############################################################
# Execute the script
$USHgfs/global_chgres_driver.sh
err=$?
if [[ $err != 0 ]]; then
    echo "FATAL ERROR in ${.sh.file}: global_chgres_driver failed!"
    exit $err
fi
#############################################################

if [[ $SENDCOM == "YES" ]]; then
    mkdir -p $COMOUT/init/$mem
    mkdir -p $GESOUT/init/$mem
    $NCP $OUTDIR/sfc* $COMOUT/init/$mem
    $NCP $OUTDIR/sfc* $GESOUT/init/$mem
    $NCP $OUTDIR/gfs_ctrl.nc $COMOUT/init/$mem
    $NCP $OUTDIR/gfs_ctrl.nc $GESOUT/init/$mem
    if [[ $mem == "c00" ]]; then
        $NCP $OUTDIR/gfs_data*.nc $COMOUT/init/$mem
    fi
fi

echo "$(date -u) end ${.sh.file}"

exit $err
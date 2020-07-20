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
SFCFILE=$COMINgfs/gfs.t${cyc}z.sfcanl.nc
if [[ -f $SFCFILE ]]; then
    $NCP $SFCFILE $INIDIR
else
    msg="FATAL ERROR in ${.sh.file}: GFS surfce analysis $SFCFILE not found!"
    echo $msg
    export err=100
    err_chk || exit $err
fi

if [[ $mem = c00 ]] ;then
    # Control intial conditions must be from current GFS cycle
    ATMFILE=$COMINgfs/gfs.t${cyc}z.atmanl.nc
    if [[ -f $ATMFILE ]]; then
        $NCP $ATMFILE $INIDIR
        export ATM_FILES_INPUT="gfs.t${cyc}z.atmanl.nc"
        export SFC_FILES_INPUT="gfs.t${cyc}z.sfcanl.nc"
    else
        msg="FATAL ERROR in ${.sh.file}: GFS atmospheric analysis file $ATMFILE not found!"
        echo "$msg"
        export err=101
        err_chk || exit $err
    fi
export CONVERT_SFC=".true."
else
    i=0
    success="NO"
	(( cmem = nmem + memshift ))
    while [[ $success == "NO" && i < MAX_ENKF_SEARCHES ]]; do
        if (( cmem > 80 )); then
            (( cmem = cmem - 80 ))
        fi

        memchar="mem"$(printf %03i $cmem)
        ATMFILE="$COMINenkf$pdyp/$cycp/$memchar/gdas.t${cycp}z.atmf006.nc"
    
        if [[ -f $ATMFILE ]]; then
            $NCP $ATMFILE $INIDIR
        export ATM_FILES_INPUT="gdas.t${cycp}z.atmf006.nc"
            success="YES"
        else
            i=i+1
            if [[ i < MAX_ENKF_SEARCHES ]]; then
                echo "EnKF atmospheric file $ATMFILE not found, trying different member"
                (( cmem = cmem + ENKF_SEARCH_LEAP ))
            else
                msg="FATAL ERROR in ${.sh.file}: Unable to find EnKF atmospheric file after $MAX_ENKF_SEARCHES attempts"
                echo $msg
                export err=102
                err_chk || exit $err
            fi
        fi # [[ -f $ATMFILE ]]
    done # [[ success == "NO" && i < MAX_ENKF_SEARCHES ]]
export CONVERT_SFC=".false."
fi
export CRES=$(echo $CASEHR |cut -c2-5)
export HOMEufs=$HOMEgfs/sorc/global-workflow.fd/sorc/ufs_utils.fd
export COMIN=$INIDIR
export INPUT_TYPE="gaussian_netcdf"
export FIXfv3=$FIXgfs/fix_fv3_gmted2010/C$CRES
export FIXsfc=$FIXfv3/fix_sfc
#############################################################
# Execute the script
$USHgfs/chgres_cube.sh
export err=$?
if [[ $err != 0 ]]; then
    echo "FATAL ERROR in ${.sh.file}: global_chgres_driver failed!"
    exit $err
fi
#############################################################
mkdir -p $GESOUT/init/$mem
#$NCP $OUTDIR/sfc* $GESOUT/init/$mem
$NCP $OUTDIR/gfs_ctrl.nc $GESOUT/init/$mem
        if [[ $mem = c00 ]] ;then
  mv ${DATA}/gfs_ctrl.nc       $OUTDIR/.

  mv ${DATA}/out.atm.tile1.nc  $OUTDIR/gfs_data.tile1.nc
  mv ${DATA}/out.atm.tile2.nc  $OUTDIR/gfs_data.tile2.nc
  mv ${DATA}/out.atm.tile3.nc  $OUTDIR/gfs_data.tile3.nc
  mv ${DATA}/out.atm.tile4.nc  $OUTDIR/gfs_data.tile4.nc
  mv ${DATA}/out.atm.tile5.nc  $OUTDIR/gfs_data.tile5.nc
  mv ${DATA}/out.atm.tile6.nc  $OUTDIR/gfs_data.tile6.nc

  mv ${DATA}/out.sfc.tile1.nc $OUTDIR/sfc_data.tile1.nc
  mv ${DATA}/out.sfc.tile2.nc $OUTDIR/sfc_data.tile2.nc
  mv ${DATA}/out.sfc.tile3.nc $OUTDIR/sfc_data.tile3.nc
  mv ${DATA}/out.sfc.tile4.nc $OUTDIR/sfc_data.tile4.nc
  mv ${DATA}/out.sfc.tile5.nc $OUTDIR/sfc_data.tile5.nc
  mv ${DATA}/out.sfc.tile6.nc $OUTDIR/sfc_data.tile6.nc

        else

  mv ${DATA}/gfs_ctrl.nc       $OUTDIR/.
  mv ${DATA}/out.atm.tile1.nc  $OUTDIR/gfs_data.tile1.nc
  mv ${DATA}/out.atm.tile2.nc  $OUTDIR/gfs_data.tile2.nc
  mv ${DATA}/out.atm.tile3.nc  $OUTDIR/gfs_data.tile3.nc
  mv ${DATA}/out.atm.tile4.nc  $OUTDIR/gfs_data.tile4.nc
  mv ${DATA}/out.atm.tile5.nc  $OUTDIR/gfs_data.tile5.nc
  mv ${DATA}/out.atm.tile6.nc  $OUTDIR/gfs_data.tile6.nc
        fi

if [[ $SENDCOM == "YES" ]]; then
    mkdir -p $COMOUT/init/$mem
    $NCP $OUTDIR/sfc* $COMOUT/init/$mem
    $NCP $OUTDIR/gfs_ctrl.nc $COMOUT/init/$mem
    if [[ $mem == "c00" ]]; then
        $NCP $OUTDIR/gfs_data*.nc $COMOUT/init/$mem
    fi
fi

echo "$(date -u) end ${.sh.file}"

exit $err

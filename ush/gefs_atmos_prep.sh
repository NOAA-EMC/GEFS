#! /usr/bin/env bash

source "${HOMEgfs:-${HOMEgefs}}/ush/preamble.sh"

export mem=$1
export nmem=$(echo $mem|cut -c 2-)
nmem=${nmem#0}

YMD=${PDY} HH=${cyc} MEMDIR=${mem} generate_com -rx OUTDIR:COM_ATMOS_INPUT_TMPL

mkdir -p $OUTDIR
cd $DATA

# Copy from set_fixed_files.sh
#---------------------------------------------------------------------------
# Set directory names and file names for orog data
# The old and new (support fractional grid) orog data have different file names
#---------------------------------------------------------------------------
ORO_DIR="${CASE}.mx${OCNRES}_frac"
ORO_NAME="oro_${CASE}.mx${OCNRES}"

if [[ $mem = c00 ]] ;then
  # Control intial conditions from current GFS cycle
  export ATM_FILES_INPUT="gfs.t${cyc}z.atmanl.nc"
  YMD=${PDY} HH=${cyc} RUN=gfs MEMDIR="" ROTDIR=${ROTDIR_IN} generate_com -rx COM_ATMOS_ANALYSIS_GFS:COM_ATMOS_ANALYSIS_TMPL
  ATMFILE=${COM_ATMOS_ANALYSIS_GFS}/${ATM_FILES_INPUT}
  if [[ -f $ATMFILE ]]; then
    $NCP $ATMFILE $DATA
    #export ATM_FILE_INPUT="gfs.t${cyc}z.atmanl.nc"
  else
    msg="FATAL ERROR in $(basename $BASH_SOURCE): GFS atmospheric analysis file $ATMFILE not found!"
    echo "$msg"
    export err=101
    err_chk || exit $err
  fi
  export CONVERT_SFC=".true."

else
  i=0
  success="NO"
  (( cmem = nmem + memshift ))
  while [[ $success == "NO" && $i < $MAX_ENKF_SEARCHES ]]; do
    if (( cmem > 80 )); then
      (( cmem = cmem - 80 ))
    fi

    memchar="mem"$(printf %03i $cmem)
    export ATM_FILES_INPUT="enkfgfs.t${cyc}z.ratmanl.nc"
    YMD=${PDY} HH=${cyc} RUN=enkfgfs MEMDIR=${memchar} ROTDIR=${ROTDIR_IN} generate_com -rx COM_ATMOS_ANALYSIS_ENKFGFS:COM_ATMOS_ANALYSIS_TMPL
    ATMFILE=${COM_ATMOS_ANALYSIS_ENKFGFS}/${ATM_FILES_INPUT}


    if [[ -f $ATMFILE ]]; then
      $NCP $ATMFILE $DATA
      success="YES"

    else
      (( i = i + 1 ))
      if [[ $i < $MAX_ENKF_SEARCHES ]]; then
        echo "EnKF atmospheric file $ATMFILE not found, trying different member"
        (( cmem = cmem + ENKF_SEARCH_LEAP ))

      else
        msg="FATAL ERROR in $(basename $BASH_SOURCE): Unable to find EnKF atmospheric file after $MAX_ENKF_SEARCHES attempts"
        echo $msg
        export err=102
        err_chk || exit $err
      fi
    fi # [[ -f $ATMFILE ]]
  done # [[ success == "NO" && $i < MAX_ENKF_SEARCHES ]]
  export CONVERT_SFC=".false."
fi

if [[ $CONVERT_SFC == ".true." ]]; then
  export SFC_FILES_INPUT="gfs.t${cyc}z.sfcanl.nc"
  SFCFILE="${COM_ATMOS_ANALYSIS_GFS}/${SFC_FILES_INPUT}"
  if [[ -f $SFCFILE ]]; then
    $NCP ${SFCFILE} ${DATA}
  else
    msg="FATAL ERROR in $(basename ${BASH_SOURCE}): GFS surfce analysis ${SFCFILE} not found!"
    echo ${msg}
    export err=100
    err_chk || exit $err
  fi
fi

export CRES=$(echo ${CASE} |cut -c2-5)
export COMIN=$DATA
export INPUT_TYPE="gaussian_netcdf"
export FIXfv3=$FIXgfs/orog/${ORO_DIR}
export FIXsfc=$FIXfv3/fix_sfc
export FIXam=${FIXam:-$FIXgfs/am}
export VCOORD_FILE=${VCOORD_FILE:-$FIXam/global_hyblev.l${LEVS}.txt}

export TRACERS_INPUT="spfh","clwmr","o3mr","icmr","rwmr","snmr","grle"
export TRACERS_TARGET="sphum","liq_wat","o3mr","ice_wat","rainwat","snowwat","graupel"

OROG_FILES_TARGET_GRID=''
for tile in {1..6}
do
  OROG_FILES_TARGET_GRID=${OROG_FILES_TARGET_GRID}"${ORO_NAME}.tile${tile}.nc"
  if [[ $tile != 6 ]]; then
    OROG_FILES_TARGET_GRID=${OROG_FILES_TARGET_GRID}'","'
  fi
done
export OROG_FILES_TARGET_GRID

#############################################################
# Execute the script
$USHgfs/chgres_cube.sh
export err=$?
if [[ $err != 0 ]]; then
  echo "FATAL ERROR in $(basename $BASH_SOURCE): chgres_cube failed!"
  exit $err
fi
#############################################################

touch ${OUTDIR}/chgres_atm.log  # recenter can start now

if [[ $SENDCOM == "YES" ]]; then
  MODCOM=$(echo ${NET}_${COMPONENT} | tr '[a-z]' '[A-Z]')
  DBNTYP=${MODCOM}_INIT
  COMDIR=${OUTDIR}
  mkdir -p $COMDIR
  $NCP ${DATA}/gfs_ctrl.nc $COMDIR
  if [[ $SENDDBN = YES ]];then
    $DBNROOT/bin/dbn_alert MODEL $DBNTYP $job $COMDIR/gfs_ctrl.nc
  fi

  for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
    $NCP ${DATA}/out.atm.${tile}.nc $OUTDIR/gfs_data.${tile}.nc
    if [[ $SENDDBN = YES ]];then
      $DBNROOT/bin/dbn_alert MODEL $DBNTYP $job $COMDIR/gfs_data.${tile}.nc
    fi
  done

  if [[ $CONVERT_SFC == ".true." ]]; then
    for mem2 in $memberlist; do
      YMD=${PDY} HH=${cyc} MEMDIR=${mem2} generate_com COMDIR2:COM_ATMOS_INPUT_TMPL
      mkdir -p $COMDIR2
      for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
        $NCP ${DATA}/out.sfc.${tile}.nc $COMDIR2/sfc_data.${tile}.nc
        if [[ $SENDDBN = YES ]];then
          $DBNROOT/bin/dbn_alert MODEL $DBNTYP $job $COMDIR2/sfc_data.${tile}.nc
        fi
      done
    done
  fi
fi

echo "$(date -u) end $(basename $BASH_SOURCE)"

exit $err


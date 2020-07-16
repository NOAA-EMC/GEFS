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
if [[ $mem = c00 ]] ;then
    echo "sfc chgres will be carried out in p01"
else
    $NCP $SFCFILE $INIDIR/sfnanl.gfs.$PDY$cyc
fi
else
    msg="FATAL ERROR in ${.sh.file}: GFS surfce analysis $SFCFILE not found!"
    echo $msg
    export err=100
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
        export err=101
        err_chk || exit $err
    fi
else
    i=0
    success="NO"
	(( cmem = nmem + memshift ))
    while [[ $success == "NO" && $i < $MAX_ENKF_SEARCHES ]]; do
        if (( cmem > 80 )); then
            (( cmem = cmem - 80 ))
        fi

        memchar="mem"$(printf %03i $cmem)
        ATMFILE="$COMINenkf$pdyp/$cycp/$memchar/gdas.t${cycp}z.atmf006.nemsio"
    
        if [[ -f $ATMFILE ]]; then
            $NCP $ATMFILE $INIDIR/gfnanl.gfs.$PDY$cyc
            success="YES"
        else
            (( i = i + 1 ))
            if [[ $i < $MAX_ENKF_SEARCHES ]]; then
                echo "EnKF atmospheric file $ATMFILE not found, trying different member"
                (( cmem = cmem + ENKF_SEARCH_LEAP ))
            else
                msg="FATAL ERROR in ${.sh.file}: Unable to find EnKF atmospheric file after $MAX_ENKF_SEARCHES attempts"
                echo $msg
                export err=102
                err_chk || exit $err
            fi
        fi # [[ -f $ATMFILE ]]
    done # [[ $success == "NO" && $i < $MAX_ENKF_SEARCHES ]]
fi

#############################################################
export OMP_NUM_THREADS_CH=${OMP_NUM_THREADS_CH:-24}
export APRUNC=${APRUNC:-"time"}
export CASE=${CASE:-C96}                     # resolution of tile: 48, 96, 192, 384, 768, 1152, 3072
export CRES=`echo $CASE | cut -c 2-`
export CDATE=${CDATE:-${cdate:-2017031900}}  # format yyyymmddhh yyyymmddhh ...
export CDUMP=${CDUMP:-gfs}                   # gfs or gdas
export LEVS=${LEVS:-65}
export LSOIL=${LSOIL:-4}
export REGIONAL=${REGIONAL:-0}               # default is to assume uniform grid, which is REGIONAL=0
export VERBOSE=YES
pwd=$(pwd)
export NWPROD=${NWPROD:-$pwd}
export HOMEgfs=${HOMEgfs:-$NWPROD/gfs.v15.0.0}
export FIXfv3=${FIXfv3:-$HOMEgfs/fix/fix_fv3_gmted2010}
export FIXam=${FIXam:-$HOMEgfs/fix/fix_am}
export CHGRESEXEC=$HOMEgfs/exec/global_chgres
export CHGRESSH=$HOMEgfs/ush/global_chgres.sh

# Location of initial conditions for GFS (before chgres) and FV3 (after chgres)
export INIDIR=${INIDIR:-$pwd}
export OUTDIR=${OUTDIR:-$pwd/INPUT}
mkdir -p $OUTDIR

export gtype=${gtype:-uniform}      # grid type = uniform, stretch, nest or stand alone regional
export ntiles=6

#---------------------------------------------------------------

# Temporary rundirectory
export DATA=${DATA:-${RUNDIR:-$pwd/rundir$$}}
if [ ! -s $DATA ]; then mkdir -p $DATA; fi
cd $DATA || exit 8

export ymd=`echo $CDATE | cut -c 1-8`
export cyc=`echo $CDATE | cut -c 9-10`

export ictype='fv3gfs'

   export ATMANL=$INIDIR/gfnanl.${CDUMP}.$CDATE
   export SFCANL=$INIDIR/sfnanl.${CDUMP}.$CDATE
  if [ -s ${INIDIR}/sfnanl.${CDUMP}.$CDATE ]; then
  export SFCANL=$INIDIR/sfnanl.${CDUMP}.$CDATE
  else
  export SFCANL=NULL
  fi
   export NSTANL=NULL
 export SOILTYPE_INP=statsgo
 export VEGTYPE_INP=igbp
 export nopdpvv=.false.
 export NTRAC=7

 LONB_ATM=0   # not used for
 LATB_ATM=0   # ops files
 JCAP_CASE=$((CRES*2-2))
 LONB_SFC=$((CRES*4))
 LATB_SFC=$((CRES*2))

# to use new albedo, soil/veg type
export CLIMO_FIELDS_OPT=3
export LANDICE_OPT=${LANDICE_OPT:-2}
export IALB=1
export SOILTYPE_OUT=statsgo
export VEGTYPE_OUT=igbp
export FNZORC=igbp

export SIGLEVEL=${FIXam}/global_hyblev.l${LEVS}.txt
export FNGLAC=${FIXam}/global_glacier.2x2.grb
export FNMXIC=${FIXam}/global_maxice.2x2.grb
export FNTSFC=${FIXam}/cfs_oi2sst1x1monclim19822001.grb
export FNSNOC=${FIXam}/global_snoclim.1.875.grb
export FNALBC2=${FIXam}/global_albedo4.1x1.grb
export FNAISC=${FIXam}/cfs_ice1x1monclim19822001.grb
export FNTG3C=${FIXam}/global_tg3clim.2.6x1.5.grb
export FNVEGC=${FIXam}/global_vegfrac.0.144.decpercent.grb
export FNVMNC=${FIXam}/global_shdmin.0.144x0.144.grb
export FNVMXC=${FIXam}/global_shdmax.0.144x0.144.grb
export FNSLPC=${FIXam}/global_slope.1x1.grb
export FNMSKH=${FIXam}/global_slmask.t1534.3072.1536.grb
export FNSMCC=$FIXam/global_soilmgldas.statsgo.t${JCAP_CASE}.${LONB_SFC}.${LATB_SFC}.grb
export FNSOTC=$FIXam/global_soiltype.statsgo.t${JCAP_CASE}.${LONB_SFC}.${LATB_SFC}.rg.grb
export FNVETC=$FIXam/global_vegtype.igbp.t${JCAP_CASE}.${LONB_SFC}.${LATB_SFC}.rg.grb
export FNABSC=$FIXam/global_mxsnoalb.uariz.t${JCAP_CASE}.${LONB_SFC}.${LATB_SFC}.rg.grb
export FNALBC=$FIXam/global_snowfree_albedo.bosu.t${JCAP_CASE}.${LONB_SFC}.${LATB_SFC}.rg.grb
#------------------------------------------------
# Convert atmospheric file.
#------------------------------------------------

  export CHGRESVARS="use_ufo=.false.,idvc=2,nvcoord=2,idvt=21,idsl=1,IDVM=0,nopdpvv=$nopdpvv"
  export SIGINP=$ATMANL
  export SFCINP=NULL
  export NSTINP=NULL
  export JCAP=$JCAP_CASE
  export LATB=$LATB_ATM
  export LONB=$LONB_ATM
  $CHGRESSH
  rc=$?
  if [[ $rc -ne 0 ]] ; then
   echo "***ERROR*** rc= $rc"
   exit $rc
  fi

  mv ${DATA}/gfs_data.tile*.nc  $OUTDIR/.
  mv ${DATA}/gfs_ctrl.nc        $OUTDIR/.

  echo "$mem" > $OUTDIR/tile.log
#---------------------------------------------------
# Convert surface and nst files one tile at a time.
#---------------------------------------------------

  export CHGRESVARS="use_ufo=.true.,idvc=2,nvcoord=2,idvt=21,idsl=1,IDVM=0,nopdpvv=$nopdpvv"
  export SIGINP=NULL
  export SFCINP=$SFCANL
  export NSTINP=$NSTANL
  export JCAP=$JCAP_CASE
  export LATB=$LATB_SFC
  export LONB=$LONB_SFC
  if [[ $SFCANL = "NULL" ]]; then
  echo "sfc chgres skipped"
  else
    tile=1
    while [ $tile -le $ntiles ]; do
      export TILE_NUM=$tile
      $CHGRESSH
      rc=$?
      if [[ $rc -ne 0 ]] ; then
        echo "***ERROR*** rc= $rc"
        exit $rc
      fi
      mv ${DATA}/out.sfc.tile${tile}.nc $OUTDIR/sfc_data.tile${tile}.nc
      tile=`expr $tile + 1 `
    done
  fi # $SFCINP = NULL

# Execute the script
#$USHgfs/global_chgres_driver.sh
#export err=$?
#if [[ $err != 0 ]]; then
#    echo "FATAL ERROR in ${.sh.file}: global_chgres_driver failed!"
#    exit $err
#fi
#############################################################

mkdir -p $GESOUT/init/$mem

if [[ $mem == "c00" ]]; then
    $NCP $OUTDIR/gfs_ctrl.nc $GESOUT/init/$mem
elif [[ $mem == "p01" ]]; then
    mkdir -p $GESOUT/init/c00
    $NCP $OUTDIR/sfc* $GESOUT/init/c00
    $NCP $OUTDIR/sfc* $GESOUT/init/$mem
    $NCP $OUTDIR/gfs_ctrl.nc $GESOUT/init/$mem
else
    $NCP $OUTDIR/sfc* $GESOUT/init/$mem
    $NCP $OUTDIR/gfs_ctrl.nc $GESOUT/init/$mem
fi

if [[ $SENDCOM == "YES" ]]; then
    mkdir -p $COMOUT/init/$mem
    if [[ $mem == "c00" ]]; then
        $NCP $OUTDIR/gfs_ctrl.nc $COMOUT/init/$mem
    elif [[ $mem == "p01" ]]; then
        mkdir -p $COMOUT/init/c00
        $NCP $OUTDIR/sfc* $COMOUT/init/c00
        $NCP $OUTDIR/sfc* $COMOUT/init/$mem
        $NCP $OUTDIR/gfs_ctrl.nc $COMOUT/init/$mem
    else
        $NCP $OUTDIR/sfc* $COMOUT/init/$mem
        $NCP $OUTDIR/gfs_ctrl.nc $COMOUT/init/$mem
    fi

    if [[ $SENDDBN = YES ]];then
      $DBNROOT/bin/dbn_alert MODEL ENS_CTR_$mem $job $COMOUT/init/$mem/gfs_ctrl.nc
      $DBNROOT/bin/dbn_alert MODEL ENS_MSC_$mem $job $COMOUT/init/$mem/sfc_data.tile6.nc
    fi
    if [[ $mem == "c00" ]]; then
      $NCP $OUTDIR/gfs_data*.nc $COMOUT/init/$mem
      if [[ $SENDDBN = YES ]];then
              $DBNROOT/bin/dbn_alert MODEL ENS_SA_$mem $job $COMOUT/init/$mem/gfs_data.tile6.nc
      fi
    fi
fi

echo "$(date -u) end ${.sh.file}"
exit $rc



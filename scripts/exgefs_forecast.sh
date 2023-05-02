#! /usr/bin/env bash

source "${HOMEgfs:-${HOMEgefs}}/ush/preamble.sh"

# Additional paths needed by child scripts

if [[ ! -d ${COM_ATMOS_RESTART} ]]; then
  mkdir -m 755 -p ${COM_ATMOS_RESTART}
fi

case $FORECAST_SEGMENT in
  hr)
    echo "Integrate the model for the Half-Month range segment"
    filecount=$(ls -l ${COM_ATMOS_RESTART}/*coupler* | wc -l )
    if (( filecount < 1 )); then
      export RERUN="NO"
    else
      export RERUN="YES"
    fi

    if [[ $RERUN != "YES" ]] ; then
      export CDATE_RST=$($NDATE +$FHINI $PDY$cyc)
    fi
    ;;
  lr)
    echo "Integrate the model for the Longer Range segment"
    CDATE_1=$($NDATE +$FHINI $PDY$cyc)
    fRestart=${COM_ATMOS_RESTART}/$(echo $CDATE_1 | cut -c1-8).$(echo $CDATE_1 | cut -c9-10)0000.coupler.res
    if [ -f $fRestart ]; then
      (( FHINI_2 = fhmaxh + restart_interval_gfs ))
      CDATE_2=$($NDATE +$FHINI_2 $PDY$cyc)
      fRestart=${COM_ATMOS_RESTART}/$(echo $CDATE_2 | cut -c1-8).$(echo $CDATE_2 | cut -c9-10)0000.coupler.res
      if [ -f $fRestart ]; then
        export CDATE_RST=
      else
        export CDATE_RST=$($NDATE +$FHINI $PDY$cyc)
      fi
    else
      echo "FATAL ERROR in ${BASH_SOURCE}: There is no $fRestart"
      export err=101
      exit $err
    fi
    ;;
  *)
    echo "FATAL ERROR in ${BASH_SOURCE}: Incorrect value of FORECAST_SEGMENT=$FORECAST_SEGMENT"
    export err=100
    exit $err
    ;;
esac

#echo "-----end of CONFIG in ${BASH_SOURCE} --------"

################################################################################
if [[ $cplwav = ".true." ]]; then
#  export COMINwave=${COMINwave:-${COMIN}/wave}
#  export COMOUTwave=${COMOUTwave:-${COMIN}/wave}
#  export COMPONENTwave=${COMPONENTwave:-${RUN}.wave}
  # CPU partitioning
  export npe_wav=${npe_wav:-88}
  export npe_fcst_wav=$(( npe_fv3 + npe_wav ))
  export atm_petlist_bounds=" 0 $((npe_fv3-1))"
  export wav_petlist_bounds=" $((npe_fv3)) $((npe_fcst_wav-1))"
  export npe_fv3=$npe_fcst_wav
  export NTASKS_FV3=$npe_fv3
fi

if [[ $cplchm = ".true." ]]; then
  CHEMIN=${CHEMIN:-$memdir/chem}
  if [[ ! -d $CHEMIN ]]; then
    echo "FATAL: cplchm is .true. but there is no chem input directry at $CHEMIN"
    exit 200
  fi

  # Check which smoke emissions we are using
  gbbepx_found=0
  any_found=0
  for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
    if [[ -f $CHEMIN/$tile/plumefrp.dat ]]; then (( gbbepx_found = gbbepx_found + 1 )); fi
    if [[ -f $CHEMIN/$tile/plumestuff.dat ]]; then (( any_found = any_found + 1 )); fi
  done

  if (( gbbepx_found == 6 )); then
    # Using GBBEPx
    echo "GBBEPx smoke emissions detected and will be used"
    export EMITYPE=2
  elif (( any_found == 6 )); then
    # Using MODIS
    echo "WARNING: Detected missing of incomplete GBBEPx emisssions. Will use alternate emissions but forecast may be degraded!"
    echo "  Check $CHEMIN and your prep emission job if this is unexpected."
    msg="WARNING: ${job} detected incomplete GBBEPx emissions, will continue with alternate emissions but forecast may be degraded!"
    echo "$msg" | mail.py -c $MAIL_LIST
    export EMITYPE=1
  else
    echo "FATAL ERROR in ${BASH_SOURCE}: cplchm is .true. but smoke emissions are not available!"
    echo "  Check input directory $CHEMIN"
    export err=10
    exit $err
  fi

  if [[ $SENDCOM == "YES" ]]; then
    # Link restart files for next cycle
    mkdir -m 775 -p $COMOUT/$COMPONENT/restart

    next_date=$($NDATE +$gefs_cych $CDATE)
    next_PDY=$(echo $next_date | cut -c1-8)
    next_cyc=$(echo $next_date | cut -c9-10)
    COM_RSTDIR=${COMOUT/$COMPONENT/restart}
    if [[ ! -d ${COM_ATMOS_RESTART} ]]; then mkdir -p ${COM_ATMOS_RESTART}; fi
    ln -sf $COM_RSTDIR/${next_PDY}.${next_cyc}0000.coupler.res ${COM_ATMOS_RESTART}/${next_PDY}.${next_cyc}0000.coupler.res
    ln -sf $COM_RSTDIR/${next_PDY}.${next_cyc}0000.fv_core.res.nc ${COM_ATMOS_RESTART}/${next_PDY}.${next_cyc}0000.fv_core.res.nc
    for kind in fv_tracer.res fv_core.res fv_srf_wnd.res phy_data sfc_data; do
      for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
        ln -sf $COM_RSTDIR/${next_PDY}.${next_cyc}0000.${kind}.${tile}.nc ${COM_ATMOS_RESTART}/${next_PDY}.${next_cyc}0000.${kind}.${tile}.nc
      done
    done
  fi

fi # [[ $cplchm = ".true." ]]

#export increment_file=$ICSDIR/fv3_increment.nc

# Increment is never used for a rerun
if [[ $RERUN == "YES" ]]; then
  export read_increment=".false."
fi

if [[ $read_increment = ".true." && $warm_start = ".true." ]]; then
  # Make sure increment file exists before using warm start
  if [[ ! -f $increment_file ]]; then
    echo "WARNING: Warm-start requested but no increment file present, reverting to cold-start!"
    export warm_start=".false."
    export read_increment=".false."
  fi
fi

########################################################
## Execute the script.
$FORECASTSH
export err=$?
if [[ $err != 0 ]]; then
  echo "FATAL ERROR in ${BASH_SOURCE[0]}: received a non-zero return code from $FORECASTSH"
  exit $err
fi

exit $err

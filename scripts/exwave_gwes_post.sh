#!/bin/bash
###############################################################################
#                                                                             #
# This script is the postprocessor for the multi scale MWW3 wave model. It    #
# sets some shell script variables for export to child scripts and copies     #
# some generally used files to the work directory. After this the actual      #
# postprocessing is performed by the following child scripts :                #
#                                                                             #
#  ww3_grib.sh              : generates GRIB2 files.                          #
#  ww3_outp.sh              : generates spectral data files for output        #
#                             locations.                                      #
#  ww3_bull.sh              : generates bulletins for output locations.       #
#                             grids for backward compatibility                #
#  ww3_tar.sh               : tars the spectral and bulletin multiple files   #
#  ww3_gint.sh              : interpolates data from new grids to old grids   #
#                                                                             #
# Remarks :                                                                   #
# - The above scripts are (mostly) run under poe in parallel.                 #
#   Each script runs in its own directory created in DATA. If all is well     #
#   this directory disappears. If this directory is still there after poe     #
#   has finished, an error has occured Only then the output of the process    #
#   is copied to the output file. Otherwise, the output is deleted.           #
# - For non-fatal errors output is witten to the wave.log file.               #
#                                                                             #
# Origination  : 03/09/2007                                                   #
# Last update  : 05/01/2019                                                   #
#                                                                             #
###############################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

  set -x
  # Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x

  cd $DATA

  postmsg "$jlogfile" "HAS BEGUN on `hostname`"

  msg="Starting MWW3 POSTPROCESSOR SCRIPT for $wavemodID"
  postmsg "$jlogfile" "$msg"

  set +x
  echo ' '
  echo '                     *********************************'
  echo '                     *** MWW3 POSTPROCESSOR SCRIPT ***'
  echo '                     *********************************'
  echo ' '
  echo "Starting at : `date`"
  echo '-------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

#  export MP_PGMMODEL=mpmd
#  export MP_CMDFILE=./cmdfile

# Check if NTASKS set for mpiserial runs
NTASKS=${NTASKS:?Var NTASKS Not Set}

# 0.b Date and time stuff

  export date=$PDY
  export YMDH=${PDY}${cyc}

# 0.c Defining model grids

  buoy=${buoy:?buoyNotSet}

# 0.c.1 Grids

  export grids=${grids:?gridsNotSet}

# 0.c.3 extended global grid and rtma transfer grid
  export out_grids=${out_grids:?Var out_grids Not Set}

# 0.c.4 Define a temporary directory for storing ascii point output files
#       and flush it

  export STA_DIR=$DATA/station_ascii_files
  if [ -d $STA_DIR ]
  then 
    rm -rf ${STA_DIR}
  fi
  mkdir -p ${STA_DIR}
  mkdir -p ${STA_DIR}/spec
  mkdir -p ${STA_DIR}/bull
  mkdir -p ${STA_DIR}/cbull
  mkdir -p ${STA_DIR}/csbull

  set +x
  echo ' '
  echo 'Grid information  :'
  echo '-------------------'
  echo "   wave grids    : $grids"
  echo "   output points : ${wavemodID}_$buoy"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x


# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most child scripts

  field_OK='yes'
  point_OK='yes'
   grib_OK='yes'
  grint_OK='yes'
  grib1_OK='no '
  grintx_OK='no '
  grintox_OK='yes'
   spec_OK='yes'
   bull_OK='yes'
  Ospec_OK='no '
  Obull_OK='no '
  Oforc_OK='no '

  exit_code=0

  set +x
  echo ' '
  echo 'Preparing input files :'
  echo '-----------------------'
  [[ "$LOUD" = YES ]] && set -x

# 1.a Model definition files and output files (set up using poe) 

# 1.a.1 Set up the poe command 

  echo "NFILE: " $NTASKS
  nfile=$NTASKS

  set +x
  echo ' '
  echo "   Setting up first command file for copying model definition and data files."
  echo "   Set up command file structure"
  echo "      Number of command files                         : $nfile"
  [[ "$LOUD" = YES ]] && set -x

  rm -f cmdfile
  touch cmdfile

  [[ "$LOUD" = YES ]] && set -x

  ifile=1

  for grdID in $grids 
  do
  
    if [ ! -f out_grd.$grdID ]
    then
      set +x
      echo "   Copying $wavemodID.out_grd.$grdID.$PDY$cyc from $COMIN to out_grd.$grdID"
      [[ "$LOUD" = YES ]] && set -x

      echo  "cp $COMIN/$wavemodID.out_grd.$grdID.$PDY$cyc out_grd.$grdID"  >> cmdfile
    fi 

  done

  if [ ! -f out_pnt.ww3 ]
  then
    set +x
    echo "   Copying $COMIN/$wavemodID.out_pnt.${buoy}.$PDY$cyc to out_pnt.ww3"
    [[ "$LOUD" = YES ]] && set -x
    echo "cp $COMIN/$wavemodID.out_pnt.${buoy}.$PDY$cyc out_pnt.ww3" >> cmdfile
  fi

# 1.a.2 Execute the poe command

  set +x
  echo ' '
  echo "   Executing the copy command file at : `date`"
  echo '   ------------------------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$nfile" -gt '1' ]
  then
    ${gwesmpexec_mpmd} cmdfile
    exit=$?
  else
    chmod 744 cmdfile
    ./cmdfile
    exit=$?
  fi

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '********************************************'
    echo '*** POE FAILURE DURING RAW DATA COPYING ***'
    echo '********************************************'
    echo '     See Details Below '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
  fi

# 1.a.3 Error checks

  for grdID in $grids
  do
    if [ ! -f out_grd.$grdID ]
    then
      set +x
      echo ' '
      echo '**************************************** '
      echo '*** ERROR : NO RAW FIELD OUTPUT FILE *** '
      echo '**************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$wavemodID post $grdID $date $cycle : field output missing." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR : NO RAW FIELD OUTPUT FILE"
      exit_code=1
      field_OK='no'
      grib_OK='no'
    else
      set +x
      echo "File out_grd.$grdID found. Syncing to all nodes ..."
      [[ "$LOUD" = YES ]] && set -x
      $FSYNC out_grd.$grdID
    fi
  done

  if [ -f out_pnt.ww3 ]
  then
    set +x
    echo "   out_pnt.ww3 exists. Syncing to all nodes ..."
    [[ "$LOUD" = YES ]] && set -x
    $FSYNC out_pnt.ww3
  else
    set +x
    echo ' '
    echo '**************************************** '
    echo '*** ERROR : NO RAW POINT OUTPUT FILE *** '
    echo '**************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$wavemodID post $date $cycle : point output missing." >> $wavelog
    postmsg "$jlogfile" "NON-FATAL ERROR NO RAW POINT OUTPUT FILE"
    exit_code=12
    point_OK='no'
    spec_OK='no'
    bull_OK='no'
    Ospec_OK='no'
    Obull_ok='no'
  fi


# 1.c Model definition files

  for grdID in $grids $buoy $out_grids
  do
    if [ -f "$COMIN/${wavemodID}.mod_def.${grdID}" ]
    then
      set +x
      echo " Mod def file for $grdID found in $COMIN. copying ...."
      [[ "$LOUD" = YES ]] && set -x

      cp $COMIN/${wavemodID}.mod_def.${grdID} mod_def.$grdID

    else
      set +x
      echo " Mod def file for $grdID not found in $COMIN. Exiting ..."
      msg="ABNORMAL EXIT: NO mod_def FILE for grid $grdID"
      postmsg "$jlogfile" "$msg"
      set +x
      echo ' '
      echo '*********************************************************** '
      echo "*** FATAL ERROR : NO mod_def FILE FOR GRID $grdID *** "
      echo '*********************************************************** '
      echo ' '
      echo $msg
      [[ "$LOUD" = YES ]] && set -x
      echo "$wavemodID post $date $cycle : mod_def.$grdID missing." >> $wavelog
      exit_code=2
      grint_OK='no'
      grintx_OK='no'
      grib_OK='no'

    fi

  done

# 1.d Output locations file

  rm -f buoy.loc

  if [ -f $FIXwave/wave_$wavemodID.buoys ]
  then
    cp $FIXwave/wave_$wavemodID.buoys buoy.loc.temp
# Reverse grep to exclude IBP points
    sed -n '/^\$.*/!p' buoy.loc.temp | grep -v IBP > buoy.loc
    rm -f buoy.loc.temp
  fi

  if [ -f buoy.loc ]
  then
    set +x
    echo "   buoy.loc copied and processed ($FIXwave/wave_$wavemodID.buoys)."
    [[ "$LOUD" = YES ]] && set -x
  else
    set +x
    echo ' '
    echo '************************************* '
    echo '*** ERROR : NO BUOY LOCATION FILE *** '
    echo '************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$wavemodID post $date $cycle : buoy location file missing." >> $wavelog
    postmsg "$jlogfile" "NON-FATAL ERROR : NO BUOY LOCATION FILE"
    exit_code=13
    point_OK='no'
    spec_OK='no'
    bull_OK='no'
    Ospec_OK='no'
    Obull_ok='no'
  fi

# 1.e Input template files

  if [ "$grint_OK" = 'yes' ]
  then
    for outGRD in $out_grids
    do
    if [ -f $FIXwave/${outGRD}_interp.inp.tmpl ]
    then
      cp $FIXwave/${outGRD}_interp.inp.tmpl ${outGRD}_interp.inp.tmpl
    fi

    if [ -f ${outGRD}_interp.inp.tmpl ]
    then
      set +x
      echo "   ${outGRD}_interp.inp.tmpl copied. Syncing to all nodes ..."
      [[ "$LOUD" = YES ]] && set -x
      $FSYNC ${outGRD}_interp.inp.tmpl
    else
      set +x
      echo ' '
      echo '*********************************************** '
      echo '*** ERROR : NO TEMPLATE FOR GRINT INPUT FILE *** '
      echo '*********************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$wavemodID post $date $cycle : GRINT template file missing." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR GRINT INPUT FILE"
      exit_code=15
      grint_OK='no'
    fi
    done
  fi
  if [ "$grib_OK" = 'yes' ]
  then
    if [ -f $FIXwave/ww3_grib2.inp.tmpl ]
    then
      cp $FIXwave/ww3_grib2.inp.tmpl ww3_grib2.inp.tmpl
    fi

    if [ -f ww3_grib2.inp.tmpl ]
    then
      set +x
      echo "   ww3_grib2.inp.tmpl copied. Syncing to all nodes ..."
      [[ "$LOUD" = YES ]] && set -x
      $FSYNC ww3_grib2.inp.tmpl
    else
      set +x
      echo ' '
      echo '*********************************************** '
      echo '*** ERROR : NO TEMPLATE FOR GRIB INPUT FILE *** '
      echo '*********************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$wavemodID post $date $cycle : GRIB2 template file missing." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR GRIB2 INPUT FILE"
      exit_code=16
      grib_OK='no'
    fi
  fi

  if [ -f $FIXwave/ww3_spec.inp.tmpl ]
  then
    cp $FIXwave/ww3_spec.inp.tmpl ww3_spec.inp.tmpl
  fi

  if [ -f ww3_spec.inp.tmpl ]
  then
    set +x
    echo "   ww3_spec.inp.tmpl copied. Syncing to all grids ..."
    [[ "$LOUD" = YES ]] && set -x
    $FSYNC ww3_spec.inp.tmpl
  else
    set +x
    echo ' '
    echo '*********************************************** '
    echo '*** ERROR : NO TEMPLATE FOR SPEC INPUT FILE *** '
    echo '*********************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$wavemodID post $date $cycle : specra template file missing." >> $wavelog
    postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR SPEC INPUT FILE"
    exit_code=18
    spec_OK='no'
    bull_OK='no'
    Ospec_OK='no'
    Obull_OK='no'
  fi

  if [ -f $FIXwave/ww3_spec_bull.inp.tmpl ]
  then
    cp $FIXwave/ww3_spec_bull.inp.tmpl ww3_spec_bull.inp.tmpl
  fi

  if [ -f ww3_spec_bull.inp.tmpl ]
  then
    set +x
    echo "   ww3_spec_bull.inp.tmpl copied. Syncing to all nodes ..."
    [[ "$LOUD" = YES ]] && set -x
    $FSYNC ww3_spec_bull.inp.tmpl
  else
    set +x
    echo ' '
    echo '*************************************************** '
    echo '*** ERROR : NO TEMPLATE FOR BULLETIN INPUT FILE *** '
    echo '*************************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$wavemodID post $date $cycle : bulletin template file missing." >> $wavelog
    postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR BULLETIN INPUT FILE"
    exit_code=19
    bull_OK='no'
    Obull_OK='no'
  fi

# 1.f Getting buoy information for points

  if [ "$spec_OK" = 'yes' ] || [ "$bull_OK" = 'yes' ]
  then
    ymdh=`$NDATE -9 $YMDH`
    tstart="`echo $ymdh | cut -c1-8` `echo $ymdh | cut -c9-10`0000"
    dtspec=3600.            # default time step (not used here)
    sed -e "s/TIME/$tstart/g" \
        -e "s/DT/$dtspec/g" \
        -e "s/POINT/1/g" \
        -e "s/ITYPE/0/g" \
        -e "s/FORMAT/F/g" \
                               ww3_spec.inp.tmpl > ww3_spec.inp
   
    ln -s mod_def.$buoy mod_def.ww3
    rm -f ww3_oup.inp
    ln -s ww3_spec.inp ww3_outp.inp  
    $EXECcode/ww3_outp > buoy_tmp.loc 
    err=$?

    if [ "$err" != '0' ]
    then
      pgm=wave_post
      msg="ABNORMAL EXIT: ERROR IN ww3_spec"
      postmsg "$jlogfile" "$msg"
      set +x
      echo ' '
      echo '******************************************** '
      echo '*** FATAL ERROR : ERROR IN ww3_spec *** '
      echo '******************************************** '
      echo ' '
      echo "$wavemodID post $date $cycle : buoy log file failed to be created." >> $wavelog
      echo $msg
      [[ "$LOUD" = YES ]] && set -x
      exit_code=19
      spec_OK='no'
      bull_OK='no'
      Ospec_OK='no'
      Obull_OK='no'
    fi

# Create new buoy_log.ww3 excluding all IBP files
    cat buoy.loc | awk '{print $3}' | sed 's/'\''//g' > ibp_tags
    grep -F -f ibp_tags buoy_log.ww3 > buoy_log.tmp
    rm -f buoy_log.ww3
    mv buoy_log.tmp buoy_log.ww3

    grep -F -f ibp_tags buoy_tmp.loc >  buoy_tmp1.loc
    sed -n '11,/^$/p' buoy_tmp1.loc > buoy_tmp2.loc
    sed    '$d' buoy_tmp2.loc > buoy_tmp3.loc
    buoys=`awk '{ print $1 }' buoy_tmp3.loc`
    Nb=`wc buoy_tmp3.loc | awk '{ print $1 }'`
    rm buoy_tmp.loc buoy_tmp1.loc buoy_tmp2.loc buoy_tmp3.loc

    if [ -f buoy_log.ww3 ]
    then
      set +x
      echo 'Buoy log file created. Syncing to all nodes ...'
      [[ "$LOUD" = YES ]] && set -x
      $FSYNC buoy_log.ww3
    else
      set +x
      echo ' '
      echo '**************************************** '
      echo '*** ERROR : NO BUOY LOG FILE CREATED *** '
      echo '**************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$wavemodID post $date $cycle : buoy log file missing." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR : NO BUOY LOG FILE GENERATED FOR SPEC AND BULLETIN FILES"
      exit_code=19
      spec_OK='no'
      bull_OK='no'
      Ospec_OK='no'
      Obull_OK='no'
    fi

  fi

# 1.g Data summary

  set +x
  echo ' '
  echo "   Input files read and processed at : `date`"
  echo ' ' 
  echo '   Data summary : '
  echo '   ---------------------------------------------'
  echo "      Sufficient data for GRID interpolation    : $grint_OK"
  echo "      Sufficient data for extending global grid : $grintx_OK"
  echo "      Sufficient data for GRIB files            : $grib_OK"
  echo "      Sufficient data for spectral files        : $spec_OK ($Nb points)"
  echo "      Sufficient data for bulletins             : $bull_OK ($Nb points)"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 2.  Make second command file(s) (GRID interpolation and GRIB generation)
# 2.a Command file set-up
#     The command file points to $nfile files named cmdfile.$ifile.
#     The actual work is distributed over these files. The skip parameter
#     is used for load balancing. GRIB packing takes more time than making
#     spectral data files or bulletins.

  set +x
  echo '   Making second command file (GRID Interpolation, GRIB, SPEC and BULLETINS) '

  rm -f cmdfile
  touch cmdfile

  [[ "$LOUD" = YES ]] && set -x

# 2.c GRIB files for main grids

# GRIB field time step -- dtgrib
# Number of GRIB fields -- ngrib
# Assigned NCEP number for grid -- GRIDNR
# Assigned NCEP number for model -- MODNR

  if [ "$grib_OK" = 'yes' ]
  then
    for grdID in $grids $out_grids
    do

      case $grdID in
      glo_30m) GRIDNR=255  ; MODNR=255  ; dtgrib=10800. ;;
      glo_15mext) gribFL=\''WND CUR ICE HS T01 T02 FP DIR SPR DP PHS PTP PDIR'\';
                  GRIDNR=11  ; MODNR=255 ; dtgrib=10800. ; ngrib=181; ymdh_int=`$NDATE -9 $YMDH`; dt_int=10800.; n_int=190 ; make_grib='yes';;
      glo_30mext) gribFL=\''WND CUR ICE HS T01 T02 FP DIR SPR DP PHS PTP PDIR'\';
                  GRIDNR=11  ; MODNR=11  ; dtgrib=3600. ; ngrib=181; ymdh_int=`$NDATE -9 $YMDH`; dt_int=3600.; n_int=190 ; make_grib='yes';;
      esac

# Recalculate ngrib based on lsth (TODO: add new interval if changes to dtgrib after given forecast hour)
      dtgi=`echo ${dtgrib} | sed 's/\.//g'`
      dtgh=`expr ${dtgi} / 3600`
      ngrib=`expr ${lsth} / ${dtgh} + 1`

      export dtgrib ngrib 
# Next line for multi_1
#      echo "$USHwave/ww3_grib2_cat.sh $grdID > grib_$grdID.out 2>&1"               >> cmdfile
      echo "$USHwave/ww3_grib2.sh $grdID $dtgrib $ngrib $GRIDNR $MODNR $gribFL > grib_$grdID.out 2>&1"               >> cmdfile

    done


$USHwave/ww3_grib2.sh $grdID $dtgrib $ngrib $GRIDNR $MODNR $gribFL


  fi
#### Done up to here: need to test grib2 output
exit

  if [ "$grintox_OK" = 'yes' ]
  then
# Other Xtended grids to be activated if need to post-process other xgrids
# Currently the global xgrid is created side by side and the unified fiel can
# be created as other regular grid types using ww3_grib2_cat
    for grdID in $OXgrids
    do
      case $grdID in
      glo_15mext) gribFL=\''WND CUR ICE HS T01 T02 FP DIR DP PHS PTP PDIR'\';
                  GRIDNR=11  ; MODNR=255 ; dtgrib=10800. ; ngrib=181; ymdh_int=`$NDATE -9 $YMDH`; dt_int=10800.; n_int=190 ; make_grib='yes';;
      glo_30mext) gribFL=\''WND CUR ICE HS T01 T02 FP DIR DP PHS PTP PDIR'\';
                  GRIDNR=11  ; MODNR=11  ; dtgrib=3600. ; ngrib=181; ymdh_int=`$NDATE -9 $YMDH`; dt_int=3600.; n_int=190 ; make_grib='yes';;
      esac

      if [ "$make_grib" = 'yes' ]
      then
        echo "$USHwave/ww3_grid_interp.sh $grdID $ymdh_int $dt_int $n_int > grint_$grdID.out 2>&1 ; $USHwave/ww3_grib2.sh $grdID $dtgrib $ngrib $GRIDNR $MODNR $gribFL > grib_$grdID.out 2>&1"  >> cmdfile
      else
        echo "$USHwave/ww3_grid_interp.sh $grdID $ymdh_int $dt_int $n_int > grint_$grdID.out 2>&1"  >> cmdfile
      fi

    done
  fi

# 2.e Spectral data files

  set +x; [ "$LOUD" = YES -a "$spec_OK" = 'yes' ] 
  if [ "$spec_OK" = 'yes' ]
  then
    export dtspec=10800.   # time step for spectra
    ymdh=`$NDATE -9 $YMDH` # start time for spectra output

    ifile=1
    ilayer=1
    for buoy in $buoys
    do
      echo "$USHwave/ww3_spec2.sh $buoy $ymdh > spec_$buoy.out 2>&1" >> cmdfile
    done
  fi
  [[ "$LOUD" = YES ]] && set -x

# 2.f Bulletins

  set +x; [ "$LOUD" = YES -a "$bull_OK" = 'yes' ] 
  if [ "$bull_OK" = 'yes' ]
  then
    export dtbull=3600.    # time step for bulletins
    ymdh=`$NDATE -9 $YMDH` # start time for bulletin output
   
    for buoy in $buoys
    do
      echo "$USHwave/ww3_spec_bull.sh $buoy $ymdh > bull_$buoy.out 2>&1" >> cmdfile
    done
  fi
  [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 3   Execute second command file
# 3.a Execution

  set +x
  echo "   Executing second command file at : `date`"
  echo '   ----------------------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$nfile" -gt '1' ]
  then
    ${gwesmpexec_mpmd} cmdfile
    exit=$?
  else
#    ./cmdfile.1
    ./cmdfile
    exit=$?
  fi

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '***********************************************************'
    echo '*** POE FAILURE DURING GRIB AND POINT OUTPUT GENERATION ***'
    echo '***********************************************************'
    echo '     See Details Below '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
  fi

# --------------------------------------------------------------------------- #
# 4.  Check for errors

  set +x
  echo ' '
  echo '   Checking for errors (error output concatenated below).'
  [[ "$LOUD" = YES ]] && set -x

# 4.a Old grid interpolation

  if [ "$grint_OK" = 'yes' ]
  then
    for grdID in $Ogrids
    do
      if [ -d grint_$grdID ]
      then
        set +x
        echo "      Error in GRID interpolation for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "NON-FATAL ERROR in GRID interpolation for $grdID."
      else
        rm -f grint_$grdID.out
        set +x
        echo "      GRID interpolation successful for $grdID."
        [[ "$LOUD" = YES ]] && set -x
      fi

      if [ -d grib_$grdID ]
      then
        set +x
        echo "      Error in GRIB encoding for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "NON-FATAL ERROR in GRIB encoding for $grdID."
      else
        rm -f grib_$grdID.out
        set +x
        echo "      GRIB encoding successful for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        touch $COMOUT/$grdID.t${cyc}z.gribdone
      fi
      
      if [ "$grib1_OK" = 'yes' ]
      then
        if [ -d grib1_$grdID ]
        then
          set +x
          echo "      Error in GRIB1 encoding for $grdID."
          [[ "$LOUD" = YES ]] && set -x
          postmsg "$jlogfile" "NON-FATAL ERROR in GRIB1 encoding for $grdID."
        else
          rm -f grib1_$grdID.out
          set +x
          echo "      GRIB1 encoding successful for $grdID."
          [[ "$LOUD" = YES ]] && set -x
          touch $COMOUT/$grdID.t${cyc}z.grib1done
        fi
      fi
    done
  fi

# 4.b Extended grid interpolation

  if [ "$grintx_OK" = 'yes' ]
  then
    for grdID in $Xgrids
    do
      if [ -d grint_$grdID ]
      then
        set +x
        echo "      Error in GRID interpolation for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "NON-FATAL ERROR in GRID interpolation for $grdID."
      else
        rm -f grint_$grdID.out
        set +x
        echo "      GRID interpolation successful for $grdID."
        [[ "$LOUD" = YES ]] && set -x
      fi

      if [ -d grib_$grdID ]
      then
        set +x
        echo "      Error in GRIB encoding for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "NON-FATAL ERROR in GRIB encoding for $grdID."
      else
        rm -f grib_$grdID.out
        set +x
        echo "      GRIB encoding successful for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        touch $COMOUT/$grdID.t${cyc}z.gribdone
      fi
    done
  fi
# 4.b Extended grid interpolation

  if [ "$grintox_OK" = 'yes' ]
  then
    for grdID in $OXgrids
    do
      if [ -d grint_$grdID ]
      then
        set +x
        echo "      Error in GRID interpolation for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "NON-FATAL ERROR in GRID interpolation for $grdID."
      else
        rm -f grint_$grdID.out
        set +x
        echo "      GRID interpolation successful for $grdID."
        [[ "$LOUD" = YES ]] && set -x
      fi

      if [ -d grib_$grdID ]
      then
        set +x
        echo "      Error in GRIB encoding for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "NON-FATAL ERROR in GRIB encoding for $grdID."
      else
        rm -f grib_$grdID.out
        set +x
        echo "      GRIB encoding successful for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        touch $COMOUT/$grdID.t${cyc}z.gribdone
      fi
    done
  fi

# 4.b GRIB file

  if [ "$grib_OK" = 'yes' ]
  then
    for grdID in $grids
    do
      if [ -d grib_$grdID ]
      then
        set +x
        echo "      Error in GRIB encoding for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "NON-FATAL ERROR in GRIB encoding for $grdID."
      else
        rm -f grib_$grdID.out
        set +x
        echo "      GRIB encoding successful for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        touch $COMOUT/$grdID.t${cyc}z.gribdone
      fi
    done
  fi

# 4.d Spectral data files and bulletins

  set +x
#  bullstring='Bulletins not generated'
#  specstring='Spectra not generated'

  if  [ "$spec_OK" = 'yes' ]
  then
    if [ -d spec_* ]
    then
      for buoy in $buoys
      do
        if [ -d spec_$buoy ]
        then
          specstring='Error in spectra.'
          postmsg "$jlogfile" "NON-FATAL ERROR in spectra."
        else
          rm -f spec_$buoy.out
        fi
      done
    else
      rm -f spec*.out
    fi
  fi
 
  if  [ "$bull_OK" = 'yes' ]
  then
    if [ -d bull_* ]
    then
      for buoy in $buoys
      do
        if [ -d bull_$buoy ]
        then
          specstring='Error in spectra.'
          postmsg "$jlogfile" "NON-FATAL ERROR in spectra."
        else
          rm -f bull_$buoy.out
        fi
      done
    else
      rm -f bull*.out
    fi
  fi

  [[ "$LOUD" = YES ]] && set -x

  if [ -f *.out ]
  then
    for grdID in $Ogrids
    do 
      if [ -f grint_$grdID.out ]
      then
        set +x
        echo ' '
        echo '********************************************'
        echo '*** ERROR OUTPUT ww3_grid_interp.sh ***'
        echo '********************************************'
        echo ' '
        [[ "$LOUD" = YES ]] && set -x 
        echo "$wavemodID post $date $cycle : error in GRID Interpolation." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR in ww3_grid_interp.sh"
        exit_code=20
        sed "s/^/grint_$grdID.out : /g"  grint_$grdID.out
        rm -f grint_$grdID.out
	grint_OK='no'
      fi

      if [ -f grib_$grdID.out ]
      then
        set +x
        echo ' '
        echo '**************************************'
        echo '*** ERROR OUTPUT ww3_grib2.sh ***'
        echo '**************************************'
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        echo "$wavemodID post $date $cycle : error in subsequent GRIB encoding (for interpolated grids)." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR in ww3_grib2.sh"
        exit_code=21
        sed "s/^/grib_$grdID.out : /g"  grib_$grdID.out
        rm -f grib_$grdID.out
	grint_OK='no'
      fi

      if [ -f grib1_$grdID.out ]
      then
        set +x
        echo ' '
        echo '**************************************'
        echo '*** ERROR OUTPUT ww3_grib1.sh ***'
        echo '**************************************'
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        echo "$wavemodID post $date $cycle : error in subsequent GRIB1 encoding (for interpolated grids)." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR in ww3_grib1.sh"
        exit_code=21
        sed "s/^/grib1_$grdID.out : /g"  grib1_$grdID.out
        rm -f grib1_$grdID.out
	grint_OK='no'
      fi
    done

    for grdID in $grids
    do 
      if [ -f grib_$grdID.out ]
      then
        set +x
        echo ' '
        echo '**************************************'
        echo '*** ERROR OUTPUT ww3_grib2.sh ***'
        echo '**************************************'
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        echo "$wavemodID post $date $cycle : error in GRIB." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR in ww3_grib2.sh"
        exit_code=22
        sed "s/^/grib_$grdID.out : /g"  grib_$grdID.out
        rm -f grib_$grdID.out
      fi

    done

    if [ -f spec_*.out ]
    then
      set +x
      echo ' '
      echo '*************************************'
      echo '*** ERROR OUTPUT ww3_spec.sh ***'
      echo '*************************************'
      echo '            Possibly in multiple calls'
      [[ "$LOUD" = YES ]] && set -x
      echo "$wavemodID post $date $cycle : error in spectra." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in ww3_spec.sh, possibly in multiple calls."
      exit_code=24
      for file in spec_*.out
      do
        echo ' '
        sed "s/^/$file : /g" $file
      done
      rm -f spec_*.out
    fi

    if [ -f bull_*.out ]
    then
      set +x
      echo ' '
      echo '******************************************'
      echo '*** ERROR OUTPUT ww3_spec_bull.sh ***'
      echo '******************************************'
      echo '            Possibly in multiple calls'
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$wavemodID post $date $cycle : error in bulletins." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in ww3_bull.sh, possibly in multiple calls."
      exit_code=25
      for file in bull_*.out
      do
        echo ' '
        sed "s/^/$file : /g" $file
      done
      rm -f bull_*.out
    fi
  fi


# --------------------------------------------------------------------------- #

if [ "$Ospec_OK" = 'yes' ]
then

# 5.  Make third command file

  set +x
#  rm -f cmdfile*
  rm -f cmdfile
  touch cmdfile
  echo ' '
  echo '   Making third command file (copying points files to old grids).'

  [[ "$LOUD" = YES ]] && set -x

  ifile=1

# 5.a Spectral files

  set +x
  if [ "$Ospec_OK" = 'yes' ] && [ "$spec_OK" = 'yes' ]
  then
    for grdID in $Ogrids
    do
      echo "$USHwave/ww3_copy.sh $grdID spec > spec_${grdID}_copy.out 2>&1" >> cmdfile
    done
  fi

# 5.b Bulletins

  if [ "$Obull_OK" = 'yes' ] && [ "$bull_OK" = 'yes' ]
  then
    for grdID in $Ogrids
    do
      echo "$USHwave/ww3_copy.sh $grdID bull > bull_${grdID}_copy.out 2>&1" >> cmdfile
      echo "$USHwave/ww3_copy.sh $grdID cbull > cbull_${grdID}_copy.out 2>&1" >> cmdfile
      echo "$USHwave/ww3_copy.sh $grdID csbull > csbull_${grdID}_copy.out 2>&1" >> cmdfile
    done
  fi
  [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 6   Execute third command file


  set +x
  echo "   Executing point copy command file at : `date`"
  echo '   --------------------------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$nfile" -gt '1' ]
  then
    ${gwesmpexec_mpmd} cmdfile
    exit=$?
  else
#    ./cmdfile.1
    ./cmdfile
    exit=$?
  fi

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '****************************************'
    echo '*** POE FAILURE DURING POINT COPYING ***'
    echo '****************************************'
    echo '     See Details Below '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
  fi

#  set +x
#  rm -f cmdfile*
#  [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 7. Check for errors 

# 7.a Spectral files

  if [ $Ospec_OK = 'yes' ] 
  then
    for grdID in $Ogrids
    do
      if [ -d COPY_spec_$grdID ]
      then
        set +x
        echo ' '
        echo '*************************************'
        echo '*** ERROR OUTPUT ww3_copy.sh ***'
        echo '*************************************'
        echo '            Possibly in multiple calls'
        [[ "$LOUD" = YES ]] && set -x
        echo "$wavemodID post $date $cycle : error in copying spectral files to $grdID." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR in ww3_copy.sh, possibly in multiple calls."
        exit_code=26
      else
        rm -f spec_${grdID}_copy.out
        set +x
        echo "  Copying of spectral files to $grdID succesful"
        [[ "$LOUD" = YES ]] && set -x
      fi
    done
  fi

# 7.b Bulletin files

  if [ $Obull_OK = 'yes' ] 
  then
    for grdID in $Ogrids
    do
      if [ -d COPY_bull_$grdID ]
      then
        set +x
        echo ' '
        echo '*************************************'
        echo '*** ERROR OUTPUT ww3_copy.sh ***'
        echo '*************************************'
        echo '            Possibly in multiple calls'
        [[ "$LOUD" = YES ]] && set -x
        echo "$wavemodID post $date $cycle : error in copying bulletin files to $grdID." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR in ww3_copy.sh, possibly in multiple calls."
        exit_code=27
      else
        rm -f bull_${grdID}_copy.out
        set +x
        echo "  Copying of bulletin files to $grdID succesful"
        [[ "$LOUD" = YES ]] && set -x
      fi

      if [ -d COPY_cbull_$grdID ]
      then
        set +x
        echo ' '
        echo '*************************************'
        echo '*** ERROR OUTPUT ww3_copy.sh ***'
        echo '*************************************'
        echo '            Possibly in multiple calls'
        [[ "$LOUD" = YES ]] && set -x
        echo "$wavemodID post $date $cycle : error in copying compressed bulletin files to $grdID." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR in ww3_copy.sh, possibly in multiple calls."
        exit_code=28
      else
        rm -f cbull_${grdID}_copy.out
        set +x
        echo "  Copying of compressed bulletin files to $grdID succesful"
        [[ "$LOUD" = YES ]] && set -x
      fi

      if [ -d COPY_csbull_$grdID ]
      then
        set +x
        echo ' '
        echo '*************************************'
        echo '*** ERROR OUTPUT ww3_copy.sh ***'
        echo '*************************************'
        echo '            Possibly in multiple calls'
        [[ "$LOUD" = YES ]] && set -x
        echo "$wavemodID post $date $cycle : error in copying csv bulletin files to $grdID." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR in ww3_copy.sh, possibly in multiple calls."
        exit_code=29
      else
        rm -f csbull_${grdID}_copy.out
        set +x
        echo "  Copying of csv bulletin files to $grdID succesful"
        [[ "$LOUD" = YES ]] && set -x
      fi
    done
  fi

# 7.c Error outputs

  if [ -f *.out ]
  then
    set +x
    echo ' '
    echo '*********************'
    echo '*** ERROR OUTPUTS ***'
    echo '*********************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    exit_code=30
    for file in *.out
    do
      echo ' '
      sed "s/^/$file : /g" $file
    done
    rm -f *.out
  fi

fi

# --------------------------------------------------------------------------- #
# 8.  Make fourth command file

  set +x
  rm -f cmdfile
  touch cmdfile
  echo ' '
  echo '   Making fourth command file for taring all point output files.'

  [[ "$LOUD" = YES ]] && set -x

  ifile=1

# 8.a Spectral data files

  if [ "$spec_OK" = 'yes' ]
  then
    echo "$USHwave/ww3_tar.sh $wavemodID spec $Nb > ${wavemodID}_spec_tar.out 2>&1 "   >> cmdfile

  fi

# 8.b Bulletins

  if [ "$bull_OK" = 'yes' ]
  then
    echo "$USHwave/ww3_tar.sh $wavemodID bull $Nb > ${wavemodID}_bull_tar.out 2>&1 "   >> cmdfile

  fi

# 8.c Compressed bulletins

  if [ "$bull_OK" = 'yes' ]
  then
     echo "$USHwave/ww3_tar.sh $wavemodID cbull $Nb > ${wavemodID}_cbull_tar.out 2>&1 " >> cmdfile
  fi

# 8.d CSV bulletins

  if [ "$bull_OK" = 'yes' ]
  then
    echo "$USHwave/ww3_tar.sh $wavemodID csbull $Nb > ${wavemodID}_csbull_tar.out 2>&1 " >> cmdfile
  fi

# 8.e Old Spectral data files

  if [ "$Ospec_OK" = 'yes' ]
  then
    for grdID in $Ogrids
    do
      Nb=`wc buoy_${grdID}.loc | awk '{ print $1 }'`
      echo "$USHwave/ww3_tar.sh $grdID spec $Nb > ${grdID}_spec_tar.out 2>&1 "   >> cmdfile
    done
  fi

# 8.f Old Bulletins

  if [ "$Obull_OK" = 'yes' ]
  then
    for grdID in $Ogrids
    do
      Nb=`wc buoy_${grdID}.loc | awk '{ print $1 }'`
      echo "$USHwave/ww3_tar.sh $grdID bull $Nb > ${grdID}_bull_tar.out 2>&1 "   >> cmdfile
    done
  fi

# 8.g Old Compressed Bulletins

  if [ "$Obull_OK" = 'yes' ]
  then
    for grdID in $Ogrids
    do
      Nb=`wc buoy_${grdID}.loc | awk '{ print $1 }'`
      echo "$USHwave/ww3_tar.sh $grdID cbull $Nb > ${grdID}_cbull_tar.out 2>&1 "   >> cmdfile
    done
  fi

# 8.h Old CSV Bulletins

  if [ "$Obull_OK" = 'yes' ]
  then
    for grdID in $Ogrids
    do
      Nb=`wc buoy_${grdID}.loc | awk '{ print $1 }'`
      echo "$USHwave/ww3_tar.sh $grdID csbull $Nb > ${grdID}_csbull_tar.out 2>&1 "   >> cmdfile
    done
  fi

# --------------------------------------------------------------------------- #
# 9.  Execute fourth command file

  set +x
  echo "   Executing tar command file at : `date`"
  echo '   -------------------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$nfile" -gt '1' ]
  then
    ${gwesmpexec_mpmd} cmdfile
    exit=$?
  else
#    ./cmdfile.1
    ./cmdfile
    exit=$?
  fi

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '**************************************'
    echo '*** POE FAILURE DURING TAR PROCESS ***'
    echo '**************************************'
    echo '     See Details Below '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
  fi


# --------------------------------------------------------------------------- #
# 10.  Check for errors

  set +x
  echo ' '
  echo '   Checking for errors (error output concatenated below).'
  [[ "$LOUD" = YES ]] && set -x

# 10.a Spectral tar file

  if [ "$spec_OK" = 'yes' ]
  then
    if [ -d TAR_spec_$wavemodID ]
    then
      set +x
      echo "      Error in $wavemodID spectral tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$wavemodID post $date $cycle : error in spectral tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $wavemodID spectral tar file."
    else
      rm -f ${wavemodID}_spec_tar.out
      set +x
      echo "      $wavemodID Spectral tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi
  fi

# 10.b Bulletin tar files

  if [ "$bull_OK" = 'yes' ]
  then
    if [ -d TAR_bull_$wavemodID ]
    then
      set +x
      echo "      Error in $wavemodID bulletin tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$wavemodID post $date $cycle : error in bulletin tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $wavemodID bulletin tar file."
    else
      rm -f ${wavemodID}_bull_tar.out
      set +x
      echo "      $wavemodID Bulletin tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi

    if [ -d TAR_cbull_$wavemodID ]
    then
      set +x
      echo "      Error in $wavemodID compressed bulletin tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$wavemodID post $date $cycle : error in compressed bulletin tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $wavemodID compressed bulletin tar file."
    else
      rm -f ${wavemodID}_cbull_tar.out
      set +x
      echo "      $wavemodID compressed bulletin tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi

    if [ -d TAR_csbull_$wavemodID ]
    then
      set +x
      echo "      Error in $wavemodID csv bulletin tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$wavemodID post $date $cycle : error in csv bulletin tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $wavemodID csv bulletin tar file."
    else
      rm -f ${wavemodID}_csbull_tar.out
      set +x
      echo "      $wavemodID csv bulletin tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi
  fi

# 10.c Old Spectral tar file

  if [ "$Ospec_OK" = 'yes' ]
  then
    for grdID in $Ogrids
    do
      if [ -d TAR_spec_$grdID ]
      then
        set +x
        echo "      Error in $grdID spectral tar file."
        [[ "$LOUD" = YES ]] && set -x
        echo "$grdID post $date $cycle : error in spectral tar." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR in $grdID spectral tar file."
      else
        rm -f ${grdID}_spec_tar.out
        set +x
        echo "      $grdID Spectral tar file OK."
        [[ "$LOUD" = YES ]] && set -x
      fi
    done
  fi

# 10.d Old Bulletin tar file

  if [ "$Obull_OK" = 'yes' ]
  then
    for grdID in $Ogrids
    do
      if [ -d TAR_bull_$grdID ]
      then
        set +x
        echo "      Error in $grdID bulletin tar file."
        [[ "$LOUD" = YES ]] && set -x
        echo "$grdID post $date $cycle : error in bulletin tar." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR in $grdID bulletin tar file."
      else
        rm -f ${grdID}_bull_tar.out
        set +x
        echo "      $grdID bulletin tar file OK."
        [[ "$LOUD" = YES ]] && set -x
      fi

      if [ -d TAR_cbull_$grdID ]
      then
        set +x
        echo "      Error in $grdID compressed bulletin tar file."
        [[ "$LOUD" = YES ]] && set -x
        echo "$grdID post $date $cycle : error in compressed bulletin tar." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR in $grdID compressed bulletin tar file."
      else
        rm -f ${grdID}_cbull_tar.out
        set +x
        echo "      $grdID compressed bulletin tar file OK."
        [[ "$LOUD" = YES ]] && set -x
      fi

      if [ -d TAR_csbull_$grdID ]
      then
        set +x
        echo "      Error in $grdID csv bulletin tar file."
        [[ "$LOUD" = YES ]] && set -x
        echo "$grdID post $date $cycle : error in csv bulletin tar." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR in $grdID csv bulletin tar file."
      else
        rm -f ${grdID}_csbull_tar.out
        set +x
        echo "      $grdID csv bulletin tar file OK."
        [[ "$LOUD" = YES ]] && set -x
      fi

    done
  fi

# 10.e Error outputs

  if [ -f *.out ]
  then
    set +x
    echo ' '
    echo '*********************'
    echo '*** ERROR OUTPUTS ***'
    echo '*********************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    exit_code=31
    for file in *.out
    do
      echo ' '
      sed "s/^/$file : /g" $file
    done
    rm -f *.out
  fi

# --------------------------------------------------------------------------- #
# 11.  CLean up and rename old grid files

  set +x
  rm -f *.tmpl
  for ID in $wavemodID $Ogrids
  do
    rm -f $ID.*.spec
    rm -f $ID.*.bull
    rm -f $ID.*.cbull
    rm -f $ID.*.csbull
  done
  [[ "$LOUD" = YES ]] && set -x

  if [ "$grint_OK" = 'yes' ]
  then
    for ID in $Ogrids
    do
#      mv $COMOUT/$wavemodID.$ID.$cycle.grib       $COMOUT/$ID.$cycle.grib   # comment out per RFC 1576 (imp May 2010)
      mv $COMOUT/$wavemodID.$ID.$cycle.outgrd     $COMOUT/$ID.$cycle.outgrd
    done
  fi

# --------------------------------------------------------------------------- #
# 12.  Ending output

  set +x
  echo ' '
  echo "Ending at : `date`"
  echo '-----------'
  echo ' '
  echo '                     *** End of MWW3 postprocessor ***'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$exit_code" -ne '0' ]
  then
     msg="ABNORMAL EXIT: Problem in MWW3 POST"
     postmsg "$jlogfile" "$msg"
     echo $msg
     err=$exit_code ; export err ; err_chk
  else
     touch $COMOUT/$wavemodID.$cycle.postdone
  fi

  msg="$job completed normally"
  postmsg "$jlogfile" "$msg"

# End of MWW3 prostprocessor script ---------------------------------------- #

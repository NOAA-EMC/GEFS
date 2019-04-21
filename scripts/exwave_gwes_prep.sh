#!/bin/bash
###############################################################################
#                                                                             #
# This script is the preprocessor for the global multi_grid wave model. It    #
# sets some shell script variables for export to child scripts and copies     #
# some generally used files to the work directory. After this the actual      #
# preprocessing is performed by the following child scripts :                 #
#                                                                             #
#  ww3_ice.sh     : preprocess ice fields.                               #
#  ww3_rtofs     : preprocess rtofs current fields.                     #
#  ww3_g2ges.sh  : find and copy wind grib2 files.                      #
#                                                                             #
# Also used is the utililty script                                            #
#                                                                             #
#  ww3_start.sh   : get initial time of most recent restart file(s)      #
#  ww3_mod_def.sh : creates the mod_def file for the grid                #
#                                                                             #
# Remarks :                                                                   #
# - For non-fatal errors output is witten to the wave.log file.               #
#                                                                             #
#  Update record :                                                            #
#                                                                             #
# - Origination:                                               01-Mar-2007    #
# - Added NCO note on resources on mist/dew.                   05-Mar-2007    #
# - Renmaing mod_def files in $FIX_wave.                       26-Apr-2007    #
# - Migrating to a vertical structure                          28-Mar-2011    #
# - Transitioning to WCOSS                                     30-Nov-2012    #
# - Adding wave-current interactions, new two polar grids, 1/4 degree res     #
#                                                              10-Jan-2018    #
# - Transitioning to GEFS workflow                              2-Apr-2019    #
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
  mkdir outtmp

  msg="HAS BEGUN on `hostname`"
  postmsg "$jlogfile" "$msg"
  msg="Starting MWW3 PREPROCESSOR SCRIPT for $modID"
  postmsg "$jlogfile" "$msg"

  set +x
  echo ' '
  echo '                      ********************************'
  echo '                      *** MWW3 PREPROCESSOR SCRIPT ***'
  echo '                      ********************************'
  echo '                          Global multi-grid model with GFS forcing'
  echo "                          Model identifier : $modID"
  echo ' '
  echo "Starting at : `date`"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

#  export MP_PGMMODEL=mpmd
#  export MP_CMDFILE=./cmdfile

  if [ "$INDRUN" = 'no' ]
  then
    lsth=${lsth:-3}
  else
    lsth=${lsth:-180}
  fi

# 0.b Date and time stuff
#     The ending time of the run always is the $lsth hour forecast. The starting
#     time depends on availablility of restart files, and is obtained with
#     ww3_start.sh
#
#     Make sure nback is set identically in the forecast script !!!
#     nback is the number of cycles to look back.

  export date=$PDY
  export YMDH=${PDY}${cyc}

#  export nback=${nback:-16}

#  $USHwave/ww3_start.sh

  ymdh_beg=`head ww3_start.out | awk '{ print $1 }'`
#  rm -f ww3_start.out
  time_beg="`echo $ymdh_beg | cut -c1-8` `echo $ymdh_beg | cut -c9-10`0000"

  ymdh_end=`$NDATE $lsth $YMDH`
  time_end="`echo $ymdh_end | cut -c1-8` `echo $ymdh_end | cut -c9-10`0000"

  set +x
  echo ' '
  echo 'Times in wave model format :'
  echo '----------------------------'
  echo "   date / cycle  : $date $cycle"
  echo "   starting time : $time_beg"
  echo "   ending time   : $time_end"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# 0.c Command file set-up
#     The command file points to $nfile files named cmdfile.$ifile.
#     The actual work is distributed over these files.

  ifile=1
  nfile=`echo $LSB_HOSTS | wc -w | awk '{ print $1}'`
  iskip=1

  if [ "$nfile" -gt '1' ]
  then
    cmdtype='mpirun.lsf'
  else
    cmdtype='bash'
    nskip='-'
  fi

  set +x
  echo ' '
  echo '   Making command file(s)'
  echo "   Set up command file structure (type = $cmdtype)."
  echo "      Number of command files  : $nfile"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most child scripts

  set +x
  echo 'Preparing input files :'
  echo '-----------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# 1.a Model definition files

  nmoddef=0

  ifile=1
  rm -f cmdfile
  touch cmdfile

  grdINP=''
  if [ "${WW3ATMINP}" = 'YES' ]; then grdINP="${grdINP} $wndID" ; fi 
  if [ "${WW3ICEINP}" = 'YES' ]; then grdINP="${grdINP} $iceID" ; fi 
  if [ "${WW3CURINP}" = 'YES' ]; then grdINP="${grdINP} $curID" ; fi 

  ifile=1

  for grdID in $grdINP $grids
  do
    if [ -f "$FIXwave/ww3_${modID}_${grdID}.moddef.${wave_sys_ver}" ]
    then
      set +x
      echo " Mod def file for $grdID found in $FIXwave. copying ...."
      [[ "$LOUD" = YES ]] && set -x
      cp $FIXwave/ww3_${modID}_${grdID}.moddef.${wave_sys_ver} mod_def.$grdID

    else
      msg="FATAL ERROR: NO INP FILE FOR MODEL DEFINITION FILE"
      postmsg "$jlogfile" "$msg"
      set +x
      echo ' '
      echo '*********************************************************** '
      echo '*** FATAL ERROR : NOT FOUND WAVE  MODEL DEFINITION FILE *** '
      echo '*********************************************************** '
      echo "                                grdID = $grdID"
      echo ' '
      echo $msg
      [[ "$LOUD" = YES ]] && set -x
      echo "$modID prep $date $cycle : ww3_${modID}_${grdID}.moddef.${wave_sys_ver} missing." >> $wavelog
      err=1;export err;err_chk
    fi
  done

# 1.a.3 File check
  for grdID in $grdINP $grids
  do
    if [ -f mod_def.$grdID ]
    then
      set +x
      echo ' '
      echo " mod_def.$grdID succesfully created/copied "
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
    else 
      msg="ABNORMAL EXIT: NO MODEL DEFINITION FILE"
      postmsg "$jlogfile" "$msg"
      set +x
      echo ' '
      echo '********************************************** '
      echo '*** FATAL ERROR : NO MODEL DEFINITION FILE *** '
      echo '********************************************** '
      echo "                                grdID = $grdID"
      echo ' '
      echo $msg
      sed "s/^/$grdID.out : /g"  $grdID.out
      [[ "$LOUD" = YES ]] && set -x
      echo "$modID prep $date $cycle : mod_def.$grdID missing." >> $wavelog
      err=2;export err;err_chk
    fi
  done

# 1.b Preprocessor template files

# 1.c Netcdf Preprocessor template files

   for grdID in $grdINP
   do

     case $grdID in
     $currID ) 
              type='curr' 
     ;;
     $wndID )
              type='wind'
     ;;
     $iceID )
              type='ice'
     ;;
     * )
              echo 'Input type not yet implelemted' 	    
              err_chk
              ;;
     esac 

     echo 'Working here,TYPE is: '$type


     if [ -f $FIXwave/ww3_prnc.${type}.$grdID.inp.tmpl ]
     then
       cp $FIXwave/ww3_prnc.${type}.$grdID.inp.tmpl .
     fi

     if [ -f ww3_prnc.${type}.$grdID.inp.tmpl ]
     then
       set +x
       echo ' '
       echo "   ww3_prnc.${type}.$grdID.inp.tmpl copied ($FIXwave)."
       echo ' '
       [[ "$LOUD" = YES ]] && set -x
     else
       msg="ABNORMAL EXIT: NO FILE $file"
       ./postmsg "$jlogfile" "$msg"
       set +x
       echo ' '
       echo '************************************** '
       echo '*** FATAL ERROR : NO TEMPLATE FILE *** '
       echo '************************************** '
       echo "             ww3_prnc.${type}.$grdID.inp.tmpl"
       echo ' '
       echo $msg
       echo ' '
       [[ "$LOUD" = YES ]] && set -x
       echo "$modID prep $date $cycle : ww3_prnc.${type}.$grdID.tmpl missing." >> $wavelog
       err=2;export err;./err_chk
     fi
   done


exit


# 1.d Data assimilation buoy file
#     *** NOT YET PORTED TO NEW SYSTEM ***


# --------------------------------------------------------------------------- #
# 2.  Ice , and wind data files

# 2.a Ice pre - processing 

  $USHwave/ww3_ice.sh > ice.out 
  err=$?

  if [ -d ice ]
  then
    postmsg "$jlogfile" "NON-FATAL ERROR in ice field (will be trapped by fcst)."
    set +x
    echo ' '
    echo '      Error in ice field (not fatal, will be trapped by fcst).'
    echo ' '
    sed "s/^/ice.out : /g" ice.out
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
  else
    mv -f ice.out $DATA/outtmp
    rm -f ww3_prep.$iceID.tmpl mod_def.$iceID
    set +x
    echo ' '
    echo '      Ice field unpacking successful.'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
  fi

# 2.b SST pre-processing

# SST pre-processing removed as wave model no longer uses it

# 2.c Data assimilation files ( Not yet ported )

# 2.d grib2 files

# 2.d.i Make command file(s) 

  ifile=1
  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile
#  rm -f cmdfile.*
 
#  set +x
#  while [ "$ifile" -le "$nfile" ]
#  do
#    touch cmdfile.$ifile
#    chmod 700 cmdfile.$ifile
#    echo "./cmdfile.$ifile" >> cmdfile
#    ifile=`expr $ifile + 1`
#  done
#  [[ "$LOUD" = YES ]] && set -x

  ymdh=$ymdh_beg

  while [ "$ymdh" -le "$ymdh_end" ]
  do
    echo "$USHwave/ww3_g2ges.sh $ymdh > grb_$ymdh.out 2>&1" >> cmdfile
    echo "$USHwave/ww3_rtofs.sh $ymdh > rtofs_$ymdh.out 2>&1" >> cmdfile

    ymdh=`$NDATE $HOUR_INC $ymdh`

#    ifile=`expr $ifile + 1`
#    if [ "$ifile" -gt "$nfile" ]
#    then
#      ifile=1
#    fi

  done

# 2.d.ii   Execute command file

  set +x
  echo ' '
  echo "   Executing command file."
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$nfile" -gt '1' ]
  then
    mpirun.lsf cfp cmdfile
    exit=$?
  else
#    cmdfile.1
    ./cmdfile
    exit=$?
  fi

# 2.d.iii  Check for errors

  set +x
  echo ' '
  echo '   Checking for errors.'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

#     We will go on if the number of errors in files is less
#     than err_max

  [[ "$LOUD" = YES ]] && set -x
  err_max=1


  ymdh=$ymdh_beg
  nr_err=0

  set +x
  echo '      Sources of grib2 files :'
  [[ "$LOUD" = YES ]] && set -x
  while [ "$ymdh" -le "$ymdh_end" ]
  do
    if [ -d grb_${ymdh} ]
    then
      set +x
      echo ' '
      echo "         File for $ymdh : error in ww3_g2ges.sh"
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      postmsg "$jlogfile" "    File for $ymdh : error in ww3_g2ges.sh"
      nr_err=`expr $nr_err + 1`
      rm -f gwnd.$ymdh
    else
      grbfile=`grep 'File for' grb_${ymdh}.out`
      if [ -z "$grbfile" ]
      then
        set +x
        echo ' '
        echo "         File for $ymdh : cannot identify source"
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        nr_err=`expr $nr_err + 1`
        rm -f gwnd.$ymdh
      else
        if [ ! -f gwnd.$ymdh ]
        then
          set +x
          echo ' '
          echo "         File for $ymdh : file not found"
          echo ' '
          [[ "$LOUD" = YES ]] && set -x
          nr_err=`expr $nr_err + 1`
        else
          set +x
          echo ' '
          echo "      $grbfile"
          echo ' '
          [[ "$LOUD" = YES ]] && set -x
          mv -f grb_${ymdh}.out $DATA/outtmp
        fi
      fi
    fi
    ymdh=`$NDATE $HOUR_INC $ymdh`
  done

  if [ -f grb_*.out ]
  then
    set +x
    echo ' '
    echo '*******************************'
    echo '*** ERROR OUTPUT ww3_g2ges.sh ***'
    echo '*******************************'
    echo '            Possibly in multiple calls'
    [[ "$LOUD" = YES ]] && set -x
    echo "$modID prep $date $cycle : error in grib2 files." >> $wavelog
    set +x
    for file in grb_*.out
    do
      echo ' '
      sed "s/^/$file : /g" $file
    done
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    mv -f grb_*.out $DATA/outtmp
    postmsg "$jlogfile" "NON-FATAL ERROR in ww3_g2ges.sh, possibly in multiple calls."
  fi

  if [ "$nr_err" -gt "$err_max" ]
  then
    msg="ABNORMAL EXIT: ERROR(S) IN SIGMA FILES"
    postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '********************************************* '
    echo '*** FATAL ERROR : ERROR(S) IN SIGMA FILES *** '
    echo '********************************************* '
    echo ' '
    echo $msg
    [[ "$LOUD" = YES ]] && set -x
    echo "$modID prep $date $cycle : fatal error in grib2 files." >> $wavelog
    err=5;export err;err_chk
  fi

  rm -f cmdfile

# --------------------------------------------------------------------------- #
# 3.  Process extracted wind fields
# 3.a Get into single file 

  set +x
  echo ' '
  echo '   Concatenate extracted wind fields ...'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  files=`ls gwnd.* 2> /dev/null`

  if [ -z "$files" ]
  then
    msg="ABNORMAL EXIT: NO gwnd.* FILES FOUND"
    postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '******************************************** '
    echo '*** FATAL ERROR : CANNOT FIND WIND FILES *** '
    echo '******************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$modID prep $date $cycle : no wind files found." >> $wavelog
    err=6;export err;err_chk
  fi

  rm -f gfs.wind

  for file in $files
  do
    cat $file >> gfs.wind
    rm -f $file
  done

# 3.b Run waveprep

# Convert gfs wind to netcdf
  $WGRIB2 gfs.wind -netcdf gfs.nc

  for grdID in $wndID $curvID
  do

    set +x
    echo ' '
    echo "   Running wind fields through preprocessor for grid $grdID"
    echo ' '
    [[ "$LOUD" = YES ]] && set -x

    sed -e "s/HDRFL/T/g" ww3_prnc.wind.$grdID.tmpl > ww3_prnc.inp
    ln -sf mod_def.$grdID mod_def.ww3

    set +x
    echo "Executing $EXECcode/ww3_prnc"
    [[ "$LOUD" = YES ]] && set -x

    $EXECcode/ww3_prnc > prnc.out
    err=$?

    if [ "$err" != '0' ]
    then
      msg="ABNORMAL EXIT: ERROR IN waveprnc"
      postmsg "$jlogfile" "$msg"
      set +x
      echo ' '
      echo '*************************************** '
      echo '*** FATAL ERROR : ERROR IN waveprnc *** '
      echo '*************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$modID prep $grdID $date $cycle : error in waveprnc." >> $wavelog
      err=7;export err;err_chk
    fi

    if [ ! -f wind.ww3 ]
    then
      msg="ABNORMAL EXIT: FILE wind.ww3 MISSING"
      postmsg "$jlogfile" "$msg"
      set +x
      echo ' '
      cat waveprep.out
      echo ' '
      echo '****************************************'
      echo '*** FATAL ERROR : wind.ww3 NOT FOUND ***'
      echo '****************************************'
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$modID prep $grdID $date $cycle : wind.ww3 missing." >> $wavelog
      err=8;export err;err_chk
    fi

    rm -f mod_def.ww3
    rm -f ww3_prep.inp

    mv wind.ww3 wind.$grdID
    mv times.WND times.$grdID

# 3.c Check to make sure wind files are properly incremented

    first_pass='yes'
    windOK='yes'
    while read line
    do
      date1=`echo $line | cut -d ' ' -f 1`
      date2=`echo $line | cut -d ' ' -f 2`
      ymdh="$date1`echo $date2 | cut -c1-2`"
      if [ "$first_pass" = 'no' ]
      then
        hr_inc=`$NHOUR $ymdh $ymdh_prev`
        if [ "${hr_inc}" -gt "${HOUR_INC}" ]
        then
          set +x
          echo "Incorrect wind forcing increment at $ymdh" 
          [[ "$LOUD" = YES ]] && set -x
          windOK='no'
        fi
      fi
      ymdh_prev=$ymdh
      first_pass='no'
    done < times.$grdID

    if [ "$windOK" = 'no' ]
    then
      set +x
      echo ' '
      echo '************************************************'
      echo '*** ERROR : WIND DATA INCREMENT INCORRECT !! ***'
      echo '************************************************'
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$modID prep $grdID $date $cycle : error in wind increment." >> $wavelog
      err=9;export err;err_chk
    fi

  done

  rm -f gfs.wind
  rm -f mod_def.ww3
  rm -f ww3_prnc.inp

#####################################################################
# 4.  Process current fields
# 4.a Get into single file 

  set +x
  echo ' '
  echo '   Concatenate binary current fields ...'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  files=`ls rtofs.* 2> /dev/null`

  if [ -z "$files" ]
  then
    msg="ABNORMAL EXIT: NO rtofs.* FILES FOUND"
    postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '******************************************** '
    echo '*** FATAL ERROR : CANNOT FIND WIND FILES *** '
    echo '******************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$modID prep $date $cycle : no current files found." >> $wavelog
    err=10;export err;err_chk
  fi

  rm -f curr.${curID}

  for file in $files
  do
    cat $file >> curr.${curID}
    rm -f $file
  done

# --------------------------------------------------------------------------- #
# 5.  Output to /com

  if [ "$SENDCOM" = 'YES' ]
  then
    for grdID in $wndID $curvID 
    do
      set +x
      echo ' '
      echo "   Saving wind.$grdID as $COMOUT/${modID}.$grdID.$cycle.wind"
      echo "   Saving times.$grdID file as $COMOUT/${modID}.times.$cycle.$grdID"
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      cp wind.$grdID $COMOUT/${modID}.$grdID.$cycle.wind
      cp times.$grdID $COMOUT/${modID}.times.$cycle.$grdID
    done
    for grdID in $curID
    do
      set +x
      echo ' '
      echo "   Saving current.$grdID as $COMOUT/${modID}.$grdID.$cycle.curr"
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      cp curr.$grdID $COMOUT/${modID}.$grdID.$cycle.curr
    done
  fi 

  rm -f wind.*
  rm -f $iceID.*
  rm -f times.*

# --------------------------------------------------------------------------- #
# 5.  Ending output

  set +x
  echo ' '
  echo "Ending at : `date`"
  echo ' '
  echo '                     *** End of MWW3 preprocessor ***'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  msg="$job completed normally"
  postmsg "$jlogfile" "$msg"

# End of MWW3 preprocessor script ------------------------------------------- #

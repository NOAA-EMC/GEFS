#!/bin/sh
###############################################################################
#                                                                             #
# This script is the preprocessor for the global wave ensemble system. It     #
# sets some shell script variables for export to child scripts and copies     #
# some generally used files to the work directory.                            # 
# Grids are generated at the first cycle when version-dependent moddef files  #
# are not found in COM by the script:                                         #
#                                                                             #
# wavemod_def.sh                                                         #
#                                                                             #
# After this the actual preprocessing is performed by the following           #
# child scripts :                                                             #
#                                                                             #
#  wave_gwes_ice.sh    :    generate ice files.                               #
#  wave_gwes_wind.sh   :    generate wind files.                              #
#                                                                             #
# Remarks :                                                                   #
# - For non-fatal errors output is witten to the wave.log) file.              #
#                                                                             #
#  Update record :                                                            #
#                                                                             #
# - Origination:                                               01-Mar-2003    #
# - Migrating to a vertical structure                          28-Mar-2012    #
# - Modified for machine transition from CCS to WCOSS          01-Nov-2012    #
# - Modified for running under new system name GWES            10-Oct-2013    #
# - Unified all members prep steps in single mpiserial driver  01-Jan-2014    #
# - Completed checking of all procedures for FY14Q2 upgrade    26-Jan-2014    #
#                                                                             #
###############################################################################
#
# --------------------------------------------------------------------------- #
#  0. Preparations
#
#  0.a. Basic modes of operation
#
  set -x
  # Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" = YES ]] && set -x

  cd $DATA

  msg="HAS BEGUN on `hostname`"
  ./postmsg "$jlogfile" "$msg"
  msg="Starting MWW3 PREPROCESSOR SCRIPT for $modID"
  ./postmsg "$jlogfile" "$msg"

  set +x
  echo ' '
  echo '                      ********************************'
  echo '                      *** NWW3ens PREPROCESSOR SCRIPT ***'
  echo '                      ********************************'
  echo ' '
  echo "Starting at : `date`"
  [[ "$LOUD" = YES ]] && set -x
#
# 0.b Define all grids used

  export buoy="points"
  #export grids='bot_30m mid_30m top_30m' # Three tier, keep for future-proofing
  export grids='glo_30m'
  export iceID=icean_5m
  export wndID=gfs_30m 
  export sstID=rtg_05m
  export IDIML=720
  export JDIML=361
  export JCLIP=0

#
# 0.c. Date and time stuff:
#
#   The ending time of the run is always the $lsth hour forecast. 
#   The starting time is given as in $PDY$cycle.
#   ** THIS PARAMATER MUST BE IDENTICAL IN PREP, FORECAST AND POST!! **
#
  export lsth=${lsth:-240}

# 0.b Date and time stuff
#     The ending time of the run always is the $lsth hour forecast. The starting
#     time depends on availablility of restart files, and is obtained with
#     wavestart_gwes.sh
#
#     Make sure nback is set identically in the forecast script !!!
#     nback is the number of cycles to look back.

  export date=$PDY
  export YMDH=${PDY}${cyc}

  export nback=${nback:-5}

  export modIE=gwes00 # Check times in control member for wavestart_gwes

# stp_hour for wave ensembles is set as argument to wavestart
  export stp_hour=24 # Parameter interval between restart files for wavestart_gwes.sh

  $USHwave/wavestart_gwes.sh

# time_beg and _end need to be exported so wave_gwes_wind.sh works
  ymdh_beg=`sed '1!d' wavestart_gwes.out | awk '{ print $1 }'`
  export time_beg="`echo $ymdh_beg | cut -c1-8` `echo $ymdh_beg | cut -c9-10`0000"

  ymdh_end=`$utilexec/ndate $lsth $YMDH`
  export time_end="`echo $ymdh_end | cut -c1-8` `echo $ymdh_end | cut -c9-10`0000"

# Hindcast extent (may vary from -24 in case restart files are older)
  export hcst_hour=`sed '2!d' wavestart_gwes.out | awk '{ print $1 }'`
  rm -f wavestart_gwes.out

  set +x
  echo ' '
  echo 'Times in wave model format :'
  echo '----------------------------'
  echo "   date / cycle  : $date $cyc"
  echo "   starting time : $time_beg"
  echo "   ending time   : $time_end"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# 0.d Serial or parallel processing parameter
  nfile=`echo $LSB_HOSTS | wc -w | awk '{ print $1}'`

#
# --------------------------------------------------------------------------- #
# 1. Get files that are used by most child scripts
#
# 1.a Model definition files 
#
  set +x
  echo 'Preparing input files :'
  echo '-----------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  nmoddef=0

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

  for grdID in $iceID $wndID $buoy $grids
  do
 
   if [ -f "$COMOUT/wave_${modID}_${grdID}.moddef.${model_ver}" ]
   then
     set +x
     echo " Mod def file for $grdID found in $COMOUT. copying ...."
     [[ "$LOUD" = YES ]] && set -x
     cp $COMOUT/wave_${modID}_${grdID}.moddef.${model_ver} mod_def.$grdID

   else

     set +x
     echo " Mod def file for $grdID not found in $COMOUT. Setting up to generate ..."
     [[ "$LOUD" = YES ]] && set -x

   if [ -f ${FIXwave}/wave_${modID}_${grdID}.inp ]
    then
      cp ${FIXwave}/wave_${modID}_${grdID}.inp ${grdID}.inp
    fi

    if [ -f ${grdID}.inp ]
    then
     set +x
      echo "   ${grdID}.inp copied (${FIXwave}/wave_${modID}_${grdID}.inp)."
    else
      msg="ABNORMAL EXIT: NO INP FILE FOR MODEL DEFINITION FILE"
      ./postmsg "${jlogfile}" "${msg}"
      set +x
      echo ' '
      echo '*********************************************************** '
      echo '*** FATAL ERROR : NO INP FILE FOR MODEL DEFINITION FILE *** '
      echo '*********************************************************** '
      echo "                                grdID = ${grdID}"
      echo ' '
      echo ${msg}
      echo "${modID} prep ${date} ${cycle} : ${grdID}.inp missing." >> ${wavelog}
      [[ "$LOUD" = YES ]] && set -x
      err=1;export err;./err_chk
    fi

  set +x
  echo "   Generating mod_def file for ${grdID} ... "
  [[ "$LOUD" = YES ]] && set -x

    echo "${USHwave}/wavemod_def.sh ${grdID} > ${grdID}.out 2>&1" >> cmdfile

      nmoddef=`expr $nmoddef + 1`

    fi
  done

# 1.a.1 Execute poe (if needed)

  if [ "$nmoddef" -gt '0' ]
  then
    set +x
    echo ' '
    echo " Generating $nmoddef mod def files"
    echo ' '
    [[ "$LOUD" = YES ]] && set -x

    if [ "$nfile" -gt '1' ]
    then
      mpirun.lsf cfp cmdfile
      exit=$?
    else
      ls -l 
      #./cmdfile.1
      ./cmdfile
      exit=$?
    fi
  fi

# 1.a.3 File check

  for grdID in $iceID $wndID
  do
    if [ -f mod_def.${grdID} ]
    then
      set +x
      echo " mod_def.${grdID} created in good shape "
      [[ "$LOUD" = YES ]] && set -x
    else
      msg="ABNORMAL EXIT: NO MODEL DEFINITION FILE"
      ./postmsg "${jlogfile}" "${msg}"
      set +x
      echo ' '
      echo '********************************************** '
      echo '*** FATAL ERROR : NO MODEL DEFINITION FILE *** '
      echo '********************************************** '
      echo "                                grdID = ${grdID}"
      echo ' '
      echo ${msg}
      echo "${modID} prep ${date} ${cycle} : mod_def.${grdID} missing." >> ${wavelog}
      [[ "$LOUD" = YES ]] && set -x
      sed "s/^/${grdID}.out : /g"  ${grdID}.out
      err=2;export err;./err_chk
    fi
  done
#
# 1.a.4 Copy mod_def to fix 
#
 for grdID in $iceID $wndID $buoy $grids
 do
  if [ ! -f "$COMOUT/wave_${modID}_${grdID}.moddef.${model_ver}" ]
  then
    cp mod_def.${grdID} $COMOUT/wave_${modID}_${grdID}.moddef.${model_ver}
    set +x
    echo " Mod def file for $grdID copied to $COMOUT. copying ...."
    [[ "$LOUD" = YES ]] && set -x
  fi
 done

# 1.b Wind preprocessor template file
#
  for grdID in $iceID $wndID
  do
  
    if [ -f $FIXwave/waveprep.$grdID.tmpl ]
    then
       cp $FIXwave/waveprep.$grdID.tmpl .
    fi  

    if [ -f waveprep.$grdID.tmpl ]
    then
      set +x
      echo "   waveprep.$grdID.tmpl copied ($FIXwave)."
      [[ "$LOUD" = YES ]] && set -x
    else
      msg="ABNORMAL EXIT: NO FILE $file"
    fi

  done
#
# --------------------------------------------------------------------------- #
# 2. Generate input files containing forcing fields
#
# 2.a Generate ice file (single file for all members) 
#
    $USHwave/wave_gwes_ice.sh

    if [ ! -f ${COMOUT}/${modID}.${iceID}.t${cyc}z.ice ]
    then
      msg="ABNORMAL EXIT: ERR in generating ice.ww3 file"
      ./postmsg "$jlogfile" "$msg"
      set +x
      echo ' '
      echo '********************************************* '
      echo "*** FATAL ERROR : No ${modID}.${iceID}.t${cyc}z.ice file *** "
      echo '********************************************* '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "No ice.ww3 " >> $wavelog
      err=3;export err;./err_chk
    fi

# 2.b Generate wind files for all members (mpiserial)
#
# 2.b.1 Preprocess command files
#
  nmodie=1
  
  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

# 2.b.2 Wind generation scripts 

  imodie=1

  while [ ${imodie} -le ${nmgwes} ]
  do

    memb=`expr ${imodie} - 1`
    if [ ${memb} -lt 10 ]; then memb="0${memb}"; fi
    modIE="gwes${memb}"

    echo "$USHwave/wave_gwes_wind.sh ${memb} 1> gwes${memb}_wind.out 2>&1" \
          >> cmdfile

    nmodie=`expr ${nmodie} + 1`
    imodie=`expr ${imodie} + 1`

  done

# 2.b.3 Execute poe (if needed)

  if [ "$nmodie" -gt '0' ]
  then

  set +x
  echo ' '
  echo " Generating $nmgwes wind files"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x
   if [ "$nfile" -gt '1' ]
   then
     mpirun.lsf cfp cmdfile
     exit=$?
   else
     ls -l
     ./cmdfile
     exit=$?
   fi
 fi

# 2.b.5 Check for errors

  imodie=1

  while [ ${imodie} -le ${nmgwes} ]
  do

   memb=`expr ${imodie} - 1`
   if [ ${memb} -lt 10 ]; then memb="0${memb}"; fi
   modIE="gwes${memb}"

# Full output of $USHwave/wave_gwes_wind.sh
   cat gwes${memb}_wind.out 

   if [ ! -f wind_${memb}/wind.ww3 ]
   then
     msg="ABNORMAL EXIT: ERR in generating wind.ww3 file"
     ./postmsg "$jlogfile" "$msg"
     set +x
     echo ' '
     echo '********************************************* '
     echo "***              FATAL ERROR              *** "
     echo "***     No wind_${memb}/wind.ww3 file     *** "
     echo '********************************************* '
     echo ' '
     [[ "$LOUD" = YES ]] && set -x
     echo "No ${DATA}/wind_${memb}/wind.ww3 " >> $wavelog
     err=5;export err;./err_chk
   else
     set +x
     echo -e "\n Wind file for member ${memb} generated OK"
     [[ "$LOUD" = YES ]] && set -x
     echo -e "${DATA}/wind_${memb}/wind.ww3"
   fi

    imodie=`expr ${imodie} + 1`

  done

# --------------------------------------------------------------------------- #
# 3. Output to /com
#
  if [ "$SENDCOM" = 'YES' ]
  then

  imodie=1

    while [ ${imodie} -le ${nmgwes} ]
    do

      memb=`expr ${imodie} - 1`
 
      if [ ${memb} -lt 10 ]; then memb="0${memb}"; fi
      modIE="gwes${memb}"
  
      echo -e "\n   Saving wind_${memb}/wind.ww3 as $COMOUT/$modIE.${wndID}.$cycle.wind\n"
      echo "   Saving times file as $COMOUT/${modIE}.${wndID}.$cycle.times"
      cp wind_${memb}/wind.ww3 $COMOUT/${modIE}.${wndID}.$cycle.wind
      cp wind_${memb}/times.WNS $COMOUT/${modIE}.${wndID}.$cycle.times
#
# 3.a Alert the wind file
#
      if [ $SENDDBN = YES ] ; then
        $DBNROOT/bin/dbn_alert MODEL WAVE_BINARY $job $COMOUT/${modIE}.${wndID}.$cycle.wind
      fi

      imodie=`expr ${imodie} + 1`

    done

  fi
#
  chmod 664 $COMOUT/$modIE.*.$cycle.*
#
# --------------------------------------------------------------------------- #
# 4. Ending output
#
  set +x
  echo ' '
  echo "Ending at : `date`"
  echo ' '
  echo "                     *** End of $modIE preprocessor ***"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x
#
  msg="$job completed normally"
  ./postmsg "$jlogfile" "$msg"
#
# End of gwes preprocessor script.
#


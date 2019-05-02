#!/bin/bash
###############################################################################
#                                                                             #
# This script is the init config  for the global multi_grid wave model. It    #
# created model definition files with all configurations of spatial and       #
# spectral grids, as well as set general physics parameters and time steps.   #
#                                                                             #
# The main script for generating mod_def files is                             #
#                                                                             #
#  wave_moddef.sh : creates the mod_def file for the grid                #
#                                                                             #
# Remarks :                                                                   #
# - For non-fatal errors output is witten to the wave.log file.               #
#                                                                             #
#  Update record :                                                            #
#                                                                             #
# - Origination:                                               02-Apr-2019    #
# - Transitioning to GEFS workflow                             02-Apr-2019    #
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
  msg="Starting MWW3 INIT CONFIG SCRIPT for $wavemodID"
  postmsg "$jlogfile" "$msg"

  set +x
  echo ' '
  echo '                      ********************************'
  echo '                      *** MWW3 PREPROCESSOR SCRIPT ***'
  echo '                      ********************************'
  echo '                          Initial configuration script'
  echo "                          Model identifier : $wavemodID"
  echo ' '
  echo "Starting at : `date`"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

#     The actual work is distributed over these files.

  nfile=`echo $LSB_HOSTS | wc -w | awk '{ print $1}'`

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

  rm -f cmdfile
  touch cmdfile

  for grdID in $curID $iceID $wndID $buoy $grids $int_grids
  do
    if [ -f "$COMIN/${wavemodID}.mod_def.${grdID}" ]
    then
      set +x
      echo " Mod def file for $grdID found in $COMIN. copying ...."
      [[ "$LOUD" = YES ]] && set -x
      cp $COMIN/${wavemodID}.mod_def.${grdID} mod_def.$grdID

    else
      set +x
      echo " Mod def file for $grdID not found in $COMIN. Setting up to generate ..."
      [[ "$LOUD" = YES ]] && set -x
      if [ -f $FIXwave/ww3_$grdID.inp ]
      then
        cp $FIXwave/ww3_$grdID.inp $grdID.inp
      fi

      if [ -f $grdID.inp ]
      then
        set +x
        echo ' '
        echo "   $grdID.inp copied ($FIXwave/ww3_$grdID.inp)."
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
      else
        msg="ABNORMAL EXIT: NO INP FILE FOR MODEL DEFINITION FILE"
        postmsg "$jlogfile" "$msg"
        set +x
        echo ' '
        echo '*********************************************************** '
        echo '*** FATAL ERROR : NO INP FILE FOR MODEL DEFINITION FILE *** '
        echo '*********************************************************** '
        echo "                                grdID = $grdID"
        echo ' '
        echo $msg
        [[ "$LOUD" = YES ]] && set -x
        echo "$wavemodID init config $date $cycle : $grdID.inp missing." >> $wavelog
        err=1;export err;err_chk;exit
      fi

      echo "$USHwave/wave_moddef.sh $grdID > $grdID.out 2>&1" >> cmdfile

      nmoddef=`expr $nmoddef + 1`

    fi
  done

# 1.a.1 Execute parallel or serialpoe 

  if [ "$nmoddef" -gt '0' ]
  then

    set +x
    echo ' '
    echo " Generating $nmoddef mod def files"
    echo ' '
    [[ "$LOUD" = YES ]] && set -x

    #if [ "$nfile" -gt '1' ]
    #then
    #  mpirun.lsf cfp cmdfile
    #  exit=$?
    #else
      chmod 744 cmdfile; ./cmdfile
      exit=$?
    #fi

  fi 

# 1.a.3 File check

  for grdID in $curID $iceID $wndID $grids $int_grids
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
      echo "$wavemodID prep $date $cycle : mod_def.$grdID missing." >> $wavelog
      err=2;export err;err_chk;exit
    fi
  done

# --------------------------------------------------------------------------- #
# 2.  Ending 

  set +x
  echo ' '
  echo "Ending at : `date`"
  echo ' '
  echo '                     *** End of MWW3 Init Config ***'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  msg="$job completed normally"
  postmsg "$jlogfile" "$msg"

# End of MWW3 init config script ------------------------------------------- #

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

  msg="Starting MWW3 POSTPROCESSOR SCRIPT for $modID"
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

# 0.b Date and time stuff

  export date=$PDY
  export YMDH=${PDY}${cyc}

# 0.c Defining model grids

  buoy="points"

# 0.c.1 old and new grids

  export grids='glo_10m ant_9km aoc_9km wcat_4m' # oconus_6m hwipac_6m'

# 0.c.2 use second line when old global model (nww3) no longer functional
 
  export Ogrids=''

# 0.c.3 extended global grid and rtma transfer grid
  export Xgrids='glo_15mext'
# If there is need for any other xgrid not generated side by side
  export OXgrids='glo_15mext'

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
  echo "   old grids     : $Ogrids"
  echo "   output points : ${modID}_$buoy"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x


# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most child scripts

  field_OK='yes'
  point_OK='yes'
   grib_OK='no '
  grint_OK='yes'
  grib1_OK='no '
  grintx_OK='no '
  grintox_OK='no '
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

  ifile=1
#  nfile=`echo $LSB_HOSTS | wc -w | awk '{ print $1}'`
  nppn=`echo $LSB_MCPU_HOSTS | awk '{ print $2}'`
  nnod=`echo $LSB_MCPU_HOSTS | wc -w | awk '{ print $1}'`
  nnod=`expr ${nnod} / 2`
  nfile=`expr $nnod \* $nppn`
  echo "NFILE: " $nfile

#  nper=4
  
  if [ "$nfile" -gt '1' ]
  then
    cmdtype='poe'
  else
    cmdtype='sh'
    nskip='-'
    nper='-'
  fi

  set +x
  echo ' '
  echo "   Setting up first command file for copying model definition and data files."
  echo "   Set up command file structure (type = $cmdtype)."
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
      echo "   Copying $modID.$grdID.$cycle.outgrd from $COMIN to out_grd.$grdID"
      [[ "$LOUD" = YES ]] && set -x

      echo  "cp $COMIN/$modID.$grdID.$cycle.outgrd out_grd.$grdID"  >> cmdfile
    fi 

  done

  if [ ! -f out_pnt.ww3 ]
  then
    set +x
    echo "   Copying $COMIN/$modID.$cycle.outpnt to out_pnt.ww3"
    [[ "$LOUD" = YES ]] && set -x
    echo "cp $COMIN/$modID.$cycle.outpnt out_pnt.ww3" >> cmdfile
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
    mpirun.lsf cfp cmdfile
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
      echo "$modID post $grdID $date $cycle : field output missing." >> $wavelog
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
    echo "$modID post $date $cycle : point output missing." >> $wavelog
    postmsg "$jlogfile" "NON-FATAL ERROR NO RAW POINT OUTPUT FILE"
    exit_code=12
    point_OK='no'
    spec_OK='no'
    bull_OK='no'
    Ospec_OK='no'
    Obull_ok='no'
  fi

# 1.c Model definition files

  for grdID in $grids
  do
    if [ -f "$COMIN/ww3_${modID}_${grdID}.moddef.${wave_multi_1_ver}" ]
    then
      set +x
      echo " Mod def file for $grdID found in $COMIN. copying ...."
      [[ "$LOUD" = YES ]] && set -x

      cp $COMIN/ww3_${modID}_${grdID}.moddef.${wave_multi_1_ver} mod_def.$grdID

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
        echo "   $grdID.inp copied ($FIXwave/ww3_$grdID.inp)."
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
        echo "$modID post $date $cycle : $grdID.inp missing." >> $wavelog
        exit_code=2
      fi

      set +x
      echo "   Generating mod_def file for $grdID ... "
      [[ "$LOUD" = YES ]] && set -x

      $USHwave/ww3_mod_def.sh $grdID > $grdID.out

    fi

    if [ -f mod_def.$grdID ]
    then
      set +x
      echo " mod_def.$grdID created. Syncing to all nodes ... "
      [[ "$LOUD" = YES ]] && set -x
      $FSYNC mod_def.$grdID
      rm -f $grdID.out
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
      [[ "$LOUD" = YES ]] && set -x
      echo "$modID post $date $cycle : mod_def.$grdID missing." >> $wavelog
      sed "s/^/$grdID.out : /g"  $grdID.out
      rm -f $grdID.out
      exit_code=3
      grint_OK='no'
      grintx_OK='no'
      grintox_OK='no'
      grib_OK='no'
    fi
  done

# 1.c Model definition and interpolation template files for old grids

  if [ "$grint_OK" = 'yes' ]
  then

    for grdID in $Ogrids
    do
      if [ -f "$COMIN/ww3_${modID}_${grdID}.moddef.${wave_multi_1_ver}" ]
      then
        set +x
        echo " Mod def file for $grdID found in $COMIN. copying ...."
        [[ "$LOUD" = YES ]] && set -x
        cp $COMIN/ww3__${modID}_${grdID}.moddef.${wave_multi_1_ver} mod_def.$grdID
      else
        set +x
        echo " Mod def file for $grdID not found in $COMIN. Setting up to generate ..."
        [[ "$LOUD" = YES ]] && set -x

        if [ -f $FIXwave/ww3__$grdID.inp ]
        then
          cp $FIXwave/ww3__$grdID.inp $grdID.inp
        fi

        if [ -f $grdID.inp ]
        then
          set +x
          echo "   $grdID.inp copied ($FIXwave/ww3__$grdID.inp)."
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
          echo "$modID post $date $cycle : $grdID.inp missing." >> $wavelog
          exit_code=4
        fi

        set +x
        echo "   Generating mod_def file for $grdID ... "
        [[ "$LOUD" = YES ]] && set -x

        $USHwave/ww3_mod_def.sh $grdID > $grdID.out

      fi

      if [ -f mod_def.$grdID ]
      then
        set +x
        echo " mod_def.$grdID created. Syncing to all nodes ... "
        [[ "$LOUD" = YES ]] && set -x
        $FSYNC mod_def.$grdID
        rm -f $grdID.out
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
        [[ "$LOUD" = YES ]] && set -x
        echo "$modID post $date $cycle : mod_def.$grdID missing." >> $wavelog
        sed "s/^/$grdID.out : /g"  $grdID.out
        rm -f $grdID.out
        exit_code=5
        grint_OK='no'
      fi

      if [ ! -f ${grdID}_interp.inp.tmpl ]
      then      
        set +x
        echo "   Copying ${grdID}_interp.inp.tmpl from $FIXwave/${grdID}_interp.inp.tmpl"
        [[ "$LOUD" = YES ]] && set -x

        if [ -f $FIXwave/${grdID}_interp.inp.tmpl ]
        then
          cp $FIXwave/${grdID}_interp.inp.tmpl ${grdID}_interp.inp.tmpl
        fi
      fi

      if [ ! -f ${grdID}_interp.inp.tmpl ]
      then
        msg="ABNORMAL EXIT: NO GRID INTERPOLATION INPUT FILE"
        postmsg "$jlogfile" "$msg"
        set +x
        echo ' '
        echo '****************************************************** '
        echo '*** FATAL ERROR : NO GRID INTERPOLATION INPUT FILE *** '
        echo '****************************************************** '
        echo ' '
        echo $msg
        [[ "$LOUD" = YES ]] && set -x
        echo "$modID fcst $date $cycle : fixed file(s) missing." >> $wavelog
        exit_code=6
        grint_OK='no'
      else
        set +x
        echo "File ${grdID}_interp.inp.tmpl found. Syncing to all nodes ..."
        [[ "$LOUD" = YES ]] && set -x
        $FSYNC ${grdID}_interp.inp.tmpl
      fi
    done
  fi

# 1.d Model definition and Grid interpolation file for extended global grid

  if [ "$grintx_OK" = 'yes' ]
  then
    for grdID in $Xgrids
    do
      if [ -f "$COMIN/ww3__${modID}_${grdID}.moddef.${wave_multi_1_ver}" ]
      then
        set +x
        echo " Mod def file for $grdID found in $COMIN. copying ...."
        [[ "$LOUD" = YES ]] && set -x
        cp $COMIN/ww3__${modID}_${grdID}.moddef.${wave_multi_1_ver} mod_def.$grdID
      else
        set +x
        echo " Mod def file for $grdID not found in $COMIN. Setting up to generate ..."
        [[ "$LOUD" = YES ]] && set -x
        if [ -f $FIXwave/ww3__$grdID.inp ]
        then
          cp $FIXwave/ww3__$grdID.inp $grdID.inp
        fi

        if [ -f $grdID.inp ]
        then
          set +x
          echo "   $grdID.inp copied ($FIXwave/ww3__$grdID.inp)."
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
          echo "$modID post $date $cycle : $grdID.inp missing." >> $wavelog
          exit_code=7
        fi

        set +x
        echo "   Generating mod_def file for $grdID ... "
        [[ "$LOUD" = YES ]] && set -x

        $USHwave/ww3_mod_def.sh $grdID > $grdID.out

      fi
      if [ -f mod_def.$grdID ]
      then
        set +x
        echo " mod_def.$grdID created "
        [[ "$LOUD" = YES ]] && set -x
        rm -f $grdID.out
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
        [[ "$LOUD" = YES ]] && set -x
        echo "$modID post $date $cycle : mod_def.$grdID missing." >> $wavelog
        sed "s/^/$grdID.out : /g"  $grdID.out
        rm -f $grdID.out
        exit_code=8
        grintx_OK='no'
      fi

      if [ ! -f ${grdID}_interp.inp.tmpl ]
      then
        set +x
        echo "   Copying ${grdID}_interp.inp.tmpl from $FIXwave/${grdID}_interp.inp.tmpl"
        [[ "$LOUD" = YES ]] && set -x
        if [ -f $FIXwave/${grdID}_interp.inp.tmpl ]
        then
          cp $FIXwave/${grdID}_interp.inp.tmpl ${grdID}_interp.inp.tmpl
        fi
      fi

      if [ ! -f ${grdID}_interp.inp.tmpl ]
      then
        msg="ABNORMAL EXIT: NO GRID INTERPOLATION INPUT FILE"
        postmsg "$jlogfile" "$msg"
        set +x
        echo ' '
        echo '****************************************************** '
        echo '*** FATAL ERROR : NO GRID INTERPOLATION INPUT FILE *** '
        echo '****************************************************** '
        echo ' '
        echo $msg
        [[ "$LOUD" = YES ]] && set -x
        echo "$modID fcst $date $cycle : fixed file(s) missing." >> $wavelog
        exit_code=9
        grintx_OK='no'
      fi

      if [ ! -f ${grdID}_interp.inp.tmpl ]
      then
        set +x
        echo "   Copying ${grdID}_interp.inp.tmpl from $FIXwave/${grdID}_interp.inp.tmpl"
        [[ "$LOUD" = YES ]] && set -x
        if [ -f $FIXwave/${grdID}_interp.inp.tmpl ]
        then
          cp $FIXwave/${grdID}_interp.inp.tmpl ${grdID}_interp.inp.tmpl
        fi
      fi

      if [ ! -f ${grdID}_interp.inp.tmpl ]
      then
        msg="ABNORMAL EXIT: NO GRID INTERPOLATION INPUT FILE"
        postmsg "$jlogfile" "$msg"
        set +x
        echo ' '
        echo '****************************************************** '
        echo '*** FATAL ERROR : NO GRID INTERPOLATION INPUT FILE *** '
        echo '****************************************************** '
        echo ' '
        echo $msg
        [[ "$LOUD" = YES ]] && set -x
        echo "$modID fcst $date $cycle : fixed file(s) missing." >> $wavelog
        exit_code=9
        grintx_OK='no'
      fi
    done
  fi


# 1.d.2 Model definition and Grid interpolation file for other extended global grid

  if [ "$grintox_OK" = 'yes' ]
  then
    for grdID in $OXgrids
    do
      if [ -f "$COMIN/ww3__${modID}_${grdID}.moddef.${wave_multi_1_ver}" ]
      then
        set +x
        echo " Mod def file for $grdID found in $COMIN. copying ...."
        [[ "$LOUD" = YES ]] && set -x
        cp $COMIN/ww3__${modID}_${grdID}.moddef.${wave_multi_1_ver} mod_def.$grdID
      else
        set +x
        echo " Mod def file for $grdID not found in $COMIN. Setting up to generate ..."
        [[ "$LOUD" = YES ]] && set -x
        if [ -f $FIXwave/ww3__$grdID.inp ]
        then
          cp $FIXwave/ww3__$grdID.inp $grdID.inp
        fi

        if [ -f $grdID.inp ]
        then
          set +x
          echo "   $grdID.inp copied ($FIXwave/ww3__$grdID.inp)."
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
          echo "$modID post $date $cycle : $grdID.inp missing." >> $wavelog
          exit_code=7
        fi

        set +x
        echo "   Generating mod_def file for $grdID ... "
        [[ "$LOUD" = YES ]] && set -x

        $USHwave/ww3_mod_def.sh $grdID > $grdID.out

      fi

      if [ -f mod_def.$grdID ]
      then
        set +x
        echo " mod_def.$grdID created "
        [[ "$LOUD" = YES ]] && set -x
        rm -f $grdID.out
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
        [[ "$LOUD" = YES ]] && set -x
        echo "$modID post $date $cycle : mod_def.$grdID missing." >> $wavelog
        sed "s/^/$grdID.out : /g"  $grdID.out
        rm -f $grdID.out
        exit_code=8
        grintx_OK='no'
      fi

      if [ ! -f ${grdID}_interp.inp.tmpl ]
      then      
        set +x
        echo "   Copying ${grdID}_interp.inp.tmpl from $FIXwave/${grdID}_interp.inp.tmpl"
        [[ "$LOUD" = YES ]] && set -x
        if [ -f $FIXwave/${grdID}_interp.inp.tmpl ]
        then
          cp $FIXwave/${grdID}_interp.inp.tmpl ${grdID}_interp.inp.tmpl
        fi
      fi

      if [ ! -f ${grdID}_interp.inp.tmpl ]
      then
        msg="ABNORMAL EXIT: NO GRID INTERPOLATION INPUT FILE"
        postmsg "$jlogfile" "$msg"
        set +x
        echo ' '
        echo '****************************************************** '
        echo '*** FATAL ERROR : NO GRID INTERPOLATION INPUT FILE *** '
        echo '****************************************************** '
        echo ' '
        echo $msg
        [[ "$LOUD" = YES ]] && set -x
        echo "$modID fcst $date $cycle : fixed file(s) missing." >> $wavelog
        exit_code=9
        grintox_OK='no'
      fi
    done
  fi

# 1.c Point moddef file and buoy list

  if [ -f "$COMIN/ww3__${modID}_${buoy}.moddef.${wave_multi_1_ver}" ]
  then
    set +x
    echo " Mod def file for $buoy found in $COMIN. copying ...."
    [[ "$LOUD" = YES ]] && set -x
    cp $COMIN/ww3__${modID}_${buoy}.moddef.${wave_multi_1_ver} mod_def.$buoy
  else
    set +x
    echo " Mod def file for $buoy not found in $COMIN. Setting up to generate ..."
    [[ "$LOUD" = YES ]] && set -x
    if [  -f $FIXwave/ww3__${buoy}.inp ]
    then 
       cp $FIXwave/ww3__$buoy.inp $buoy.inp
    fi 

    if [  -f $buoy.inp ]
    then
      set +x
      echo "   $buoy.inp copied ($FIXwave/ww3__$buoy.inp)."
      [[ "$LOUD" = YES ]] && set -x
    else
      msg="ABNORMAL EXIT: NO INP FILE FOR MODEL DEFINITION FILE"
      postmsg "$jlogfile" "$msg"
      set +x
      echo ' '
      echo '*********************************************************** '
      echo '*** FATAL ERROR : NO INP FILE FOR MODEL DEFINITION FILE *** '
      echo '*********************************************************** '
      echo "                                grdID = $buoy"
      echo ' '
      echo $msg
      [[ "$LOUD" = YES ]] && set -x
      echo "$modID post $date $cycle : $buoy.inp missing." >> $wavelog
      exit_code=10
    fi

    set +x
    echo "   Generating mod_def file for $buoy ... "
    [[ "$LOUD" = YES ]] && set -x

    $USHwave/ww3_mod_def.sh $buoy > $buoy.out

  fi

  if [ -f mod_def.$buoy ]
  then
    set +x
    echo " mod_def.$buoy created. Syncing to all nodes ... "
    [[ "$LOUD" = YES ]] && set -x
    $FSYNC mod_def.$buoy
    rm -f $buoy.out
  else
    msg="ABNORMAL EXIT: NO MODEL DEFINITION FILE"
    postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '********************************************** '
    echo '*** FATAL ERROR : NO MODEL DEFINITION FILE *** '
    echo '********************************************** '
    echo "                                grdID = $buoy"
    echo ' '
    echo $msg
    [[ "$LOUD" = YES ]] && set -x
    echo "$modID post $date $cycle : mod_def.$buoy missing." >> $wavelog
    sed "s/^/$buoy.out : /g"  $buoy.out
    rm -f $buoy.out
    exit_code=11
    point_OK='no'
    spec_OK='no'
    bull_OK='no'
    Ospec_OK='no'
    Obull_ok='no'
  fi



# 1.d Output locations file

  rm -f buoy.loc

  if [ -f $FIXwave/wave_$modID.buoys ]
  then
    cp $FIXwave/wave_$modID.buoys buoy.loc.temp
# Reverse grep to exclude IBP points
    sed -n '/^\$.*/!p' buoy.loc.temp | grep -v IBP > buoy.loc
    rm -f buoy.loc.temp
  fi

  if [ -f buoy.loc ]
  then
    set +x
    echo "   buoy.loc copied and processed ($FIXwave/wave_$modID.buoys)."
    [[ "$LOUD" = YES ]] && set -x
  else
    set +x
    echo ' '
    echo '************************************* '
    echo '*** ERROR : NO BUOY LOCATION FILE *** '
    echo '************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$modID post $date $cycle : buoy location file missing." >> $wavelog
    postmsg "$jlogfile" "NON-FATAL ERROR : NO BUOY LOCATION FILE"
    exit_code=13
    point_OK='no'
    spec_OK='no'
    bull_OK='no'
    Ospec_OK='no'
    Obull_ok='no'
  fi

  for grdID in $Ogrids
  do
    rm -f buoy_$grdID.loc
    if [ -f $FIXwave/wave_$grdID.buoys ]
    then
      cp $FIXwave/wave_$grdID.buoys buoy_$grdID.tmp.loc
      sed -n '/^\$.*/!p' buoy_$grdID.tmp.loc > buoy_$grdID.loc
      rm -f buoy_$grdID.tmp.loc
    fi

    if [ -f buoy_$grdID.loc ]
    then
      set +x
      echo "   wave_$grdID.buoys copied and processed for old model processing."
      [[ "$LOUD" = YES ]] && set -x
    else
      set +x
      echo ' '
      echo '************************************* '
      echo '*** ERROR : NO BUOY LOCATION FILE *** '
      echo '************************************* '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$grdID post $date $cycle : buoy location file missing." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR : NO BUOY LOCATION FILE"
      exit_code=14
      Ospec_OK='no'
      Obull_OK='no'
    fi
  done

# 1.e Input template files

  if [ "$grint_OK" = 'yes' ]
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
      echo "$modID post $date $cycle : GRIB template file missing." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR GRIB INPUT FILE"
      exit_code=15
      grib_OK='no'
    fi

    if [ "$grib1_OK" = 'yes' ]
    then
      if [ -f $FIXwave/ww3_grib1.inp.tmpl ]
      then
        cp $FIXwave/ww3_grib1.inp.tmpl ww3_grib1.inp.tmpl
      fi

      if [ -f ww3_grib1.inp.tmpl ]
      then
        set +x
        echo "   ww3_grib1.inp.tmpl copied. Syncing to all nodes ..."
        [[ "$LOUD" = YES ]] && set -x
        $FSYNC ww3_grib1.inp.tmpl
      else
        set +x
        echo ' '
        echo '*********************************************** '
        echo '*** ERROR : NO TEMPLATE FOR GRIB INPUT FILE *** '
        echo '*********************************************** '
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        echo "$modID post $date $cycle : GRIB template file missing." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR GRIB INPUT FILE"
        exit_code=15
        grib1_OK='no'
      fi
    fi
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
      echo "$modID post $date $cycle : GRIB2 template file missing." >> $wavelog
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
    echo "$modID post $date $cycle : specra template file missing." >> $wavelog
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
    echo "$modID post $date $cycle : bulletin template file missing." >> $wavelog
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

    $EXECcode/ww3_spec > buoy_tmp.loc 
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
      echo "$modID post $date $cycle : buoy log file failed to be created." >> $wavelog
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
      echo "$modID post $date $cycle : buoy log file missing." >> $wavelog
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

  ifile=1
  nskip=1
  nlayer=10  # These are the additional point extractions being loaded on the
            # first processor to balance the load

  if [ "$nfile" -gt '1' ]
  then
    cmdtype='poe'
  else
    cmdtype='sh'
    nskip='-'
  fi

  set +x
  echo '   Making second command file (GRID Interpolation, GRIB, SPEC and BULLETINS) '
  echo "   Set up command file structure (type = $cmdtype)."
  echo "      Number of command files                : $nfile"
  echo "      Number of skips for I/O intensive jobs : $nskip"

  rm -f cmdfile
  touch cmdfile

  [[ "$LOUD" = YES ]] && set -x

  ifile=1

# 2.b GRID Interpolation and corresponding GRIB generation files

  if [ "$grint_OK" = 'yes' ]
  then
    gribFLO1=\''F F T F F  F T F F T  F F T T F  T F T F T  F F F F F  F F F F F  F F F F F   F F F F F   F F F F F   F F F F F  F F F'\'
    gribFLO2=\''F F T F F  F T F F F  F F T F F  T T T F T  F F F F F  F F F F F  F F F F F   F F F F F   F F F F F   F F F F F  F F F'\'
    ymdh_int=`$NDATE -9 $YMDH`
    dt_int=10800.
    n_int=64

    for grdID in $Ogrids
    do
      case $grdID in
      nww3)     GRIDNR=233 ; MODNR=88  ; dtgrib=10800. ; ngrib=61 ;;
       wna)     GRIDNR=238 ; MODNR=121 ; dtgrib=10800. ; ngrib=61 ;;
       akw)     GRIDNR=239 ; MODNR=122 ; dtgrib=10800. ; ngrib=61 ;;
       enp)     GRIDNR=253 ; MODNR=124 ; dtgrib=10800. ; ngrib=61 ;;
      esac

      if [ "$grib1_OK" = 'yes' ]
      then
        echo "$USHwave/ww3_grid_interp.sh $grdID $ymdh_int $dt_int $n_int > grint_$grdID.out 2>&1;$USHwave/ww3_grib2.sh $grdID $dtgrib $ngrib $GRIDNR $MODNR $gribFLO2 > grib_$grdID.out 2>&1;$USHwave/ww3_grib1.sh $grdID $dtgrib $ngrib $GRIDNR $MODNR $gribFLO1 > grib1_$grdID.out 2>&1"               >> cmdfile
      else
        echo "$USHwave/ww3_grid_interp.sh $grdID $ymdh_int $dt_int $n_int > grint_$grdID.out 2>&1;$USHwave/ww3_grib2.sh $grdID $dtgrib $ngrib $GRIDNR $MODNR $gribFLO2 > grib_$grdID.out 2>&1"               >> cmdfile
      fi

    done
  fi

# 2.c GRIB files

# GRIB field time step -- dtgrib
# Number of GRIB fields -- ngrib
# Assigned NCEP number for grid -- GRIDNR
# Assigned NCEP number for model -- MODNR

  if [ "$grib_OK" = 'yes' ]
  then
    for grdID in $grids $Xgrids
    do

      echo "$USHwave/ww3_grib2_cat.sh $grdID > grib_$grdID.out 2>&1"               >> cmdfile

    done
  fi

  if [ "$grintox_OK" = 'yes' ]
  then
# Other Xtended grids to be activated if need to post-process other xgrids
# Currently the global xgrid is created side by side and the unified fiel can
# be created as other regular grid types using ww3_grib2_cat
    for grdID in $OXgrids
    do
      case $grdID in
      glo_15mext) gribFL=\''F F T F F  F T F F F  F F T F F  T T T F T  F F F F F  F F F F F  F F F F F   F F F F F   F F F F F   F F F F F  F F F'\';
                  GRIDNR=11  ; MODNR=255 ; dtgrib=10800. ; ngrib=181; ymdh_int=`$NDATE -9 $YMDH`; dt_int=10800.; n_int=190 ; make_grib='yes';;
      glo_30mext) gribFL=\''F F T F F  F T F F F  F F T F F  T T T F T  F F F F F  F F F F F  F F F F F   F F F F F   F F F F F   F F F F F  F F F'\';
                  GRIDNR=11  ; MODNR=11  ; dtgrib=3600. ; ngrib=181; ymdh_int=`$NDATE -9 $YMDH`; dt_int=3600.; n_int=190 ; make_grib='yes';;
      rtma) gribFL=\''F F T F F  F T F F F  F F T F F  F F F F F  F F F F F  F F F F F  F F F F F   F F F F F   F F F F F   F F F F F  F F F'\';
            GRIDNR=11  ; MODNR=20  ; dtgrib=3600. ; ngrib=25; ymdh_int=${YMDH} ; dt_int=3600.; n_int=25 ; make_grib='yes';;
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
    mpirun.lsf cfp cmdfile
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
        echo "$modID post $date $cycle : error in GRID Interpolation." >> $wavelog
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
        echo "$modID post $date $cycle : error in subsequent GRIB encoding (for interpolated grids)." >> $wavelog
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
        echo "$modID post $date $cycle : error in subsequent GRIB1 encoding (for interpolated grids)." >> $wavelog
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
        echo "$modID post $date $cycle : error in GRIB." >> $wavelog
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
      echo "$modID post $date $cycle : error in spectra." >> $wavelog
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
      echo "$modID post $date $cycle : error in bulletins." >> $wavelog
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
    mpirun.lsf cfp cmdfile
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
        echo "$modID post $date $cycle : error in copying spectral files to $grdID." >> $wavelog
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
        echo "$modID post $date $cycle : error in copying bulletin files to $grdID." >> $wavelog
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
        echo "$modID post $date $cycle : error in copying compressed bulletin files to $grdID." >> $wavelog
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
        echo "$modID post $date $cycle : error in copying csv bulletin files to $grdID." >> $wavelog
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
    echo "$USHwave/ww3_tar.sh $modID spec $Nb > ${modID}_spec_tar.out 2>&1 "   >> cmdfile

  fi

# 8.b Bulletins

  if [ "$bull_OK" = 'yes' ]
  then
    echo "$USHwave/ww3_tar.sh $modID bull $Nb > ${modID}_bull_tar.out 2>&1 "   >> cmdfile

  fi

# 8.c Compressed bulletins

  if [ "$bull_OK" = 'yes' ]
  then
     echo "$USHwave/ww3_tar.sh $modID cbull $Nb > ${modID}_cbull_tar.out 2>&1 " >> cmdfile
  fi

# 8.d CSV bulletins

  if [ "$bull_OK" = 'yes' ]
  then
    echo "$USHwave/ww3_tar.sh $modID csbull $Nb > ${modID}_csbull_tar.out 2>&1 " >> cmdfile
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
    mpirun.lsf cfp cmdfile
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
    if [ -d TAR_spec_$modID ]
    then
      set +x
      echo "      Error in $modID spectral tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$modID post $date $cycle : error in spectral tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $modID spectral tar file."
    else
      rm -f ${modID}_spec_tar.out
      set +x
      echo "      $modID Spectral tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi
  fi

# 10.b Bulletin tar files

  if [ "$bull_OK" = 'yes' ]
  then
    if [ -d TAR_bull_$modID ]
    then
      set +x
      echo "      Error in $modID bulletin tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$modID post $date $cycle : error in bulletin tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $modID bulletin tar file."
    else
      rm -f ${modID}_bull_tar.out
      set +x
      echo "      $modID Bulletin tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi

    if [ -d TAR_cbull_$modID ]
    then
      set +x
      echo "      Error in $modID compressed bulletin tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$modID post $date $cycle : error in compressed bulletin tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $modID compressed bulletin tar file."
    else
      rm -f ${modID}_cbull_tar.out
      set +x
      echo "      $modID compressed bulletin tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi

    if [ -d TAR_csbull_$modID ]
    then
      set +x
      echo "      Error in $modID csv bulletin tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$modID post $date $cycle : error in csv bulletin tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $modID csv bulletin tar file."
    else
      rm -f ${modID}_csbull_tar.out
      set +x
      echo "      $modID csv bulletin tar file OK."
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
  for ID in $modID $Ogrids
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
#      mv $COMOUT/$modID.$ID.$cycle.grib       $COMOUT/$ID.$cycle.grib   # comment out per RFC 1576 (imp May 2010)
      mv $COMOUT/$modID.$ID.$cycle.outgrd     $COMOUT/$ID.$cycle.outgrd
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
     touch $COMOUT/$modID.$cycle.postdone
  fi

  msg="$job completed normally"
  postmsg "$jlogfile" "$msg"

# End of MWW3 prostprocessor script ---------------------------------------- #

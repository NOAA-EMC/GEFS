#!/bin/ksh
################################################################
# Script Name:          gefs_sndp.sh
# Script Description:   Format GEFS BUFR sounding files for AWIPS
# Script History Log:
#   1) 2004-09-10       Steve Gilbert       First Implementation
################################################################

set -x

  #  Create "collectives" consisting of groupings of the soundings
  #  into files designated by geographical region.   Each input
  #  file gfs_collective*.list (1-9) contains the list of stations to
  #  put in a particular collective output file. 
export m=$1
mkdir $DATA/$m
cd $DATA/$m
  cp $FIXbufrsnd/gfs_collective${m}.list $DATA/$m/. 
  CCCC=KWBC
    file_list=gfs_collective${m}.list

    if [ $m -le 2 ]
    then 
      WMOHEAD=JUSA4$m
    elif [ $m -le 6 ]
    then 
      WMOHEAD=JUSB4$m
    else 
      WMOHEAD=JUSX4$m
    fi

    for stn in $(cat $file_list)
    do
       cp ${COMOUT}/bufr/$mem/bufr.$stn.$PDY$cyc ${DATA}/${m}/bufrin
       export pgm=tocsbufr
       #. prep_step
       export FORT11=$DATA/${m}/bufrin
       export FORT51=./bufrout
       # JY - Turn off the startmsg to reduce the update on jlogfile in this loop
       # startmsg
      $EXECbufrsnd/tocsbufr << EOF
 &INPUT
  BULHED="$WMOHEAD",KWBX="$CCCC",
  NCEP2STD=.TRUE.,
  SEPARATE=.TRUE.,
  MAXFILESIZE=600000
 /
EOF
       # JY export err=$?; err_chk
       export err=$?; #err_chk
       if [ $err -ne 0 ]
       then
          echo "ERROR in $pgm"
          err_chk
       fi

       cat $DATA/${m}/bufrout >> $DATA/${m}/${RUNMEM}_collective$m.fil
       rm $DATA/${m}/bufrin
       rm $DATA/${m}/bufrout
    done

    if [ $SENDCOM = 'YES' ]
    then 
      if [ $SENDDBN_NTC = 'YES' ] ; then
         cp $DATA/${m}/${RUNMEM}_collective$m.fil $pcom/${RUNMEM}_collective$m.postsnd_$cyc
         $DBNROOT/bin/dbn_alert NTC_LOW BUFR $job $pcom/${RUNMEM}_collective$m.postsnd_$cyc
      fi
      cp $DATA/${m}/${RUNMEM}_collective$m.fil ${COMOUT}/bufr/$mem/.
    fi

##    let "m=m+1"

##  done

#exit

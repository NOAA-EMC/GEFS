#!/bin/sh

#####################################################################
echo "-----------------------------------------------------"
echo " Script: global_transfer.sh" 
echo " "
echo " Purpose - Copy Global Posts to /nwges and /com"
echo "           Alert posted files to DBNet"
echo " "
echo " History - "
echo "    Facey   - 9/10/96 - Implemented on C90"
echo "    Michaud - 6/03/99 - Converted to IBM SP"
echo "    Wobus   - 8/25/03 - Changes for Ens 4cyc x 180hr "
echo "    Wobus   - 6/08/05 - Changes for 6-hour breeding "
echo "    Wobus   - 2/06/06 - version for gfs in ensemble "
echo "-----------------------------------------------------"
#####################################################################
set -xa
 
if test "$SENDCOM" = "YES"
then
   #
   # Save Pressure GRIB/GRIB Index files
   #
   cp pgbafile $COMOUT/$cyc/pgrba/ge${RUN}.${cycle}.pgrbaf$fhr
      testfile=$COMOUT/$cyc/pgrba/ge${RUN}.${cycle}.pgrbaf$fhr
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
   if [[ "$makegrb1i" = "yes" ]]; then
     cp pgbaifile $COMOUT/$cyc/pgrba/ge${RUN}.${cycle}.pgrbaif$fhr
	 testfile=$COMOUT/$cyc/pgrba/ge${RUN}.${cycle}.pgrbaif$fhr
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
   fi
   cp pgb2afile $COMOUT/$cyc/pgrb2a/ge${RUN}.${cycle}.pgrb2af$fhr
       testfile=$COMOUT/$cyc/pgrb2a/ge${RUN}.${cycle}.pgrb2af$fhr
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
   if [[ "$makegrb2i" = "yes" ]]; then
     cp pgb2aifile $COMOUT/$cyc/pgrb2a/ge${RUN}.${cycle}.pgrb2aif$fhr
	  testfile=$COMOUT/$cyc/pgrb2a/ge${RUN}.${cycle}.pgrb2aif$fhr
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
   fi
 
   # Save lowres files if we are supposed to
   #
   if test "$DO_LOW_RES" = 'YES' -a `expr $fhr % 6` -eq 0
   then
     cp pgbafile.2 $COMOUT/$cyc/pgrbalr/ge${RUN}.${cycle}.pgrbaf$fhr.2
	  testfile=$COMOUT/$cyc/pgrbalr/ge${RUN}.${cycle}.pgrbaf$fhr.2
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
     if [[ "$makegrb1i" = "yes" ]]; then
       cp pgbaifile.2 $COMOUT/$cyc/pgrbalr/ge${RUN}.${cycle}.pgrbaif$fhr.2
	     testfile=$COMOUT/$cyc/pgrbalr/ge${RUN}.${cycle}.pgrbaif$fhr.2
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
     fi
     cp pgb2afile.2 $COMOUT/$cyc/pgrb2alr/ge${RUN}.${cycle}.pgrb2af$fhr.2
	   testfile=$COMOUT/$cyc/pgrb2alr/ge${RUN}.${cycle}.pgrb2af$fhr.2
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
     if [[ "$makegrb2i" = "yes" ]]; then
       cp pgb2aifile.2 $COMOUT/$cyc/pgrb2alr/ge${RUN}.${cycle}.pgrb2aif$fhr.2
	      testfile=$COMOUT/$cyc/pgrb2alr/ge${RUN}.${cycle}.pgrb2aif$fhr.2
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
     fi
   fi
fi

#
# DBNet Alerts for gfs ensemble
#

############## CHECK DBN ALERTS BEFORE USING ########## especially time limits
############## CHECK DBN ALERTS BEFORE USING ########## especially time limits
############## CHECK DBN ALERTS BEFORE USING ########## especially time limits

if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 12 ` -eq 0
then

    MEMBER=$RUN | tr '[a-z]' '[A-Z]'
    if [[ $fhr -ge 0 && $fhr -le 84 && ` expr $fhr % 6 ` -eq 0 ]]
    then
      $DBNROOT/bin/dbn_alert MODEL ENS_LEGACY_PGBA_$MEMBER $job $COMOUT/$cyc/pgrba/ge${RUN}.${cycle}.pgrbaf$fhr
      if [[ "$makegrb1i" = "yes" ]]; then
	$DBNROOT/bin/dbn_alert MODEL ENS_LEGACY_PGBAI_$MEMBER $job $COMOUT/$cyc/pgrba/ge${RUN}.${cycle}.pgrbaif$fhr
      fi
      $DBNROOT/bin/dbn_alert MODEL ENS_LEGACY_PGB2A_$MEMBER $job $COMOUT/$cyc/pgrb2a/ge${RUN}.${cycle}.pgrb2af$fhr
      if [[ "$makegrb2i" = "yes" ]]; then
	$DBNROOT/bin/dbn_alert MODEL ENS_LEGACY_PGB2AI_$MEMBER $job $COMOUT/$cyc/pgrb2a/ge${RUN}.${cycle}.pgrb2aif$fhr
      fi
    fi
    if test "$DO_LOW_RES" = 'YES' -a ` expr $fhr % 12 ` -eq 0 -a $fhr -ge 96
    then
      $DBNROOT/bin/dbn_alert MODEL ENS_LEGACY_PGBA2_$MEMBER $job $ENS_COM/$cyc/pgrbalr/ge${RUN}.${cycle}.pgrbaf$fhr.2
      if [[ "$makegrb1i" = "yes" ]]; then
	$DBNROOT/bin/dbn_alert MODEL ENS_LEGACY_PGBAI2_$MEMBER $job $ENS_COM/$cyc/pgrbalr/ge${RUN}.${cycle}.pgrbaif$fhr.2
      fi
      $DBNROOT/bin/dbn_alert MODEL ENS_LEGACY_PGB2A2_$MEMBER $job $ENS_COM/$cyc/pgrb2alr/ge${RUN}.${cycle}.pgrb2af$fhr.2
      if [[ "$makegrb2i" = "yes" ]]; then
	$DBNROOT/bin/dbn_alert MODEL ENS_LEGACY_PGB2AI2_$MEMBER $job $ENS_COM/$cyc/pgrb2alr/ge${RUN}.${cycle}.pgrb2aif$fhr.2
      fi
    fi
    if [[ $fhr -ge 192 && ` expr $fhr % 12 ` -eq 0 ]]
    then
      $DBNROOT/bin/dbn_alert MODEL ENS_LEGACY_PGBA_$MEMBER $job $COMOUT/$cyc/pgrba/ge${RUN}.${cycle}.pgrbaf$fhr
      if [[ "$makegrb1i" = "yes" ]]; then
	$DBNROOT/bin/dbn_alert MODEL ENS_LEGACY_PGBAI_$MEMBER $job $COMOUT/$cyc/pgrba/ge${RUN}.${cycle}.pgrbaif$fhr
      fi
      $DBNROOT/bin/dbn_alert MODEL ENS_LEGACY_PGB2A_$MEMBER $job $COMOUT/$cyc/pgrb2a/ge${RUN}.${cycle}.pgrb2af$fhr
      if [[ "$makegrb2i" = "yes" ]]; then
	$DBNROOT/bin/dbn_alert MODEL ENS_LEGACY_PGB2AI_$MEMBER $job $COMOUT/$cyc/pgrb2a/ge${RUN}.${cycle}.pgrb2aif$fhr
      fi
    fi
fi

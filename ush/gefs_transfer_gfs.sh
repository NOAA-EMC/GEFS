#!/bin/ksh

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
 
#PGRBA=$1
#PGRB2A=$2
GRBDIR=$1
GRBDIR2=$2
GRBFIL=$3
GRBFIL2=$4
echo $PGRBA $PGRB2A

if test "$SENDCOM" = "YES"
then
   #
   # Save Pressure GRIB/GRIB Index files
   #
# if [[ $jobgrid == "" ]]; then
   if [[ "$makepgrb1" = "yes" ]]; then
   cp pgbafile $COMOUT/$cyc/$GRBDIR/ge${RUN}.${cycle}.$GRBFIL\f$pgfhr
    if [[ "$makegrb1i" = "yes" ]]; then
     cp pgbaifile $COMOUT/$cyc/$GRBDIR/ge${RUN}.${cycle}.$GRBFIL\if$pgfhr
    fi
   fi
# fi 
   cp pgb2afile $COMOUT/$cyc/$GRBDIR2/ge${RUN}.${cycle}.$GRBFIL2\f$pgfhr
   if [[ "$makegrb2i" = "yes" ]]; then
     # JY cp pgb2aifile $COMOUT/$cyc/$GRBDIR2/ge${RUN}.${cycle}.$GRBFIL2\if$pgfhr
     cp pgb2aifile $COMOUT/$cyc/$GRBDIR2/ge${RUN}.${cycle}.$GRBFIL2\f${pgfhr}.idx
   fi
 
   # Save lowres files if we are supposed to
   #
   if test "$DO_LOW_RES" = 'YES' -a `expr $fhr % 6` -eq 0
   then
     if [[ "$makepgrb1" = "yes" ]]; then
     cp pgbafile.2 $COMOUT/$cyc/pgrbalr/ge${RUN}.${cycle}.pgrbaf$pgfhr.2
      if [[ "$makegrb1i" = "yes" ]]; then
       cp pgbaifile.2 $COMOUT/$cyc/pgrbalr/ge${RUN}.${cycle}.pgrbaif$pgfhr.2
      fi
     fi
     cp pgb2afile.2 $COMOUT/$cyc/pgrb2alr/ge${RUN}.${cycle}.pgrb2af$pgfhr.2
     if [[ "$makegrb2i" = "yes" ]]; then
       # JY cp pgb2aifile.2 $COMOUT/$cyc/pgrb2alr/ge${RUN}.${cycle}.pgrb2aif$pgfhr.2
       cp pgb2aifile.2 $COMOUT/$cyc/pgrb2alr/ge${RUN}.${cycle}.pgrb2af$pgfhr.2.idx
     fi
   fi
fi

#
# DBNet Alerts for gfs ensemble
#

############## CHECK DBN ALERTS BEFORE USING ########## especially time limits
############## CHECK DBN ALERTS BEFORE USING ########## especially time limits
############## CHECK DBN ALERTS BEFORE USING ########## especially time limits

GRID=
FHINCDBN=6
if [[ $jobgrid == _p5 ]]; then
 GRID=_0P5
 FHINCDBN=3
fi

if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 12 ` -eq 0
then

#   MEMBER=$RUN | tr '[a-z]' '[A-Z]'
    MEMBER=`echo $RUN |tr '[a-z]' '[A-Z]'`
   
    if [[ $fhr -ge 0 && $fhr -le 84 && ` expr $fhr % $FHINCDBN ` -eq 0 ]]
    then
      if [[ "$makepgrb1" = "yes" ]]; then
      $DBNROOT/bin/dbn_alert MODEL ENS_PGBA${GRID}_$MEMBER $job $COMOUT/$cyc/$GRBDIR/ge${RUN}.${cycle}.$GRBFIL\f$pgfhr
      if [[ "$makegrb1i" = "yes" ]]; then
	$DBNROOT/bin/dbn_alert MODEL ENS_PGBAI${GRID}_$MEMBER $job $COMOUT/$cyc/$GRBDIR/ge${RUN}.${cycle}.$GRBFIL\if$pgfhr
      fi
      fi
      $DBNROOT/bin/dbn_alert MODEL ENS_PGB2A${GRID}_$MEMBER $job $COMOUT/$cyc/$GRBDIR2/ge${RUN}.${cycle}.$GRBFIL2\f$pgfhr
     # JY if [[ "$makegrb2i" = "yes" ]]; then
     # JY	$DBNROOT/bin/dbn_alert MODEL ENS_PGB2AI${GRID}_$MEMBER $job $COMOUT/$cyc/$GRBDIR2/ge${RUN}.${cycle}.$GRBFIL2\f$pgfhr.idx
     # JY  fi
    fi

    if test "$DO_LOW_RES" = 'YES' -a ` expr $fhr % 12 ` -eq 0 -a $fhr -ge 96
    then
      if [[ "$makepgrb1" = "yes" ]]; then
      $DBNROOT/bin/dbn_alert MODEL ENS_PGBA2_$MEMBER $job $COMOUT/$cyc/pgrbalr/ge${RUN}.${cycle}.pgrbaf$pgfhr.2
      if [[ "$makegrb1i" = "yes" ]]; then 
      $DBNROOT/bin/dbn_alert MODEL ENS_PGBAI2_$MEMBER $job $COMOUT/$cyc/pgrbalr/ge${RUN}.${cycle}.pgrbaif$pgfhr.2
      fi
      fi
      $DBNROOT/bin/dbn_alert MODEL ENS_PGB2A2_$MEMBER $job $COMOUT/$cyc/pgrb2alr/ge${RUN}.${cycle}.pgrb2af$pgfhr.2
      # JY if [[ "$makegrb2i" = "yes" ]]; then
      # JY	$DBNROOT/bin/dbn_alert MODEL ENS_PGB2AI2_$MEMBER $job $COMOUT/$cyc/pgrb2alr/ge${RUN}.${cycle}.pgrb2af$pgfhr.2.idx
      # JY fi
    fi

    if [[ $fhr -ge 240 && ` expr $fhr % 12 ` -eq 0 ]]
    then
      if [[ "$makepgrb1" = "yes" ]]; then
      $DBNROOT/bin/dbn_alert MODEL ENS_PGBA${GRID}_$MEMBER $job $COMOUT/$cyc/$GRBDIR/ge${RUN}.${cycle}.$GRBFIL\f$pgfhr
      if [[ "$makegrb1i" = "yes" ]]; then
	$DBNROOT/bin/dbn_alert MODEL ENS_PGBAI${GRID}_$MEMBER $job $COMOUT/$cyc/$GRBDIR/ge${RUN}.${cycle}.$GRBFIL\if$pgfhr
      fi
      fi
      $DBNROOT/bin/dbn_alert MODEL ENS_PGB2A${GRID}_$MEMBER $job $COMOUT/$cyc/$GRBDIR2/ge${RUN}.${cycle}.$GRBFIL2\f$pgfhr
      # JY if [[ "$makegrb2i" = "yes" ]]; then
      # JY	$DBNROOT/bin/dbn_alert MODEL ENS_PGB2AI${GRID}_$MEMBER $job $COMOUT/$cyc/$GRBDIR2/ge${RUN}.${cycle}.$GRBFIL2\f$pgfhr.idx
      # JY fi
    fi
fi

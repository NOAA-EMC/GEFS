#!/bin/ksh

#####################################################################
echo "-----------------------------------------------------"
echo " Script: gefs_restart_cleanup.sh" 
echo " "
echo " Purpose - Remove selected GEFS log and control files"
echo "           before rerunning GEFS forecast, post, or"
echo "           prdgen"
echo " "
echo " History - "
echo "    Wobus   - 6/15/15 - New GEFS script on WCOSS "
echo " "
echo " Arguments:"
echo "  1    Hour to start"
echo "  2    Hour to end"
echo "  3+   log/control directories to clean:"
echo "           fcst"
echo "           post"
echo "           prd0p5"
echo "           prd1p0"
echo "           prd2p5"
echo "-----------------------------------------------------"
#####################################################################
set -xa
   
dirmisc=$COMOUT/$cyc/misc
(( iarg = 0 ))
for arg in $*
do
  (( iarg = iarg + 1 ))
  case $arg in
    (1)
      fhb=$arg
      echo fhb=$fhb
    ;;
    (2)
      fhe=$arg
      echo fhe=$fhe
    ;;
    (*)
      subdir=$arg
      logdir=$dirmisc/$subdir
      if [[ -d $logdir ]]; then
	(( nrem = 0 ))
	(( fh = fhb ))
	while (( fh < fhe )); do
	  if (( fh < 10 )); then
	    fh=0$fh
	  fi
	  for file in $logdir/*f$fh $logdir/*f_$fh
	  do
	    if [[ -f $file ]]; then
	      ls -al $file
	      rm $file
	      (( nrem = nrem + 1 ))
	    fi
	  done
	  (( fh = fh + FH_INC ))
	done
	echo nrem=$nrem files removed from $logdir
      echo
        echo logdir=$logdir DOES NOT EXIST AS A DIRECTORY
      fi
    ;;
  esac
done
exit 


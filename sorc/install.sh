#!/bin/bash
#
# build all of the GEFS codes
#
echo "`date`   `pwd`   $0 $*   begin"
logfile=`basename $0`.log
if [[ -f $logfile ]]; then
  dtg=`date +%Y%m%d%H%M%S`
  mv $logfile $logfile.$dtg
fi
(
echo "`date`   `pwd`   $0 $*   begin log"
pwd
dirsaved=`pwd`
mkdir -p ../exec
if [[ -d ../exec ]]; then
  mkdir -p ../util/exec
  if [[ -d ../util/exec ]]; then
    for dir in ../sorc.tracker *.fd ../util/sorc/*.fd
    do
      cd $dir
      pwd
      case $dir in
	(../sorc.tracker)
	  echo
	  pwd
	  echo
	  sh install.sh
	  echo
	;;
	(gefs_global_fcst.fd)
	  echo
	  pwd
	  echo
	  #makefile.sh_wcoss
	  #rc=$?
	  #echo
	  #if (( rc == 0 )); then
	    #echo makefile.sh_wcoss ran successfully
	    #ls -alrt ../../exec
	    filex=`basename $dir | sed -e"s/\.fd$//"`
	    ls -al $filex
	    if [[ -s $filex ]]; then
	      mv $filex ../../exec
	      ls -al ../../exec/$filex
	    else
	      echo filex=$fileex DOES NOT EXIST AS A FILE
	    fi
	  #else
	    #echo make FAILED IN dir=$dir rc=$rc
	  #fi
	  echo
	;;
	(*)
	  echo
	  pwd
	  echo
	  #make -f makefile_wcoss clean
	  #echo
	  #make -f makefile_wcoss
	  #rc=$?
	  #echo
	  #if (( rc == 0 )); then
	    #echo make -f makefile_wcoss ran successfully
	    #filex=`ls -1rt | tail -1`
	    filex=`basename $dir | sed -e"s/\.fd$//"`
	    ls -al $filex
	    if [[ -s $filex ]]; then
	      mv $filex ../../exec
	      ls -al ../../exec/$filex
	    else
	      echo filex=$fileex DOES NOT EXIST AS A FILE
	    fi
	    #mkdir -p ../../exec
	    #if [[ -d ../../exec ]]; then
	      #ls -al $filex
	      #mv $filex ../../exec
	      #ls -alrt ../../exec
	    #else
	      #echo dir=`pwd`/../../exec DOES NOT EXIST AS A DIRECTORY
	    #fi
	    #echo
	  #else
	  #  echo make FAILED IN dir=$dir rc=$rc
	  #fi
	  echo
	;;
      esac
      pwd
      cd $dirsaved
      pwd
    done
    echo
    ls -ald ../exec
    ls -al ../exec
    echo
    ls -ald ../util/exec
    ls -al ../util/exec
    echo
  else
     echo dir=`pwd`/../util/exec DOES NOT EXIST AS A DIRECTORY
  fi
else
   echo dir=`pwd`/../exec DOES NOT EXIST AS A DIRECTORY
fi
echo "`date`   `pwd`   $0 $*   end of log"
) 2>&1 | tee $logfile
echo "`date`   `pwd`   $0 $*   end"

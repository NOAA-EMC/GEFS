#!/bin/ksh
###################################################################
echo "----------------------------------------------------"
echo "exnawips - convert NCEP GRIB files into GEMPAK Grids"
echo "----------------------------------------------------"
echo "History: Mar 2000 - First implementation of this new script."
echo "S Lilly: May 2008 - add logic to make sure that all of the "
echo "                    data produced from the restricted ECMWF"
echo "                    data on the CCS is properly protected."
echo "C. Magee: 10/2013 - swap X and Y for rtgssthr Atl and Pac."
#####################################################################

set -xa

cd $DATA

RUNM=${1:-${RUNM}}
member=${2:-${member}}
resolution=${3:-${resolution}}
GEMPAKgefs=${4:-${GEMPAKgefs}}
COMIN=${5:-${COMIN}}
COMOUT=${6:-${COMOUT}}
fstart=${7:-${fstart}}
fend=${8:-${fend}}
FHMAXHF=${9:-${FHMAXHF}}
FHOUTHF=${10:-${FHOUTHF}}
FHOUTLF=${11:-${FHOUTLF}}
pgrdbType=${12:-${pdgrbType}} #:-pgrb2a}

DATA_member=$DATA/${member}/$resolution
mkdir -p $DATA_member
cd $DATA_member

msg="Begin job for $job"
postmsg "$jlogfile" "$msg"

#
# Now set up GEMPAK/NTRANS environment
#
#
# Copy model specific GEMPAK tables into working directory
cp $GEMPAKgefs/fix/*.tbl .

NAGRIB_TABLE=${NAGRIB_TABLE:-${GEMPAKgefs}/fix/nagrib.tbl}
NAGRIB=nagrib2_nc
#

entry=$(grep "^$RUNM " $NAGRIB_TABLE | awk 'index($1,"#") != 1 {print $0}')

if [ "$entry" != "" ] ; then
    cpyfil=$(echo $entry  | awk 'BEGIN {FS="|"} {print $2}')
    garea=$(echo $entry   | awk 'BEGIN {FS="|"} {print $3}')
    gbtbls=$(echo $entry  | awk 'BEGIN {FS="|"} {print $4}')
    maxgrd=$(echo $entry  | awk 'BEGIN {FS="|"} {print $5}')
    kxky=$(echo $entry    | awk 'BEGIN {FS="|"} {print $6}')
    grdarea=$(echo $entry | awk 'BEGIN {FS="|"} {print $7}')
    proj=$(echo $entry    | awk 'BEGIN {FS="|"} {print $8}')
    output=$(echo $entry  | awk 'BEGIN {FS="|"} {print $9}')
else
    cpyfil=gds
    garea=dset
    gbtbls=
    maxgrd=4999
    kxky=
    grdarea=
    proj=
    output=T
fi
pdsext=no

# for gefs
COMOUT_hold=$COMOUT

maxtries=180
fhcnt=$fstart
while [ $fhcnt -le $fend ] ; do

    fhr=$(printf %03i $fhcnt)
    
    finc1=${FHOUTHF}
    if [ $fhcnt -ge ${FHMAXHF} ]; then
        finc1=${FHOUTLF}
    fi
    case $RUNM in
    ens*)  
        GRIBIN=$COMIN/${model}.${member}.${PDY}.${cyc}
        GEMGRD=${RUNM}_${PDY}${cyc} 
        ;;
    ge*)  
        if [ "$model" = "bc" -o "$model" = "an" -o "$model" = "wt" -o "$model" = "me" -o "$model" = "anv" ]; then
            GRIBIN=$COMIN/${RUNM}.${cycle}.pgrb2a_${model}f${fhr}
            GEMGRD=${RUNM}${model}_${PDY}${cyc}f${fhr3}

            # create subdirectory for the bc and an gefs files, -- 05/16/2013
            # so that the mag system can only take the expected gefs files
            # COMOUT_hold=$COMOUT
            if [ "$model" = "bc" -o "$model" = "an" ]; then
                COMOUT=${COMOUT_hold}/${model}
                mkdir -p -m 775 $COMOUT
             fi
        elif [ "$model" = "glbanl" ]; then
            GRIBIN=$COMIN/${model}.${cycle}.pgrb2a_mdf${fhr}
            GEMGRD=${model}_${PDYm2}${cyc}f${fhr3}
        elif [ "$model" = "ndgd" ]; then
            GRIBIN=$COMIN/${RUNM}.${cycle}.${model}_conusf${fhr}
            GEMGRD=${RUNM}${model}_${PDY}${cyc}f${fhr3}
        elif [ "$model" = "ndgd_alaska" ]; then
            GRIBIN=$COMIN/${RUNM}.${cycle}.${model}f${fhr}
            GEMGRD=${RUNM}${model}_${PDY}${cyc}f${fhr3}
        else
            # This is for gefs
            GRIBIN=${COMIN}/${RUNM}.${cycle}.${pgrdbType}.${resolution}.f${fhr}
            if [ $resolution = "1p00" ]; then
            #if [ $resolution = "0p50" ]; then
                GEMGRD=${RUNM}_${PDY}${cyc}f${fhr}
            else
                GEMGRD=${RUNM}_${resolution}_${PDY}${cyc}f${fhr}
            fi
        fi
        ;;

    *) 
        GRIBIN=$COMIN/${model}.${cycle}.${GRIB}${fhr}${EXT}
        GEMGRD=${RUNM}_${PDY}${cyc}f${fhr3} 
    esac

    GRIBIN_chk=${GRIBIN}.idx

    icnt=1
    while [ $icnt -lt 1000 ]; do
        if [ -r $GRIBIN_chk ]; then
            break
        else
            let "icnt=icnt+1"
            sleep 20
        fi
        if [ $icnt -ge $maxtries ]; then
            msg="ABORTING after 1 hour of waiting for F$fhr to end."
            err_exit $msg
        fi
    done

    ln -s $GRIBIN grib$fhr

	$GEMEXE/$NAGRIB <<- EOF
		GBFILE   = grib$fhr
		INDXFL   = 
		GDOUTF   = $GEMGRD
		PROJ     = $proj
		GRDAREA  = $grdarea
		KXKY     = $kxky
		MAXGRD   = $maxgrd
		CPYFIL   = $cpyfil
		GAREA    = $garea
		OUTPUT   = $output
		GBTBLS   = $gbtbls
		GBDIAG   = 
		PDSEXT   = $pdsext
		l
		r
		EOF
    
    export err=$?;err_chk

    #####################################################
    # GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
    # WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
    # FOR THIS CASE HERE.
    #####################################################
    ls -l $GEMGRD

    export pgm="GEMPAK CHECK FILE"
    if [ $err -ne 0 ]; then
        echo "Error: failed to create gempak file $GEMGRD"
        err_chk
    fi

    if [ "$NAGRIB" = "nagrib2_nc" ]; then
        gpend
    fi
    if [ $SENDCOM = "YES" ]; then
        mv $GEMGRD $COMOUT/$GEMGRD
        if [ $SENDDBN = "YES" ]; then
            $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
            $COMOUT/$GEMGRD
        else
            echo "##### DBN_ALERT_TYPE is: ${DBN_ALERT_TYPE} #####"
        fi
    fi

    let fhcnt=fhcnt+finc1
done

if [ $err -eq 0 ] ; then 
    echo "**************JOB $RUN NAWIPS COMPLETED NORMALLY ON THE IBM"
    msg='Job completed normally.'
    echo $msg
    postmsg "$jlogfile" "$msg"
else
    echo "FAILED to convert NCEP GRIB files into GEMPAK Grids"
    exit $err
fi

############################### END OF SCRIPT #######################

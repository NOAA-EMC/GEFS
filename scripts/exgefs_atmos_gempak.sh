#!/bin/ksh

echo "$(date -u) begin ${.sh.file}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
    # Turn on strict bash error checking
    set -eu
fi

# Print inherited environmnent variables
for var in DATA npert GEMPAK_RES COMIN COMOUT COMPONENT fstart fend FHMAXHF FHOUTHF FHOUTLF pdgrbType; do
	echo "$var = ${!var}"
done

if [[ $SENDCOM == "YES" ]]; then
    gempak_out=${COMOUT}/$COMPONENT/gempak
else
    gempak_out=${DATA}/gempak_out
fi


########################################################
## Get member list
########################################################
memberlist="$memberlist avg spr"
echo memberlist=$memberlist

########################################################
## Generate poescript
########################################################
for member in $memberlist; do
    export RUNM=ge${member}
    for resolution in ${GEMPAK_RES}; do
        case $resolution in
            (1p00)
                gempak_in=$COMIN/$COMPONENT/pgrb2a1p0
                export pdgrbType=pgrb2a
                export fend=${fhmaxh:-384}
                ;;
            (0p50)
                gempak_in=$COMIN/$COMPONENT/pgrb2ap5
                export pdgrbType=pgrb2a
                export fend=${fhmaxh:-384}
                ;;
            (0p25)
                gempak_in=$COMIN/$COMPONENT/pgrb2sp25
                export pdgrbType=pgrb2s
                export fend=${FHMAXHF:-240}
                ;;
            (*)
                echo "FATAL ERROR in ${.sh.file}: resolution $resoltion not supported!"
                export err=5
                exit $err
                ;;
        esac
        
        echo "$HOMEgefs/ush/gefs_nawips.sh $RUNM $member $resolution ${GEMPAKgefs} ${gempak_in} ${gempak_out} ${fstart} ${fend} ${FHMAXHF} ${FHOUTHF} ${FHOUTLF} ${pdgrbType}" >> poescript
    done
done

cat poescript

chmod 775 $DATA/poescript
export MP_PGMMODEL=mpmd
export MP_CMDFILE=$DATA/poescript
export SCR_CMDFILE=$MP_CMDFILE  # Used by mpiserial on Theia
rm -f mpmd_cmdfile
ln -s $MP_CMDFILE mpmd_cmdfile

#############################################################
# Execute the script
$APRUN_MPMD
export err=$?

if [[ $err != 0 ]]; then
    echo "FATAL ERROR in ${.sh.file}: One or more gempak resolutions in $MP_CMDFILE failed!"
    exit $err
fi
#############################################################

echo "$(date -u) end ${.sh.file}"

exit $err

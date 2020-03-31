#!/bin/sh
#
# Metafile Script : gefs_meta_qpf
#
# Log :
# J. Carr/HPC    12/12/2003    Moved over from IBM.
#
# Set Up Local Variables
#
set -x
export PS4='gefs_meta_qpf:$SECONDS + '

########################################################
## Get member list
########################################################
export npert=${npert:-30}
memberlist=""
(( imem = 0 ))
while (( imem < npert+1 )); do
    if (( imem == 0 )); then
        smem=c$(printf %02i $imem)
    else
        smem=p$(printf %02i $imem)
    fi
    memberlist="$memberlist $smem"
    (( imem = imem + 1 ))
done # while (( imem < npert ))
echo memberlist=$memberlist
########################################################
## Get member list
########################################################

sGrid=0p50_

mkdir $DATA/gefs_meta_qpf
cd $DATA/gefs_meta_qpf
#cp $FIXgempak/datatype.tbl datatype.tbl

#mdl=gefs
#MDL=GEFS

ddate=`echo $PDY | cut -c3-8`
ddatem1=`echo $PDYm1 | cut -c3-8`
if [ ${cyc} = "00" ];then
   cycm12=12
else
   cycm12=00
fi

if [ ${cyc} = "00" ]; then
    grids="gfs $memberlist ecmwf" # EC" #"GFS C00 P01 P02 P03 P04 P05 P06 P07 P08 P09 P10 P11 P12 P13 P14 P15 P16 P17 P18 P19 P20 EC"
elif [ ${cyc} = "12" ]; then
    grids="gfs $memberlist" #"GFS C00 P01 P02 P03 P04 P05 P06 P07 P08 P09 P10 P11 P12 P13 P14 P15 P16 P17 P18 P19 P20 "
fi

for area in us sam us24 us12
do
    # GENERATE 48 HR PCPN TOTALS FOR THE MEDIUM RANGE FORECASTER.
    if [ ${area} = "us" ]; then

        metaname="gefs_${sGrid}${PDY}_${cyc}_meta_qpf_medr"
        device="nc | $metaname"

        if [ ${cyc} = "00" ]; then
            gdattim="f132"
            fcsthrs="132"
        else
            gdattim="f120"
            fcsthrs="120"
        fi

        garea="us"
        proj=" "
        glevel="0"
        gvcord="none"
        gdpfun="p48i"
        type="f"
        contur="1"
        cint="0"
        line="0"
        fint=".1;.25;.5;.75;1;1.25;1.5;1.75;2;2.5;3;4;5;6;7;8;9"
        fline="0;22-30;14-20;5"
        hilo="31;0/x#2/.03-20/50;50//y"
        hlsym="1//22/2/hw"
        scale="0"
        clrbar="1"
        name2="48-HR PCPN"
        fcmdfile=cmdfile_meta_qpf_medr

    # GENERATE 24 HR PCPN TOTALS FOR QPF.
    elif [ ${area} = "us24" ]; then
        metaname="gefs_${sGrid}${PDY}_${cyc}_meta_qpf_us24"
        device="nc | $metaname"

        gdattim="f24-f216-12"
        fcsthrs="024 036 048 060 072 084 096 108 120 132 144 156 168 180 192 204 216"
        garea="us"
        proj=" "
        glevel="0"
        gvcord="none"
        gdpfun="p24i"
        type="f"
        contur="1"
        cint="0"
        line="0"
        fint=".1;.25;.5;.75;1;1.25;1.5;1.75;2;2.5;3;4;5;6;7;8;9"
        fline="0;22-30;14-20;5"
        hilo="31;0/x#2/.03-20/50;50//y"
        hlsym="1//22/2/hw"
        scale="0"
        clrbar="1"
        name2="24-HR PCPN"
        fcmdfile=cmdfile_meta_qpf_us24

    # GENERATE 12 HR PCPN TOTALS FOR QPF.
    elif [ ${area} = "us12" ]; then
        metaname="gefs_${sGrid}${PDY}_${cyc}_meta_qpf_us12"
        device="nc | $metaname"

        gdattim="f12-f216-12"
        fcsthrs="012 024 036 048 060 072 084 096 108 120 132 144 156 168 180 192 204 216"
        garea="us"
        proj=" "
        glevel="0"
        gvcord="none"
        gdpfun="p12i"
        type="f"
        contur="1"
        cint="0"
        line="0"
        fint=".1;.25;.5;.75;1;1.25;1.5;1.75;2;2.5;3;4;5;6;7;8;9"
        fline="0;22-30;14-20;5"
        hilo="31;0/x#2/.03-20/50;50//y"
        hlsym="1//22/2/hw"
        scale="0"
        clrbar="1"
        name2="12-HR PCPN"
        fcmdfile=cmdfile_meta_qpf_us12

    # GENERATE 24 HR PCPN TOTALS FOR SAM QPF.
    elif [ ${area} = "sam" ]; then
        metaname="gefs_${sGrid}${PDY}_${cyc}_meta_samqpf"
        device="nc | $metaname"

        if [ ${cyc} = "00" ]; then
            gdattim="f24-f144-12"
            fcsthrs="024 036 048 060 072 084 096 108 120 132 144 156 168 180 192 204 216"
        else
            gdattim="f24-f144-12"
            fcsthrs="024 036 048 060 072 084 096 108 120 132 144 156 168 180 192 204 216"
        fi
        garea="-28.2;-140.5;14.1;-32.6"
        proj="str/-85;-70;0"
        glevel="0"
        gvcord="none"
        gdpfun="p24m"
        type="f"
        contur="1"
        cint="0"
        line="0"
        fint="1;5;10;15;20;25;30;35;40;45;50;55;60;65;70;75;80;85"
        fline="0;21-30;14-20;5"
        hilo="31;0/x#/10-500/50;50//y"
        hlsym="1"
        scale="0"
        clrbar="1"
        name2="24-HR PCPN"
        fcmdfile=cmdfile_meta_samqpf
    fi
    
    for fcsthr in ${fcsthrs}
    do
        for fn in `echo $memberlist`
        do
            rm -rf $fn 
            if [ -r $COMIN/ge${fn}_${sGrid}${PDY}${cyc}f${fcsthr} ]; then
                ln -s $COMIN/ge${fn}_${sGrid}${PDY}${cyc}f${fcsthr} $fn
            fi
        done

        fn=gfs
        rm -rf ${fn}
        if [ -r $COMINsgfs/gfs.${PDY}/${cyc}/gempak/gfs_${sGrid}${PDY}${cyc}f${fcsthr} ]; then
            #  ln -s $COMINs/gfs.${PDY}/gfs_${PDY}${cyc}f${fcsthr} gfs
            ln -s $COMINsgfs/gfs.${PDY}/${cyc}/gempak/gfs_${sGrid}${PDY}${cyc}f${fcsthr} ${fn}
        fi

        if [ ${cyc} = "00" ]; then
            fn=ecmwf
            rm -rf ${fn}
            if [ -r $COMINecmwf.${PDYm1}/gempak/ecmwf_hr_${PDYm1}${cycm12}f${fcsthr} ]; then
                ln -s $COMINecmwf.${PDYm1}/gempak/ecmwf_hr_${PDYm1}${cycm12}f${fcsthr} ${fn}
            fi
        fi

        for grid in ${grids}
   	    do
            name="${grid} ${name2}"
            title="1/0/~ ? ${name}|~${name}"

            if [[ ${area} == "us" ]]; then
                if [[ ${grid} == "ecmwf" ]]; then 
                    gdattim="f144"
                fi
            fi
#        if [ ${grid} = "GFS" ]; then
#            GDFILE="F-GFS | ${ddate}/${cyc}00"
#            COMINtmp=$COMIN
#            export COMIN=$COMINgfs/gfs.${PDY}/${cyc}/gempak/
#        elif [ ${grid} = "EC" ]; then
#            if [ $cyc = "12" ]; then
#               COMINtmp=$COMIN
#               export COMIN=$COMINecmwf
#               GDFILE="$COMIN/ecmwf_glob_${PDY}$cycm12"
#            else
#               COMINtmp=$COMIN
#               export COMIN=$COMINm1ecmwf
#               GDFILE="$COMIN/ecmwf_glob_${PDYm1}$cycm12"
#            fi
#            if [ ${area} = "us" ]; then
#                gdattim="f144"
#            fi
#        else
#            GDFILE="F-GEFS$grid | ${ddate}/${cyc}00"
#            COMINtmp=$COMIN
#        fi

            GDFILE=$grid
        
            gdattim=F${fcsthr}

			cat > $fcmdfile  <<- EOF 
				GDFILE	= ${GDFILE}
				GDATTIM	= ${gdattim}
				DEVICE	= ${device}
				PANEL	= 0
				TEXT	= 1/22/1/1/hw
				MAP	= 11!0
				CLEAR	= yes
				GAREA   = ${garea}
				PROJ    = ${proj}
				LATLON  = 11/10/1/1/20;20!0

				GLEVEL  = ${glevel}
				GVCORD  = ${gvcord}
				GDPFUN  = ${gdpfun}
				TYPE    = ${type}
				CONTUR  = ${contur}
				CINT    = ${cint}
				LINE    = ${line}
				FINT    = ${fint}
				FLINE   = ${fline}
				HILO    = ${hilo}
				HLSYM   = ${hlsym}
				SKIP    = 0
				SCALE   = ${scale}
				CLRBAR  = ${clrbar}
				WIND    = 0
				REFVEC  =
				TITLE   = ${title}
				run

				exit
				EOF
            

            cat $fcmdfile

            gdplot2_nc < $fcmdfile

            export err=$?;export pgm="GEMPAK CHECK FILE";err_chk


            export COMIN=$COMINtmp
        done

    done

    if [ $SENDCOM = "YES" ] ; then
        mv ${metaname} ${COMOUT}/$metaname
        if [ $SENDDBN = "YES" ] ; then
            $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job ${COMOUT}/$metaname
        fi
    fi

done

for grid in ${grids}
do
    name2="500 & PMSL"
    name="${grid} ${name2}"
    title="1/-1/~ ? ${name}|~${name}"
    metaname="gefs_${PDY}_${cyc}_nam"
    device="nc | $metaname"
#    if [ ${grid} = "GFS" ]; then
#        GDFILE="F-GFS | ${ddate}/${cyc}00"
#        COMINtmp=$COMIN
#        export COMIN=$COMINgfs/gfs.${PDY}/${cyc}/gempak/
#    elif [ ${grid} = "EC" ]; then
#        if [ $cyc = "12" ]; then
#           COMINtmp=$COMIN
#           export COMIN=$COMINecmwf
#           GDFILE="$COMIN/ecmwf_glob_${PDY}$cycm12"
#        else
#           COMINtmp=$COMIN
#           export COMIN=$COMINm1ecmwf
#           GDFILE="$COMIN/ecmwf_glob_${PDYm1}$cycm12"
#        fi
#    else
#        GDFILE="F-GEFS${grid} | ${ddate}/${cyc}00"
#        COMINtmp=$COMIN
#    fi
    GDFILE=$grid

    fcsthrs="024 036 048 060 072 084 096 108 120 132 144 156 168 180 192 204"

    for fcsthr in ${fcsthrs}
    do
        gdattim=F${fcsthr}

		gdplot2_nc <<- EOF
			GDFILE	= ${GDFILE}
			GDATTIM	= ${gdattim}
			DEVICE	= ${device}
			PANEL	= 0
			TEXT	= 1/22/1/1/hw
			MAP	= 11!0
			CLEAR	= yes
			GAREA   = 2;-139;27;-22
			PROJ    = STR/90;-105;0
			LATLON  = 11/10/1/1/20;20!0
			glevel  = 0              !500
			gvcord  = none           !pres
			gdpfun  = sm5s(pmsl)     !sm9s(hght)
			type    = c              !c
			contur  = 2
			skip    = 0
			cint    = 8              !6
			line    = 19/1/3/1       !2/2/3/1
			scale   = 0              !-1
			fint    = 0
			fline   = 0
			hilo    = 19/H#;L#/1020-1070;900-1012 !0
			hlsym   = 1;1//22;22/2;2/hw           !0
			clrbar  = 0
			TITLE   = ${title}
			run

			exit
			EOF

    done
    
    export err=$?;export pgm="GEMPAK CHECK FILE";err_chk
    export COMIN=$COMINtmp

done

if [ $SENDCOM = "YES" ] ; then
    mv ${metaname} ${COMOUT}/$metaname
    if [ $SENDDBN = "YES" ] ; then
        $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job ${COMOUT}/$metaname
    fi
fi


exit $err

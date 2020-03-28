#!/bin/sh
#
#
# Log :
# J. Carr/PMB      12/15/2004     Pushed into production
# Luke Lin         02/15/2006     point to new gefs
# C. Magee/NCO     10/06/2008     Changed to use COMINs and COMIN for input 
#                                 file locations (to make testing easier).
#
# Set Up Local Variables
#
set -x
export PS4='mar_00Z:$SECONDS + '

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

mkdir $DATA/mar_00Z
cd $DATA/mar_00Z
cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gefs
MDL=GEFS
PDY2=`echo $PDY | cut -c3-`

if [ ${cyc} != "00" ] ; then
    echo " "
    echo "EXITING GEMPAK SCRIPT BECAUSE THIS SCRIPT DOES NOT EXECUTE"
    echo "AT ANY OTHER TIME EXCEPT 00Z."
    echo " "
    exit
fi

# DEFINE YESTERDAY
yesterday=`${NDATE} -24 ${PDY}${cyc} | cut -c -8`
shrtyesterday=`${NDATE} -24 ${PDY}${cyc} | cut -c3-8`

# SET GFS PARAMETERS
gfscyc="12"
gfscyc2="06"
ecmwfcyc="12"
ecmwfdate="${yesterday}"
ukmetcyc="12"
ukmetdate="${yesterday}"

fcsthrs="000 012 024 036 048 060 072 084 096 108 120"
levels="528 534 540 546 552 564 576"

for metaarea in pac atl
do
    if [ ${metaarea} == "pac" ] ; then
        garea="MPAC"
        proj=" "

    else
        garea="15;-100;70;5"
        proj="mer"
    fi
    metatype="${metaarea}_mar"
    metaname="${mdl}_${metatype}_${cyc}.meta"
    device="nc | ${metaname}"
    for level in ${levels}
    do
        for fcsthr in ${fcsthrs}
        do
            fcsthrsgfs=`expr ${fcsthr} + 12`
            #typeset -Z3 fcsthrsgfs

            fcsthrsgfs=$(printf %03i $fcsthrsgfs)

            grids=${memberlist}
            for fn in `echo $grids`
            do
                rm -rf $fn 
                if [ -r $COMIN/ge${fn}_${sGrid}${PDY}${cyc}f${fcsthr} ]; then
                    ln -s $COMIN/ge${fn}_${sGrid}${PDY}${cyc}f${fcsthr} $fn
                fi
            done

            fn=gfs
            rm -rf ${fn}
            #f [ -r $COMINs/gfs.${PDY}/gfs_${PDY}${cyc}f${fcsthr} ]
            # \$COMINsgfs/gfs.${yesterday}/${gfscyc}/gempak/gfs_${yesterday}${gfscyc}f${fcsthrsgfs}
            if [ -r $COMINsgfs/gfs.${yesterday}/${gfscyc}/gempak/gfs_${sGrid}${yesterday}${gfscyc}f${fcsthrsgfs} ]; then
                #  ln -s $COMINs/gfs.${PDY}/gfs_${PDY}${cyc}f${fcsthr} gfs
                ln -s $COMINsgfs/gfs.${yesterday}/${gfscyc}/gempak/gfs_${sGrid}${yesterday}${gfscyc}f${fcsthrsgfs} ${fn}
            fi

            fn=ecmwf
            rm -rf ${fn}
            if [ -r $COMINm1ecmwf.${ecmwfdate}/gempak/ecmwf_hr_${ecmwfdate}${ecmwfcyc}f${fcsthr} ]; then
                ln -s $COMINm1ecmwf.${ecmwfdate}/gempak/ecmwf_hr_${ecmwfdate}${ecmwfcyc}f${fcsthr} ${fn}
            fi

            fn=ukmet
            rm -rf ${fn}
            if [ -r ${COMINukmet}.${PDY}/ukmet_hr_${PDY}${cyc}f${fcsthr} ]; then
                ln -s ${COMINukmet}.${PDY}/ukmet_hr_${PDY}${cyc}f${fcsthr} ${fn}
            fi


            export pgm=gdplot2_nc;. prep_step; startmsg

			cat > cmdfilemar  <<- EOF
				DEVICE	= ${device}
				PANEL	= 0
				TEXT	= m/22/1/1/hw
				CONTUR	= 2
				MAP	= 1
				CLEAR	= yes
				GAREA   = ${garea}
				PROJ    = ${proj}
				LATLON  = 1/10/1/2/10;10

				GLEVEL  = 500 
				GVCORD  = pres 
				SKIP    = 0 
				SCALE   = -1 
				GDPFUN  = sm9s(hght)
				TYPE    = c
				CINT    = ${level}
				FINT    =
				FLINE   =
				HILO    = 0
				HLSYM   = 0
				CLRBAR  = 0
				WIND    = 0
				REFVEC  =

				EOF

            WrottenZERO=0
            grids=${memberlist}
            line_count=2
            color_number=9
            for gridl in ${grids}
            do
                # ----- gridl -----
                gdfn=${gridl} 
                
                if [ ${gdfn} == c00 ]; then
                    color_number=6
                    sline_count="-2"
                    sCNTL="(CNTL)"

                else
        
                    if [[ ${gdfn} == p01 ]]; then
                        color_number=17
                    elif [[ ${gdfn} == p02 ]]; then
                        color_number=2
                    elif [[ ${gdfn} == p03 ]]; then
                        color_number=4
                    elif [[ ${gdfn} == p04 ]]; then
                        color_number=7
                    elif [[ ${gdfn} == p05 ]]; then
                        color_number=8
                    elif [[ ${gdfn} == p06 ]]; then
                        color_number=9
                    elif [[ ${gdfn} == p07 ]]; then
                        color_number=10
                    elif [[ ${gdfn} == p08 ]]; then
                        color_number=11
                    elif [[ ${gdfn} == p09 ]]; then
                        color_number=12
                    elif [[ ${gdfn} == p10 ]]; then
                        color_number=14
                    elif [[ ${gdfn} == p11 ]]; then
                        color_number=15
                    elif [[ ${gdfn} == p12 ]]; then
                        color_number=16
                    elif [[ ${gdfn} == p13 ]]; then
                        color_number=17
                    elif [[ ${gdfn} == p14 ]]; then
                        color_number=18
                    elif [[ ${gdfn} == p15 ]]; then
                        color_number=2
                    else
                        color_number=18
                    fi

                    sline_count="+${line_count}"
                    sCNTL=""

                    let line_count=$line_count+1
                fi

                if [ -e ${gdfn} ]; then

					cat >> cmdfilemar  <<- EOF
						GDFILE  = ${gdfn}
						LINE    = ${color_number}/1/1/0
						TITLE   = ${color_number}/${sline_count}/~ ? ${gdfn} ${sCNTL} |~${metaarea} ${level} DM
						GDATTIM = F${fcsthr}
						run

						EOF
                    if [ $WrottenZERO -eq 0 ]; then            
						cat >> cmdfilemar  <<- EOF
							MAP     = 0
							LATLON  = 0
							CLEAR   = no

							EOF
                    fi
                    WrottenZERO=1
                fi


            done

            # ----- gfs -----
            gdfn=gfs 
            if [ -e ${gdfn} ]; then
				cat >> cmdfilemar  <<- EOF
					GDFILE	= ${gdfn}
					LINE    = 3/1/3/0
					GDATTIM	= F${fcsthrsgfs}
					TITLE   = 3/+11/~ ? ${gdfn} 12Z YEST|~${metaarea} ${level} DM
					run

					EOF
                if [ $WrottenZERO -eq 0 ]; then            
					cat >> cmdfilemar  <<- EOF
						MAP     = 0
						LATLON  = 0
						CLEAR   = no

						EOF
                fi
                WrottenZERO=1
            fi

            # ----- ecmwf -----
            gdfn=ecmwf 
            if [ -e ${gdfn} ]; then
				cat >> cmdfilemar  <<- EOF
					GDFILE	= ${gdfn}
					LINE    = 31/1/2/0
					GDATTIM	= F${fcsthrsgfs}
					TITLE   = 31/+12/~ ? ECMWF 12Z YEST|~${metaarea} ${level} DM
					run

					EOF
                if [ $WrottenZERO -eq 0 ]; then            
					cat >> cmdfilemar  <<- EOF
						MAP     = 0
						LATLON  = 0
						CLEAR   = no

						EOF
                fi
                WrottenZERO=1
            fi

            # ----- ukmet -----
            gdfn=ukmet 
            if [ -e ${gdfn} ]; then
				cat >> cmdfilemar  <<- EOF
					GDFILE	= ${gdfn}
					LINE    = 26/2/2/0
					GDATTIM	= F${fcsthr}
					TITLE   = 26/+13/~ ? UKMET 00Z|~${metaarea} ${level} DM
					run

					EOF
            fi

            cat cmdfilemar
            gdplot2_nc < cmdfilemar

            #echo "before first err chk"
            export err=$?;err_chk
            echo "after first err chk"
        done
    done

    # GENERATE THE PMSL LOW CENTERS
    num=" "
    metashname="LOW CNTRS"
    for fcsthr in ${fcsthrs}
    do
        fcsthrsgfs=`expr ${fcsthr} + 12`
        #typeset -Z3 fcsthrsgfs
        fcsthrsgfs=$(printf %03i $fcsthrsgfs)

        fcsthrsgfs2=`expr ${fcsthr} - 6`
        #typeset -Z3 fcsthrsgfs2
        fcsthrsgfs2=$(printf %03i $fcsthrsgfs2)

        grids=${memberlist}
        for fn in `echo $grids`
        do
            rm -rf $fn 
            if [ -r $COMIN/ge${fn}_${sGrid}${PDY}${cyc}f${fcsthr} ]; then
                ln -s $COMIN/ge${fn}_${sGrid}${PDY}${cyc}f${fcsthr} $fn
            fi
        done

        fn=gfs
        rm -rf ${fn}
        #f [ -r $COMINs/gfs.${PDY}/gfs_${PDY}${cyc}f${fcsthr} ]
        # \$COMINsgfs/gfs.${yesterday}/${gfscyc}/gempak/gfs_${yesterday}${gfscyc}f${fcsthrsgfs}
        if [ -r $COMINsgfs/gfs.${yesterday}/${gfscyc}/gempak/gfs_${sGrid}${yesterday}${gfscyc}f${fcsthrsgfs} ]; then
            #  ln -s $COMINs/gfs.${PDY}/gfs_${PDY}${cyc}f${fcsthr} gfs
            ln -s $COMINsgfs/gfs.${yesterday}/${gfscyc}/gempak/gfs_${sGrid}${yesterday}${gfscyc}f${fcsthrsgfs} ${fn}
        fi

        fn=ecmwf
        rm -rf ${fn}
        if [ -r $COMINm1ecmwf.${ecmwfdate}/gempak/ecmwf_hr_${ecmwfdate}${ecmwfcyc}f${fcsthr} ]; then
            ln -s $COMINm1ecmwf.${ecmwfdate}/gempak/ecmwf_hr_${ecmwfdate}${ecmwfcyc}f${fcsthr} ${fn}
        fi

        fn=ukmet
        rm -rf ${fn}
        if [ -r $COMINukmet.${PDY}/ukmet_hr_${PDY}${cyc}f${fcsthr} ]; then
            ln -s $COMINukmet.${PDY}/ukmet_hr_${PDY}${cyc}f${fcsthr} ${fn}
        fi



        export pgm=gdplot2_nc;. prep_step; startmsg

		cat > cmdfilemar_low  <<- EOF
			DEVICE	= ${device}
			PANEL	= 0
			TEXT	= s/22/1/1/hw
			CONTUR	= 2
			MAP	= 1
			CLEAR	= yes
			GAREA   = ${garea}
			PROJ    = ${proj}
			LATLON  = 1/10/1/2/10;10

			GLEVEL  = 0 
			GVCORD  = none 
			SKIP    = 0 
			SCALE   = 0 
			GDPFUN  = pmsl
			TYPE    = c
			CINT    = 4/1/8
			LINE    = 0
			FINT    =
			FLINE   =

			HLSYM   = l/22/3/hw
			CLRBAR  = 0
			WIND    = 0
			REFVEC  =

			EOF
        WrottenZERO=0
        grids=${memberlist}
        line_count=2
        color_number=9
        for gridl in ${grids}
        do
            # ----- gridl -----
            gdfn=${gridl} 
            
            if [ ${gdfn} == c00 ]; then
                color_number=6
                sline_count="-2"
                sCNTL="(CNTL)"

            else
    
                if [[ ${gdfn} == p01 ]]; then
                    color_number=17
                elif [[ ${gdfn} == p02 ]]; then
                    color_number=2
                elif [[ ${gdfn} == p03 ]]; then
                    color_number=4
                elif [[ ${gdfn} == p04 ]]; then
                    color_number=7
                elif [[ ${gdfn} == p05 ]]; then
                    color_number=8
                elif [[ ${gdfn} == p06 ]]; then
                    color_number=9
                elif [[ ${gdfn} == p07 ]]; then
                    color_number=10
                elif [[ ${gdfn} == p08 ]]; then
                    color_number=11
                elif [[ ${gdfn} == p09 ]]; then
                    color_number=12
                elif [[ ${gdfn} == p10 ]]; then
                    color_number=14
                elif [[ ${gdfn} == p11 ]]; then
                    color_number=15
                elif [[ ${gdfn} == p12 ]]; then
                    color_number=16
                elif [[ ${gdfn} == p13 ]]; then
                    color_number=17
                elif [[ ${gdfn} == p14 ]]; then
                    color_number=18
                elif [[ ${gdfn} == p15 ]]; then
                    color_number=2
                else
                    color_number=18
                fi

                sline_count="+${line_count}"
                sCNTL=""

                let line_count=$line_count+1
            fi

            if [ -e ${gdfn} ]; then
				cat >> cmdfilemar_low  <<- EOF
					GDFILE  = ${gdfn}
					HILO    = ${color_number}/L${num}/900-1016/5/50/y
					TITLE   = ${color_number}/${sline_count}/~ ? ${gdfn} ${sCNTL} |~${metaarea} ${metashname}
					GDATTIM	= F${fcsthr}
					run

					EOF
                if [ $WrottenZERO -eq 0 ]; then            
					cat >> cmdfilemar_low  <<- EOF
						MAP     = 0
						LATLON  = 0
						CLEAR   = no

						EOF
                fi
                WrottenZERO=1
            fi

        done

        # ----- gfs -----
        gdfn=gfs 
        if [ -e ${gdfn} ]; then
			cat >> cmdfilemar_low  <<- EOF
				GDFILE	= ${gdfn}
				HILO    = 3/L${num}/900-1016/5/50/y
				TITLE   = 3/+11/~ ? ${gdfn} 12Z YEST|~${metaarea} ${metashname}
				GDATTIM	= F${fcsthrsgfs}
				run

				EOF
            if [ $WrottenZERO -eq 0 ]; then            
				cat >> cmdfilemar_low  <<- EOF
					MAP     = 0
					LATLON  = 0
					CLEAR   = no

					EOF
            fi
            WrottenZERO=1
        fi


        # ----- ecmwf -----
        gdfn=ecmwf 
        if [ -e ${gdfn} ]; then
			cat >> cmdfilemar_low  <<- EOF
				GDFILE	= ${gdfn}
				HILO    = 31/L${num}/900-1016/5/50/y
				TITLE   = 31/+12/~ ? ${gdfn} 12Z YEST|~${metaarea} ${metashname}
				GDATTIM	= F${fcsthrsgfs}
				run

				EOF
            if [ $WrottenZERO -eq 0 ]; then            
				cat >> cmdfilemar_low  <<- EOF
					MAP     = 0
					LATLON  = 0
					CLEAR   = no

					EOF
            fi
            WrottenZERO=1
        fi

        # ----- ukmet -----
        gdfn=ukmet 
        if [ -e ${gdfn} ]; then
			cat >> cmdfilemar_low  <<- EOF
				GDFILE	= ${gdfn}
				HILO    = 26/L${num}/900-1016/5/50/y
				TITLE   = 26/+13/~ ? ${gdfn} 00Z|~${metaarea} ${metashname}
				GDATTIM	= F${fcsthr}
				run

				EOF
        fi

        cat cmdfilemar_low
        gdplot2_nc < cmdfilemar_low

        echo "before 2nd err chk"
        export err=$?;err_chk
        echo "after 2nd err chk"
    done

    #####################################################
    # GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
    # WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
    # FOR THIS CASE HERE.
    #####################################################

    ls -l ${metaname}
    export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

    if [ $SENDCOM = "YES" ] ; then
        mv ${metaname} ${COMOUT}/gefs_${sGrid}${PDY}_${cyc}_${metatype}
        if [ $SENDDBN = "YES" ] ; then
            $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job ${COMOUT}/gefs_${sGrid}${PDY}_${cyc}_${metatype}
        fi
    fi
done

exit

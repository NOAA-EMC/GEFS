#!/bin/sh
#
# Metafile Script : gefs_avg_gempak_vgf.sh
#
# Log :
# J. Carr/PMB      12/25/2004     Pushed into production.
# A. Robson/HPC    07/06/2005     Changed map line thickness to 1.
# Luke Lin/NCO     02/22/2006     Modified for gefs
#
# Set Up Local Variables
#
set -x
export PS4='gefs_avg_vgf:$SECONDS + '
mkdir $DATA/gefs_avg_vgf
cd $DATA/gefs_avg_vgf
#cp $FIXgempak/datatype.tbl datatype.tbl

sGrid=0p50_

mdl=gefs_avg_vgf
MDL=GEFS_AVG_VGF
PDY2=`echo $PDY | cut -c3-`

# MAKE VG FILES FOR OPC

for fcsthrs in 096 108
do
    for area in NATL MPAC
    do
        ocean=`echo ${area} | cut -c2-`

        #export pgm=gdplot2_vg;. prep_step; startmsg

        fn=avg
        rm -rf $fn
        if [ -r $COMIN/ge${fn}_${sGrid}${PDY}${cyc}f${fcsthrs} ]; then
            ln -s $COMIN/ge${fn}_${sGrid}${PDY}${cyc}f${fcsthrs} $fn
        fi

		cat > cmdfile_vgf <<- EOF
			GDFILE  = avg
			GDATTIM = F${fcsthrs}
			GAREA   = ${area}
			PROJ    =
			DEVICE  = vg | gefs_avg500_${sGrid}${PDY2}_${cyc}_F${fcsthrs}_${ocean}.vgf
			GLEVEL  = 500
			GVCORD  = PRES
			PANEL   = 0
			SKIP    = 0
			SCALE   = -1
			GDPFUN  = hght!hght
			TYPE    = c!c
			CONTUR  = 0
			CINT    = 6/-99/558!6/570/999
			LINE    = 20/1/3/2/2/.13!20/1/3/2/2/.13
			FINT    =
			FLINE   =
			HILO    = 2;6/H#;L#///5;5!0
			HLSYM   = 3.7;2.5/2/22;31/3;3/hw!
			CLRBAR  =
			WIND    =
			REFVEC  =
			TITLE   =
			TEXT    = 1.5/21/2.2/hw
			CLEAR   = y
			STNPLT  =
			SATFIL  =
			RADFIL  =
			STREAM  =
			POSN    = 4
			COLORS  = 2
			MARKER  = 2
			GRDLBL  = 5
			LUTFIL  = none
			FILTER  = no
			MAP     = 0
			LATLON  = 0
			STNPLT  =
			list
			run

			CLEAR   = no
			GDPFUN  = hght!vge(kntv(wnd),30)
			TYPE    = c!b
			CINT    = 6/564/564
			LINE    = 20/1/6/2/2/.13
			WIND    = bk25/1.6/803/114
			HILO    =
			HLSYM   =
			list
			run

			CLEAR   = yes
			DEVICE  = vg|gefs_avgPMSL_${sGrid}${PDY2}_${cyc}_F${fcsthrs}_${ocean}.vgf
			GLEVEL  = 0
			GVCORD  = none
			SCALE   = 0
			GDPFUN  = sm5s(sm9s(pmsl))
			TYPE    = c
			CONTUR  = 1
			CINT    = 4
			LINE    = 5/1/3/-5/2/.13
			WIND    =
			TEXT    = 1.3/21/2/hw
			list
			run

			EOF

        cat cmdfile_vgf

        gdplot2_vg < cmdfile_vgf

        export err=$?;err_chk

        if [ $SENDCOM = "YES" ] ; then
            mv *.vgf ${COMOUT}
            if [ $SENDDBN = "YES" ] ; then
                ${DBNROOT}/bin/dbn_alert VGF OPC $job ${COMOUT}/${ocean}_gefs_avgPMSL_${sGrid}${PDY2}_${cyc}_F${fcsthrs}.vgf
                ${DBNROOT}/bin/dbn_alert VGF OPC $job ${COMOUT}/${ocean}_gefs_avg500_${sGrid}${PDY2}_${cyc}_F${fcsthrs}.vgf
            fi
        fi
    done
done

exit $err



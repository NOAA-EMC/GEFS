#! /bin/ksh

echo "$(date -u) begin ${.sh.file}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

export RUNMEM=$RUNMEM
export mem=$(echo $RUNMEM|cut -c3-5)
export machine="WCOSS_D35"

# Additional paths needed by child scripts
export HOMEgfs=${HOMEgfs:-$HOMEgefs}
export PARMgfs=$HOMEgfs/parm
export EXECgfs=$HOMEgfs/exec
export FIXgfs=$HOMEgfs/fix
export FIX_DIR=$FIXgfs
export FIX_AM=$FIX_DIR/fix_am
export FIXfv3=$FIX_DIR/fix_fv3_gmted2010
export FIXemi=${FIXemi:-$HOMEgefs/fix/fix_emission/emi_}
export SSTDIR=${SSTDIR:-$COMIN/atmos/cfssst}

export COMINWW3=${COMINWW3:-${COMIN}/wave}
export COMOUTWW3=${COMOUTWW3:-${COMIN}/wave}
export COMPONENTwave=${COMPONENTwave:-${RUN}.wave}

export ERRSCRIPT=err_chk
export LOGSCRIPT=startmsg

export memdir=$GESIN/init/$mem
export gmemdir=$GESIN/init/$mem
export ICSDIR=$GESIN/init/$mem
export RSTDIR=$GESIN/init/$mem/RESTART
export RSTDIR_TMP=$RSTDIR

if [[ $SENDCOM == "YES" ]]; then
	export FCSTDIR=$COMOUT/$COMPONENT/sfcsig
else
	export FCSTDIR=$DATA
fi


case $FORECAST_SEGMENT in
	hr) 
		echo "Integrate the model for the Half-Month range segment"
		filecount=$(ls -l $RSTDIR/*coupler* | wc -l )
		if (( filecount < 3 )); then
			export RERUN="NO" 
		else
			export RERUN="YES"
		fi

		FHINI=${FHINI:-0}
		 if [[ $RERUN != "YES" ]] ; then
			export CDATE_RST=$($NDATE +$FHINI $PDY$cyc)
		fi
		CASE=$CASEHR; FHMAX=$fhmaxh; FHOUT=$FHOUTLF; FHZER=6;
		MTNRSL=$MTNRSLFV; LONB=$LONBFV; LATB=$LATBFV;
		FHMAX_HF=$FHMAXHF; FHOUT_HF=$FHOUTHF; 
		FHINI=$FHINI;
		(( LEVS = LEVSHR + 1 ))
		;;
	lr)
		echo "Integrate the model for the Longer Range segment"
		FHINI=${FHINI:-$fhmaxh}
		CDATE_1=$($NDATE +$FHINI $PDY$cyc)
		fRestart=$RSTDIR/$(echo $CDATE_1 | cut -c1-8).$(echo $CDATE_1 | cut -c9-10)0000.coupler.res
		if [ -f $fRestart ]; then
			(( FHINI_2 = fhmaxh + restart_interval ))
			CDATE_2=$($NDATE +$FHINI_2 $PDY$cyc)
			#fRestart=$RSTDIR/${CDATE_2}.${cyc}0000.coupler.res
			fRestart=$RSTDIR/$(echo $CDATE_2 | cut -c1-8).$(echo $CDATE_2 | cut -c9-10)0000.coupler.res
			if [ -f $fRestart ]; then
				export CDATE_RST=
			else
				export CDATE_RST=$($NDATE +$FHINI $PDY$cyc)
			fi
		else
			echo "FATAL ERROR in ${.sh.file}: There is no $fRestart"
			export err=101
			exit $err
		fi
		export RERUN="YES"
		export cplwav=.false.
		CASE=$CASELR; FHMAX=$fhmax; FHOUT=$FHOUTLF; FHZER=6;
		MTNRSL=$MTNRSLLR; LONB=$LONBLR; LATB=$LATBLR;
		FHMAX_HF=$FHMAXHF; FHOUT_HF=$FHOUTHF;
		FHINI=$FHINI;
		(( LEVS = LEVSLR + 1 ))
		;;
	*)
		echo "FATAL ERROR in ${.sh.file}: Incorrect value of FORECAST_SEGMENT=$FORECAST_SEGMENT"
		export err=100
		exit $err
		;;
esac

mkdir -m 775 -p $FCSTDIR
mkdir -m 755 -p $GESIN/init/$mem/RESTART

export RERUN=${RERUN:-NO}

export FHMIN=$FHINI

#  The VEGTYPE fix file:
export FNVETC=${FNVETC:-${FIX_AM}/global_vegtype.igbp.t$MTNRSL.rg.grb}
export FNTSFC=${FNTSFC:-${FIX_AM}/RTGSST.1982.2012.monthly.clim.grb}
export FNAISC=${FNAISC:-${FIX_AM}/CFSR.SEAICE.1982.2012.monthly.clim.grb}
export FNABSC=${FNABSC:-${FIX_AM}/global_mxsnoalb.uariz.t$MTNRSL.rg.grb}
export FNALBC=$FIX_AM/global_snowfree_albedo.bosu.t$MTNRSL.rg.grb
export FNALBC2=$FIX_AM/global_albedo4.1x1.grb
export FNSMCC=$FIX_AM/global_soilmgldas.t$MTNRSL.grb
export FNSOTC=$FIX_AM/global_soiltype.statsgo.t$MTNRSL.rg.grb

if [[ $sst2tier = YES ]]; then
	export nstf_name="2,0,0,0,0"
	export FTSFS=0
	export FNTSFA=$SSTDIR/TMPsfc.$PDY$cyc.24hr.anom.grb
fi

export ENS_NUM=1

#
# Forecast Input Variables
#
export fhstoch=$restart_interval

if [[ $FORECAST_SEGMENT = hr ]] ; then
	(( FHMAX = FHMAX + 1 ))
fi

if [[ $RERUN = "YES" ]] ; then
	export warm_start=.true.
	export restart_hour=$FHMIN
	export restart_run=.true.    
	export output_1st_tstep=.true.
	export stochini=${stochini:-".true."} #true=read in pattern, false=initialize from seed
else
	export stochini=${stochini:-".false."} #true=read in pattern, false=initialize from seed
fi

#
# Forecast Input Variables
#
#--------------------------------------------
export KMP_AFFINITY=disabled
export OMP_STACKSIZE=1024m
export MP_LABELIO=yes

if [[ $mem = c00 ]] ;then 
	MEMBER=$((npert+1))
	WAV_MEMBER="00"
elif [[ $mem = aer ]] ;then 
	MEMBER="00"
	WAV_MEMBER="00"
else
	MEMBER=$(echo $mem|cut -c2-3)
	WAV_MEMBER=$MEMBER
fi
export MEMBER=$MEMBER
memchar=mem$(printf %03i $MEMBER)

export SET_STP_SEED=${SET_STP_SEED:-"YES"}

if [[ $FORECAST_SEGMENT = hr ]] ; then

	if [[ $DO_SPPT = YES ]] ; then
		export SPPT=$SPPT_hr
		export ISEED_SPPT=$ISEED_SPPT_hr
		export SPPT_LOGIT=$SPPT_LOGIT_hr
		export SPPT_TAU=$SPPT_TAU_hr
		export SPPT_LSCALE=$SPPT_LSCALE_hr
		export sppt_sfclimit=$sppt_sfclimit_hr
	fi

	if [[ $DO_SHUM = YES ]] ; then
		export SHUM=$SHUM_hr
		export ISEED_SHUM=$ISEED_SHUM_hr
		export SHUM_TAU=$SHUM_TAU_hr
		export SHUM_LSCALE=$SHUM_LSCALE_hr
	fi

	if [[ $DO_SKEB = YES ]] ; then
		export SKEB=$SKEB_hr
		export ISEED_SKEB=$ISEED_SKEB_hr
		export SKEB_TAU=$SKEB_TAU_hr
		export SKEB_LSCALE=$SKEB_LSCALE_hr
		export SKEBNORM=${SKEBNORM:-"1"}
	fi
fi

if [[ $FORECAST_SEGMENT = lr ]] ; then

	if [[ $DO_SPPT = YES ]] ; then
		export SPPT=$SPPT_hr
		export ISEED_SPPT=$ISEED_SPPT_hr
		export SPPT_LOGIT=$SPPT_LOGIT_hr
		export SPPT_TAU=$SPPT_TAU_hr
		export SPPT_LSCALE=$SPPT_LSCALE_hr
		export sppt_sfclimit=$sppt_sfclimit_hr
	fi

	if [[ $DO_SHUM = YES ]] ; then
		export SHUM=$SHUM_hr
		export ISEED_SHUM=$ISEED_SHUM_hr
		export SHUM_TAU=$SHUM_TAU_hr
		export SHUM_LSCALE=$SHUM_LSCALE_hr
	fi

	if [[ $DO_SKEB = YES ]] ; then
		export SKEB=$SKEB_hr
		export ISEED_SKEB=$ISEED_SKEB_hr
		export SKEB_TAU=$SKEB_TAU_hr
		export SKEB_LSCALE=$SKEB_LSCALE_hr
		export SKEBNORM=${SKEBNORM:-"1"}
	fi
fi

#
# Forecast Input Variables
#
export fcstscript=${fcstscript:-$HOMEgfs/scripts/exglobal_fcst_nemsfv3gfs.sh}
export FORECASTSH=$fcstscript
export FCSTEXECDIR=${FCSTEXECDIR:-$EXECgfs}
export PARM_FV3DIAG=${PARM_FV3DIAG:-$PARMgfs/parm_fv3diag}
export ROTDIR=${ROTDIR:-$DATA}

export APRUN=${APRUN:-""}

echo $fcstscript
echo $FORECASTSH
echo $FCSTEXECDIR
echo $FCSTEXEC
echo $PARM_FV3DIAG
echo $APRUN

NCP=${NCP:-"/bin/cp -p"}

SEND=NO
export VERBOSE=YES

#echo "-----end of CONFIG in ${.sh.file} --------"

################################################################################
export CDATE=$PDY$cyc
export rCDUMP=$RUNMEM
export CDUMP=$RUNMEM

if [[ $cplwav = ".true." ]]; then
	# CPU partitioning
	export npe_wav=${npe_wav:-88}
	export npe_fcst_wav=$(( npe_fv3 + npe_wav ))
	export atm_petlist_bounds=" 0 $((npe_fv3-1))"
	export wav_petlist_bounds=" $((npe_fv3)) $((npe_fcst_wav-1))"
	export npe_fv3=$npe_fcst_wav
	export NTASKS_FV3=$npe_fv3
fi

if [[ $cplchm = ".true." ]]; then
	CHEMIN=${CHEMIN:-$memdir/chem}
	if [[ ! -d $CHEMIN ]]; then
		echo "FATAL: cplchm is .true. but there is no chem input directry at $CHEMIN"
		exit 200
	fi

	# Check which smoke emissions we are using
	gbbepx_found=0
	any_found=0
	for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
		if [[ -f $CHEMIN/$tile/plumefrp.dat ]]; then (( gbbepx_found = gbbepx_found + 1 )); fi
		if [[ -f $CHEMIN/$tile/plumestuff.dat ]]; then (( any_found = any_found + 1 )); fi
	done

	if (( gbbepx_found == 6 )); then
		# Using GBBEPx
		echo "GBBEPx smoke emissions detected and will be used"
		export EMITYPE=2
	elif (( any_found == 6 )); then
		# Using MODIS
		echo "WARNING: Detected missing of incomplete GBBEPx emisssions. Will use alternate emissions but forecast may be degraded!"
		echo "  Check $CHEMIN and your prep emission job if this is unexpected."
		msg="WARNING: ${job} detected incomplete GBBEPx emissions, will continue with alternate emissions but forecast may be degraded!"
		echo "$msg" | mail.py -c $MAIL_LIST
		export EMITYPE=1
	else
		echo "FATAL ERROR in ${.sh.file}: cplchm is .true. but smoke emissions are not available!"
		echo "  Check input directory $CHEMIN"
		export err=10
		exit $err
	fi

	if [[ $SENDCOM == "YES" ]]; then
		# Link restart files for next cycle
		mkdir -m 775 -p $COMOUT/$COMPONENT/restart

		next_date=$($NDATE +$gefs_cych $CDATE)
		next_PDY=$(echo $next_date | cut -c1-8)
		next_cyc=$(echo $next_date | cut -c9-10)
		COM_RSTDIR=$COMOUT/$COMPONENT/restart
		if [[ ! -d $RSTDIR ]]; then mkdir -p $RSTDIR; fi
		ln -sf $COM_RSTDIR/${next_PDY}.${next_cyc}0000.coupler.res $RSTDIR/${next_PDY}.${next_cyc}0000.coupler.res
		ln -sf $COM_RSTDIR/${next_PDY}.${next_cyc}0000.fv_core.res.nc $RSTDIR/${next_PDY}.${next_cyc}0000.fv_core.res.nc
		for kind in fv_tracer.res fv_core.res fv_srf_wnd.res phy_data sfc_data; do
			for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
				ln -sf $COM_RSTDIR/${next_PDY}.${next_cyc}0000.${kind}.${tile}.nc $RSTDIR/${next_PDY}.${next_cyc}0000.${kind}.${tile}.nc
			done
		done
	fi

fi # [[ $cplchm = ".true." ]]

export increment_file=$ICSDIR/fv3_increment.nc

export FIX_DIR=$FIXgfs
export FIX_AM=$FIX_AM

# Increment is never used for a rerun
if [[ $RERUN == "YES" ]]; then
	export read_increment=".false."
fi

if [[ $read_increment = ".true." && $warm_start = ".true." ]]; then
	# Make sure increment file exists before using warm start
	if [[ ! -f $increment_file ]]; then
		echo "WARNING: Warm-start requested but no increment file present, reverting to cold-start!"
		export warm_start=".false."
		export read_increment=".false."
	fi
fi

########################################################
## Execute the script.
$FORECASTSH
export err=$?
if [[ $err != 0 ]]; then
	echo "FATAL ERROR in ${.sh.file}: received a non-zero return code from $FORECASTSH"
	exit $err
fi

echo "$(date -u) end ${.sh.file}"

exit $err

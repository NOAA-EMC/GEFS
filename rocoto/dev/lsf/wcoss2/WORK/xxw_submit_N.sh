#!/bin/bash

# qdel `qselect -u $USER`

set -xa
export PS4='$SECONDS + $(basename $(basename ${0}))[$LINENO] '

export PDY=${PDY:-20210824}
export cyc=${cyc:-00}
export EXPID=${EXPID:-"gefs_wcoss2_canned_test"}
DoLR=${DoLR:-yes}
DoLR_Sep=${DoLR_Sep:-yes} # Run lr like prod
if [[ $DoLR_Sep == "no" ]]; then
    if [[ $cyc == "00" ]]; then
        DoLR=yes
    else
        DoLR=no
    fi
fi

npert=${npert:-30}

DoAllMEMs=yes # "yes" means doing all members in one time
if [[ $DoAllMEMs == no ]]; then
    mem_per_group=5
fi
  
memberlist="c00"
imem=1
while [[ imem -le $npert ]];
do
    memberlist="$memberlist p$(printf %02i $imem)"
    (( imem++ ))
done
echo $memberlist


#export PBS_O_WORKDIR=`pwd`/tmp

getcfssst=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jatmos_getcfssst_${cyc} ../init/atmos/jgefs_atmos_getcfssst.ecf)
wave_init=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jwave_init_${cyc} ../init/wave/jgefs_wave_init.ecf)

atmos_prep_dep=""
imem=1
for mem in $memberlist
do
    #sed -e "s/c00/$mem/g" ../d0_16/atmos/jgefs_atmos_prep.ecf > jgefs_atmos_prep.ecf_$mem
    #atmos_prep[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" jgefs_atmos_prep.ecf_$mem)
    atmos_prep[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID},MEMBER=${mem}" -N jatmos_prep_${mem}_${cyc} ../d0_16/atmos/jgefs_atmos_prep.ecf)
    atmos_prep_dep="$atmos_prep_dep:${atmos_prep[$imem]}"
    (( imem++ ))
done


atmos_init=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jatmos_init_recenter_${cyc} -W depend=afterok${atmos_prep_dep} ../init/atmos/jgefs_atmos_init_recenter.ecf)

# -- wave_prep
wave_prep_dep=""
imem=1
for mem in ${memberlist}
do
    # sed -e "s/c00/$mem/g" ../d0_16/wave/jgefs_wave_prep.ecf > jgefs_wave_prep.ecf_$mem
    # wave_prep[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -W depend=afterok:$wave_init jgefs_wave_prep.ecf_$mem)
    wave_prep[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID},MEMBER=${mem}" -N jwave_prep_${mem}_${cyc} -W depend=afterok:$wave_init ../d0_16/wave/jgefs_wave_prep.ecf)
    wave_prep_dep="$wave_prep_dep:${wave_prep[$imem]}"
    (( imem++ ))
done

# --- chem
chem_prep=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jchem_prep_emissions_${cyc} ../chem/jgefs_chem_prep_emissions.ecf)
chem_init=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jchem_init_${cyc} -W depend=afterok:$chem_prep:$atmos_init ../chem/jgefs_chem_init.ecf)
chem_fcst=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jchem_forecast_${cyc} -W depend=afterok:$chem_init:$getcfssst ../chem/jgefs_chem_forecast.ecf)
chem_post=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jchem_post_${cyc} -W depend=after:$chem_fcst ../chem/jgefs_chem_post.ecf)
chem_prdgen=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jchem_prdgen_${cyc} -W depend=after:$chem_post ../chem/jgefs_chem_prdgen.ecf)


#------
atmos_prdgen_gfs=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jatmos_prdgen_gfs_${cyc} ../post_processing/d0_16/atmos/jgefs_atmos_prdgen_gfs.ecf)

atmos_prdgen_dep=""
wave_post_dep=""
imem=1
for mem in $memberlist
do
    # sed -e "s/c00/$mem/g" ../d0_16/jgefs_forecast.ecf > jgefs_forecast.ecf_$mem
    if [[ $DoAllMEMs == "yes" ]]; then
        if [[ $imem == 1 ]]; then
            fcst[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID},MEMBER=${mem}" -N jforecast_${mem}_${cyc} -W depend=afterok:$getcfssst:$atmos_init:${wave_prep[$imem]} ../d0_16/jgefs_forecast.ecf)
        else
            fcst[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID},MEMBER=${mem}" -N jforecast_${mem}_${cyc} -W depend=afterok:$getcfssst:$atmos_init:${wave_prep[$imem]}:${wave_prep[1]} ../d0_16/jgefs_forecast.ecf)
        fi
    else
        num_mems_for_first_group=$(( mem_per_group + 1 ))
        if (( imem > num_mems_for_first_group )); then
            imem_adj=$(( imem - mem_per_group ))
            fcst[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID},MEMBER=${mem}" -N jforecast_${mem}_${cyc} -W depend=afterok:$getcfssst:$atmos_init:${wave_prep[$imem]}:${wave_prep[1]}:${fcst[$imem_adj]} ../d0_16/jgefs_forecast.ecf)
        else
            if [[ $imem == 1 ]]; then
                fcst[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID},MEMBER=${mem}" -N jforecast_${mem}_${cyc} -W depend=afterok:$getcfssst:$atmos_init:${wave_prep[$imem]} ../d0_16/jgefs_forecast.ecf)
            else
                fcst[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID},MEMBER=${mem}" -N jforecast_${mem}_${cyc} -W depend=afterok:$getcfssst:$atmos_init:${wave_prep[$imem]}:${wave_prep[1]} ../d0_16/jgefs_forecast.ecf)
            fi
        fi
    fi

    #sed -e "s/c00/$mem/g" ../d0_16/atmos/jgefs_atmos_post.ecf > jgefs_atmos_post.ecf_$mem
    #atmos_post[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -W depend=after:${fcst[$imem]} jgefs_atmos_post.ecf_$mem)
    atmos_post[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID},MEMBER=${mem}" -N jatmos_post_${mem}_${cyc} -W depend=after:${fcst[$imem]} ../d0_16/atmos/jgefs_atmos_post.ecf)

    #sed -e "s/c00/$mem/g" ../d0_16/atmos/jgefs_atmos_prdgen.ecf > jgefs_atmos_prdgen.ecf_$mem
    #atmos_prdgen[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -W depend=after:${atmos_post[$imem]} jgefs_atmos_prdgen.ecf_$mem)
    atmos_prdgen[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID},MEMBER=${mem}" -N jatmos_prdgen_${mem}_${cyc} -W depend=after:${atmos_post[$imem]} ../d0_16/atmos/jgefs_atmos_prdgen.ecf)
    atmos_prdgen_dep="$atmos_prdgen_dep:${atmos_prdgen[$imem]}"

    #sed -e "s/c00/$mem/g" ../d0_16/atmos/jgefs_atmos_postsnd.ecf > jgefs_atmos_postsnd.ecf_$mem
    #atmos_postsnd[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -W depend=after:${fcst[$imem]} jgefs_atmos_postsnd.ecf_$mem)
    atmos_postsnd[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID},MEMBER=${mem}" -N jatmos_postsnd_${mem}_${cyc} -W depend=after:${fcst[$imem]} ../d0_16/atmos/jgefs_atmos_postsnd.ecf)

    #sed -e "s/c00/$mem/g" ../d0_16/jgefs_fcst_post_manager.ecf > jgefs_fcst_post_manager.ecf_$mem
    #fcst_post_manager[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -W depend=after:${fcst[$imem]} jgefs_fcst_post_manager.ecf_$mem)
    fcst_post_manager[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID},MEMBER=${mem}" -N jfcst_post_manager_${mem}_${cyc} -W depend=after:${fcst[$imem]} ../d0_16/jgefs_fcst_post_manager.ecf)

    # --- wave
    #sed -e "s/c00/$mem/g" ../d0_16/wave/jgefs_wave_post.ecf > jgefs_wave_post.ecf_$mem
    #wave_post[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -W depend=after:${fcst[$imem]} jgefs_wave_post.ecf_$mem)
    wave_post[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID},MEMBER=${mem}" -N jwave_post_${mem}_${cyc} -W depend=after:${fcst[$imem]} ../d0_16/wave/jgefs_wave_post.ecf)
    wave_post_dep="$wave_post_dep:${wave_post[$imem]}"

    #sed -e "s/c00/$mem/g" ../d0_16/wave/jgefs_wave_gempak.ecf > jgefs_wave_gempak.ecf_$mem
    #wave_gempak[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -W depend=after:${wave_post[$imem]} jgefs_wave_gempak.ecf_$mem)
    wave_gempak[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID},MEMBER=${mem}" -N jwave_gempak_${mem}_${cyc} -W depend=after:${wave_post[$imem]} ../d0_16/wave/jgefs_wave_gempak.ecf)

    (( imem++ ))
done

atmos_ensstat=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jatmos_ensstat_${cyc} -W depend=after${atmos_prdgen_dep} ../post_processing/d0_16/atmos/jgefs_atmos_ensstat.ecf)
atmos_enspost=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jatmos_enspost_${cyc} -W depend=afterok${atmos_prdgen_dep}:$atmos_prdgen_gfs ../post_processing/d0_16/atmos/jgefs_atmos_enspost.ecf)

atmos_ensavg_nemsio=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jatmos_ensavg_nemsio_${cyc} -W depend=after${atmos_prdgen_dep} ../post_processing/d0_16/atmos/jgefs_atmos_ensavg_nemsio.ecf)

atmos_avg_postsnd=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jatmos_avg_postsnd_${cyc} -W depend=after:$atmos_ensavg_nemsio ../post_processing/d0_16/atmos/jgefs_atmos_avg_postsnd.ecf)

atmos_gempak=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jatmos_gempak_${cyc} -W depend=after:${atmos_ensstat}${atmos_prdgen_dep} ../gempak/atmos/jgefs_atmos_gempak.ecf)
atmos_gempak_meta=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jatmos_gempak_meta_${cyc} -W depend=afterok:$atmos_gempak ../gempak/atmos/jgefs_atmos_gempak_meta.ecf)
atmos_avgspr_gempak_meta=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jatmos_avgspr_gempak_meta_${cyc} -W depend=afterok:$atmos_gempak ../gempak/atmos/jgefs_atmos_avgspr_gempak_meta.ecf)

wave_stat=$(qsub -v "PDY=${PDY},cyc=${cyc},EXPID=${EXPID}" -N jwave_stat_${cyc} -W depend=afterok$wave_post_dep ../post_processing/d0_16/wave/jgefs_wave_stat.ecf)


if [[ $DoLR == "yes" ]]; then

    memberlist_lr=""
    case $cyc in
        00)
          memberlist_lr="p01 p02 p03 p04 p05 p06 p07 p08"
          ;;
        06)
          memberlist_lr="p09 p10 p11 p12 p13 p14 p15 p16"
          ;;
        12)
          memberlist_lr="p17 p18 p19 p20 p21 p22 p23 p24"
          ;;
        18)
          memberlist_lr="c00 p25 p26 p27 p28 p29 p30"
          ;;
        *)
          echo "Invalid cyc: $cyc"
          break
          ;;
    esac

    if [[ $DoLR_Sep == "yes" ]]; then
      cyc_lr=00
    else
      cyc_lr=$cyc
      memberlist_lr=$memberlist
    fi

    #--d
    atmos_prdgen_lr_dep=""
    imem=1
    for mem in $memberlist_lr
    do
        #sed -e "s/c00/$mem/g" ../d16_35/atmos/jgefs_atmos_forecast.ecf > jgefs_atmoslr_forecast.ecf_$mem
        #atmos_fcst_lr[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc_lr},EXPID=${EXPID}" -W depend=afterok:${fcst[$imem]} jgefs_atmoslr_forecast.ecf_$mem)
        atmos_fcst_lr[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc_lr},EXPID=${EXPID},MEMBER=${mem}" -N jatmos_forecast_lr_${mem}_${cyc_lr} -W depend=afterok:${atmos_prdgen[$imem]} ../d16_35/atmos/jgefs_atmos_forecast.ecf)

        #sed -e "s/c00/$mem/g" ../d16_35/atmos/jgefs_atmos_post.ecf > jgefs_atmoslr_post.ecf_$mem
        #atmos_post_lr[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc_lr},EXPID=${EXPID}" -W depend=after:${atmos_fcst_lr[$imem]} jgefs_atmoslr_post.ecf_$mem)
        atmos_post_lr[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc_lr},EXPID=${EXPID},MEMBER=${mem}" -N jatmos_post_lr_${mem}_${cyc_lr} -W depend=after:${atmos_fcst_lr[$imem]} ../d16_35/atmos/jgefs_atmos_post.ecf)

        #sed -e "s/c00/$mem/g" ../d16_35/atmos/jgefs_atmos_prdgen.ecf > jgefs_atmoslr_prdgen.ecf_$mem
        #atmos_prdgen_lr[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc_lr},EXPID=${EXPID}" -W depend=after:${atmos_post_lr[$imem]} jgefs_atmoslr_prdgen.ecf_$mem)
        atmos_prdgen_lr[$imem]=$(qsub -v "PDY=${PDY},cyc=${cyc_lr},EXPID=${EXPID},MEMBER=${mem}" -N jatmos_prdgen_lr_${mem}_${cyc_lr} -W depend=after:${atmos_post_lr[$imem]} ../d16_35/atmos/jgefs_atmos_prdgen.ecf)
        atmos_prdgen_lr_dep="$atmos_prdgen_lr_dep:${atmos_prdgen_lr[$imem]}"

        (( imem++ ))
    done


    if [[ $cyc == 18 ]]; then
        atmos_ensstat_lr=$(qsub -v "PDY=${PDY},cyc=${cyc_lr},EXPID=${EXPID}" -N jatmos_ensstat_lr_${cyc_lr} -W depend=after${atmos_prdgen_lr_dep} ../post_processing/d16_35/atmos/jgefs_atmos_ensstat.ecf)
        atmos_ens_lr=$(qsub -v "PDY=${PDY},cyc=${cyc_lr},EXPID=${EXPID}" -N jatmos_enspost_lr_${cyc_lr} -W depend=afterok${atmos_prdgen_lr_dep} ../post_processing/d16_35/atmos/jgefs_atmos_enspost.ecf)
    fi
fi

#!/bin/ksh
set -ex

#--make symbolic links for EMC installation and hardcopies for NCO delivery

while getopts e:m: option
do
    case "${option}"
    in
        e) RUN_ENVIR=${OPTARG};;
        m) machine=${OPTARG};;

    esac

done

RUN_ENVIR=${RUN_ENVIR:-emc}
machine=${machine:-dell}

echo $RUN_ENVIR
echo $machine

LINK="ln -fs"
[[ $RUN_ENVIR = nco ]] && LINK="cp -rp"

pwd=$(pwd -P)

#------------------------------
#--model fix fields
#------------------------------
if [ $machine == "cray" ]; then
    FIX_DIR="/gpfs/hps3/emc/ensemble/noscrub/emc.enspara/common/git/fv3gefs/fix_sst721_short"
    FIX_DIR_FV3="/gpfs/hps3/emc/global/noscrub/emc.glopara/git/fv3gfs/fix"
elif [ $machine = "dell" ]; then
    FIX_DIR="/gpfs/dell2/emc/verification/noscrub/emc.enspara/common/git/fv3gefs/fix_sst721_short"
    FIX_DIR_FV3="/gpfs/dell2/emc/modeling/noscrub/emc.glopara/git/fv3gfs/fix"
elif [ $machine = "theia" ]; then
    FIX_DIR="/scratch4/NCEPDEV/ensemble/noscrub/common/git/fv3gefs/fix_sst721"
    FIX_DIR_FV3="/scratch4/NCEPDEV/global/save/glopara/git/fv3gfs/fix"
elif [ $machine = "hera" ]; then
    FIX_DIR="/scratch2/NCEPDEV/ensemble/noscrub/common/git/fv3gefs/fix_sst721_short"
    FIX_DIR_FV3="/scratch1/NCEPDEV/global/glopara/fix"
fi

# Delete Fix folder and relink/recopy it
cd ${pwd}/../fix
if [[ -d fix_gefs ]]; then
    echo "Fix folder exists, deleting it..."
    rm -f fix_gefs
fi
$LINK $FIX_DIR fix_gefs
cd ${pwd}

if [[ -d global-workflow.fd ]] ; then
    cd ${pwd}/../fix
    # fix_am
    sFolder=fix_am
    if [[ -d $sFolder ]]; then
         rm -rf $sFolder
    fi
    $LINK $FIX_DIR_FV3/$sFolder $sFolder

    # fix_fv3_gmted2010/C384
    sFolder=fix_fv3_gmted2010/C384
    if [[ -d $sFolder ]]; then
        rm -rf $sFolder
    fi
    sFolder2=fix_fv3_gmted2010
    if [[ ! -d $sFolder2 ]]; then
         mkdir $sFolder2
    fi
    $LINK $FIX_DIR_FV3/$sFolder $sFolder

    cd ${pwd}
fi

# global-workflow
cd $pwd
if [[ -d global-workflow.fd ]] ; then
    if [[ ! -L global-workflow.fd ]] ; then
        echo "not link"
        cd global-workflow.fd/sorc
        ./link_fv3gfs.sh $RUN_ENVIR $machine
        cd ../../
    fi
fi

# copy/link exec files
if [[ -d global-workflow.fd ]] ; then
    $LINK ../sorc/global-workflow.fd/sorc/ufs_utils.fd/exec/nemsio_read ../exec/
    $LINK ../sorc/global-workflow.fd/sorc/ufs_utils.fd/exec/global_chgres ../exec/
fi


# Copy/Link ush files
if [[ -d global-workflow.fd ]] ; then
    $LINK ../sorc/global-workflow.fd/sorc/ufs_utils.fd/ush/global_chgres_driver.sh ../ush/
    $LINK ../sorc/global-workflow.fd/sorc/ufs_utils.fd/ush/global_chgres.sh ../ush/
fi



exit 0

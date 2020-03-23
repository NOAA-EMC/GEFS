# =======================================================
def config_tasknames(dicBase):
    sVarName = 'taskname_num'.upper()
    iTaskName_Num = int(dicBase[sVarName])

    if iTaskName_Num > 0:
        if DoesTaskExist(dicBase, "post_high"):
            Replace_task_UsingSubjobs(dicBase, "post_high", sNSubJobs='N_SUBJOBS_POST_HIGH')

        if DoesTaskExist(dicBase, "post_aerosol"):
            Replace_task_UsingSubjobs(dicBase, "post_aerosol", sNSubJobs='N_SUBJOBS_POST_HIGH')

        if DoesTaskExist(dicBase, "ensavg_nemsio"):
            Replace_task_UsingSubjobs(dicBase, "ensavg_nemsio", sNSubJobs='N_SUBJOBS_ENSAVG_NEMSIO')

    if iTaskName_Num <= 0:
        iTaskName_Num = 0

        # #    <!-- RUN_GETCFSSST jobs -->
        if dicBase['RUN_GETCFSSST'].upper()[0] == "Y":
            # ---sigchgres
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "getcfssst"

        if dicBase['RUN_WAVE_PREP'].upper()[0] == "Y":
            # ---wave init
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "wave_init"

            # ---wave prep
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "wave_prep"

        # #   <!-- initial jobs -->
        if dicBase['RUN_INIT'].upper() == "FV3_COLD":
            # ---init_fv3chgrs
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "init_fv3chgrs"

            # ---init_recenter
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "init_recenter"

        elif dicBase['RUN_INIT'] == "FV3_WARM":
            # ---init_recenter
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "init_recenter"

        elif dicBase['RUN_INIT'] == "COPY_INIT":
            # ---copy_init
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "copy_init"

        if dicBase['KEEP_INIT'].upper()[0] == "Y" and dicBase['RUN_INIT'] != "COPY_INIT":
            # ---keep_init
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "keep_init"

        if dicBase['RUN_AEROSOL_MEMBER'].upper()[0] == "Y":
            for task in ['prep_emissions', 'init_aerosol', 'forecast_aerosol', 'post_aerosol', 'prdgen_aerosol']:
                iTaskName_Num += 1
                sTaskName = "taskname_{0}".format(iTaskName_Num)
                dicBase[sTaskName.upper()] = task

        # #    <!-- high resolution forecast and post process jobs -->
        if dicBase['RUN_FORECAST_HIGH'].upper()[0] == "Y":
            # ---forecast_high
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "forecast_high"

            # ---post_high
            iTaskName_Num = Add_Subjobs_to_dicBase(dicBase, iTaskName_Num, taskname="post_high", sNSubJobs='N_SUBJOBS_POST_HIGH')

            # ---prdgen_high
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "prdgen_high"

            # ---ensstat_high
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "ensstat_high"

            if dicBase['cplwav'] == ".true.":
                # ---wave_post
                iTaskName_Num += 1
                sTaskName = "taskname_{0}".format(iTaskName_Num)
                dicBase[sTaskName.upper()] = "wave_post"

                # ---wave_stat
                iTaskName_Num += 1
                sTaskName = "taskname_{0}".format(iTaskName_Num)
                dicBase[sTaskName.upper()] = "wave_stat"

        # #    <!-- RUN_PRDGEN_GFS jobs -->
        if dicBase['RUN_PRDGEN_GFS'].upper()[0] == "Y":
            # ---sigchgres
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "prdgen_gfs"

        # #    <!-- low resolution forecast and post process jobs -->
        if dicBase['RUN_FORECAST_LOW'].upper()[0] == "Y":
            # ---forecast_low
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "forecast_low"

            # ---post_low
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "post_low"

            # ---prdgen_low
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "prdgen_low"

            # ---ensstat_low
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "ensstat_low"

        # #    <!-- gempak jobs -->
        if dicBase['RUN_GEMPAK'].upper()[0] == "Y":
            # ---gempak
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "gempak"

            # ---avgspr_gempak
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "avgspr_gempak"

            WHERE_AM_I = dicBase['WHERE_AM_I']
            if WHERE_AM_I.upper() not in ["hera".upper()]:
                # ---avg_gempak_vgf There is no gdplot2_vg on hera, so this task can not run.
                iTaskName_Num += 1
                sTaskName = "taskname_{0}".format(iTaskName_Num)
                dicBase[sTaskName.upper()] = "avg_gempak_vgf"

            # ---avgspr_gempak_meta
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "avgspr_gempak_meta"

            # ---gempak_meta
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "gempak_meta"

        # #    <!-- postsnd  Post Sound -->
        if dicBase['RUN_POSTSND'].upper()[0] == "Y":
            # ---ensavg_nemsio
            iTaskName_Num = Add_Subjobs_to_dicBase(dicBase, iTaskName_Num, taskname="ensavg_nemsio", sNSubJobs='N_SUBJOBS_ENSAVG_NEMSIO')

            # ---postsnd
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "postsnd"

        # #    <!-- track and gensis jobs -->
        if dicBase['RUN_TRACK'].upper()[0] == "Y":
            # ---enkf_track
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "post_track"

            # ---post_genesis
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "post_genesis"

        # #    <!-- other jobs -->
        if dicBase['RUN_OTHERS'].upper()[0] == "Y":
            # ---enspost
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "enspost"
            
            # ---cqpf
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "cqpf"

        # #    <!-- RUN_CLEANUP -->
        if dicBase['RUN_CLEANUP'].upper()[0] == "Y":
            # ---keep_data_atm
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "keep_data_atm"
            if dicBase['cplwav'] == ".true.":
                # ---keep_data_wave
                iTaskName_Num += 1
                sTaskName = "taskname_{0}".format(iTaskName_Num)
                dicBase[sTaskName.upper()] = "keep_data_wave"
            # ---archive_atm
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "archive_atm"
            if dicBase['cplwav'] == ".true.":
                # ---archive_wave
                iTaskName_Num += 1
                sTaskName = "taskname_{0}".format(iTaskName_Num)
                dicBase[sTaskName.upper()] = "archive_wave"
            # ---cleanup_atm
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "cleanup_atm"
            if dicBase['cplwav'] == ".true.":
                # ---cleanup_wave
                iTaskName_Num += 1
                sTaskName = "taskname_{0}".format(iTaskName_Num)
                dicBase[sTaskName.upper()] = "cleanup_wave"

        # final
        dicBase[sVarName] = iTaskName_Num

# =======================================================
def create_metatask_task(dicBase, taskname="init_fv3chgrs", sPre="\t", GenTaskEnt=False):
    # --------------------------
    WHERE_AM_I = dicBase['WHERE_AM_I']
    sWalltime, sNodes, sMemory, sJoin, sDep, sQueue, sPartition = get_param_of_task(dicBase, taskname)

    metatask_names = get_metatask_names(taskname=taskname)

    jobname = get_jobname(taskname)
    if taskname in metatask_names:
        jobname += "_#member#"
    # --------------------------

    cycledef = "gefs"
    maxtries = 1

    strings = ""

    if taskname in metatask_names:
        sPre_2 = sPre + "\t\t"
    else:
        sPre_2 = sPre + "\t"

    if GenTaskEnt:
        sENV_VARS = sPre_2 + "&ENV_VARS;\n"
        sDATE_VARS = sPre_2 + "&DATE_VARS;\n"
    else:
        sENV_VARS = get_ENV_VARS(sPre_2)
        sDATE_VARS = get_DATE_VARS(sPre_2)

    strings += sPre + '<!-- **********{0}********** -->\n'.format(taskname)

    # For Specific need of the task
    if taskname in metatask_names:
        if taskname == "prdgen_high" or taskname == "prdgen_low":
            strings += sPre + '<metatask name="{0}" mode="parallel">\n'.format(taskname)
        else:
            strings += sPre + '<metatask name="{0}">\n'.format(taskname)

        if taskname == "postsnd":
            strings += sPre + '\t' + '<var name="member">&MEMLIST; avg</var>\n'
        else:
            strings += sPre + '\t' + '<var name="member">&MEMLIST;</var>\n'

        strings += sPre + '\t' + '<task name="{0}_#member#" cycledefs="{1}" maxtries="{2}">\n'.format(taskname, cycledef, maxtries)
    else:
        strings += sPre + '<task name="{0}" cycledefs="{1}" maxtries="{2}">\n'.format(taskname, cycledef, maxtries)

    if "@" in jobname:
        strings += sPre_2 + '<jobname><cyclestr>{0}</cyclestr></jobname>\n'.format(jobname)
    else:
        strings += sPre_2 + '<jobname>{0}</jobname>\n'.format(jobname)

    account = "&ACCOUNT;"
    strings += sPre_2 + '<account>{0}</account>\n'.format(account)

    # -------------------Join-------------------
    if sJoin == "":
        if taskname in metatask_names:
            sJoin = "&LOG_DIR;/@Y@m@d/gefs_#member#_{0}_@H".format(taskname)
        else:
            sJoin = "&LOG_DIR;/@Y@m@d/gefs_{0}_@H".format(taskname)

        if WHERE_AM_I.upper().startswith("WCOSS"):
            sJoin += ".%J"
        else:
            sJoin += ".%J"

    if "@" in sJoin:
        strings += sPre_2 + '<join><cyclestr>{0}</cyclestr></join>\n'.format(sJoin)
    else:
        strings += sPre_2 + '<join>{0}</join>\n'.format(sJoin)
    # -------------------Join-------------------

    if sWalltime != "":
        strings += sPre_2 + '<walltime>{0}</walltime>\n'.format(sWalltime)

    if sQueue != "":
        strings += sPre_2 + '<queue>{0}</queue>\n'.format(sQueue)
    # strings += sPre + '\t\t' + '<queue>&CUE2RUN;</queue>\n'

    if sPartition != "":
        strings += sPre_2 + '<partition>{0}</partition>\n'.format(sPartition)

    # -------------------sNodes-------------------
    if sNodes != "":
        if WHERE_AM_I.upper() == "cray".upper() and sQueue.upper() == "&TRANSFER_QUEUE;":
            strings += sPre_2 + '<nodes>{0}</nodes><shared></shared>\n'.format(sNodes)
        else:
            strings += sPre_2 + '<nodes>{0}</nodes>\n'.format(sNodes)
        
        # ---    
        if WHERE_AM_I.upper() == "wcoss_ibm".upper():
            if sQueue.upper() == "&TRANSFER_QUEUE;":
                strings += sPre_2 + '<native>-R "affinity[core]"</native>'
            else:
                strings += sPre_2 + '<native>-a poe</native>'

        elif WHERE_AM_I.upper() in ["wcoss_dell_p3".upper()]: #, "cray".upper()]:
            if taskname in ["prdgen_high"]:
                if sQueue.endswith("_shared"):
                    strings += sPre_2 + '<native>-R "affinity[core(4):distribute=pack]"</native>\n'
                    strings += sPre_2 + '<native>-R "rusage[mem=4608]"</native>\n'

    # -------------------sNodes-------------------

    if WHERE_AM_I.upper() == "cray".upper():
        strings += sPre_2 + '<native>-cwd &tmpnwprd;</native>\n'
    elif WHERE_AM_I.upper() == "hera".upper():
        strings += ""
    elif WHERE_AM_I.upper() == "wcoss_ibm".upper():
        strings += ""
    elif WHERE_AM_I.upper() == "wcoss_dell_p3".upper():
        strings += ""
    else:
        strings += sPre_2 + '<native>-cwd &tmpnwprd;</native>\n'

    # -------------------Memory-------------------
    if sMemory != "":
        strings += sPre_2 + '<memory>{0}</memory>\n'.format(sMemory)
    # -------------------Memory-------------------

    # -------------------Native-------------------
    if WHERE_AM_I.upper() == "cray".upper():
        if taskname in [ "archive_atm", "archive_wave" ]:
            strings += ""
        else:
            strings += sPre_2 + '<native>-extsched "CRAYLINUX[]"</native>\n'
    elif WHERE_AM_I.upper() == "Hera".upper():
        strings += ""  # \n
    elif WHERE_AM_I.upper() == "wcoss_ibm".upper():
        strings += ""
    elif WHERE_AM_I.upper() == "wcoss_dell_p3".upper():
        if taskname in metatask_names:
            strings += ""
        else:
            strings += sPre_2 + "<native>-R 'affinity[core(1)]'</native>\n"
    else:
        strings += sPre_2 + '<native>-extsched "CRAYLINUX[]"</native>\n'
    # -------------------Native-------------------

    strings += sPre + '\n'
    strings += sENV_VARS
    strings += sDATE_VARS
    strings += sPre_2 + '<!-- Other Environment Variables -->\n'
    strings += (create_envar(name="SOURCEDIR", value="&SOURCEDIR;", sPre=sPre_2))
    strings += (create_envar(name="job", value=jobname, sPre=sPre_2))

    # -------------------RUNMEM-------------------
    if taskname in metatask_names:
        strings += (create_envar(name="RUNMEM", value="ge#member#", sPre=sPre_2))
    elif taskname in ["forecast_aerosol", "post_aerosol", "prdgen_aerosol"]:
        strings += (create_envar(name="RUNMEM", value="geaer", sPre=sPre_2))
    else:
        if taskname in ["prdgen_gfs"]:
            strings += (create_envar(name="RUNMEM", value="gegfs", sPre=sPre_2))
    # -------------------RUNMEM-------------------

    #\/ -------------------Add Source Vars----------
    strings += AddSourceVarsToXML_ENT(sNodes, dicBase, taskname, sPre_2)
    #/\ -------------------Add Source Vars----------

    # -------------------Other envar and command-------------------
    ## Add new envir
    if taskname in ['keep_init', 'copy_init']:
        strings += (create_envar(name="MEMBER", value="#member#", sPre=sPre_2))

    ## For FORECAST_SEGMENT
    if (taskname in ['forecast_high', 'prdgen_high', 'post_high', 'ensstat_high', 'forecast_aerosol', 'post_aerosol', 'prdgen_aerosol']) \
     or taskname.startswith("post_high_") or taskname.startswith('post_aerosol_'):
        strings += (create_envar(name="FORECAST_SEGMENT", value="hr", sPre=sPre_2))
    elif taskname in ['forecast_low', 'prdgen_low', 'post_low', 'ensstat_low']:
        strings += (create_envar(name="FORECAST_SEGMENT", value="lr", sPre=sPre_2))
	
    ## For SUBJOB
    elif taskname.startswith("post_high_"):
        strings += (create_envar(name="SUBJOB", value=taskname.replace("post_high_", ""), sPre=sPre_2))
    elif taskname.startswith("post_aerosol_"):
        strings += (create_envar(name="SUBJOB", value=taskname.replace("post_aerosol_", ""), sPre=sPre_2))
    elif taskname.startswith("ensavg_nemsio_"):
        strings += (create_envar(name="SUBJOB", value=taskname.replace("ensavg_nemsio_", ""), sPre=sPre_2))
        
    ## Add command
    if taskname in ['keep_init', 'copy_init', 'keep_data_atm', 'archive_atm', 'cleanup_atm', 'keep_data_wave', 'archive_wave', 'cleanup_wave']:
        strings += sPre_2 + '<command><cyclestr>&PRE; &BIN;/../py/{0}.py</cyclestr></command>\n'.format(taskname)
    elif taskname in ['forecast_high', 'forecast_low', 'forecast_aerosol']:
        strings += sPre_2 + '<command><cyclestr>&PRE; &BIN;/{0}.sh</cyclestr></command>\n'.format("forecast_high")
    elif taskname in ['prdgen_high', 'prdgen_low', 'prdgen_gfs', 'prdgen_aerosol']:
        strings += sPre_2 + '<command><cyclestr>&PRE; . &BIN;/{0}.sh</cyclestr></command>\n'.format("prdgen_high")
    elif taskname in ['post_high', 'post_low', 'post_aerosol']:
        strings += sPre_2 + '<command><cyclestr>&PRE; &BIN;/{0}.sh</cyclestr></command>\n'.format("post_high")
    elif taskname in ['ensstat_high', 'ensstat_low']:
        strings += sPre_2 + '<command><cyclestr>&PRE; . &BIN;/{0}.sh</cyclestr></command>\n'.format("ensstat_high")
    elif taskname.startswith("post_high_"):
        strings += sPre_2 + '<command><cyclestr>&PRE; &BIN;/{0}.sh</cyclestr></command>\n'.format("post_high")
    elif taskname.startswith("ensavg_nemsio_"):
        strings += sPre_2 + '<command><cyclestr>&PRE; &BIN;/{0}.sh</cyclestr></command>\n'.format("ensavg_nemsio")
    else:
        strings += sPre_2 + '<command><cyclestr>&PRE; &BIN;/{0}.sh</cyclestr></command>\n'.format(taskname)
    # -------------------Other envar and command-------------------

    # -------------------Dependency-------------------
    if sDep != "":
        if "\\n" in sDep:
            sDep = sDep.replace('\\n', '\n')
        if "\\t" in sDep:
            sDep = sDep.replace('\\t', '\t')
        if "\n\t" in sDep:
            sDep = sDep.replace("\n", "\n{0}".format(sPre_2 + '\t'))

        strings += sPre_2 + '<dependency>\n'
        strings += sPre_2 + '\t' + sDep + '\n'  # '\t<taskdep task="{0}"/>\n'.format(taskdep)
        strings += sPre_2 + '</dependency>\n'
    # -------------------Dependency-------------------

    # End
    if taskname in metatask_names:
        strings += sPre + '\t' + '</task>\n'
        strings += sPre + '</metatask>\n\n'
    else:
        strings += sPre + '</task>\n\n'

    return strings

# =======================================================
def AddSourceVarsToXML_ENT(sNodes, dicBase, taskname, sPre_2):
    #print(sNodes)
    #print(taskname)
    
    strings = ""
    GEFS_NODES = 1
    GEFS_PPN = 1
    GEFS_TPP = 1
    
    sNodes_3 = sNodes.split(":")
    if len(sNodes_3) == 1:
        if sNodes_3[0].startswith("ppn="):
            GEFS_PPN = int(sNodes_3[0].split("ppn=")[1])
        elif sNodes_3[0].startswith("tpp="):
            GEFS_TPP = int(sNodes_3[0].split("tpp=")[1])
        else:
            GEFS_NODES = int(sNodes_3[0])
    elif len(sNodes_3) == 2:
        GEFS_NODES = int(sNodes_3[0])
        if sNodes_3[1].startswith("ppn="):
            GEFS_PPN = int(sNodes_3[1].split("ppn=")[1])
        if sNodes_3[1].startswith("tpp="):
            GEFS_TPP = int(sNodes_3[1].split("tpp=")[1])
    elif len(sNodes_3) == 3:
        GEFS_NODES = int(sNodes_3[0])
        if sNodes_3[1].startswith("ppn="):
            GEFS_PPN = int(sNodes_3[1].split("ppn=")[1])
        if sNodes_3[2].startswith("tpp="):
            GEFS_TPP = int(sNodes_3[2].split("tpp=")[1])
    else:
        print("Please check the format of your sNodes")
        strings = "Wrong Format"
        return strings
       
    if taskname in ["forecast_high", "forecast_low"]:
        GEFS_TPP = int(dicBase['parallel_threads'.upper()])

    GEFS_NTASKS = GEFS_NODES * GEFS_PPN
    GEFS_NCORES_PER_NODE = GEFS_PPN * GEFS_TPP
 
    strings += (create_envar(name="GEFS_NTASKS", value="{0}".format(GEFS_NTASKS), sPre=sPre_2))
    strings += (create_envar(name="GEFS_NCORES_PER_NODE", value="{0}".format(GEFS_NCORES_PER_NODE), sPre=sPre_2))
    strings += (create_envar(name="GEFS_TPP", value="{0}".format(GEFS_TPP), sPre=sPre_2))
    strings += (create_envar(name="GEFS_PPN", value="{0}".format(GEFS_PPN), sPre=sPre_2))
    strings += (create_envar(name="GEFS_NODES", value="{0}".format(GEFS_NODES), sPre=sPre_2))

    return strings

# =======================================================
def GetIndexOfTask(dicBase, taskname):
    taskname_num = int(dicBase['taskname_num'.upper()])
    if taskname_num <= 0:
        return -1

    # print(taskname_num)
    for k in range(taskname_num):
        sTaskName = dicBase["taskname_{0}".format(k + 1).upper()]
        if sTaskName == taskname:
            return k

    return False


# =======================================================
def Replace_task_UsingSubjobs(dicBase, taskname="post_high", sNSubJobs='N_SUBJOBS_POST_HIGH'):
    IsDebug = False

    taskname_num = int(dicBase['taskname_num'.upper()])
    if taskname_num <= 0:
        return

    sNSubJobs = sNSubJobs.upper()

    if IsDebug:
        print("=================", taskname)

    if sNSubJobs in dicBase:
        N_SubJobs = int(dicBase[sNSubJobs])
    else:
        N_SubJobs = 0
        # dicBase['N_SUBJOBS_POST_HIGH'] = 0

    if N_SubJobs <= 0:
        return

    itaskname = GetIndexOfTask(dicBase, taskname)

    Added_NewTasks = N_SubJobs - 1
    taskname_num_new = taskname_num + Added_NewTasks

    if IsDebug:
        for k in range(taskname_num):
            sVarName = "taskname_{0}".format(k + 1).upper()
            sTaskName = dicBase[sVarName]
            if IsDebug:
                print(sVarName, sTaskName)

        if IsDebug:
            print("=================")

    # print(itaskname)
    for k in range(taskname_num_new - 1, itaskname + N_SubJobs - 1, -1):
        kk = k - Added_NewTasks
        sVarName = "taskname_{0}".format(k + 1).upper()
        sVarName_k_1 = "taskname_{0}".format(kk + 1).upper()  # k+1-N_SubJobs).upper()
        dicBase[sVarName] = dicBase[sVarName_k_1]
        if IsDebug:
            print(sVarName, "-", dicBase[sVarName], k, sVarName_k_1, "-", dicBase[sVarName_k_1], kk)

    if IsDebug:
        print("===")
    for k in range(N_SubJobs):
        kk = k + itaskname
        sVarName = "taskname_{0}".format(kk + 1).upper()
        if IsDebug:
            print(sVarName, kk)
        dicBase[sVarName] = "{0}_{1}{2}".format(taskname, N_SubJobs, chr(65 + k))  # SubExts[k])

    dicBase['taskname_num'.upper()] = taskname_num_new

    if IsDebug:
        print("===")
        taskname_num = int(dicBase['taskname_num'.upper()])
        for k in range(taskname_num):
            sVarName = "taskname_{0}".format(k + 1).upper()
            sTaskName = dicBase[sVarName]
            print(sVarName, sTaskName)

    return


# =======================================================
def Add_Subjobs_to_dicBase(dicBase, iTaskName_Num, taskname="post_high", sNSubJobs='N_SUBJOBS_POST_HIGH'):
    # taskname_num = int(dicBase['taskname_num'.upper()])
    # if taskname_num <= 0:
    #    return iTaskName_Num

    sNSubJobs = sNSubJobs.upper()
    if sNSubJobs in dicBase:
        N_SubJobs = int(dicBase[sNSubJobs])
    else:
        N_SubJobs = 0
        dicBase[sNSubJobs] = 0

    if N_SubJobs <= 1:
        iTaskName_Num += 1
        sTaskName = "taskname_{0}".format(iTaskName_Num)
        dicBase[sTaskName.upper()] = taskname
    else:
        for k in range(N_SubJobs):
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "{0}_{1}{2}".format(taskname, N_SubJobs, chr(65 + k))

    return iTaskName_Num


# =======================================================
def write_to_all_ent(GenTaskEnt, dicBase):
    if GenTaskEnt:
        import os, sys
        # sPath = dicBase["GEFS_ROCOTO"] + r"/tasks/" + dicBase["WHERE_AM_I"] + r"/"

        sSep = "/"
        if sys.platform == 'win32':
            sSep = r'\\'

        sPath = dicBase["GEFS_ROCOTO"]
        sPath += sSep + "tasks"

        if not os.path.exists(sPath):
            os.mkdir(sPath)

        sAllEnt_File = sPath + sSep + "all.ent"
        fh = open(sAllEnt_File, 'w')

        fh.write('<!-- List of all GEFS tasks -->\n')

        taskname_num = int(dicBase['taskname_num'.upper()])
        for k in range(taskname_num):
            sTaskName = "taskname_{0}".format(k + 1).upper()
            if sTaskName not in dicBase:
                print('You must assign value of "{0}" in the configure file!'.format(sTaskName))
                exit(0)
            taskname = dicBase[sTaskName]

            fh.write('<!ENTITY {0}\tSYSTEM\t"{0}.ent">\n'.format(taskname))

        fh.flush()
        fh.close()

        # ----
        sPath = dicBase["GEFS_ROCOTO"] + sSep + "tasks"
        # create  date_vars.ent
        sFile = sPath + sSep + "date_vars.ent"
        fh = open(sFile, 'w')
        strings = get_DATE_VARS("")
        fh.write(strings)
        fh.flush()
        fh.close()
        # create env_vars.ent
        sFile = sPath + sSep + "env_vars.ent"
        fh = open(sFile, 'w')
        strings = get_ENV_VARS("")
        fh.write(strings)
        fh.flush()
        fh.close()


# =======================================================
def write_to_ent(taskname, dicBase, GenTaskEnt=False):
    import sys
    import os

    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    strings = create_metatask_task(dicBase, taskname=taskname, sPre="", GenTaskEnt=GenTaskEnt)

    strings = ''.join(strings)

    sPath = dicBase["GEFS_ROCOTO"]
    sPath += sSep + "tasks"

    if not os.path.exists(sPath):
        os.mkdir(sPath)

    sFile = sPath + sSep + "{0}.ent".format(taskname)

    fh = open(sFile, 'w')

    fh.write(strings)

    fh.close()
    # print("exit")

# =======================================================
def calc_fcst_resources(dicBase, taskname="forecast_high"):
    import math

    layout_x = int(dicBase['layout_x'.upper()])
    layout_y = int(dicBase['layout_y'.upper()])
    WRITE_GROUP = int(dicBase['WRITE_GROUP'.upper()])
    WRTTASK_PER_GROUP = int(dicBase['WRTTASK_PER_GROUP'.upper()])
    parallel_threads = int(dicBase['parallel_threads'.upper()])

    ncores_per_node = Get_NCORES_PER_NODE(dicBase)

    dicBase['COREPERNODE'] = ncores_per_node

    iTotal_Tasks = layout_x * layout_y * 6 + WRITE_GROUP * WRTTASK_PER_GROUP

    if dicBase['cplwav'] == ".true.":
        if taskname == "forecast_high":
            iWaveThreads = int(dicBase['NPE_WAV'])
            iTotal_Tasks = iTotal_Tasks + iWaveThreads

    iPPN = int(math.ceil(ncores_per_node * 1.0 / parallel_threads))
    iNodes = int(math.ceil( iTotal_Tasks * 1.0 / iPPN))
    iTPP = parallel_threads

    return iTotal_Tasks, iNodes, iPPN, iTPP

# =======================================================
def get_param_of_task(dicBase, taskname):
    import math
    import textwrap
    sWalltime = ""
    sNodes = ""
    sMemory = ""
    sJoin = ""
    sDep = ""
    sQueue = ""
    sPartition = ""

    taskname_org = taskname
    if taskname.startswith("post_high_"):
        taskname = "post_high"
    elif taskname.startswith("ensavg_nemsio_"):
        taskname = "ensavg_nemsio"

    sVarName = "{0}_walltime".format(taskname).upper()
    if sVarName in dicBase:
        sWalltime = dicBase[sVarName.upper()]

    sVarName = "{0}_memory".format(taskname).upper()
    if sVarName in dicBase:
        sMemory = dicBase[sVarName.upper()]
    else:
        sMemory = ""

    sVarName_nodes = "{0}_nodes".format(taskname).upper()
    sVarName_ppn = "{0}_ppn".format(taskname).upper()
    sVarName_tpp = "{0}_tpp".format(taskname).upper()

    sNodes = ""
    if sVarName_nodes in dicBase:
        sNodes = "{0}".format(dicBase[sVarName_nodes])

    if sVarName_ppn in dicBase:
        ppn = dicBase[sVarName_ppn]
        if taskname.lower() in ["prdgen_high", "prdgen_gfs", "ensstat_high"]:
            # print(taskname)
            # print("{0}".format("PRDGEN_STREAMS" in dicBase))
            # print(dicBase["PRDGEN_STREAMS"])
            # print(dicBase["PRDGEN_STREAMS"].split())
            if "PRDGEN_STREAMS" in dicBase:
                ppn = len(dicBase["PRDGEN_STREAMS"].split())
            # print(ppn)
        elif taskname.lower() in ["prdgen_low", "ensstat_low"]:
            ppn = 3

        if sNodes != "":
            sNodes += ":ppn={0}".format(ppn)
        else:
            sNodes += "ppn={0}".format(ppn)

    if sVarName_tpp in dicBase:
        if sNodes != "":
            sNodes += ":tpp={0}".format(dicBase[sVarName_tpp])
        else:
            sNodes += "tpp={0}".format(dicBase[sVarName_tpp])

    # for queue
    sVarName = "{0}_queue".format(taskname).upper()
    if sVarName in dicBase:
        sQueue = dicBase[sVarName.upper()]

    # for partition (RDHPCS only)
    sVarName = "{0}_partition".format(taskname).upper()
    if sVarName in dicBase:
        sPartition = dicBase[sVarName.upper()]

    # for Join
    sVarName = "{0}_join".format(taskname).upper()
    if sVarName in dicBase:
        sJoin = dicBase[sVarName.upper()]
        if taskname_org.startswith("post_high_"):
            sJoin = sJoin.replace("post_high", taskname_org)
        elif taskname_org.startswith("ensavg_nemsio_"):
            sJoin = sJoin.replace("ensavg_nemsio", taskname_org)

    # for dependency
    sVarName = "{0}_dep".format(taskname).upper()
    if sVarName in dicBase:
        sDep = dicBase[sVarName.upper()]
        if sDep.strip() != "":  # identify whether include 'init_recenter' or not

            # For 'init_fv3chgrs' task
            if taskname.lower() == "init_fv3chgrs":
                if DoesTaskExist(dicBase, "init_combine"):
                    sDep = '<taskdep task="init_combine"/>'
                else:
                    sDep = ""

            # For 'init_recenter' task
            if taskname.lower() == "init_recenter":
                if DoesTaskExist(dicBase, "init_fv3chgrs"):
                    sDep = '<metataskdep metatask="init_fv3chgrs"/>'
                else:
                    sDep = ""

            # For 'init_aerosol' task
            if taskname.lower() == "init_aerosol":
                sDep = "<and>"
                for task in ["prep_emissions", "init_recenter", "copy_init"]:
                    if DoesTaskExist(dicBase, task):
                        sDep += "\n\t<taskdep task=\"{task}\"/>".format(task=task)

                for task in ["forecast_aerosol"]:
                    if DoesTaskExist(dicBase, task):
                        aerosol_init_type = dicBase['AEROSOL_INIT_TYPE']
                        gefs_cych = int(dicBase['INCYC'])
                        if aerosol_init_type == "warm":
                            sDep += '\t'.join(textwrap.dedent("""
                            <or>
                                <not><cycleexistdep cycle_offset=\"-&INCYC;:00:00\"/></not>
                                <and>
                                    <datadep><cyclestr offset=\"-&INCYC;:00:00\">&DATA_DIR;/gefs.@Y@m@d/@H/sfcsig/geaer.t@Hz.logf{gefs_cych:03}.nemsio</cyclestr></datadep>
                                    <datadep minsize="670M"><cyclestr offset=\"-&INCYC;:00:00\">&DATA_DIR;/gefs.@Y@m@d/@H/sfcsig/geaer.t@Hz.atmf{gefs_cych:03}.nemsio</cyclestr></datadep>
                                    <datadep age="60"><cyclestr offset=\"-&INCYC;:00:00\">&DATA_DIR;/gefs.@Y@m@d/@H/restart/aer/</cyclestr><cyclestr>@Y@m@d.@H@M@S.coupler.res</cyclestr></datadep>
                                    <datadep age="60"><cyclestr offset=\"-&INCYC;:00:00\">&DATA_DIR;/gefs.@Y@m@d/@H/restart/aer/</cyclestr><cyclestr>@Y@m@d.@H@M@S.fv_core.res.nc</cyclestr></datadep>
                            """.format(gefs_cych=gefs_cych)).splitlines(True))

                            for kind in ["fv_tracer.res", "fv_core.res", "fv_srf_wnd.res", "phy_data", "sfc_data"]:
                                for tile in map(lambda t: "tile" + str(t), range(1, 7)):
                                    sDep += '\t\t\t'.join(textwrap.dedent("""
                                    <datadep age="60"><cyclestr offset=\"-&INCYC;:00:00\">&DATA_DIR;/gefs.@Y@m@d/@H/restart/aer/</cyclestr><cyclestr>@Y@m@d.@H@M@S.{kind}.{tile}.nc</cyclestr></datadep>""".format(kind=kind, tile=tile)).splitlines(True))

                            sDep += '\t'.join(textwrap.dedent("""
                                </and>
                            </or>
                            """).splitlines(True))

                        elif aerosol_init_type == "cold":
                            # sDep += "\n\t<or>\n\t\t<not><cycleexistdep cycle_offset=\"-&INCYC;:00:00\"/></not>\n\t\t<taskdep task=\"{task}\" cycle_offset=\"-&INCYC;:00:00\"/>\n\t</or>".format(task=task)
                            sDep += '\t'.join(textwrap.dedent("""
                            <or>
                                <not><cycleexistdep cycle_offset=\"-&INCYC;:00:00\"/></not>
                                <and>
                                    <datadep age="60" minsize="1000M"><cyclestr offset=\"-&INCYC;:00:00\">&DATA_DIR;/gefs.@Y@m@d/@H/restart/aer/</cyclestr><cyclestr>@Y@m@d.@H@M@S.fv_tracer.res.tile1.nc</cyclestr></datadep>
                                    <datadep age="60" minsize="1000M"><cyclestr offset=\"-&INCYC;:00:00\">&DATA_DIR;/gefs.@Y@m@d/@H/restart/aer/</cyclestr><cyclestr>@Y@m@d.@H@M@S.fv_tracer.res.tile2.nc</cyclestr></datadep>
                                    <datadep age="60" minsize="1000M"><cyclestr offset=\"-&INCYC;:00:00\">&DATA_DIR;/gefs.@Y@m@d/@H/restart/aer/</cyclestr><cyclestr>@Y@m@d.@H@M@S.fv_tracer.res.tile3.nc</cyclestr></datadep>
                                    <datadep age="60" minsize="1000M"><cyclestr offset=\"-&INCYC;:00:00\">&DATA_DIR;/gefs.@Y@m@d/@H/restart/aer/</cyclestr><cyclestr>@Y@m@d.@H@M@S.fv_tracer.res.tile4.nc</cyclestr></datadep>
                                    <datadep age="60" minsize="1000M"><cyclestr offset=\"-&INCYC;:00:00\">&DATA_DIR;/gefs.@Y@m@d/@H/restart/aer/</cyclestr><cyclestr>@Y@m@d.@H@M@S.fv_tracer.res.tile5.nc</cyclestr></datadep>
                                    <datadep age="60" minsize="1000M"><cyclestr offset=\"-&INCYC;:00:00\">&DATA_DIR;/gefs.@Y@m@d/@H/restart/aer/</cyclestr><cyclestr>@Y@m@d.@H@M@S.fv_tracer.res.tile6.nc</cyclestr></datadep>
                                </and>
                            </or>""").splitlines(True))
                        else:
                            print("FATAL: AEROSOL_INIT_TYPE {aerosol_init_type} not recognized, can't determine dependency".format(aerosol_init_type=aerosol_init_type))
                            exit(105)

                if sDep == "<and>":
                    sDep = ""
                else:
                    sDep += "\n</and>"

            # For 'forecast_high' task
            if taskname.lower() == "forecast_high":
                sDep = '<and>'
                if DoesTaskExist(dicBase, "getcfssst"):
                    sDep += '\n\t<taskdep task="getcfssst"/>'
                if DoesTaskExist(dicBase, "init_recenter"):
                    if DoesTaskExist(dicBase, "init_fv3chgrs"):  # Cold Restart
                        sDep += '\n\t<taskdep task="init_recenter"/>'
                    else:  # Warm Start  ???
                        sDep += '\n\t<datadep><cyclestr>&WORKDIR;/nwges/dev/gefs.@Y@m@d/@H/c00/fv3_increment.nc</cyclestr></datadep>'

                elif DoesTaskExist(dicBase, "init_fv3chgrs"):  # *_Reloc
                    sDep += '\n\t<taskdep task="init_fv3chgrs_#member#"/>'

                if DoesTaskExist(dicBase, "copy_init"):
                    sDep += '\n\t<taskdep task="copy_init_#member#"/>'
                if DoesTaskExist(dicBase, "wave_prep"):  # Wave prep
                    sDep += '\n\t<taskdep task="wave_prep_#member#"/>'
                    sDep += '\n\t<taskdep task="wave_prep_c00"/>'
                if sDep == '<and>':
                    sDep = ""
                else:
                    sDep += '\n</and>'

            # For 'forecast_low' task
            if taskname.lower() == "forecast_low":
                if DoesTaskExist(dicBase, "forecast_high"):
                    sDep = '<taskdep task="forecast_high_#member#"/>'
                else:
                    if DoesTaskExist(dicBase, "init_fv3chgrs"):
                        if DoesTaskExist(dicBase, "getcfssst"):
                            sDep = '<and>\n\t<taskdep task="init_fv3chgrs_#member#"/>\n\t<taskdep task="getcfssst"/>\n</and>'
                        else:
                            sDep = '<taskdep task="init_fv3chgrs_#member#"/>'
                    elif DoesTaskExist(dicBase, "rf_prep"):
                        if DoesTaskExist(dicBase, "getcfssst"):
                            sDep = '<and>\n\t<taskdep task="rf_prep"/>\n\t<taskdep task="getcfssst"/>\n</and>'
                        else:
                            sDep = '<taskdep task="rf_prep"/>'
                    else:  # For Warm Start
                        if DoesTaskExist(dicBase, "getcfssst"):
                            sDep = '<and>\n\t<taskdep task="getcfssst"/>\n</and>'
                        else:
                            sDep = ''

            # For 'forecast_aerosol' task
            if taskname.lower() == "forecast_aerosol" :
                sDep = '<and>'
                if DoesTaskExist(dicBase, "getcfssst"):
                    sDep += '\n\t<taskdep task="getcfssst"/>'

                if DoesTaskExist(dicBase, "init_aerosol"):  # Cold Restart
                    sDep += '\n\t<taskdep task="init_aerosol"/>'
                else:  # Warm Start  ???
                    sDep += '\n\t<datadep><cyclestr>&WORKDIR;/nwges/dev/gefs.@Y@m@d/@H/c00/fv3_increment.nc</cyclestr></datadep>'

                if DoesTaskExist(dicBase, "prep_emissions"):
                    sDep += '\n\t<taskdep task="prep_emissions"/>'

                if sDep == '<and>':
                    sDep = ""
                else:
                    sDep += '\n</and>'

            # For ensavg_nemsio
            if taskname.lower() == "ensavg_nemsio":
                npert = int(dicBase["NPERT"])
                sDep = '<and>'
                for i in range(npert):
                    sDep += '\n\t<datadep><cyclestr>&DATA_DIR;/gefs.@Y@m@d/@H/sfcsig/gep{0:02}.t@Hz.logf000.nemsio</cyclestr></datadep>'.format(i + 1)
                sDep += '\n\t<datadep><cyclestr>&DATA_DIR;/gefs.@Y@m@d/@H/sfcsig/gec00.t@Hz.logf000.nemsio</cyclestr></datadep>'
                sDep += '\n</and>'

            # For ensstat_high
            if taskname.lower() == "ensstat_high":
                npert = int(dicBase["NPERT"])
                sDep = '<and>'
                for i in range(npert):
                    sDep += '\n\t<datadep><cyclestr>&DATA_DIR;/gefs.@Y@m@d/@H/misc/prd1p0/gep{0:02}.t@Hz.prdgen.control.f000</cyclestr></datadep>'.format(i + 1)
                sDep += '\n\t<datadep><cyclestr>&DATA_DIR;/gefs.@Y@m@d/@H/misc/prd1p0/gec00.t@Hz.prdgen.control.f000</cyclestr></datadep>'
                sDep += '\n</and>'

            # For ensstat_low
            if taskname.lower() == "ensstat_low":
                npert = int(dicBase["NPERT"])
                sDep = '<and>'
                ifhmaxh = int(dicBase["fhmaxh".upper()])
                iFHOUTHF = int(dicBase["FHOUTHF"])
                iFHOUTLF = int(dicBase["FHOUTLF"])
                iFHMAXHF = int(dicBase["FHMAXHF"])

                iStartHourLF = 0
                if iFHMAXHF >= ifhmaxh:
                    iStartHourLF = ifhmaxh + iFHOUTHF
                else:
                    iStartHourLF = ifhmaxh + iFHOUTLF

                for i in range(npert):
                    sDep += '\n\t<datadep><cyclestr>&DATA_DIR;/gefs.@Y@m@d/@H/misc/prd1p0/gep{0:02}.t@Hz.prdgen.control.f{1:03}</cyclestr></datadep>'.format(i + 1, iStartHourLF)
                sDep += '\n\t<datadep><cyclestr>&DATA_DIR;/gefs.@Y@m@d/@H/misc/prd1p0/gec00.t@Hz.prdgen.control.f{0:03}</cyclestr></datadep>'.format(iStartHourLF)
                sDep += '\n</and>'

            # For extractvars
            if taskname.lower() == "extractvars":
                if DoesTaskExist(dicBase, "prdgen_low"):
                    sDep = '<metataskdep metatask="prdgen_low"/>'
                elif DoesTaskExist(dicBase, "prdgen_high"):
                    sDep = '<metataskdep metatask="prdgen_high"/>'
                else:
                    sDep = ''

            # For Low Resolution
            if taskname.lower() == "post_low" or taskname.lower() == "prdgen_low":
                FHOUTHF = int(dicBase["FHOUTHF".upper()])
                FHOUTLF = int(dicBase["FHOUTLF".upper()])
                fhmaxh = int(dicBase["fhmaxh".upper()])
                FHMAXHF = int(dicBase["FHMAXHF".upper()])

                if FHMAXHF <= fhmaxh:
                    start_hr_low = fhmaxh + FHOUTLF
                else:
                    start_hr_low = fhmaxh + FHOUTHF
                sDep = dicBase[sVarName].replace("fXXX", "f{0:03d}".format(start_hr_low))

            # For 'enspost' task
            if taskname.lower() == "enspost":
                sDep = '<and>'
                if DoesTaskExist(dicBase, "prdgen_low"):
                    sDep += '\n\t<metataskdep metatask="prdgen_low"/>'
                    if DoesTaskExist(dicBase, "prdgen_gfs"):
                        sDep += '\n\t<taskdep task="prdgen_gfs"/>'
                elif DoesTaskExist(dicBase, "prdgen_high"):
                    sDep += '\n\t<metataskdep metatask="prdgen_high"/>'
                    if DoesTaskExist(dicBase, "prdgen_gfs"):
                        sDep += '\n\t<taskdep task="prdgen_gfs"/>'

                if sDep == '<and>':
                    sDep = ""
                else:
                    sDep += '\n</and>'

            # For "cqpf" task
            if taskname.lower() == "cqpf":
                if DoesTaskExist(dicBase, "enspost"):
                    sDep = '<taskdep task="enspost"/>'
                else:
                    sDep = ""
                

            # For 'keep_data_atm' and 'archive_atm' tasks
            if taskname.lower() == "keep_data_atm" or taskname.lower() == "archive_atm":
                sDep = '<and>'
                if DoesTaskExist(dicBase, "enspost"):
                    sDep += '\n\t<taskdep task="enspost"/>'
                if DoesTaskExist(dicBase, "cqpf"):
                    sDep += '\n\t<taskdep task="cqpf"/>'
                if DoesTaskExist(dicBase, "post_track"):
                    sDep += '\n\t<taskdep task="post_track"/>'
                if DoesTaskExist(dicBase, "post_genesis"):
                    sDep += '\n\t<taskdep task="post_genesis"/>'
                if DoesTaskExist(dicBase, "extractvars"):
                    sDep += '\n\t<taskdep task="extractvars"/>'
                if DoesTaskExist(dicBase, "ensstat_low"):
                    sDep += '\n\t<taskdep task="ensstat_low"/>'
                if DoesTaskExist(dicBase, "prdgen_low"):
                    sDep += '\n\t<metataskdep metatask="prdgen_low"/>'
                if DoesTaskExist(dicBase, "ensstat_high"):
                    sDep += '\n\t<taskdep task="ensstat_high"/>'
                if DoesTaskExist(dicBase, "prdgen_high"):
                    sDep += '\n\t<metataskdep metatask="prdgen_high"/>'
                if DoesTaskExist(dicBase, "postsnd"):
                    sDep += '\n\t<metataskdep metatask="postsnd"/>'
                if DoesTaskExist(dicBase, "getcfssst"):
                    sDep += '\n\t<taskdep task="getcfssst"/>'
                if DoesTaskExist(dicBase, "prdgen_aerosol"):
                    sDep += '\n\t<taskdep task="prdgen_aerosol"/>'                    

                if sDep == '<and>':
                    sDep = ""
                else:
                    sDep += '\n</and>'

            if taskname.lower() in [ "wave_stat" ]:
                if DoesTaskExist(dicBase, "wave_post"):
                    sDep = '<metataskdep metatask="wave_post"/>'
                else:
                    sDep = ""

            if taskname.lower() in [ "keep_data_wave", "archive_wave" ]:
                if DoesTaskExist(dicBase, "wave_stat"):
                    sDep = '<taskdep task="wave_stat"/>'
                else:
                    sDep = ""

            # For keep_init
            if taskname.lower() == "keep_init":
                if DoesTaskExist(dicBase, "init_recenter"):
                    sDep = '<taskdep task="init_recenter"/>'

            # Don't clean up if keep_init isn't finished
            if taskname.lower() == "cleanup_atm":
                sDep = '<and>'
                if DoesTaskExist(dicBase, "keep_init"):
                    sDep += '\n\t<metataskdep metatask="keep_init"/>'
                if DoesTaskExist(dicBase, "keep_data_atm"):
                    sDep += '\n\t<taskdep task="keep_data_atm"/>'
                if DoesTaskExist(dicBase, "archive_atm"):
                    sDep += '\n\t<taskdep task="archive_atm"/>'
                if sDep == '<and>':
                    sDep = ""
                else:
                    sDep += '\n</and>'

            if taskname.lower() == "cleanup_wave":
                sDep = '<and>'
                for t in [ "keep_data_wave", "archive_wave" ]:
                    if DoesTaskExist(dicBase, t):
                        sDep += '\n\t<taskdep task="{task}"/>'.format(task=t)
                if sDep == '<and>':
                    sDep = ""
                else:
                    sDep += '\n</and>'

            # For GEMPAK
            if taskname.lower() == "gempak":
                #if DoesTaskExist(dicBase, "prdgen_low"):
                #    sDep = '<metataskdep metatask="prdgen_low"/>'
                if DoesTaskExist(dicBase, "prdgen_high"):
                    sDep = '<metataskdep metatask="prdgen_high"/>'
                else:
                    sDep = ''

            # For AVGSPR_GEMPAK
            if taskname.lower() == "avgspr_gempak":
                #if DoesTaskExist(dicBase, "ensstat_low"):
                #    sDep = '<taskdep task="ensstat_low"/>'
                if DoesTaskExist(dicBase, "ensstat_high"):
                    sDep = '<taskdep task="ensstat_high"/>'
                else:
                    sDep = ''

            # For avg_gempak_vgf
            if taskname.lower() == "avg_gempak_vgf":
                if DoesTaskExist(dicBase, "avgspr_gempak"):
                    sDep = '<taskdep task="avgspr_gempak"/>'
                else:
                    sDep = ''

            # For gempak_meta
            if taskname.lower() == "gempak_meta":
                sDep = '<and>'
                if DoesTaskExist(dicBase, "gempak"):
                    sDep += '\n\t<taskdep task="gempak"/>'
                if DoesTaskExist(dicBase, "avgspr_gempak"):
                    sDep += '\n\t<taskdep task="avgspr_gempak"/>'

                if sDep == '<and>':
                    sDep = ""
                else:
                    sDep += '\n</and>'

            # For avgsgempak_meta
            if taskname.lower() == "avgspr_gempak_meta":
                if DoesTaskExist(dicBase, "avgspr_gempak"):
                    sDep = '<taskdep task="avgspr_gempak"/>'
                else:
                    sDep = ''

    # Forecast can be derive from the parm items
    if taskname in ['forecast_high', 'forecast_low', 'forecast_aerosol']:

        iTotal_Tasks, iNodes, iPPN, iTPP = calc_fcst_resources(dicBase, taskname=taskname)

        WHERE_AM_I = dicBase['WHERE_AM_I'].upper()
        
        if WHERE_AM_I.upper() == "wcoss_dell_p3".upper():
            sNodes = "{0}:ppn={1}".format(iNodes, iPPN)
        else:
            sNodes = "{0}:ppn={1}:tpp={2}".format(iNodes, iPPN, iTPP)

    # For gempak
    if taskname == "gempak":
        iTotal_Tasks, iNodes, iPPN, iTPP = calc_gempak_resources(dicBase)
        sNodes = "{0}:ppn={1}:tpp={2}".format(iNodes, iPPN, iTPP)

    # For avgspr_gempak
    if taskname == "avgspr_gempak":
        iTotal_Tasks, iNodes, iPPN, iTPP = calc_avgspr_gempak_resources(dicBase)
        sNodes = "{0}:ppn={1}:tpp={2}".format(iNodes, iPPN, iTPP)

    return sWalltime, sNodes, sMemory, sJoin, sDep, sQueue, sPartition


# =======================================================
def calc_avgspr_gempak_resources(dicBase):
    ncores_per_node = Get_NCORES_PER_NODE(dicBase)
    WHERE_AM_I = dicBase['WHERE_AM_I'].upper()
    npert = int(dicBase["NPERT"])
    
    iTotal_Tasks = 2
    nGEMPAK_RES = 1
    if "GEMPAK_RES" in dicBase:
        nGEMPAK_RES = len(dicBase["GEMPAK_RES"].split())
        iTotal_Tasks *= nGEMPAK_RES

    iTPP = 1
    if WHERE_AM_I.upper() == "CRAY":
        iNodes = iTotal_Tasks
        iPPN = 1
        
    else:
        iNodes = iTotal_Tasks
        iPPN = 1

    return iTotal_Tasks, iNodes, iPPN, iTPP

# =======================================================
def calc_gempak_resources(dicBase):
    import math
    ncores_per_node = Get_NCORES_PER_NODE(dicBase)
    WHERE_AM_I = dicBase['WHERE_AM_I'].upper()
    npert = int(dicBase["NPERT"])
    iTotal_Tasks = npert + 1
    nGEMPAK_RES = 1
    if "GEMPAK_RES" in dicBase:
        nGEMPAK_RES = len(dicBase["GEMPAK_RES"].split())
        iTotal_Tasks *= nGEMPAK_RES

    iTPP = 1
    if WHERE_AM_I.upper() == "CRAY":
        iNodes = iTotal_Tasks
        iPPN = 1
        
    elif WHERE_AM_I.upper() == "HERA":
        if (npert + 1) <= ncores_per_node:
            iNodes = nGEMPAK_RES
            iPPN = (npert + 1)
        elif nGEMPAK_RES <= ncores_per_node:
            iNodes = (npert + 1)
            iPPN = nGEMPAK_RES
        else:
            iNodes = (npert + 1)
            iPPN = nGEMPAK_RES
            
    elif WHERE_AM_I.upper() == "wcoss_dell_p3".upper():
        if (npert + 1) <= ncores_per_node:
            iNodes = nGEMPAK_RES
            iPPN = (npert + 1)
        else:
            iPPN = ncores_per_node
            iNodes = math.ceil(iTotal_Tasks / (iPPN * 1.0))

    else:
        if (npert + 1) <= ncores_per_node:
            iNodes = nGEMPAK_RES
            iPPN = (npert + 1)
        elif nGEMPAK_RES <= ncores_per_node:
            iNodes = (npert + 1)
            iPPN = nGEMPAK_RES
        else:
            iNodes = (npert + 1)
            iPPN = nGEMPAK_RES

    return iTotal_Tasks, iNodes, iPPN, iTPP

# =======================================================
def Get_NCORES_PER_NODE(dicBase):
    WHERE_AM_I = dicBase['WHERE_AM_I'].upper()

    if WHERE_AM_I == 'cray'.upper():
        ncores_per_node = 24
    elif WHERE_AM_I == "hera".upper():
        ncores_per_node = 40
    elif WHERE_AM_I == "wcoss_dell_p3".upper():
        ncores_per_node = 28
    else:
        ncores_per_node = 24

    return ncores_per_node


# =======================================================
def DoesTaskExist(dicBase, taskname):
    taskname_num = int(dicBase['taskname_num'.upper()])

    if taskname_num <= 0:
        return False

    for k in range(taskname_num):
        sTaskName = dicBase["taskname_{0}".format(k + 1).upper()]
        if sTaskName == taskname:
            return True

    return False


# =======================================================
def get_metatask_names(taskname=""):
    metatask_names = []
    metatask_names.append('init_fv3chgrs')
    metatask_names.append('keep_init')
    metatask_names.append('copy_init')
    # forecast
    metatask_names.append('forecast_high')
    metatask_names.append('forecast_low')
    # post
    metatask_names.append('post_high')
    if taskname.startswith("post_high_"):
        metatask_names.append(taskname)

    metatask_names.append('post_low')
    # prdgen
    metatask_names.append('prdgen_high')
    metatask_names.append('prdgen_low')
    # wave
    metatask_names.append('wave_prep')
    metatask_names.append('wave_post')
    metatask_names.append('wave_stats')
    # postsnd
    metatask_names.append('postsnd')

    return metatask_names


# =======================================================
def get_jobname(taskname):
    import os, sys
    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    sDefaultJobID_File = sys.path[0] + sSep + "job_id.conf"
    jobname_short = "--"
    if os.path.exists(sDefaultJobID_File):
        # print("---Default Job-ID Configure file was found! Reading ...")
        # print(sDefaultJobID_File)
        dicJobID = read_jobid_config(sDefaultJobID_File)

        if taskname in dicJobID:
            jobname_short = dicJobID[taskname]
            jobname = "&EXPID;_@Y@m@d@H_" + jobname_short

            return jobname

    # else if this file does not exist and if the task name is not in the job_id.conf
    tasknames = taskname.split("_")
    if len(tasknames) == 1:
        jobname_short = tasknames[0][0:2] + "_" + tasknames[0][-2:]
    elif len(tasknames) == 2:
        jobname_short = tasknames[0][0:2] + "_" + tasknames[1][-2:]
    else:
        jobname_short = tasknames[1][0] + tasknames[1][-1] + "_" + tasknames[2][0] + tasknames[2][-1]

    jobname = "&EXPID;_@Y@m@d@H_" + jobname_short

    return jobname


# =======================================================
def read_jobid_config(sConfig):
    # read config file
    dicBase = {}
    with open(sConfig, "r")as f:
        for sLine in f:
            # print(sLine)
            sLine = sLine.strip()

            if len(sLine) != 0:
                if str(sLine).startswith("#"):
                    # print("This is the comment: {0}".format(sLine))
                    continue
                else:
                    # print(sLine)
                    a, b = sLine.split("=", 1)
                    b = b.split(" #", 1)[0]

                    a = str(a).strip()
                    b = str(b).strip()

                    if b.startswith('"'):
                        b = b.replace('"', "", 1)
                    if b.endswith('"'):
                        b = b[:-1]

                    if b.startswith("'"):
                        b = b.replace(",", "", 1)
                    if b.endswith(","):
                        b = b[:-1]

                    b = str(b).strip()

                    dicBase[a] = b

    return dicBase


# =======================================================
def get_DATE_VARS(sPre="\t\t"):
    dicDATE_VARS = {}
    dicDATE_VARS['PDY'] = '@Y@m@d'
    dicDATE_VARS['cyc'] = '@H'
    dicDATE_VARS['cyc_fcst'] = '@H'
    sDATE_VARS = ""
    # sPre = "\t\t"
    sDATE_VARS += sPre + '<!-- PDY and cycle variables -->\n'
    for sKey in dicDATE_VARS:
        sDATE_VARS += (create_envar(name=sKey, value=dicDATE_VARS[sKey], sPre=sPre))

    return sDATE_VARS


# =======================================================
def get_ENV_VARS(sPre="\t\t"):
    dicENV_VARS = {}
    dicENV_VARS['envir'] = 'dev'
    dicENV_VARS['RUN_ENVIR'] = 'dev'
    #dicENV_VARS['gefsmpexec'] = 'mpirun.lsf'
    #dicENV_VARS['wavempexec'] = 'mpirun.lsf'
    #dicENV_VARS['gefsmpexec_mpmd'] = 'mpirun.lsf'
    dicENV_VARS['WHERE_AM_I'] = '&WHERE_AM_I;'
    dicENV_VARS['GEFS_ROCOTO'] = '&GEFS_ROCOTO;'
    dicENV_VARS['WORKDIR'] = '&WORKDIR;'
    dicENV_VARS['EXPID'] = '&EXPID;'
    dicENV_VARS['KEEP_DIR'] = '&KEEP_DIR;'
    dicENV_VARS['HPSS_DIR'] = '&HPSS_DIR;'
    dicENV_VARS['INIT_DIR'] = '&INIT_DIR;'
    dicENV_VARS['DIRS_TO_KEEP'] = '&DIRS_TO_KEEP;'
    dicENV_VARS['DIRS_TO_ARCHIVE'] = '&DIRS_TO_ARCHIVE;'
    dicENV_VARS['DIRS_TO_KEEP_WAVE'] = '&DIRS_TO_KEEP_WAVE;'
    dicENV_VARS['DIRS_TO_ARCHIVE_WAVE'] = '&DIRS_TO_ARCHIVE_WAVE;'
    dicENV_VARS['gefs_cych']= '&INCYC;'
    sENV_VARS = ""

    sENV_VARS += sPre + '<!-- Environment Variables -->\n'
    for sKey in dicENV_VARS:
        sENV_VARS += create_envar(name=sKey, value=dicENV_VARS[sKey], sPre=sPre)

    return sENV_VARS


# =======================================================
def create_envar(name=None, value=None, sPre="\t\t"):
    '''
    create an Rocoto environment variable given name and value
    returns the environment variable as a string
    :param name: name of the environment variable
    :type name: str
    :param value: value of the environment variable
    :type value: str or float or int or unicode
    :return: Rocoto environment variable key-value pair
    :rtype: str
    '''
    string = ''
    string += sPre + '<envar>\n'
    string += sPre + '\t<name>{0}</name>\n'.format(name)
    # if value.startswith("@"):
    if "@" in value:
        string += sPre + '\t<value><cyclestr>{0}</cyclestr></value>\n'.format(value)
    else:
        string += sPre + '\t<value>{0}</value>\n'.format(value)
    string += sPre + '</envar>\n'

    return string

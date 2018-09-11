# =======================================================
def config_tasknames(dicBase):
    sVarName = 'taskname_num'.upper()
    iTaskName_Num = int(dicBase[sVarName])

    if iTaskName_Num <= 0:
        iTaskName_Num = 0

        # #    <!-- RUN_GETCFSSST jobs -->
        if dicBase['RUN_GETCFSSST'].upper()[0] == "Y":
            # ---sigchgres
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "getcfssst"

        # #   <!-- initial jobs -->
        if dicBase['RUN_INIT'].upper() == "GSM_RELOC":
            # ---enkf_track
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "enkf_track"

            # ---init_separate
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "init_separate"

            # ---init_process
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "init_process"

            # ---init_combine
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "init_combine"

            # ---init_fv3chgrs
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "init_fv3chgrs"

        elif dicBase['RUN_INIT'].upper() == "FV3_RELOC":
            # ---enkf_track
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "enkf_track"

            # ---init_separate
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "init_separate"

            # ---init_process
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "init_process"

            # ---init_combine
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "init_combine"

            # ---init_fv3chgrs
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "init_fv3chgrs"
            
        elif dicBase['RUN_INIT'].upper() == "FV3_COLD":
            # ---init_recenter
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "init_recenter"

            # ---init_fv3chgrs
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "init_fv3chgrs"
            
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

        # #    <!-- high resolution forecast and post process jobs -->
        if dicBase['RUN_FORECAST_HIGH'].upper()[0] == "Y":
            # ---forecast_high
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "forecast_high"

            # ---post_high
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "post_high"

            # ---prdgen_high
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "prdgen_high"

            # ---ensstat_high
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "ensstat_high"

        # #    <!-- CHGRES jobs -->
        if dicBase['RUN_CHGRES'].upper()[0] == "Y":
            # ---sigchgres
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "sigchgres"

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
        
        # #    <!-- RUN_CLEANUP -->
        if dicBase['RUN_CLEANUP'].upper()[0] == "Y":
            # ---keep_data
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "keep_data"
            # ---archive
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "archive"
            # ---cleanup
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "cleanup"
            
        # final
        dicBase[sVarName] = iTaskName_Num

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

        sPath += sSep + dicBase["WHERE_AM_I"]
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

    sPath += sSep + dicBase["WHERE_AM_I"]
    if not os.path.exists(sPath):
        os.mkdir(sPath)

    sFile = sPath + sSep + "{0}.ent".format(taskname)

    fh = open(sFile, 'w')

    fh.write(strings)

    fh.close()
    # print("exit")

# =======================================================
def get_param_of_task(dicBase, taskname):
    sWalltime = ""
    sNodes = ""
    sMemory = ""
    sJoin = ""
    sDep = ""
    sQueue = ""

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
    if sVarName_nodes in dicBase:
        if sVarName_ppn in dicBase:
            if sVarName_tpp in dicBase:
                sNodes = "{0}:ppn={1}:tpp={2}".format(dicBase[sVarName_nodes], dicBase[sVarName_ppn],
                                                      dicBase[sVarName_tpp])
            else:
                sNodes = "{0}:ppn={1}".format(dicBase[sVarName_nodes], dicBase[sVarName_ppn])
        else:
            if sVarName_tpp in dicBase:
                sNodes = "{0}:tpp={1}".format(dicBase[sVarName_nodes], dicBase[sVarName_tpp])
            else:
                sNodes = "{0}".format(dicBase[sVarName_nodes])

    # for queue
    sVarName = "{0}_queue".format(taskname).upper()
    if sVarName in dicBase:
        sQueue = dicBase[sVarName.upper()]
    
    # for Join
    sVarName = "{0}_join".format(taskname).upper()
    if sVarName in dicBase:
        sJoin = dicBase[sVarName.upper()]

    # for dependency
    sVarName = "{0}_dep".format(taskname).upper()
    if sVarName in dicBase:
        sDep = dicBase[sVarName.upper()]            
        if sDep.strip() != "": # identify whether include 'init_recenter' or not
            
            # For 'init_fv3chgrs' task
            if taskname.lower() == "init_fv3chgrs":
                if DoesTaskExist(dicBase, "init_recenter"): # For Cold Start
                    sDep = '<taskdep task="init_recenter"/>' 
                elif DoesTaskExist(dicBase, "init_combine"):
                    sDep = '<taskdep task="init_combine"/>'
                    

            # For 'forecast_high' task
            if taskname.lower() == "forecast_high": 
                if DoesTaskExist(dicBase, "init_recenter"):
                    if DoesTaskExist(dicBase, "init_fv3chgrs"):
                        if DoesTaskExist(dicBase, "getcfssst"):
                            sDep = '<and>\n\t<taskdep task="init_fv3chgrs_#member#"/>\n\t<taskdep task="getcfssst"/>\n</and>'
                        else:
                            sDep = '<taskdep task="init_fv3chgrs_#member#"/>'
                    else:  # For Warm Start
                        if DoesTaskExist(dicBase, "getcfssst"):   
                            sDep = '<and>\n\t<datadep><cyclestr>&WORKDIR;/nwges/dev/gefs.@Y@m@d/@H/c00/fv3_increment.nc</cyclestr></datadep>\n\t<taskdep task="getcfssst"/>\n</and>'
                        else:
                            sDep = '<datadep><cyclestr>&WORKDIR;/nwges/dev/gefs.@Y@m@d/@H/c00/fv3_increment.nc</cyclestr></datadep>'
                elif DoesTaskExist(dicBase, "copy_init"):
                    if DoesTaskExist(dicBase, "getcfssst"):
                        sDep = '<and>\n\t<taskdep task="copy_init_#member#"/>\n\t<taskdep task="getcfssst"/>\n</and>'
                    else:
                        sDep = '<taskdep task="copy_init_#member#"/>'
                            
            # For Low Resolution
            if taskname.lower() == "post_low" or taskname.lower() == "prdgen_low":
                FHOUTHF=int(dicBase["FHOUTHF".upper()])
                FHOUTLF=int(dicBase["FHOUTLF".upper()])
                fhmaxh=int(dicBase["fhmaxh".upper()])
                FHMAXHF=int(dicBase["FHMAXHF".upper()])

                if FHMAXHF <= fhmaxh:
                    start_hr_low =  fhmaxh + FHOUTLF
                else:
                    start_hr_low =  fhmaxh + FHOUTHF
                sDep = dicBase[sVarName].replace("fXXX","f{0:03d}".format(start_hr_low))

            # For 'enspost' task
            if taskname.lower() == "enspost":
                if DoesTaskExist(dicBase, "ensstat_low"):
                    if DoesTaskExist(dicBase, "prdgen_gfs"):
                        sDep = '<and>\n\t<taskdep task="ensstat_low"/>\n\t<taskdep task="prdgen_gfs"/>\n</and>'
                    else:
                        sDep = '<taskdep task="ensstat_low"/>'
                elif DoesTaskExist(dicBase, "ensstat_high"):  
                    if DoesTaskExist(dicBase, "prdgen_gfs"):
                        sDep = '<and>\n\t<taskdep task="ensstat_high"/>\n\t<taskdep task="prdgen_gfs"/>\n</and>'
                    #else:
                    #    sDep = '<taskdep task="ensstat_high"/>' #Default

            # For 'keep_data' and 'archive' tasks
            if taskname.lower() == "keep_data" or taskname.lower() == "archive":
                if DoesTaskExist(dicBase, "enspost"):
                    if DoesTaskExist(dicBase, "post_track"):
                        if DoesTaskExist(dicBase, "post_genesis"):
                            sDep = '<and>\n\t<taskdep task="enspost"/>\n\t<taskdep task="post_track"/>\n\t<taskdep task="post_genesis"/>\n</and>'
                        else:
                            sDep = '<and>\n\t<taskdep task="enspost"/>\n\t<taskdep task="post_track"/>\n</and>'
                    else:
                        if DoesTaskExist(dicBase, "post_genesis"):
                            sDep = '<and>\n\t<taskdep task="enspost"/>\n\t<taskdep task="post_genesis"/>\n</and>'
                        else:
                            sDep = '<and>\n\t<taskdep task="enspost"/>\n</and>'
                else:
                    if DoesTaskExist(dicBase, "post_track"):
                        if DoesTaskExist(dicBase, "post_genesis"):
                            sDep = '<and>\n\t<taskdep task="post_track"/>\n\t<taskdep task="post_genesis"/>\n</and>'
                        else:
                            sDep = '<and>\n\t<taskdep task="post_track"/>\n</and>'
                    else:
                        if DoesTaskExist(dicBase, "post_genesis"):
                            sDep = '<and>\n\t<taskdep task="post_genesis"/>\n</and>'
                        else:
                            sDep = ''

            # Don't clean up if keep_init isn't finished
            if taskname.lower() == "cleanup" and DoesTaskExist(dicBase, "keep_init"):
                sDep = '<and>\n\t' + sDep + '\n\t<metataskdep metatask="keep_init"/>\n</and>'

    # Forecast can be derive from the parm items
    if taskname == 'forecast_high' or taskname == 'forecast_low':
        layout_x = int(dicBase['layout_x'.upper()])
        layout_y = int(dicBase['layout_y'.upper()])
        WRITE_GROUP = int(dicBase['WRITE_GROUP'.upper()])
        WRTTASK_PER_GROUP = int(dicBase['WRTTASK_PER_GROUP'.upper()])
        parallel_threads = int(dicBase['parallel_threads'.upper()])

        WHERE_AM_I = dicBase['WHERE_AM_I'].upper()

        if WHERE_AM_I == 'cray'.upper():
            Task_Node = 24
        elif WHERE_AM_I == "theia".upper():
            Task_Node = 24
        elif WHERE_AM_I == "wcoss_dell_p3".upper():
            Task_Node = 24
        else:
            Task_Node = 24

        iNodes = int((layout_x * layout_y * 6 + WRITE_GROUP * WRTTASK_PER_GROUP) / (Task_Node / parallel_threads))
        iPPN = int((Task_Node / parallel_threads))
        iTPP = parallel_threads

        if WHERE_AM_I.upper() == "wcoss_dell_p3".upper():
            sNodes = "{0}:ppn={1}".format(iNodes, iPPN)
        else:
            sNodes = "{0}:ppn={1}:tpp={2}".format(iNodes, iPPN, iTPP)


    return sWalltime, sNodes, sMemory, sJoin, sDep, sQueue

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
def create_metatask_task(dicBase, taskname="init_fv3chgrs", sPre="\t", GenTaskEnt=False):
    # walltime = "00:30:00"
    # nodes = "1:ppn=12:tpp=2"
    # memory = "600M"
    WHERE_AM_I = dicBase['WHERE_AM_I']
    sWalltime, sNodes, sMemory, sJoin, sDep, sQueue = get_param_of_task(dicBase, taskname)

    metatask_names = []
    metatask_names.append('init_fv3chgrs')
    metatask_names.append('keep_init')
    metatask_names.append('copy_init')
    # forecast
    metatask_names.append('forecast_high')
    metatask_names.append('forecast_low')
    # post
    metatask_names.append('post_high')
    metatask_names.append('post_low')
    # prdgen
    metatask_names.append('prdgen_high')
    metatask_names.append('prdgen_low')

    jobname = get_jobname(taskname)
    if taskname in metatask_names:
        jobname += "_#member#"


    strings = ""
    if taskname in metatask_names:
        strings += create_metatask(taskname=taskname, jobname=jobname, sWalltime=sWalltime, sNodes=sNodes, \
                                   sMemory=sMemory, sJoin=sJoin, sDep=sDep, sQueue=sQueue, sPre=sPre, \
                                   GenTaskEnt=GenTaskEnt, WHERE_AM_I=WHERE_AM_I)
    else:
        strings += create_task(taskname=taskname, jobname=jobname, sWalltime=sWalltime, sNodes=sNodes, \
                               sMemory=sMemory, sJoin=sJoin, sQueue=sQueue, sDep=sDep, sPre=sPre, GenTaskEnt=GenTaskEnt, \
                               WHERE_AM_I=WHERE_AM_I)

    return strings

# =======================================================
def get_jobname(taskname):
    import os, sys
    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    sDefaultJobID_File = sys.path[0] + sSep + "job_id.conf"
    jobname_short="--"
    if os.path.exists(sDefaultJobID_File):
        # print("---Default Job-ID Configure file was found! Reading ...")
        # print(sDefaultJobID_File)
        dicJobID = read_jobid_config(sDefaultJobID_File)

        if taskname in dicJobID:
            jobname_short = dicJobID[taskname]
            jobname = "&EXPID;@Y@m@d@H_" + jobname_short

            return jobname

    # else if this file does not exist and if the task name is not in the job_id.conf
    tasknames = taskname.split("_")
    if len(tasknames) == 2:
        jobname_short = tasknames[1][0:2] + "_" + tasknames[1][-2:]
    else:
        jobname_short = tasknames[1][0] + tasknames[1][-1] + "_" + tasknames[2][0] + tasknames[2][-1]

    jobname = "&EXPID;@Y@m@d@H_" + jobname_short

    return jobname

# =======================================================
def read_jobid_config(sConfig):
    # read config file
    dicBase={}
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
                    a, b = sLine.split("=",1)

                    a = str(a).strip()
                    b = str(b).strip()

                    if b.startswith('"'):
                        b = b.replace('"', "",1)
                    if b.endswith('"'):
                        b = b[:-1]

                    if b.startswith("'"):
                        b = b.replace(",", "",1)
                    if b.endswith(","):
                        b = b[:-1]

                    b = str(b).strip()

                    dicBase[a] = b

    return dicBase

# =======================================================
def create_metatask(taskname="init_fv3chgrs", jobname="&EXPID;@Y@m@d@H15_#member#", \
                    sWalltime="00:30:00", sNodes="1:ppn=12:tpp=2", sMemory="600M", sJoin="", sDep="", sQueue="", \
                    sPre="\t", GenTaskEnt=False, WHERE_AM_I="cray"):
    cycledef = "gefs"
    maxtries = 1

    strings = ""

    sPre_2 = sPre + "\t\t"

    if GenTaskEnt:
        sENV_VARS = sPre_2 + "&ENV_VARS;\n"
        sDATE_VARS = sPre_2 + "&DATE_VARS;\n"
    else:
        sENV_VARS = get_ENV_VARS(sPre_2)
        sDATE_VARS = get_DATE_VARS(sPre_2)

    strings += sPre + '<!-- **********{0}********** -->\n'.format(taskname)
    if taskname == "prdgen_high" or taskname == "prdgen_low":
        strings += sPre + '<metatask name="{0}" mode="parallel">\n'.format(taskname)
    else:
        strings += sPre + '<metatask name="{0}">\n'.format(taskname)
    strings += sPre + '\t' + '<var name="member">&MEMLIST;</var>\n'

    strings += sPre + '\t' + '<task name="{0}_#member#" cycledefs="{1}" maxtries="{2}">\n'.format(taskname, cycledef, maxtries)

    if "@" in jobname:
        strings += sPre + '\t\t' + '<jobname><cyclestr>{0}</cyclestr></jobname>\n'.format(jobname)
    else:
        strings += sPre + '\t\t' + '<jobname>{0}</jobname>\n'.format(jobname)

    account = "&ACCOUNT;"
    strings += sPre + '\t\t' + '<account>{0}</account>\n'.format(account)

    if sJoin == "":
        if taskname == "init_fv3chgrs":
            strings += sPre + '\t\t' + '<sJoin><cyclestr>&LOG_DIR;/@Y@m@d/gefs_#member#{0}@H.%J</cyclestr></sJoin>\n'.format(
                taskname.replace("jgefs", ""))
        else:
            strings += sPre + '\t\t' + '<join><cyclestr>&LOG_DIR;/@Y@m@d/gefs_#member#{0}_@H.%J</cyclestr></join>\n'.format(
                taskname.replace("jgefs", ""))
    else:
        if "@" in sJoin:
            strings += sPre + '\t\t' + '<join><cyclestr>{0}</cyclestr></join>\n'.format(sJoin)
        else:
            strings += sPre + '\t\t' + '<join>{0}</join>\n'.format(sJoin)

    if sWalltime != "":
        strings += sPre + '\t\t' + '<walltime>{0}</walltime>\n'.format(sWalltime)

    if sQueue != "":
        strings += sPre + '\t\t' + '<queue>{0}</queue>\n'.format(sQueue)
    # strings += sPre + '\t\t' + '<queue>&CUE2RUN;</queue>\n'

    if sNodes != "":
        strings += sPre + '\t\t' + '<nodes>{0}</nodes>\n'.format(sNodes)

    if WHERE_AM_I.upper() == "cray".upper():
        strings += sPre + '\t\t' + '<native>-cwd &tmpnwprd;</native>\n'
    elif WHERE_AM_I.upper() == "Theia".upper():
        strings += "\n"
    elif WHERE_AM_I.upper() == "wcoss_dell_p3".upper():
        strings += "\n"
    else:
        strings += sPre + '\t\t' + '<native>-cwd &tmpnwprd;</native>\n'

    # For sMemory
    if sMemory != "":
        strings += sPre + '\t\t' + '<memory>{0}</memory>\n'.format(sMemory)

    if WHERE_AM_I.upper() == "cray".upper():
        strings += sPre + '\t\t' + '<native>-extsched "CRAYLINUX[]"</native>\n'
    elif WHERE_AM_I.upper() == "Theia".upper():
        strings += "\n"
    elif WHERE_AM_I.upper() == "wcoss_dell_p3".upper():
        strings += "\n"
    else:
        strings += sPre + '\t\t' + '<native>-extsched "CRAYLINUX[]"</native>\n'


    strings += sPre + '\n'
    strings += sENV_VARS
    strings += sDATE_VARS
    strings += sPre + '\t\t' + '<!-- Other Environment Variables -->\n'
    strings += (create_envar(name="SOURCEDIR", value="&SOURCEDIR;", sPre=sPre_2))
    # strings += (create_envar(name="WORKDIR", value="&WORKDIR;", sPre=sPre_2))
    strings += (create_envar(name="job", value=jobname.replace("_#member#", "#member#"), sPre=sPre_2))
    strings += (create_envar(name="RUNMEM", value="ge#member#", sPre=sPre_2))

    if taskname in ['keep_init', 'copy_init']:
        strings += (create_envar(name="MEMBER", value="#member#", sPre=sPre_2))
        strings += sPre + '\t\t' + '<command><cyclestr>&PRE; &BIN;/{0}.py</cyclestr></command>\n'.format(taskname)
    else:
        strings += sPre + '\t\t' + '<command><cyclestr>&PRE; &BIN;/{0}.sh</cyclestr></command>\n'.format(taskname)

    sDep = sDep.replace('\\n', '\n')
    sDep = sDep.replace('\\t', '\t')

    if sDep != "":
        if "\n\t" in sDep:
            sDep = sDep.replace("\n","\n{0}".format(sPre_2 + '\t'))
        strings += sPre_2 + '<dependency>\n'
        strings += sPre_2 + '\t' + sDep + '\n'  # '\t<taskdep task="{0}"/>\n'.format(taskdep)
        strings += sPre_2 + '</dependency>\n'

    strings += sPre + '\t' + '</task>\n'
    strings += sPre + '</metatask>\n\n'
    return strings

# =======================================================
def create_task( \
        taskname="enkf_track", jobname="&EXPID;@Y@m@d@H010", \
        sWalltime="01:50:00", sNodes="2:ppn=20", sMemory="3000M", sJoin="", sDep="",sQueue="", \
        sPre="\t", GenTaskEnt=False, WHERE_AM_I="cray"):
    cycledef = "gefs"
    maxtries = 1

    strings = ""

    sPre_2 = sPre + "\t"

    if GenTaskEnt:
        sENV_VARS = sPre_2 + "&ENV_VARS;\n"
        sDATE_VARS = sPre_2 + "&DATE_VARS;\n"
    else:
        sENV_VARS = get_ENV_VARS(sPre_2)
        sDATE_VARS = get_DATE_VARS(sPre_2)

    strings += sPre + '<!-- **********{0}********** -->\n'.format(taskname)
    strings += sPre + '<task name="{0}" cycledefs="{1}" maxtries="{2}">\n'.format(taskname, cycledef, maxtries)
    if "@" in jobname:
        strings += sPre + '\t' + '<jobname><cyclestr>{0}</cyclestr></jobname>\n'.format(jobname)
    else:
        strings += sPre + '\t' + '<jobname>{0}</jobname>\n'.format(jobname)

    account = "&ACCOUNT;"
    strings += sPre + '\t' + '<account>{0}</account>\n'.format(account)

    if sJoin == "":
        if WHERE_AM_I.upper() == "CRAY":
            strings += sPre + '\t' + '<join><cyclestr>&LOG_DIR;/@Y@m@d/{0}_@H.%J</cyclestr></join>\n'.format(taskname[1:])
        elif WHERE_AM_I.upper() == "THEIA":
            strings += sPre + '\t' + '<join><cyclestr>&LOG_DIR;/@Y@m@d/{0}_@H.%PBS_JOBID</cyclestr></join>\n'.format(taskname[1:])
        elif WHERE_AM_I.upper() == "WCOSS_DELL_P3":
            strings += sPre + '\t' + '<join><cyclestr>&LOG_DIR;/@Y@m@d/{0}_@H.%J</cyclestr></join>\n'.format(taskname[1:])
        else:
            strings += sPre + '\t' + '<join><cyclestr>&LOG_DIR;/@Y@m@d/{0}_@H.%J</cyclestr></join>\n'.format(taskname[1:])
    else:
        if "@" in sJoin:
            strings += sPre + '\t' + '<join><cyclestr>{0}</cyclestr></join>\n'.format(sJoin)
        else:
            strings += sPre + '\t' + '<join>{0}</join>\n'.format(sJoin)

    if sWalltime != "":
        strings += sPre + '\t' + '<walltime>{0}</walltime>\n'.format(sWalltime)

    if sQueue != "":
        strings += sPre + '\t' + '<queue>{0}</queue>\n'.format(sQueue)
    # strings += sPre + '\t' + '<queue>&CUE2RUN;</queue>\n'

    if sNodes != "":
        if WHERE_AM_I.upper() == "cray".upper():
            if taskname == "archive":
                strings += sPre + '\t' + '<nodes>{0}</nodes><shared></shared>\n'.format(sNodes)
            else:
                strings += sPre + '\t' + '<nodes>{0}</nodes>\n'.format(sNodes)
        else:
            strings += sPre + '\t' + '<nodes>{0}</nodes>\n'.format(sNodes)

    if WHERE_AM_I.upper() == "cray".upper():
        strings += sPre + '\t' + '<native>-cwd &tmpnwprd;</native>\n'
    elif WHERE_AM_I.upper() == "theia".upper():
        strings += "\n"
    elif WHERE_AM_I.upper() == "wcoss_dell_p3".upper():
        strings += "\n"
    else:
        strings += sPre + '\t' + '<native>-cwd &tmpnwprd;</native>\n'

    if sMemory != "":
        strings += sPre + '\t' + '<memory>{0}</memory>\n'.format(sMemory)

    if WHERE_AM_I.upper() == "cray".upper():
        if taskname == "archive":
            strings += ""
        else:
            strings += sPre + '\t' + '<native>-extsched "CRAYLINUX[]"</native>\n'
    elif WHERE_AM_I.upper() == "theia".upper():
        strings += "\n"
    elif WHERE_AM_I.upper() == "wcoss_dell_p3".upper():
        strings += "\n"
    else:
        strings += sPre + '\t' + '<native>-extsched "CRAYLINUX[]"</native>\n'

    strings += sPre + '\n'
    strings += sENV_VARS
    strings += sDATE_VARS
    strings += sPre + '\t' + '<!-- Other Environment Variables -->\n'
    strings += (create_envar(name="SOURCEDIR", value="&SOURCEDIR;", sPre=sPre_2))
    # strings += (create_envar(name="WORKDIR", value="&WORKDIR;", sPre=sPre_2))
    #if taskname == "post_genesis":
    #    strings += (create_envar(name="job", value=jobname.replace('H397', 'H307'), sPre=sPre_2))
    #elif taskname == "enspost":
    #    strings += (create_envar(name="job", value=jobname.replace('H395', 'H305'), sPre=sPre_2))
    #else:
    strings += (create_envar(name="job", value=jobname, sPre=sPre_2))

    if taskname in ["ensstat_high", "post_track", "post_genesis", "enspost"]:
        strings += (create_envar(name="RUNMEM", value="#member#", sPre=sPre_2))

    if taskname in ["prdgen_gfs"]:
        strings += (create_envar(name="RUNMEM", value="gfs", sPre=sPre_2))

    if taskname in ['keep_data', 'archive', 'cleanup']:
        strings += sPre + '\t' + '<command><cyclestr>&PRE; &BIN;/{0}.py</cyclestr></command>\n'.format(taskname)
    else:
        strings += sPre + '\t' + '<command><cyclestr>&PRE; &BIN;/{0}.sh</cyclestr></command>\n'.format(taskname)

    sDep = sDep.replace('\\n', '\n')
    sDep = sDep.replace('\\t', '\t')

    if sDep != "":
        if "\n\t" in sDep:
            sDep = sDep.replace("\n","\n{0}".format(sPre_2 + '\t'))
        strings += sPre_2 + '<dependency>\n'
        strings += sPre_2 + '\t' + sDep + '\n'  # '\t<taskdep task="{0}"/>\n'.format(taskdep)
        strings += sPre_2 + '</dependency>\n'

    strings += sPre + '</task>\n\n'
    return strings

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
    dicENV_VARS['gefsmpexec'] = 'mpirun.lsf'
    dicENV_VARS['gefsmpexec_mpmd'] = 'mpirun.lsf'
    dicENV_VARS['WHERE_AM_I'] = '&WHERE_AM_I;'
    dicENV_VARS['GEFS_ROCOTO'] = '&GEFS_ROCOTO;'
    dicENV_VARS['WORKDIR'] = '&WORKDIR;'
    dicENV_VARS['EXPID'] = '&EXPID;'
    dicENV_VARS['KEEP_DIR'] = '&KEEP_DIR;'
    dicENV_VARS['HPSS_DIR'] = '&HPSS_DIR;'
    dicENV_VARS['INIT_DIR'] = '&INIT_DIR;'
    dicENV_VARS['DIRS_TO_KEEP'] = '&DIRS_TO_KEEP;'
    dicENV_VARS['DIRS_TO_ARCHIVE'] = '&DIRS_TO_ARCHIVE;'
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


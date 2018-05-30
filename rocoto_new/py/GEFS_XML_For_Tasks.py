# =======================================================
def config_tasknames(dicBase):
    sVarName = 'taskname_num'.upper()
    iTaskName_Num = int(dicBase[sVarName])

    if iTaskName_Num <= 0:
        iTaskName_Num = 0

	# #    <!-- initial jobs -->
        if dicBase['RUN_INIT'] == "GSM_RELOC":
            # ---jgefs_enkf_track
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_enkf_track"

            # ---jgefs_init_separate
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_init_separate"

            # ---
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_init_process"

            # ---jgefs_init_combine
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_init_combine"

            # ---jgefs_init_fv3chgrs
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_init_fv3chgrs"

        elif dicBase['RUN_INIT'] == "FV3_RELOC":
            # ---jgefs_enkf_track
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_enkf_track"

            # ---jgefs_init_separate
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_init_separate"

            # ---
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_init_process"

            # ---jgefs_init_combine
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_init_combine"

            # ---jgefs_init_fv3chgrs
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_init_fv3chgrs"
        elif dicBase['RUN_INIT'] == "FV3_COLD":
            # ---jgefs_init_recenter
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_init_recenter"

            # ---jgefs_init_fv3chgrs
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_init_fv3chgrs"
        elif dicBase['RUN_INIT'] == "FV3_WARM":
            # ---jgefs_init_recenter
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_init_recenter"

        # #    <!-- high resolution forecast and post process jobs -->
        if dicBase['RUN_FORECAST_HIGH'] == "YES" or dicBase['RUN_FORECAST_HIGH'][0] == "Y":
            # ---jgefs_forecast_high
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_forecast_high"

            # ---jgefs_post_high
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_post_high"

            # ---jgefs_prdgen_high
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_prdgen_high"

            # ---jgefs_ensstat_high
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_ensstat_high"

        # will be used in the future
        # @** if RUN_CHGRES==YES
        #   <!-- chargeres jobs -->
        #    &jgefs_sigchgres;
        # @** endif
   

        # #    <!-- low resolution forecast and post process jobs -->
        if dicBase['RUN_FORECAST_LOW'] == "YES" or dicBase['RUN_FORECAST_LOW'][0] == "Y":
            # ---jgefs_forecast_low
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_forecast_low"

            # ---jgefs_post_low
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_post_low"

            # ---jgefs_prdgen_low
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_prdgen_low"

            # ---jgefs_ensstat_low
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_ensstat_low"

        # #    <!-- track and gensis jobs -->
        if dicBase['RUN_TRACK'] == "YES" or dicBase['RUN_TRACK'][0] == "Y":
            # ---jgefs_enkf_track
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_post_track"

            # ---jgefs_post_genesis
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_post_genesis"

        # #    <!-- other jobs -->
        if dicBase['RUN_OTHERS'] == "YES" or dicBase['RUN_OTHERS'][0] == "Y":
            # ---jgefs_enspost
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_enspost"

        if dicBase['RUN_CLEANUP'] == "YES" or dicBase['RUN_CLEANUP'][0] == "Y":
            # ---jgefs_keep_data
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_keep_data"
            # ---jgefs_archive
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_archive"
            # ---jgefs_cleanup
            iTaskName_Num += 1
            sTaskName = "taskname_{0}".format(iTaskName_Num)
            dicBase[sTaskName.upper()] = "jgefs_cleanup"
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

    # print(strings)
    sFile = sPath + sSep + "{0}.ent".format(taskname)
    # print(sFile)
    # exit()
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

    sWalltime, sNodes, sMemory, sJoin, sDep, sQueue= get_Default_TaskParam(taskname)

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
        if sDep.strip() != "": # identify whether include 'jgefs_init_recenter' or not
            # For Cold Start
            if taskname.lower() == "jgefs_init_fv3chgrs":
                sRecenterTask = "jgefs_init_recenter"
                if dicBase['taskname_1'.upper()].lower() == sRecenterTask:
                    if dicBase['taskname_2'.upper()].lower() == "jgefs_init_fv3chgrs":
                        sDep = '<taskdep task="jgefs_init_recenter"/>'

            # For Warm Start
            if taskname.lower() == "jgefs_forecast_high":
                sRecenterTask = "jgefs_init_recenter"
                if dicBase['taskname_1'.upper()].lower() == sRecenterTask:
                    if dicBase['taskname_2'.upper()].lower() == "jgefs_forecast_high":
                        sDep = '<datadep><cyclestr>&WORKDIR;/nwges/dev/gefs.@Y@m@d/@H/c00/C384_@Y@m@d@H/fv3_increment.nc</cyclestr></datadep>'

            # For Low Resolution
            if taskname.lower() == "jgefs_post_low" or taskname.lower() == "jgefs_prdgen_low":
                start_hr_low = int(dicBase["fhmaxh".upper()]) + int(dicBase["FHOUTHR".upper()])
                sDep = dicBase[sVarName].replace("fXXX","f{0:03d}".format(start_hr_low))

    # Forecast can be derive from the parm items
    if taskname == 'jgefs_forecast_high' or taskname == 'jgefs_forecast_low':
        layout_x = int(dicBase['layout_x'.upper()])
        layout_y = int(dicBase['layout_y'.upper()])
        WRITE_GROUP = int(dicBase['WRITE_GROUP'.upper()])
        WRTTASK_PER_GROUP = int(dicBase['WRTTASK_PER_GROUP'.upper()])
        parallel_threads = int(dicBase['parallel_threads'.upper()])

        WHERE_AM_I = dicBase['WHERE_AM_I']

        if WHERE_AM_I == 'cray':
            Task_Node = 24
        elif WHERE_AM_I == "theia":
            Task_Node = 24
        else:
            Task_Node = 24

        iNodes = int((layout_x * layout_y * 6 + WRITE_GROUP * WRTTASK_PER_GROUP) / (Task_Node / parallel_threads))
        iPPN = int((Task_Node / parallel_threads))
        iTPP = parallel_threads

        sNodes = "{0}:ppn={1}:tpp={2}".format(iNodes, iPPN, iTPP)

    return sWalltime, sNodes, sMemory, sJoin, sDep, sQueue

# =======================================================
def get_Default_TaskParam(taskname):
    sWalltime = ""
    sNodes = ""
    sMemory = ""
    sJoin = ""
    sDep = ""

    if taskname == 'jgefs_enkf_track':  # 01
        sWalltime = "01:50:00"
        sNodes = "2:ppn=20"
        sMemory = "3000M"
        sJoin = "&LOG_DIR;/@Y@m@d/gefs_enkf_track_@H.$PBS_JOBID"
        sDep = ""
        sQueue = '&CUE2RUN;'
    elif taskname == 'jgefs_init_separate':  # 02
        sWalltime = "01:50:00"
        sNodes = "10:ppn=4:tpp=6"
        sMemory = "3000M"
        sJoin = "&LOG_DIR;/@Y@m@d/gefs_init_separate_@H.$PBS_JOBID"
        sDep = '<taskdep task="jgefs_enkf_track"/>'
        sQueue = '&CUE2RUN;'
    elif taskname == 'jgefs_init_process':  # 03
        sWalltime = "01:50:00"
        sNodes = "8:ppn=4:tpp=6"
        sMemory = "3000M"
        sJoin = "&LOG_DIR;/@Y@m@d/gefs_init_process_@H.$PBS_JOBID"
        sDep = '<taskdep task="jgefs_init_separate"/>'
        sQueue = '&CUE2RUN;'
    elif taskname == 'jgefs_init_combine':  # 04
        sWalltime = "01:50:00"
        sNodes = "20:ppn=2"
        sMemory = "3000M"
        sJoin = "&LOG_DIR;/@Y@m@d/gefs_init_combine_@H.$PBS_JOBID"
        sDep = '<taskdep task="jgefs_init_process"/>'
        sQueue = '&CUE2RUN;'
    elif taskname == 'jgefs_init_fv3chgrs':  # 05
        sWalltime = "00:30:00"
        sNodes = "1:ppn=12:tpp=2"
        sMemory = "600M"
        sJoin = "&LOG_DIR;/@Y@m@d/gefs_#member#_init_fv3chgrs@H.$PBS_JOBID"
        sDep = '<taskdep task="jgefs_init_combine"/>'
        sQueue = '&CUE2RUN;'
    elif taskname == 'jgefs_forecast_high':  # 06
        sWalltime = "03:00:00"
        sNodes = "20:ppn=12:tpp=2"
        sMemory = "600M"
        sJoin = "&LOG_DIR;/@Y@m@d/gefs_#member#_forecast_high_@H.$PBS_JOBID"
        sDep = '<taskdep task="jgefs_init_fv3chgrs_#member#"/>'
        sQueue = '&CUE2RUN;'
    elif taskname == 'jgefs_post_high':  # 07
        sWalltime = "01:30:00"
        sNodes = "1:ppn=12:tpp=2"
        sMemory = "3000M"
        sJoin = "&LOG_DIR;/@Y@m@d/gefs_#member#_post_high_@H.$PBS_JOBID"
        sDep = "<datadep><cyclestr>&DATA_DIR;/gefs.@Y@m@d/@H/sfcsig/#member#/ge#member#.t@Hz.logf000.nemsio</cyclestr></datadep>"
        sQueue = '&CUE2RUN;'
    elif taskname == 'jgefs_prdgen_high':  # 08
        sWalltime = "01:30:00"
        sNodes = "1:ppn=5:tpp=4"
        sMemory = "3000M"
        sJoin = "&LOG_DIR;/@Y@m@d/gefs_#member#_prdgen_high_@H.$PBS_JOBID"
        sDep = '<datadep><cyclestr>&DATA_DIR;/gefs.@Y@m@d/@H/misc/post/ge#member#.t@Hz.master.control.f00</cyclestr></datadep>'
        sQueue = '&CUE2RUN;'
    elif taskname == 'jgefs_ensstat_high':  # 09
        sWalltime = "01:30:00"
        sNodes = "1:ppn=3:tpp=4"
        sMemory = "3000M"
        sJoin = "&LOG_DIR;/@Y@m@d/gefs_ensstat_high_@H.$PBS_JOBID" # "&LOG_DIR;/@Y@m@d/gefs_#member#_prdgen_high_@H.$PBS_JOBID"
        sDep = '<metataskdep metatask="jgefs_prdgen_high"/>' #'<datadep><cyclestr>&DATA_DIR;/gefs.@Y@m@d/@H/misc/post/ge#member#.t@Hz.master.control.f00</cyclestr></datadep>'
        sQueue = '&CUE2RUN;'
    elif taskname == 'jgefs_post_track':  # 10
        sWalltime = "00:45:00"
        sNodes = "3:ppn=7"
        sMemory = "1000M"
        sJoin = "&LOG_DIR;/@Y@m@d/gefs_post_track_@H.$PBS_JOBID"
        sDep = '<metataskdep metatask="jgefs_prdgen_high"/>'
        sQueue = '&CUE2RUN;'
    elif taskname == 'jgefs_post_genesis':  # 11
        sWalltime = "00:45:00"
        sNodes = "3:ppn=7"
        sMemory = "3000M"
        sJoin = "&LOG_DIR;/@Y@m@d/gefs_post_genesis_@H.$PBS_JOBID"
        sDep = '<metataskdep metatask="jgefs_prdgen_high"/>'
        sQueue = '&CUE2RUN;'
    elif taskname == 'jgefs_enspost':  # 12
        sWalltime = "01:29:00"
        sNodes = "1:ppn=1:tpp=24"
        sMemory = "3000M"
        sJoin = "&LOG_DIR;/@Y@m@d/gefs_enspost_@H.$PBS_JOBID"
        sDep = '<taskdep task="jgefs_ensstat_high"/>'
        sQueue = '&CUE2RUN;'
    elif taskname == 'jgefs_keep_data':  # 13
        sWalltime = "00:30:00"
        sNodes = "1:ppn=1:tpp=1"
        sMemory = "3000M"
        sJoin = "&LOG_DIR;/@Y@m@d/gefs_keep_data_@H.$PBS_JOBID"
        sDep = '<taskdep task="jgefs_enspost"/>'
        sQueue = '&CUE2RUN;'
    elif taskname == 'jgefs_archive':  # 14
        sWalltime = "06:00:00"
        sNodes = "1:ppn=1:tpp=1"
        sMemory = "3000M"
        sJoin = "&LOG_DIR;/@Y@m@d/gefs_archive_@H.$PBS_JOBID"
        sDep = '<taskdep task="jgefs_enspost"/>'
        sQueue = '&TRANSFER_QUEUE;'
    elif taskname == 'jgefs_keep_data':  # 15
        sWalltime = "00:30:00"
        sNodes = "1:ppn=1:tpp=1"
        sMemory = "3000M"
        sJoin = "&LOG_DIR;/@Y@m@d/gefs_keep_data_@H.$PBS_JOBID"
        sDep = '<taskdep task="jgefs_enspost"/>'
        sQueue = '&CUE2RUN;'
    else:
        sWalltime = "01:50:00"
        sNodes = "2:ppn=20"
        sMemory = "3000M"
        sJoin = ""
        sDep = ""
        sQueue = ""
    return sWalltime, sNodes, sMemory, sJoin, sDep, sQueue

# =======================================================
def create_metatask_task(dicBase, taskname="jgefs_init_fv3chgrs", sPre="\t", GenTaskEnt=False):
    # walltime = "00:30:00"
    # nodes = "1:ppn=12:tpp=2"
    # memory = "600M"
    WHERE_AM_I = dicBase['WHERE_AM_I']
    sWalltime, sNodes, sMemory, sJoin, sDep, sQueue = get_param_of_task(dicBase, taskname)

    metatask_names = []
    metatask_names.append('jgefs_init_fv3chgrs')
    # forecast
    metatask_names.append('jgefs_forecast_high')
    metatask_names.append('jgefs_forecast_low')
    # post
    metatask_names.append('jgefs_post_high')
    metatask_names.append('jgefs_post_low')
    # prdgen
    metatask_names.append('jgefs_prdgen_high')
    metatask_names.append('jgefs_prdgen_low')

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
def create_metatask(taskname="jgefs_init_fv3chgrs", jobname="&EXPID;@Y@m@d@H15_#member#", \
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
    if taskname == "jgefs_prdgen_high" or taskname == "jgefs_prdgen_low":
        strings += sPre + '<metatask name="{0}" mode="parallel">\n'.format(taskname)
    else:
        strings += sPre + '<metatask>\n'
    strings += sPre + '\t' + '<var name="member">&MEMLIST;</var>\n'

    if taskname == "jgefs_forecast_high" or taskname == "jgefs_post_high":
        strings += sPre + '\t' + '<task name="{0}1_#member#" cycledefs="{1}" maxtries="{2}">\n'.format(taskname,
                                                                                                       cycledef,
                                                                                                       maxtries)
    else:
        strings += sPre + '\t' + '<task name="{0}_#member#" cycledefs="{1}" maxtries="{2}">\n'.format(taskname,
                                                                                                      cycledef,
                                                                                                      maxtries)

    if "@" in jobname:
        strings += sPre + '\t\t' + '<jobname><cyclestr>{0}</cyclestr></jobname>\n'.format(jobname)
    else:
        strings += sPre + '\t\t' + '<jobname>{0}</jobname>\n'.format(jobname)

    account = "&ACCOUNT;"
    strings += sPre + '\t\t' + '<account>{0}</account>\n'.format(account)

    if sJoin == "":
        if taskname == "jgefs_init_fv3chgrs":
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
    else:
        strings += sPre + '\t\t' + '<native>-cwd &tmpnwprd;</native>\n'

    # For sMemory
    if sMemory != "":
        strings += sPre + '\t\t' + '<memory>{0}</memory>\n'.format(sMemory)

    if WHERE_AM_I.upper() == "cray".upper():
        strings += sPre + '\t\t' + '<native>-extsched "CRAYLINUX[]"</native>\n'
    elif WHERE_AM_I.upper() == "Theia".upper():
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

    strings += sPre + '\t\t' + '<command><cyclestr>&PRE; &BIN;/{0}.sh</cyclestr></command>\n'.format(taskname)

    if sDep != "":
        strings += sPre_2 + '<dependency>\n'
        strings += sPre_2 + '\t' + sDep + '\n'  # '\t<taskdep task="{0}"/>\n'.format(taskdep)
        strings += sPre_2 + '</dependency>\n'
    # strings += get_dependency(taskname, sPre=sPre_2)

    strings += sPre + '\t' + '</task>\n'
    strings += sPre + '</metatask>\n\n'
    return strings

# =======================================================
def create_task( \
        taskname="jgefs_enkf_track", jobname="&EXPID;@Y@m@d@H010", \
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
            if taskname == "jgefs_archive":
                strings += sPre + '\t' + '<nodes>{0}</nodes><shared></shared>\n'.format(sNodes)
            else:
                strings += sPre + '\t' + '<nodes>{0}</nodes>\n'.format(sNodes)
        else:
            strings += sPre + '\t' + '<nodes>{0}</nodes>\n'.format(sNodes)

    if WHERE_AM_I.upper() == "cray".upper():
        strings += sPre + '\t' + '<native>-cwd &tmpnwprd;</native>\n'
    elif WHERE_AM_I.upper() == "theia".upper():
        strings += "\n"
    else:
        strings += sPre + '\t' + '<native>-cwd &tmpnwprd;</native>\n'

    if sMemory != "":
        strings += sPre + '\t' + '<memory>{0}</memory>\n'.format(sMemory)

    if WHERE_AM_I.upper() == "cray".upper():
        if taskname == "jgefs_archive":
            strings += ""
        else:
            strings += sPre + '\t' + '<native>-extsched "CRAYLINUX[]"</native>\n'
    elif WHERE_AM_I.upper() == "theia".upper():
        strings += "\n"
    else:
        strings += sPre + '\t' + '<native>-extsched "CRAYLINUX[]"</native>\n'

    strings += sPre + '\n'
    strings += sENV_VARS
    strings += sDATE_VARS
    strings += sPre + '\t' + '<!-- Other Environment Variables -->\n'
    strings += (create_envar(name="SOURCEDIR", value="&SOURCEDIR;", sPre=sPre_2))
    # strings += (create_envar(name="WORKDIR", value="&WORKDIR;", sPre=sPre_2))
    if taskname == "jgefs_post_genesis":
        strings += (create_envar(name="job", value=jobname.replace('H397', 'H307'), sPre=sPre_2))
    elif taskname == "jgefs_enspost":
        strings += (create_envar(name="job", value=jobname.replace('H395', 'H305'), sPre=sPre_2))
    else:
        strings += (create_envar(name="job", value=jobname, sPre=sPre_2))

    if taskname in ["jgefs_ensstat_high", "jgefs_post_track", "jgefs_post_genesis", "jgefs_enspost"]:
        strings += (create_envar(name="RUNMEM", value="#member#", sPre=sPre_2))

    if taskname in ['jgefs_keep_data', 'jgefs_archive', 'jgefs_cleanup']:
        strings += sPre + sPre + '<command><cyclestr>&PRE; &BIN;/{0}.py</cyclestr></command>\n'.format(taskname)
    else:
        strings += sPre + sPre + '<command><cyclestr>&PRE; &BIN;/{0}.sh</cyclestr></command>\n'.format(taskname)

    sDep = sDep.replace('\\n', '\n')
    sDep = sDep.replace('\\t', '\t')

    if sDep != "":
        strings += sPre_2 + '<dependency>\n'
        strings += sPre_2 + '\t' + sDep + '\n'  # '\t<taskdep task="{0}"/>\n'.format(taskdep)
        strings += sPre_2 + '</dependency>\n'
    # strings += get_dependency(taskname, sPre=sPre_2)

    strings += sPre + '</task>\n\n'
    return strings

# =======================================================
def get_dependency(taskname, sPre="\t\t"):
    strings = ""

    if taskname != "jgefs_enkf_track":
        taskdep = ""
        if taskname == "jgefs_init_separate":  # 02
            taskdep = '<taskdep task="jgefs_enkf_track"/>'
        elif taskname == "jgefs_init_process":  # 03
            taskdep = '<taskdep task="jgefs_init_separate"/>'
        elif taskname == "jgefs_init_combine":  # 04
            taskdep = '<taskdep task="jgefs_init_process"/>'
        elif taskname == "jgefs_init_fv3chgrs":  # 05
            taskdep = '<taskdep task="jgefs_init_combine"/>'
        elif taskname == "jgefs_forecast_high":  # 06
            taskdep = '<taskdep task="jgefs_init_fv3chgrs_#member#"/>'
        elif taskname == "jgefs_post_high":  # 07
            taskdep = '<datadep><cyclestr>&DATA_DIR;/gefs.@Y@m@d/@H/sfcsig/#member#/ge#member#.t@Hz.atmf330.nemsio</cyclestr></datadep>'
        elif taskname == "jgefs_prdgen_high":  # 08
            taskdep = '<datadep><cyclestr>&DATA_DIR;/gefs.@Y@m@d/@H/misc/post/ge#member#.t@Hz.master.control.f00</cyclestr></datadep>'
        elif taskname == "jgefs_ensstat_high":  # 09
            taskdep = '<metataskdep metatask="jgefs_prdgen_high"/>'
        elif taskname == "jgefs_post_track":  # 10
            taskdep = '<metataskdep metatask="jgefs_prdgen_high"/>'
        elif taskname == "jgefs_post_genesis":  # 11
            taskdep = '<metataskdep metatask="jgefs_prdgen_high"/>'
        elif taskname == "jgefs_enspost":  # 12
            taskdep = '<taskdep task="jgefs_ensstat_high"/>'
        elif taskname == "jgefs_archive":  # 13
            taskdep = '<taskdep task="jgefs_enspost"/>'
        elif taskname == "jgefs_cleanup":  # 14
            taskdep = '<and>\n\t<taskdep task="jgefs_keep_data"/>\n\t<taskdep task="jgefs_archive"/>\n</and>'
        elif taskname == "jgefs_keep_data":  # 15
            taskdep = '<taskdep task="jgefs_enspost"/>'

        strings += sPre + '<dependency>\n'
        strings += sPre + '\t' + taskdep + '\n'  # '\t<taskdep task="{0}"/>\n'.format(taskdep)
        strings += sPre + '</dependency>\n'

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


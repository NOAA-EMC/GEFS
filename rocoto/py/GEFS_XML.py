# =======================================================
def assign_default_for_xml_def(dicBase, sRocoto_WS=""):
    import sys
    import os

    sSep = "/"
    # ==
    get_MEMLIST(dicBase)
    # ==
    sVarName = "First".upper()
    sVarValue = 'Xianwu'
    if sVarName not in dicBase:
        import os
        sVarValue = os.environ.get("USER")
        if sVarValue in ['emc.ens']:
            sVarValue = os.environ.get("SUDO_USER")
        if "." in sVarValue:
            sVarValue = sVarValue.split(".")[0]
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "Last".upper()
    sVarValue = 'Xue'
    if sVarName not in dicBase:
        import os
        sVarValue = os.environ.get("USER")
        if sVarValue in ['emc.ens']:
            sVarValue = os.environ.get("SUDO_USER")
        if "." in sVarValue:
            sVarValue = sVarValue.split(".")[1]
        dicBase[sVarName] = sVarValue


    WHERE_AM_I = dicBase["WHERE_AM_I".upper()]

    # ===
    sVarName = "GEFS_ROCOTO".upper()
    sVarValue = ""
    if sVarName not in dicBase:
        sVarValue = sRocoto_WS
    else:
        sVarValue = replace_First_Last(dicBase, sVarName)
    dicBase[sVarName] = sVarValue
    # ===
    sVarName = "EXPID".upper()
    sVarValue = "extsp"
    if sVarName not in dicBase:
        sVarName_2 = "GEFS_ROCOTO".upper()
        sRocoto_Path = dicBase[sVarName_2]
        sVarValue = os.path.basename(os.path.abspath(sRocoto_Path))
        if sVarValue.startswith("rocoto"):
            sVarValue = os.path.basename(os.path.abspath(sRocoto_Path + sSep + ".."))
            if sVarValue == "nwdev":
                sVarValue = os.path.basename(os.path.abspath(sRocoto_Path + sSep + ".." + sSep + ".."))

        dicBase[sVarName] = sVarValue
    else:
        sVarValue = dicBase[sVarName]
    # PSLOT is used by rocoto_viewer and should always be the same as EXPID
    dicBase["PSLOT"] = sVarValue

    # ===
    sVarName = "SOURCEDIR".upper()
    sVarValue = ""
    if sVarName not in dicBase:
        sVarValue = os.path.abspath(sRocoto_WS + sSep + "..")
        if not (os.path.exists(sVarValue + sSep + "parm") and os.path.exists(sVarValue + sSep + "sorc")):
            print('!!! It seems that your GEFS SOURE is not in the same path with ROCOTO, therefore, you must assign a avalue for "SOURCEDIR" in the user configure file!!!')
            exit(-5)
    else:
        sVarValue = replace_First_Last(dicBase, sVarName)
        if not (os.path.exists(sVarValue + sSep + "parm") and os.path.exists(sVarValue + sSep + "sorc")):
            sPathTem = sVarValue + sSep + dicBase["EXPID"]
            if os.path.exists(sPathTem):
                if os.path.exists(sPathTem + sSep + "parm") and os.path.exists(sPathTem + sSep + "sorc"):
                    sVarValue = sPathTem
                else:
                    sPathTem = sVarValue + sSep + dicBase["EXPID"] + sSep + "nwdev"
                    if os.path.exists(sPathTem):
                        if os.path.exists(sPathTem + sSep + "parm") and os.path.exists(sPathTem + sSep + "sorc"):
                            sVarValue = sPathTem
                        else:
                            print("Please check your SOURCEDIR - {0}".format(sVarValue))
                            exit(-6)
            else:
                sPathTem = sVarValue + sSep + "nwdev"
                if os.path.exists(sPathTem) and os.path.exists(sPathTem + sSep + "parm") and os.path.exists(sPathTem + sSep + "sorc"):
                    sVarValue = sPathTem
                    # sVarValue += "/&EXPID;/nwdev"

    dicBase[sVarName] = sVarValue
    # ===
    sVarName = "WORKDIR".upper()
    if sVarName not in dicBase:
        sVarValue = ""
        if WHERE_AM_I.lower() == 'hera':
            sVarValue = "/scratch2/NCEPDEV/stmp3/First.Last/o/&EXPID;"
        elif WHERE_AM_I.lower() == 'wcoss2':
            sVarValue = "/lfs/HPS_PTMP/emc/ptmp/First.Last/o/&EXPID;"
        else:
            sVarValue = "/gpfs/HPS_PTMP/ptmp/First.Last/o/&EXPID;"

        dicBase[sVarName] = sVarValue

    sVarValue = replace_First_Last(dicBase, sVarName)
    sVarValue = sVarValue.replace("HPS_PTMP", dicBase["HPS_PTMP"])
    if sVarValue.endswith("/o"):
        sVarValue += '/&EXPID;'
    else:
        sVarValue += '/o/&EXPID;'
    sVarValue = sVarValue.replace('&EXPID;', dicBase['EXPID'])
    dicBase[sVarName] = sVarValue

    # ===
    sVarName = "KEEP_DIR".upper()
    sVarValue = ""
    if sVarName not in dicBase:
        if WHERE_AM_I.lower() == 'hera':
            sVarValue = "/scratch2/NCEPDEV/stmp3/First.Last/GEFS/&EXPID;"
        elif WHERE_AM_I.lower() == 'wcoss2':
            sVarValue = "/lfs/h2/emc/ens/noscrub/First.Last/GEFS/&EXPID;"
        else:
            sVarValue = "/gpfs/HPS_PTMP/emc/ensemble/noscrub/First.Last/GEFS/&EXPID;"

        dicBase[sVarName] = sVarValue
    else:
        sVarValue = dicBase[sVarName] + "/&EXPID;"
        dicBase[sVarName] = sVarValue

    sVarValue = replace_First_Last(dicBase, sVarName)
    sVarValue = sVarValue.replace("HPS_PTMP", dicBase["HPS_PTMP"])
    if sVarValue.endswith("/&EXPID;"):
        sVarValue = sVarValue.replace('&EXPID;', dicBase['EXPID'])
    else:
        sVarValue += dicBase['EXPID']

    if not sVarValue.endswith(dicBase['EXPID']):
        sVarValue += dicBase['EXPID']

    dicBase[sVarName] = sVarValue

    # ===
    sVarName = "HPSS_DIR".upper()
    if sVarName not in dicBase:
        sVarValue = ""
        if WHERE_AM_I.lower() == 'hera':
            sVarValue = "/NCEPDEV/emc-ensemble/2year/First.Last/GEFS/&EXPID;"
        elif WHERE_AM_I.lower() == 'wcoss2':
            sVarValue = "/NCEPDEV/emc-ensemble/2year/First.Last/GEFS/&EXPID;"
        else:
            sVarValue = "/NCEPDEV/emc-ensemble/2year/First.Last/GEFS/&EXPID;"

        dicBase[sVarName] = sVarValue
    else:
        sVarValue = dicBase[sVarName] + "/&EXPID;"
        dicBase[sVarName] = sVarValue

    sVarValue = replace_First_Last(dicBase, sVarName)
    sVarValue = sVarValue.replace("HPS_PTMP", dicBase["HPS_PTMP"])
    if sVarValue.endswith("/&EXPID;"):
        sVarValue = sVarValue.replace('&EXPID;', dicBase['EXPID'])
    else:
        sVarValue += dicBase['EXPID']

    if not sVarValue.endswith(dicBase['EXPID']):
        sVarValue += dicBase['EXPID']

    dicBase[sVarName] = sVarValue
    # ===
    sVarName = "INIT_DIR".upper()
    sVarValue = ""
    if sVarName not in dicBase:
        if WHERE_AM_I.lower() == 'hera':
            sVarValue = "/scratch2/NCEPDEV/stmp3/First.Last/GEFS_INIT/" + dicBase['RUN_INIT'].lower() + "_init"
        elif WHERE_AM_I.lower() == 'wcoss2':
            sVarValue = "/lfs/h1/emc/ens/noscrub/First.Last/GEFS_INIT/" + dicBase['RUN_INIT'].lower() + "_init"
        else:
            sVarValue = "/gpfs/HPS_PTMP/emc/ensemble/noscrub/First.Last/GEFS_INIT/" + dicBase['RUN_INIT'].lower() + "_init"

        dicBase[sVarName] = sVarValue

    sVarValue = replace_First_Last(dicBase, sVarName)
    sVarValue = sVarValue.replace("HPS_PTMP", dicBase["HPS_PTMP"])
    dicBase[sVarName] = sVarValue


# =======================================================
def replace_First_Last(dicBase, sVarName):
    # to replace the first and last names in the strValue
    # Modified on 10/17/2018 to avoid the path has individual "First" or "Last" to be replaced by mistake. If just replace the "First.Last", it will be safer.
    import os
    sUSER = os.environ.get("USER")
    GroupNames = ['emc.ens']
    if sUSER in GroupNames:
        sVarValue = str(dicBase[sVarName]).replace("First.Last", sUSER + "/" + dicBase["FIRST"] + "." + dicBase["LAST"])
    else:
        sVarValue = str(dicBase[sVarName]).replace("First.Last", dicBase["FIRST"] + "." + dicBase["LAST"])

    return sVarValue


# =======================================================
def create_xml(dicBase):
    # print("..Generating XML file ...")
    preamble = get_preamble()
    definitions = get_definitions(dicBase)
    workflow = get_workflow_body(dicBase)

    # Start writing the XML file
    sXML_File = dicBase["GEFS_ROCOTO"] + "/" + dicBase["XML"]

    fh = open(sXML_File, 'w')

    fh.write(preamble)
    fh.flush()

    fh.write(definitions)
    fh.flush()

    # fh.write(resources)
    fh.write(workflow)
    fh.flush()

    fh.close()

    # print("..Generated XML file!")


# =======================================================
def get_preamble():
    '''
        Generate preamble for XML
    '''
    from datetime import datetime

    strings = []

    strings.append('<?xml version="1.0"?>\n')
    strings.append('<!DOCTYPE workflow\n')
    strings.append('[\n')
    strings.append('\t<!--\n')
    strings.append('\tPROGRAM\n')
    strings.append('\t\tMain workflow manager for Global Ensemble Forecast System\n')
    strings.append('\n')
    strings.append('\tAUTHOR:\n')
    strings.append('\t\tXianwu Xue\n')
    strings.append('\t\tXianwu.Xue@noaa.gov\n')
    strings.append('\n')
    strings.append('\tNOTES:\n')
    strings.append('\t\tThis workflow was automatically generated at %s\n' % datetime.now())
    strings.append('\t-->\n')

    return ''.join(strings)


# =======================================================
def get_definitions(dicBase):
    '''
        Create entities related to the experiment
    '''

    lstEntity = ["MEMLIST", "CYCLE_THROTTLE", "TASK_THROTTLE", "SDATE", "EDATE", \
                 "INCYC", "WHERE_AM_I", "GEFS_ROCOTO", "BIN", "PRE", \
                 "WORKFLOW_LOG_DIR", "LOG_DIR", "tmpnwprd", "DATA_DIR", "EXPID", \
                 "PSLOT", "SOURCEDIR", "WORKDIR", "KEEP_DIR", "INIT_DIR", \
                 "HPSS_DIR", "DIRS_TO_KEEP", "DIRS_TO_ARCHIVE", "DIRS_TO_KEEP_WAVE", "DIRS_TO_ARCHIVE_WAVE", \
                 "ACCOUNT", "CUE2RUN", "TRANSFER_QUEUE", "SCHEDULER"]

    strings = []
    strings.append('\n')

    for sVarName in lstEntity:
        sVarValue = dicBase[sVarName.upper()]
        strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    strings.append('\n')


    GenTaskEnt = get_GenTaskEnt(dicBase)
    if GenTaskEnt:
        import sys
        sSep = "/"

        # -----------------------------------------------------------------------------------------------
        strings.append('\t<!-- External entities -->\n')
        strings.append('\t<!ENTITY ENV_VARS   SYSTEM "{0}{1}tasks{1}env_vars.ent">\n'.format(dicBase['GEFS_ROCOTO'], sSep))
        strings.append('\t<!ENTITY DATE_VARS   SYSTEM "{0}{1}tasks{1}date_vars.ent">\n'.format(dicBase['GEFS_ROCOTO'], sSep))
        strings.append('\n')

        # -----------------------------------------------------------------------------------------------
        strings.append('\t<!-- External parameter entities -->\n')
        strings.append('\t<!ENTITY % TASKS    SYSTEM "{0}{1}tasks{1}all.ent">\n'.format(dicBase['GEFS_ROCOTO'], sSep))
        strings.append('\t%TASKS;\n')
        strings.append('\n')

    strings.append('\t<!-- END: Resource requirements for the workflow -->\n')
    strings.append(']>\n')

    return ''.join(strings)


# =======================================================
def get_workflow_body(dicBase):
    '''
        Create the workflow body
    '''

    import datetime as dt
    StartDate = dt.datetime.strptime(dicBase['SDATE'][0:10], "%Y%m%d%H")
    EndDate = dt.datetime.strptime(dicBase['EDATE'][0:10], "%Y%m%d%H")

    import GEFS_XML_For_Tasks as gefs_xml_for_tasks

    GenTaskEnt = get_GenTaskEnt(dicBase)

    # print("---Config your tasks...")
    gefs_xml_for_tasks.config_tasknames(dicBase)

    gefs_xml_for_tasks.write_to_all_ent(GenTaskEnt, dicBase)

    strings = []

    strings.append('\n')
    strings.append('<workflow realtime="F" cyclethrottle="&CYCLE_THROTTLE;" scheduler="&SCHEDULER;" taskthrottle="&TASK_THROTTLE;">\n')
    strings.append('\n')
    strings.append('\t<log><cyclestr>&WORKFLOW_LOG_DIR;/gefs@Y@m@d@H.log</cyclestr></log>\n')
    strings.append('\n')
    strings.append('\t<cycledef group="gefs">&SDATE;00 &EDATE;00 &INCYC;:00:00</cycledef>\n')

    for t in range(0,19,6):
        C_Date = dt.datetime(StartDate.year,StartDate.month,StartDate.day,t,0,0)
        if C_Date < StartDate:
            C_Date = C_Date + dt.timedelta(days=1)
        if C_Date <= EndDate:
            strings.append('\t<cycledef group="gefs_{0:02d}z">{1}00 &EDATE;00 24:00:00</cycledef>\n'.format(t,C_Date.strftime('%Y%m%d%H')))

    strings.append('\n')

    sPre = "\t"

    strings.append(sPre + '<!--- init jobs -->\n')

    taskname_num = int(dicBase['taskname_num'.upper()])
    for k in range(taskname_num):
        sTaskName = "taskname_{0}".format(k + 1).upper()
        if sTaskName not in dicBase:
            print('You must assign value of "{0}" in the configure file!'.format(sTaskName))
            exit(0)
        taskname = dicBase[sTaskName]

        # print(taskname)
        if GenTaskEnt:
            strings.append(sPre + "&{0};\n".format(taskname))
            gefs_xml_for_tasks.write_to_ent(taskname, dicBase, GenTaskEnt=GenTaskEnt)
        else:
            strings.append(gefs_xml_for_tasks.create_metatask_task(dicBase, taskname=taskname, sPre=sPre, GenTaskEnt=GenTaskEnt))

    strings.append('\n')
    strings.append('</workflow>\n')

    return ''.join(strings)


# =======================================================
def get_MEMLIST(dicBase):
    ### npert
    ### npert means Number of Perturbation, default value is 20
    ###
    npert = 20
    # To Generate member list
    bltGenerateMEMLIST = False
    sVarName_Num = "npert".upper()
    sVarName_List = 'MEMLIST'.upper()
    if sVarName_Num in dicBase:
        bltGenerateMEMLIST = True
        npert = int(dicBase[sVarName_Num])
    else:
        if sVarName_List in dicBase:
            bltGenerateMEMLIST = False
        else:
            npert = 20
            dicBase[sVarName_Num] = npert
            bltGenerateMEMLIST = True

        npert = int(dicBase[sVarName_Num])

    if npert < 2:
        print("Please note that because of npert<2, so some tasks may not run!\n Especially enstat_hr and enstat_lr!")

    if bltGenerateMEMLIST:
        MEMLIST_Value = ""
        for iNum in range(1, npert + 1):
            MEMLIST_Value += "p{0:02d} ".format(iNum)
        MEMLIST_Value += "c00"
        # print(MEMLIST_Value)
        dicBase[sVarName_List] = MEMLIST_Value


# =======================================================
def get_GenTaskEnt(dicBase):
    sVarName = "GenTaskEnt".upper()
    if sVarName in dicBase:
        sGenTaskEnt = dicBase[sVarName]
        sValue = str(sGenTaskEnt)
        if sValue.upper().startswith('Y'): #str(sGenTaskEnt).upper() == "YES" or str(sGenTaskEnt)[0].upper() == "Y":
            GenTaskEnt = True
        else:
            GenTaskEnt = False
    else:
        GenTaskEnt = False

    return GenTaskEnt

#=======================================================
def assign_default_for_xml_def(dicBase, sRocoto_WS=""):
    import sys
    import os

    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    sVarName = "XML".upper()
    sVarValue = 'gefs.xml'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "db".upper()
    sVarValue = 'gefs.db'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "crontab".upper()
    sVarValue = 'cron_rocoto'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "First".upper()
    sVarValue = 'Xianwu'
    if sVarName not in dicBase:
        import os
        sVarValue = os.environ.get("USER")
        if sVarValue in ['emc.enspara', 'emc.enspara1']:
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
        if sVarValue in ['emc.enspara', 'emc.enspara1']:
            sVarValue = os.environ.get("SUDO_USER")
        if "." in sVarValue:
            sVarValue = sVarValue.split(".")[1]
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "HPS_PTMP".upper()
    sVarValue = 'hps'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "WHERE_AM_I".upper()
    sVarValue = "theia"
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    WHERE_AM_I = dicBase[sVarName]
    # ==
    sVarName = "SDATE".upper()
    sVarValue = "2018012900"
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "EDATE".upper()
    sVarValue = "2018013000"
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ===
    sVarName = "GEFS_ROCOTO".upper()
    sVarValue = ""
    if sVarName not in dicBase:
        sVarValue = sRocoto_WS
    else:
        #sVarValue = str(dicBase[sVarName]).replace("First", dicBase["FIRST"])
        #sVarValue = sVarValue.replace("Last", dicBase["LAST"])
        sVarValue = replace_First_Last(dicBase, sVarName)

    dicBase[sVarName] = sVarValue
    # ===
    sVarName = "EXPID".upper()
    sVarValue = "extsp"
    if sVarName not in dicBase:
        sVarName_2 = "GEFS_ROCOTO".upper()
        sRocoto_Path = dicBase[sVarName_2]
        sVarValue = os.path.basename(os.path.abspath(sRocoto_Path + sSep + ".."))
        if sVarValue == "nwdev":
            sVarValue = os.path.basename(os.path.abspath(sRocoto_Path + sSep + ".." + sSep + ".."))
        dicBase[sVarName] = sVarValue
    # PSLOT is used by rocoto_viewer and should always be the same as EXPID
    dicBase["PSLOT"] = sVarValue

    # ===
    sVarName = "SOURCEDIR".upper()
    sVarValue = ""
    if sVarName not in dicBase:
        sVarValue = os.path.abspath(sRocoto_WS + sSep + "..")
    else:
        #sVarValue = str(dicBase[sVarName]).replace("First", dicBase["FIRST"])
        #sVarValue = sVarValue.replace("Last", dicBase["LAST"])
        sVarValue = replace_First_Last(dicBase, sVarName)
        sVarValue += "/&EXPID;/nwdev"
    dicBase[sVarName] = sVarValue
    # ===
    sVarName = "WORKDIR".upper()
    if sVarName not in dicBase:
        sVarValue = ""
        if WHERE_AM_I.lower() == "cray":
            sVarValue = "/gpfs/HPS_PTMP/ptmp/{0}.{1}/o/&EXPID;" \
                .format(dicBase["FIRST"], dicBase["LAST"])
        elif WHERE_AM_I.lower() == 'wcoss':
            sVarValue = "/gpfs/HPS_PTMP/ptmp/{0}.{1}/o/&EXPID;" \
                .format(dicBase["FIRST"], dicBase["LAST"])
        elif WHERE_AM_I.lower() == 'theia':
            sVarValue = "/scratch4/NCEPDEV/stmp4/{0}.{1}/o/&EXPID;" \
                .format(dicBase["FIRST"], dicBase["LAST"])
        elif WHERE_AM_I.lower() == 'wins':
            sVarValue = os.path.abspath(sRocoto_WS + sSep + "o")
        else:
            sVarValue = "/gpfs/hps3/ptmp/emc.enspara/{0}.{1}/o/&EXPID;" \
                .format(dicBase["FIRST"], dicBase["LAST"])

        sVarValue = sVarValue.replace("HPS_PTMP", dicBase["HPS_PTMP"])
        sVarValue = sVarValue.replace('&EXPID;',dicBase['EXPID'])
        # dicBase[sVarName] = sVarValue
    else:
        #sVarValue = str(dicBase[sVarName]).replace("First", dicBase["FIRST"])
        #sVarValue = sVarValue.replace("Last", dicBase["LAST"])
        sVarValue = replace_First_Last(dicBase, sVarName)
        sVarValue = sVarValue.replace("HPS_PTMP", dicBase["HPS_PTMP"])
        if sVarValue.endswith("/o"):
            sVarValue += '/&EXPID;'
        else:
            sVarValue += '/o/&EXPID;'

    dicBase[sVarName] = sVarValue

    # ===
    sVarName = "KEEP_DIR".upper()
    sVarValue = ""
    if sVarName not in dicBase:
        if WHERE_AM_I.lower() == "cray":
            sVarValue = "/gpfs/hps3/emc/ensemble/noscrub/{0}.{1}/GEFS/&EXPID;" \
                .format(dicBase["FIRST"], dicBase["LAST"])
        elif WHERE_AM_I.lower() == 'wcoss':
            sVarValue = "/gpfs/hps3/emc/ensemble/noscrub/{0}.{1}/GEFS/&EXPID;" \
                .format(dicBase["FIRST"], dicBase["LAST"])
        elif WHERE_AM_I.lower() == 'theia':
            sVarValue = "/scratch4/NCEPDEV/stmp4/{0}.{1}/GEFS/&EXPID;" \
                .format(dicBase["FIRST"], dicBase["LAST"])
        elif WHERE_AM_I.lower() == 'wins':
            sVarValue = os.path.abspath(sRocoto_WS + sSep + "o")
        else:
            sVarValue = "/gpfs/hps3/emc/ensemble/noscrub/{0}.{1}/GEFS/&EXPID;" \
                .format(dicBase["FIRST"], dicBase["LAST"])

        sVarValue = sVarValue.replace("HPS_PTMP", dicBase["HPS_PTMP"])
        sVarValue = sVarValue.replace('&EXPID;', dicBase['EXPID'])
        # dicBase[sVarName] = sVarValue
    else:
        #sVarValue = str(dicBase[sVarName]).replace("First", dicBase["FIRST"])
        #sVarValue = sVarValue.replace("Last", dicBase["LAST"])
        sVarValue = replace_First_Last(dicBase, sVarName)
        sVarValue = sVarValue.replace("HPS_PTMP", dicBase["HPS_PTMP"])
        sVarValue += '/&EXPID;'
    dicBase[sVarName] = sVarValue
    # ===
    sVarName = "HPSS_DIR".upper()
    if sVarName not in dicBase:
        sVarValue = ""
        if WHERE_AM_I.lower() == "cray":
            sVarValue = "/NCEPDEV/emc-ensemble/2year/{0}.{1}/GEFS/&EXPID;" \
                .format(dicBase["FIRST"], dicBase["LAST"])
        elif WHERE_AM_I.lower() == 'wcoss':
            sVarValue = "/NCEPDEV/emc-ensemble/2year/{0}.{1}/GEFS/&EXPID;" \
                .format(dicBase["FIRST"], dicBase["LAST"])
        elif WHERE_AM_I.lower() == 'theia':
            sVarValue = "/NCEPDEV/emc-ensemble/2year/{0}.{1}/GEFS/&EXPID;" \
                .format(dicBase["FIRST"], dicBase["LAST"])
        elif WHERE_AM_I.lower() == 'wins':
            sVarValue = os.path.abspath(sRocoto_WS + sSep + "o")
        else:
            sVarValue = "/NCEPDEV/emc-ensemble/2year/{0}.{1}/GEFS/&EXPID;" \
                .format(dicBase["FIRST"], dicBase["LAST"])

        sVarValue = sVarValue.replace("HPS_PTMP", dicBase["HPS_PTMP"])
        sVarValue = sVarValue.replace('&EXPID;', dicBase['EXPID'])
        # dicBase[sVarName] = sVarValue
    else:
        #sVarValue = str(dicBase[sVarName]).replace("First", dicBase["FIRST"])
        #sVarValue = sVarValue.replace("Last", dicBase["LAST"])
        sVarValue = replace_First_Last(dicBase, sVarName)
        sVarValue = sVarValue.replace("HPS_PTMP", dicBase["HPS_PTMP"])
        sVarValue += '/&EXPID;'
    dicBase[sVarName] = sVarValue
    # ===
    sVarName = "DIRS_TO_KEEP".upper()
    if sVarName not in dicBase:
        sVarValue = ""
        if WHERE_AM_I.lower() == "cray":
            sVarValue = "ensstat,pgrb2a1p0,pgrb2a2p5,pgrb2ap5,tctrack"
        elif WHERE_AM_I.lower() == 'wcoss':
            sVarValue = "ensstat,pgrb2a1p0,pgrb2a2p5,pgrb2ap5,tctrack"
        elif WHERE_AM_I.lower() == 'theia':
            sVarValue = "ensstat,pgrb2a1p0,pgrb2a2p5,pgrb2ap5,tctrack"
        elif WHERE_AM_I.lower() == 'wins':
            sVarValue = "ensstat,pgrb2a1p0,pgrb2a2p5,pgrb2ap5,tctrack"
        else:
            sVarValue = "ensstat,pgrb2a1p0,pgrb2a2p5,pgrb2ap5,tctrack"

        dicBase[sVarName] = sVarValue
    # ===
    sVarName = "DIRS_TO_ARCHIVE".upper()
    if sVarName not in dicBase:
        sVarValue = ""
        if WHERE_AM_I.lower() == "cray":
            sVarValue = "ensstat,pgrb2a1p0,pgrb2a2p5,pgrb2ap5,tctrack"
        elif WHERE_AM_I.lower() == 'wcoss':
            sVarValue = "ensstat,pgrb2a1p0,pgrb2a2p5,pgrb2ap5,tctrack"
        elif WHERE_AM_I.lower() == 'theia':
            sVarValue = "ensstat,pgrb2a1p0,pgrb2a2p5,pgrb2ap5,tctrack"
        elif WHERE_AM_I.lower() == 'wins':
            sVarValue = "ensstat,pgrb2a1p0,pgrb2a2p5,pgrb2ap5,tctrack"
        else:
            sVarValue = "ensstat,pgrb2a1p0,pgrb2a2p5,pgrb2ap5,tctrack"

        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "INCYC".upper()
    sVarValue = 24
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue

    # ===== Default, you don't need to change them
    # ==
    get_MEMLIST(dicBase)
    # ===
    sVarName = "MEMLIST".upper()
    sVarValue = "p01 p02 p03 p04 p05 p06 p07 p08 p09 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 c00"
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "CYCLE_THROTTLE".upper()
    sVarValue = 1
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "TASK_THROTTLE".upper()
    sVarValue = 65
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ===
    sVarName = "BIN".upper()
    sVarValue = "&GEFS_ROCOTO;/bin/&WHERE_AM_I;"
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # =====
    sVarName = "PRE".upper()
    sVarValue = "&GEFS_ROCOTO;/bin/gefs_pre_job.sh"
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # =====
    sVarName = "WORKFLOW_LOG_DIR".upper()
    sVarValue = "&GEFS_ROCOTO;/logs"
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ===
    sVarName = "LOG_DIR".upper()
    sVarValue = "&WORKDIR;/com/output/dev"
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ===
    sVarName = "tmpnwprd".upper()
    sVarValue = "&WORKDIR;/tmpnwprd"
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ===
    sVarName = "DATA_DIR".upper()
    sVarValue = "&WORKDIR;/com/gens/dev"
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # -----------------------------------------------------------------------------------------------
    if WHERE_AM_I.lower() == "wcoss":
        sVarName = "ACCOUNT".upper()
        sVarValue = "GEN-T2O"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue
        # ===
        sVarName = "CUE2RUN".upper()
        sVarValue = "dev2"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue
        # ===
        sVarName = "TRANSFER_QUEUE".upper()
        sVarValue = "dev_transfer"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue
        # ===
        sVarName = "SCHEDULER".upper()
        sVarValue = "lsf"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue
        # ===
    elif WHERE_AM_I.lower() == "cray":
        sVarName = "ACCOUNT".upper()
        sVarValue = "GEN-T2O"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue
        # ===
        sVarName = "CUE2RUN".upper()
        sVarValue = "dev"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue
        # ===
        sVarName = "TRANSFER_QUEUE".upper()
        sVarValue = "dev_transfer"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue
        # ===
        sVarName = "SCHEDULER".upper()
        sVarValue = "lsfcray"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue
    elif WHERE_AM_I.lower() == "theia":
        sVarName = "ACCOUNT".upper()
        sVarValue = "fv3-cpu"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue
        # ===
        sVarName = "CUE2RUN".upper()
        sVarValue = "batch"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue
        # ===
        sVarName = "TRANSFER_QUEUE".upper()
        sVarValue = "service"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue
        # ===
        sVarName = "SCHEDULER".upper()
        sVarValue = "moabtorque"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue
    else:
        sVarName = "ACCOUNT".upper()
        sVarValue = "fv3-cpu"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue
        # ===
        sVarName = "CUE2RUN".upper()
        sVarValue = "batch"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue
        # ===
        sVarName = "TRANSFER_QUEUE".upper()
        sVarValue = "service"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue
        # ===
        sVarName = "SCHEDULER".upper()
        sVarValue = "moabtorque"
        if sVarName not in dicBase:
            dicBase[sVarName] = sVarValue

#=======================================================
def replace_First_Last(dicBase, sVarName):
    # to replace the first and last names in the strValue
    import os
    sUSER = os.environ.get("USER")
    GroupNames = ['emc.enspara', 'emc.enspara1']
    if sUSER in GroupNames:
        sVarValue = str(dicBase[sVarName]).replace("First", sUSER + "/" + dicBase["FIRST"])
    else:
        sVarValue = str(dicBase[sVarName]).replace("First", dicBase["FIRST"])

    sVarValue = sVarValue.replace("Last", dicBase["LAST"])

    return sVarValue

#=======================================================
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

#=======================================================
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

#=======================================================
def get_definitions(dicBase):
    '''
        Create entities related to the experiment
    '''

    strings = []

    strings.append('\n')
    # # if base['INTERVAL'] is None:
    # #     print('cycle INTERVAL cannot be None')
    # #     sys.exit(1)

    sVarName = "MEMLIST"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    # strings.append('\t<!-- <!ENTITY MEMLIST "p01 p02 p03 p04c00">  -->\n')

    sVarName = "CYCLE_THROTTLE"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "TASK_THROTTLE"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "SDATE"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "EDATE"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "INCYC"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    # =====
    sVarName = "WHERE_AM_I"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    # =====
    sVarName = "GEFS_ROCOTO"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    # ===== Default, you don't need to change them
    sVarName = "BIN"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    # =====
    sVarName = "PRE"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    # =====
    sVarName = "WORKFLOW_LOG_DIR"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "LOG_DIR"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "tmpnwprd"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "DATA_DIR"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "EXPID"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "PSLOT"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "SOURCEDIR"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "WORKDIR"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "KEEP_DIR"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "HPSS_DIR"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "DIRS_TO_KEEP"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "DIRS_TO_ARCHIVE"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    strings.append('\n')

    # # -----------------------------------------------------------------------------------------------
    # strings.append('\t<!--  <!ENTITY INIT_FHR "9 12 15"> -->\n')
    # strings.append('\n')

    GenTaskEnt = get_GenTaskEnt(dicBase)

    if GenTaskEnt:
        import sys
        sSep = "/"
        if sys.platform == 'win32':
            sSep = r'\\'

        # -----------------------------------------------------------------------------------------------
        strings.append('\t<!-- External entities -->\n')
        strings.append(
            '\t<!ENTITY ENV_VARS   SYSTEM "{0}{1}tasks{1}env_vars.ent">\n'.format(dicBase['GEFS_ROCOTO'], sSep))
        strings.append(
            '\t<!ENTITY DATE_VARS   SYSTEM "{0}{1}tasks{1}date_vars.ent">\n'.format(dicBase['GEFS_ROCOTO'], sSep))
        strings.append('\n')

        # -----------------------------------------------------------------------------------------------
        strings.append('\t<!-- External parameter entities -->\n')
        strings.append(
            '\t<!ENTITY % TASKS    SYSTEM "{0}{2}tasks{2}{1}{2}all.ent">\n'.format(dicBase['GEFS_ROCOTO'],
                                                                                   dicBase['WHERE_AM_I'], sSep))
        strings.append('\t%TASKS;\n')
        strings.append('\n')

    # -----------------------------------------------------------------------------------------------
    strings.append('\t<!-- Machine related entities -->\n')

    sVarName = "ACCOUNT"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "CUE2RUN"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "TRANSFER_QUEUE"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    sVarName = "SCHEDULER"
    sVarValue = dicBase[sVarName.upper()]
    strings.append('\t<!ENTITY {0} "{1}">\n'.format(sVarName, sVarValue))

    strings.append('\n')

    strings.append('\t<!-- END: Resource requirements for the workflow -->\n')
    strings.append(']>\n')

    return ''.join(strings)

#=======================================================
def get_workflow_body(dicBase):
    '''
        Create the workflow body
    '''

    import GEFS_XML_For_Tasks as gefs_xml_for_tasks

    GenTaskEnt = get_GenTaskEnt(dicBase)

    gefs_xml_for_tasks.write_to_all_ent(GenTaskEnt, dicBase)

    strings = []

    strings.append('\n')
    strings.append(
        '<workflow realtime="F" cyclethrottle="&CYCLE_THROTTLE;" scheduler="&SCHEDULER;" taskthrottle="&TASK_THROTTLE;">\n')
    strings.append('\n')
    strings.append('\t<log><cyclestr>&WORKFLOW_LOG_DIR;/gefs@Y@m@d@H.log</cyclestr></log>\n')
    strings.append('\n')
    strings.append('\t<cycledef group="gefs">&SDATE;00 &EDATE;00 &INCYC;:00:00</cycledef>\n')
    strings.append('\n')

    sPre = "\t"

    strings.append(sPre + '<!--- init jobs -->\n')

    taskname_num = int(dicBase['taskname_num'.upper()])

    WHERE_AM_I = dicBase['WHERE_AM_I'.upper()]

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
            strings.append(gefs_xml_for_tasks.create_metatask_task(dicBase, taskname=taskname, sPre=sPre, \
                                                GenTaskEnt=GenTaskEnt))

    strings.append('\n')
    strings.append('</workflow>\n')

    return ''.join(strings)

#=======================================================
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

    if npert%2 != 0:
        print("please select the right number of memebers!")
        import sys
        sys.exit(1)

    if bltGenerateMEMLIST:
        MEMLIST_Value = ""
        for iNum in range(1, npert + 1):
            MEMLIST_Value += "p{0:02d} ".format(iNum)
        MEMLIST_Value += "c00"
        # print(MEMLIST_Value)
        dicBase[sVarName_List] = MEMLIST_Value

#=======================================================
def get_GenTaskEnt(dicBase):
    sVarName = "GenTaskEnt".upper()
    if sVarName in dicBase:
        sGenTaskEnt = dicBase[sVarName]

        if str(sGenTaskEnt).upper() == "YES" or str(sGenTaskEnt)[0].upper() == "Y":
            GenTaskEnt = True
        else:
            GenTaskEnt = False
    else:
        GenTaskEnt = False

    return GenTaskEnt


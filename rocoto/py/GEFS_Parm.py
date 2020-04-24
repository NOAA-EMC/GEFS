# =======================================================
def create_parm(sConfig, dicBase):
    # For gets_dev.parm
    lstBaseParm = get_lstParm(sConfig, dicBase)
    assign_default_for_gets_dev_parm(dicBase, lstBaseParm)
    create_gets_dev_parm(dicBase, lstBaseParm)


# =======================================================
def get_lstParm(sConfig, dicBase):
    dicBaseParm = read_dicParm(sConfig)
    WHERE_AM_I = dicBase["WHERE_AM_I"]
    get_and_merge_default_dicParm(dicBaseParm, WHERE_AM_I)

    return list(dicBaseParm.keys())


# =======================================================
def read_dicParm(sConfig):
    # read config file
    from collections import OrderedDict
    dicBaseParm = OrderedDict()
    IsParm = False
    StartParm = "# Start Parm"
    EndParm = "# End Parm"
    with open(sConfig, "r")as f:
        for sLine in f:
            # print(sLine)
            sLine = sLine.strip()

            if len(sLine) != 0:
                if str(sLine).startswith("#"):
                    if str(sLine).startswith(StartParm):
                        IsParm = True

                    if str(sLine).startswith(EndParm):
                        IsParm = False

                        return dicBaseParm

                    continue
                else:
                    if not IsParm:
                        continue

                    # they are the parameter for param
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

                    dicBaseParm[a] = b

    return dicBaseParm


# =======================================================
def get_and_merge_default_dicParm(dicParm, WHERE_AM_I):
    import os, sys
    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    # To get the WHERE_AM_I from dicParm or identify it using default methode
    sDefaultConfig_File = sys.path[0] + sSep + "user_{0}.conf".format(WHERE_AM_I)

    if os.path.exists(sDefaultConfig_File):
        #print("----Getting default parameters' value ...")
        #print("----Default User Configure file was found! Reading ...")
        dicParm_Default = read_dicParm(sDefaultConfig_File)

        #print("----Merging ...")
        for sDic in dicParm_Default:
            if sDic not in dicParm:
                dicParm[sDic] = dicParm_Default[sDic]


# =======================================================
def assign_default_for_gets_dev_parm(dicBase, lstBaseParm):
    import GEFS_XML_For_Tasks as gefs_xml_for_tasks

    # ==
    sVarName = "First"
    if sVarName not in lstBaseParm:
        lstBaseParm.insert(0, sVarName)
    # ==
    sVarName = "Last"
    if sVarName not in lstBaseParm:
        lstBaseParm.insert(1, sVarName)

    # ==
    sVarName = "npert"
    npert = int(dicBase[sVarName.upper()])
    if sVarName not in lstBaseParm:
        lstBaseParm.append(sVarName)

    # ==
    sVarName = "navg_min"
    if sVarName.upper() not in dicBase:
        dicBase[sVarName.upper()] = npert
    else:
        navg_min = int(dicBase[sVarName.upper()])
        if navg_min >= npert:
            navg_min = npert - 1
            dicBase[sVarName.upper()] = navg_min

    if sVarName not in lstBaseParm:
        lstBaseParm.append(sVarName)

    # ==
    if gefs_xml_for_tasks.DoesTaskExist(dicBase, "forecast_hr") or gefs_xml_for_tasks.DoesTaskExist(dicBase, "forecast_lr"):
        sVarName = "COREPERNODE"
        if sVarName not in lstBaseParm:
            lstBaseParm.append(sVarName)

    # ==
    sVarName = "MEMLIST"
    if sVarName not in lstBaseParm:
        lstBaseParm.append(sVarName)

    # ==
    sVarName = "cplwav"
    if sVarName not in lstBaseParm:
        lstBaseParm.append(sVarName)


# =======================================================
def get_lstParm2(sConfig, dicBase):
    dicDevParm = read_dicParm(sConfig)
    
    WHERE_AM_I = dicBase["WHERE_AM_I"]
    get_and_merge_default_dicParm(dicDevParm, WHERE_AM_I)

    lstDevParm = list(dicDevParm.keys())

    assign_default_for_gets_dev_parm(dicBase, lstDevParm)

    return lstDevParm

# =======================================================
def create_parm2(sConfig, dicBase):
    # For gets_dev.parm

    sVarName = "GenParm".upper()
    if sVarName in dicBase:
        sValue = dicBase[sVarName]
        if sValue.upper().startswith('Y'):
            # check gets_dev_parm items in configure file
            print("--Generating gefs_dev.parm ...")
            lstDevParm = get_lstParm2(sConfig, dicBase)
            create_gets_dev_parm(dicBase, lstDevParm)

# =======================================================
def create_gets_dev_parm(dicBase, listBaseParm):
    import sys
    import os

    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    strings = []

    strings.append('#!/bin/ksh\n')
    strings.append('\n')
    strings.append('#############################################################################\n')
    strings.append('# This section is some parameter setup for development testing only\n')
    strings.append('##############################################################################\n')
    strings.append('echo $(date) $0 test section begin\n')
    strings.append('\n')
    strings.append('# Setup associative arrays for prdgen\n')
    strings.append('typeset -A PRDGEN_GRID\n')
    strings.append('typeset -A PRDGEN_GRID_SPEC\n')
    strings.append('typeset -A PRDGEN_HOURS\n')
    strings.append('typeset -A PRDGEN_SUBMC\n')
    strings.append('typeset -A PRDGEN_A_DIR\n')
    strings.append('typeset -A PRDGEN_A_PREFIX\n')
    strings.append('typeset -A PRDGEN_A_LIST_F00\n')
    strings.append('typeset -A PRDGEN_A_LIST_FHH\n')
    strings.append('typeset -A PRDGEN_B_DIR\n')
    strings.append('typeset -A PRDGEN_B_PREFIX\n')
    strings.append('typeset -A PRDGEN_DO_ANALYSIS\n')
    strings.append('typeset -A PRDGEN_B_LIST_F00\n')
    strings.append('typeset -A PRDGEN_B_LIST_FHH\n')
    strings.append('\n')

    strings = "".join(strings)

    sPath = dicBase["GEFS_ROCOTO"]

    sPath += sSep + "parm"

    if not os.path.exists(sPath):
        os.mkdir(sPath)

    sgefs_dev_parm_File = sPath + sSep + "gefs_dev.parm"
    fh = open(sgefs_dev_parm_File, 'w')

    fh.write(strings)
    for sVarName in listBaseParm:
        if sVarName.upper() == 'ENS_SPS':
            fh.write('\n#Define using STTP(ENS_SPS=.true.) or physics stochastic\n')
        elif sVarName.upper() == 'DELTIM':
            fh.write('\n#define tmp time step\n')
        elif sVarName.upper() == 'layout_x'.upper():
            fh.write('\n# cpu geometry\n')
        elif sVarName.upper() == 'gfssource'.upper():
            fh.write('\n# for test, NOT USED FOR PRODUCTION gfssource = dev, para, prod\n')
        elif sVarName.upper() == 'gfssource'.upper():
            fh.write('\n# for test, NOT USED FOR PRODUCTION gfssource = dev, para, prod\n')
        elif sVarName.upper() == 'makepgrba'.upper():
            fh.write('\n# set all the following "make" and "save" flags to "yes" to simulate production\n')

        #==
        if sVarName == "cplwav":
            fh.write('export {0}="{1}"\n'.format(sVarName, dicBase[sVarName]))
        else:
            fh.write('export {0}="{1}"\n'.format(sVarName, dicBase[sVarName.upper()]))

    # fh.write(strings)
    fh.write("\necho $(date) $0 test section end\n")
    fh.flush()
    fh.close()

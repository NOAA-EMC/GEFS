
def create_parm(sConfig, dicBase):
    # For gets_dev.parm
    lstBaseParm = get_lstParm(sConfig, dicBase)
    assign_default_for_gets_dev_parm(dicBase, lstBaseParm)
    create_gets_dev_parm(dicBase, lstBaseParm)

#=======================================================
def get_lstParm(sConfig, dicBase):
    dicBaseParm = read_dicParm(sConfig)
    WHERE_AM_I = dicBase["WHERE_AM_I"]
    get_and_merge_default_dicParm(dicBaseParm, WHERE_AM_I)

    return list(dicBaseParm.keys())

#=======================================================
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

#=======================================================
def get_and_merge_default_dicParm(dicParm, WHERE_AM_I):
    import os, sys
    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    # To get the WHERE_AM_I from dicParm or identify it using default methode
    sDefaultConfig_File = sys.path[0] + sSep + "user_new_{0}.conf".format(WHERE_AM_I)

    if os.path.exists(sDefaultConfig_File):
        print("---Default User Configure file was found! Reading ...")
        dicParm_Default = read_dicParm(sDefaultConfig_File)

        print("---Merging ...")
        for sDic in dicParm_Default:
            if sDic not in dicParm:
                dicParm[sDic] = dicParm_Default[sDic]


def assign_default_for_gets_dev_parm(dicBase, lstBaseParm):
    # ==
    sVarName_Num = "MEM_Num".upper()
    MEM_Num = int(dicBase[sVarName_Num])

    sVarName = "npair"
    dicBase[sVarName.upper()] = int(MEM_Num / 2)
    if sVarName not in lstBaseParm:
        lstBaseParm.append(sVarName)
    # ==
    sVarName = "npert"
    dicBase[sVarName.upper()] = int(MEM_Num)
    if sVarName not in lstBaseParm:
        lstBaseParm.append(sVarName)

#------
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
    strings.append('# This section is some parameter setup for for development testing only\n')
    strings.append('##############################################################################\n')
    strings.append('echo `date` $0 test section begin\n')

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
        fh.write('export {0}={1}\n'.format(sVarName, dicBase[sVarName.upper()]))

    # fh.write(strings)
    fh.write("\necho `date` $0 test section end\n")
    fh.flush()
    fh.close()

#---
def assign_default_for_gets_dev_parm0(dicBase):
    # ==
    sVarName = "GenParm".upper()
    sVarValue = 'NO'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "ENS_SPS".upper()
    sVarValue = '.false.'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue

    # ==
    sVarName = "DO_SPPT".upper()
    sVarValue = 'YES'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue

    # ==
    sVarName = "DO_SHUM".upper()
    sVarValue = 'YES'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "DO_SKEB".upper()
    sVarValue = 'YES'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue

    # # define tmp time step
    # ==
    sVarName = "DELTIM".upper()
    sVarValue = 300
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue

    # ==
    sVarName = "k_split".upper()
    sVarValue = 1
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue

    # ==
    sVarName = "n_split".upper()
    sVarValue = 8
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue

    # ==
    sVarName = "TYPE".upper()
    sVarValue = 'nh'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "MONO".upper()
    sVarValue = 'non - mono'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # # cpu geometry
    # ==
    sVarName = "layout_x".upper()
    sVarValue = 4
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue

    # ==
    sVarName = "layout_y".upper()
    sVarValue = 8
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue

    # ==
    sVarName = "WRITE_GROUP".upper()
    sVarValue = 2
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue

    # ==
    sVarName = "WRTTASK_PER_GROUP".upper()
    sVarValue = 24
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue

    # ==
    sVarName = "parallel_threads".upper()
    sVarValue = 2
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue

    # ==
    sVarName = "npair".upper()
    sVarName_Num = "MEM_Num".upper()
    MEM_Num = int(dicBase[sVarName_Num])
    dicBase[sVarName] = int(MEM_Num / 2)

    # ==
    sVarName = "npert".upper()
    sVarName_Num = "MEM_Num".upper()
    MEM_Num = int(dicBase[sVarName_Num])
    dicBase[sVarName] = int(MEM_Num)

    # ==
    sVarName = "fhmax".upper()
    sVarValue = 384
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "fhmaxh".upper()
    sVarValue = 384
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # # ==
    # sVarName = "COMROOTp2".upper()
    # sVarValue = '/gpfs/hps/emc/ensemble/noscrub/emc.enspara/nems_gefs/com'
    # if sVarName not in dicBase:
    #     dicBase[sVarName] = sVarValue

    # # for test, NOT USED FOR PRODUCTION gfssource = dev, para, prod
    # ==
    sVarName = "gfssource".upper()
    sVarValue = 'prod'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # # set all the following "make" and "save" flags to "yes" to simulate production
    # ==
    sVarName = "makepgrba".upper()
    sVarValue = 'no'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
        # ==
    sVarName = "makepgrbb".upper()
    sVarValue = 'no'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "makepgrb2b".upper()
    sVarValue = 'yes'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "saveflux".upper()
    sVarValue = 'yes'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "savesfcsig".upper()
    sVarValue = 'no'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue
    # ==
    sVarName = "sigzvd".upper()
    sVarValue = 'no'
    if sVarName not in dicBase:
        dicBase[sVarName] = sVarValue


def create_gets_dev_parm0(dicBase):
    import sys
    import os

    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    strings = []

    strings.append('#!/bin/ksh\n')
    strings.append('\n')
    strings.append('#############################################################################\n')
    strings.append('# This section is some parameter setup for for development testing only\n')
    strings.append('##############################################################################\n')
    strings.append('echo `date` $0 test section begin\n')
    strings.append('\n')
    strings.append('#Define using STTP(ENS_SPS=.true.) or physics stochastic\n')
    sVarName = 'ENS_SPS'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export ENS_SPS={0}\n'.format(sVarValue))
    #
    sVarName = 'DO_SPPT'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export DO_SPPT={0}\n'.format(sVarValue))
    #
    sVarName = 'DO_SHUM'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export DO_SHUM={0}\n'.format(sVarValue))
    #
    sVarName = 'DO_SKEB'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export DO_SKEB={0}\n'.format(sVarValue))

    strings.append('#define tmp time step\n')
    #
    sVarName = 'DELTIM'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export DELTIM={0}\n'.format(sVarValue))
    #
    sVarName = 'k_split'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export k_split={0}\n'.format(sVarValue))
    #
    sVarName = 'n_split'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export n_split={0}\n'.format(sVarValue))
    strings.append('\n')
    #
    sVarName = 'TYPE'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export TYPE={0}\n'.format(sVarValue))
    #
    sVarName = 'MONO'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export MONO={0}\n'.format(sVarValue))
    strings.append('\n')
    strings.append('# cpu geometry\n')
    #
    sVarName = 'layout_x'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export layout_x={0}\n'.format(sVarValue))
    #
    sVarName = 'layout_y'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export layout_y={0}\n'.format(sVarValue))
    #
    sVarName = 'WRITE_GROUP'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export WRITE_GROUP={0}\n'.format(sVarValue))
    #
    sVarName = 'WRTTASK_PER_GROUP'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export WRTTASK_PER_GROUP={0}\n'.format(sVarValue))
    #
    sVarName = 'parallel_threads'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export parallel_threads={0}\n'.format(sVarValue))
    strings.append('\n')
    #
    sVarName = 'npair'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export npair={0}\n'.format(sVarValue))
    strings.append('\n')
    #
    sVarName = 'fhmax'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export fhmax={0}\n'.format(sVarValue))
    #
    sVarName = 'fhmaxh'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export fhmaxh={0}\n'.format(sVarValue))
    strings.append('\n')
    # COMROOTp2 was removed by Dr. Dingchen Hou, 03/12/2018
    # sVarName = 'COMROOTp2'.upper()
    # sVarValue = dicBase[sVarName]
    # strings.append('export COMROOTp2={0}\n'.format(sVarValue))
    # strings.append('\n')

    strings.append('# for test, NOT USED FOR PRODUCTION gfssource = dev, para, prod\n')
    strings.append('\n')
    #
    sVarName = 'gfssource'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export gfssource={0}\n'.format(sVarValue))
    strings.append('\n')
    strings.append('# set all the following "make" and "save" flags to "yes" to simulate production\n')
    #
    sVarName = 'makepgrba'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export makepgrba={0}\n'.format(sVarValue))
    #
    sVarName = 'makepgrbb'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export makepgrbb={0}\n'.format(sVarValue))
    #
    sVarName = 'makepgrb2b'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export makepgrb2b={0}\n'.format(sVarValue))
    #
    sVarName = 'saveflux'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export saveflux={0}\n'.format(sVarValue))
    #
    sVarName = 'savesfcsig'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export savesfcsig={0}\n'.format(sVarValue))
    strings.append('\n')
    #
    sVarName = 'sigzvd'.upper()
    sVarValue = dicBase[sVarName]
    strings.append('export sigzvd={0}\n'.format(sVarValue))
    strings.append('\n')
    strings.append('echo `date` $0 test section end\n')
    strings = "".join(strings)

    sPath = dicBase["GEFS_ROCOTO"]

    sPath += sSep + "parm"

    if not os.path.exists(sPath):
        os.mkdir(sPath)

    sgefs_dev_parm_File = sPath + sSep + "gefs_dev.parm"
    fh = open(sgefs_dev_parm_File, 'w')

    fh.write(strings)
    fh.flush()
    fh.close()

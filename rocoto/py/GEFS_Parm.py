def assign_default_for_gets_dev_parm(dicBase):

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

    sVarValue = 10

    if sVarName not in dicBase:

        dicBase[sVarName] = sVarValue

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

    # ==

    sVarName = "COMROOTp2".upper()

    sVarValue = '/gpfs/hps/emc/ensemble/noscrub/emc.enspara/nems_gefs/com'

    if sVarName not in dicBase:

        dicBase[sVarName] = sVarValue



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





def create_gets_dev_parm(dicBase):

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



    sgefs_dev_parm_File = sPath + sSep +"gefs_dev.parm"

    fh = open(sgefs_dev_parm_File, 'w')



    fh.write(strings)

    fh.flush()

    fh.close()


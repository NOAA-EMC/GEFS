def create_bin_file(dicBase):
    '''
        Create crontab to execute rocotorun every cronint (5) minutes
    '''
    ##########################################################
    # Generate crotab file based on different computers.
    #
    # Inputs (via environment variables):
    #         dicBase         : The base configuration from user config file
    #
    # Outputs:
    #         modify bin files and write back to the corresponsding
    #
    #
    ##########################################################

    taskname ='forecast_high'  # 06
    if DoesTaskExist(dicBase, taskname):
        rw_bin_forecast_high(taskname, dicBase)

    taskname ='forecast_low'  
    if DoesTaskExist(dicBase, taskname):
        rw_bin_forecast_high(taskname, dicBase)

    taskname ='prdgen_high'
    if DoesTaskExist(dicBase, taskname):
        rw_bin_prdgen(taskname, dicBase)

    taskname ='prdgen_low'
    if DoesTaskExist(dicBase, taskname):
        rw_bin_prdgen(taskname, dicBase)

    taskname ='prdgen_gfs'
    if DoesTaskExist(dicBase, taskname):
        if not DoesTaskExist(dicBase, 'prdgen_high'):
            rw_bin_prdgen(taskname, dicBase)

    taskname ='init_fv3chgrs'
    if DoesTaskExist(dicBase, taskname):
        rw_bin_init_fv3chgrs(taskname, dicBase)

def rw_bin_init_fv3chgrs(taskname, dicBase):
    import sys
    import os

    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    WHERE_AM_I = dicBase['WHERE_AM_I'].lower()
    sPath = dicBase["GEFS_ROCOTO"]
    sPath += sSep + "bin" + sSep + WHERE_AM_I + sSep

    sInput_File = sPath + taskname + ".sh"

    if not os.path.exists(sInput_File):
        print("Please check whether you have the input file: "+sInput_File )
        return

    sLines = ""
    with open(sInput_File, "r") as f:
        for sLine in f:
            sLine1 = sLine.strip()

            if sLine1.startswith("$SOURCEDIR/jobs/JGEFS_INIT_FV3CHGRS"):
                if DoesTaskExist(dicBase, 'init_combine'):
                    sLine = "$SOURCEDIR/jobs/JGEFS_INIT_FV3CHGRS_old"
                else:
                    sLine = "$SOURCEDIR/jobs/JGEFS_INIT_FV3CHGRS"

            
            sLines += sLine                 


    fh = open(sInput_File, 'w')
    fh.writelines(sLines)
    fh.flush()
    fh.close()


def rw_bin_prdgen(taskname, dicBase):
    import sys
    import os
    
    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    sPath = dicBase["GEFS_ROCOTO"]

    sPath += sSep + "bin" + sSep + dicBase["WHERE_AM_I"] + sSep
    if taskname != "prdgen_gfs":
        sInput_File = sPath + taskname + ".sh"
    else:
        sInput_File = sPath + "prdgen_high.sh"

    if not os.path.exists(sInput_File):
        print("Please check whether you have the input file: "+sInput_File )
        return

    if "PRDGEN_STREAMS" not in dicBase:
        print("Please check whether you have the 'PRDGEN_STREAMS' variable in your user_full.conf and gefs_dev.parm" )
        return

    iTotal_Tasks = len(dicBase["PRDGEN_STREAMS"].split())
    WHERE_AM_I = dicBase['WHERE_AM_I'].lower()
    sLines = ""
    with open(sInput_File, "r") as f:
        for sLine in f:
            # print(sLine)
            sLine1 = sLine.strip()

            if WHERE_AM_I == "cray":
                if sLine1.startswith("export total_tasks="):
                    sLine = 'export total_tasks={0}\n'.format(iTotal_Tasks)

            elif WHERE_AM_I == "theia":
                pass

            elif WHERE_AM_I == "wcoss_dell_p3":
                if sLine1.startswith("export total_tasks="):
                    sLine = 'export total_tasks={0}\n'.format(iTotal_Tasks)
            
            elif WHERE_AM_I == "wcoss_ibm":
                if sLine1.startswith("export total_tasks="):
                    sLine = 'export total_tasks={0}\n'.format(iTotal_Tasks)

            sLines += sLine
            # fh.write(sLine)


    fh = open(sInput_File, 'w')
    fh.writelines(sLines)
    fh.flush()
    fh.close()    


def rw_bin_forecast_high(taskname, dicBase):
    import sys
    import os

    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    sPath = dicBase["GEFS_ROCOTO"]

    sPath += sSep + "bin" + sSep + dicBase["WHERE_AM_I"] + sSep
    sInput_File = sPath + taskname + ".sh"

    if not os.path.exists(sInput_File):
        print("Please check whether you have the input file: "+sInput_File )
        return

    layout_x = int(dicBase['layout_x'.upper()])
    layout_y = int(dicBase['layout_y'.upper()])
    WRITE_GROUP = int(dicBase['WRITE_GROUP'.upper()])
    WRTTASK_PER_GROUP = int(dicBase['WRTTASK_PER_GROUP'.upper()])
    parallel_threads = int(dicBase['parallel_threads'.upper()])

    WHERE_AM_I = dicBase['WHERE_AM_I']

    if WHERE_AM_I == 'cray':
        ncores_per_node = 24
    elif WHERE_AM_I == "theia":
        ncores_per_node = 24
    elif WHERE_AM_I == "wcoss_dell_p3":
        ncores_per_node = 28
    elif WHERE_AM_I == "wcoss_ibm":
        ncores_per_node = 24
    else:
        ncores_per_node = 24

    # PPN: Processes per node
    # TPP: Threads per process
    iTotal_Tasks = layout_x * layout_y * 6 + WRITE_GROUP * WRTTASK_PER_GROUP
    iNodes = int((layout_x * layout_y * 6 + WRITE_GROUP * WRTTASK_PER_GROUP) / (ncores_per_node / parallel_threads))
    iPPN = int((ncores_per_node / parallel_threads))
    iTPP = parallel_threads

    # sNodes = "{0}:ppn={1}:tpp={2}".format(iNodes, iPPN, iTPP)

    sLines = ""
    with open(sInput_File, "r") as f:
        for sLine in f:
            # print(sLine)
            sLine1 = sLine.strip()

            if WHERE_AM_I == "cray":
                if sLine1.startswith("export gefsmpexec="):
                    sLine = 'export gefsmpexec=" aprun -b -j1 -n{0} -N{1} -d{2} -cc depth "\n'.format(iTotal_Tasks,iPPN,iTPP)

            elif WHERE_AM_I == "theia":
                if sLine1.startswith("export total_tasks="):
                    sLine = 'export total_tasks={0}\n'.format(iTotal_Tasks)

                if sLine1.startswith("export OMP_NUM_THREADS="):
                    sLine = 'export OMP_NUM_THREADS={0}\n'.format(iTPP)

                if sLine1.startswith("export taskspernode"):
                    sLine = 'export taskspernode={0}\n'.format(iPPN)

            elif WHERE_AM_I == "wcoss_dell_p3":
                if sLine1.startswith("export gefsmpexec="):
                    sLine = 'export gefsmpexec=" mpirun -n {0} "\n'.format(iTotal_Tasks)


            sLines += sLine
            # fh.write(sLine)


    fh = open(sInput_File, 'w')
    fh.writelines(sLines)
    fh.flush()
    fh.close()

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


def main():
    # for test this function

    g_OnlyForTest = True
    g_Rocoto_ForTest = ""

    import GEFS_Crontab as gefs_crontab
    import GEFS_Parm as gefs_parm
    import GEFS_UserConfig as gefs_config

    import GEFS_XML as gefs_xml
    import GEFS_XML_For_Tasks as gefs_xml_for_tasks

    import os, sys
    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    print("--Starting to generate all files you need!")

    print("--Getting user config file!")
    sConfig, sRocoto_WS = gefs_config.get_config_file(OnlyForTest=g_OnlyForTest)

    g_Rocoto_ForTest = sRocoto_WS

    print("--Reading user config file...")

    dicBase = gefs_config.read_config(sConfig)

    print("--Checking the must parameters for the config file")
    sMust_Items = ['SDATE', 'EDATE']  # , 'First'.upper(), 'Last'.upper()]
    for sMust_Item in sMust_Items:
        if sMust_Item not in dicBase:
            print("You need assign value of {0}".format(sMust_Item))
            exit(-1)

    # Get the default value
    print("--Getting default values from default user config file!")
    gefs_config.get_and_merge_default_config(dicBase)

    # Only for test
    if g_OnlyForTest:
        dicBase["GEFS_ROCOTO"] = g_Rocoto_ForTest
        dicBase["WORKDIR"] = g_Rocoto_ForTest

    print("--Assign default values for the config file")
    gefs_xml.assign_default_for_xml_def(dicBase, sRocoto_WS=sRocoto_WS)

    print(dicBase["WHERE_AM_I"])

    create_bin_file(dicBase)

if __name__ == '__main__':
    import sys

    main()

    print("--Done to generate all files!")
    sys.exit(0)





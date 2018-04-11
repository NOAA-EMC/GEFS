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

    taskname ='jgefs_forecast_high'  # 06
    rw_bin_forecast_high(taskname, dicBase)

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
        Task_Node = 24
    elif WHERE_AM_I == "theia":
        Task_Node = 24
    else:
        Task_Node = 24
    
    # PPN: Processes per node
    # TPP: Threads per process
    iTotal_Tasks = layout_x * layout_y * 6 + WRITE_GROUP * WRTTASK_PER_GROUP
    iNodes = int((layout_x * layout_y * 6 + WRITE_GROUP * WRTTASK_PER_GROUP) / (Task_Node / parallel_threads))
    iPPN = int((Task_Node / parallel_threads))
    iTPP = parallel_threads

    # sNodes = "{0}:ppn={1}:tpp={2}".format(iNodes, iPPN, iTPP)

    sLines=""
    with open(sInput_File, "r")as f:
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

            sLines += sLine
            # fh.write(sLine)


    fh = open(sInput_File, 'w')
    fh.writelines(sLines)
    fh.flush()
    fh.close()



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





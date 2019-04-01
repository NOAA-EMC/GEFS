#!/usr/bin/env python
# program to write the ensemble of IC files 
# calculate ensemble mean from 6 tile file and recentered to control
# written by Xiaqiong Zhou IMSG and Xianwu Xue at EMC/NCEP/NOAA on 11/15/2018
# Inputs: npert, ntiles, filename, sInWS, sOutWS
##
## Example
#Npert=2 # 1
#NTile=6 # 2
#sFileName='gfs_data.tile' # 3
#sInWS='/gpfs/dell2/emc/retros/noscrub/Xiaqiong.Zhou/my_util/recent_tile/00' # 4
#sOutWS="/gpfs/dell2/emc/retros/noscrub/Xianwu.Xue/ForKate/recent_tile/00"   # 5
#write_c384ic.py $Npert $NTile $sFileName $sInWS $sOutWS
##

def main():
    import sys
    # Npert=2 # 1
    #
    # Ntile=6 # 2
    #
    # filename='gfs_data.tile' # 3
    #
    # sInWS='/gpfs/dell2/emc/retros/noscrub/Xiaqiong.Zhou/my_util/recent_tile/00' # 4
    # sOutWS="/gpfs/dell2/emc/retros/noscrub/Xianwu.Xue/ForKate/recent_tile/00"   # 5

    if len(sys.argv) == 6:
        print(sys.argv)
        Npert = int(sys.argv[1])
        NTiles = int(sys.argv[2])
        sFileName = sys.argv[3]
        sInWS = sys.argv[4]
        sOutWS = sys.argv[5]

        print("Npert = {0}".format(Npert))
        print("NTiles = {0}".format(NTiles))
        print("sFileName = {0}".format(sFileName))
        print("Input Workspace: {0}".format(sInWS))
        print("Output Workspace: {0}".format(sOutWS))
    else:
        print('You should run "write_c384ic.py Npert Ntiles filename sInWS sOutWS"')
        exit(-1)

    sVars = []
    sVars.append('ps')
    sVars.append('w')
    sVars.append('zh')
    sVars.append('t')
    sVars.append('sphum')
    sVars.append('o3mr')
    sVars.append('liq_wat')
    sVars.append('rainwat')
    sVars.append('ice_wat')
    sVars.append('snowwat')
    sVars.append('graupel')
    sVars.append('u_w')
    sVars.append('v_w')
    sVars.append('u_s')
    sVars.append('v_s')

    import os, sys

    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    for iTile in range(NTiles):
        print("Dealing with Tile: {0}".format(iTile + 1))
        getMems_mean(iTile, Npert, sInWS, sOutWS, sFileName, sVars)


def getMems_mean(iTile, Npert, sInWS, sOutWS, sFileName, sVars):
    from netCDF4 import Dataset
    import numpy as np
    import sys, os
    from netCDF4 import num2date, date2num, date2index
    from subprocess import call
    w = []
    wmeans = [None] * len(sVars)
    w_c = [None] * len(sVars)

    print("  Calculating Ensemble Mean ...")
    for iPert in range(Npert):
        #print(iPert)

        sPath = sOutWS + "/p{0:02}".format(iPert + 1)
        if not os.path.exists(sPath):
            os.mkdir(sPath)

        sInFile = sInWS + "/p{0:02}/{1}{2}.nc".format(iPert + 1, sFileName, iTile + 1)
        sOutFile = sOutWS + "/p{0:02}/{1}{2}.nc".format(iPert + 1, sFileName, iTile + 1)
        call(["cp", sInFile, sOutFile])

        nc_fid = Dataset(sInFile, 'r')

        for k in range(len(sVars)):
            sVar = sVars[k]
            wmeans[k] = calValue(nc_fid, sVar, w, wmeans[k], iPert, Npert=Npert)

        nc_fid.close

    #print("  Reading Control Member Data ...")
    sCFile = sInWS + "/c00/{1}{2}.nc".format(iPert + 1, sFileName, iTile + 1)
    #print(sCFile)
    nc_fid = Dataset(sCFile, 'r')
    for k in range(len(sVars)):
        sVar = sVars[k]
        w_c[k] = nc_fid[sVar][:]

    nc_fid.close

    print("  Writing to netCDF files ...")
    for iPert in range(Npert):
        #print(iPert)
        sOutFile = sOutWS + "/p{0:02}/{1}{2}.nc".format(iPert + 1, sFileName, iTile + 1)
        #print(sOutFile)
        nc_fid = Dataset(sOutFile, 'a')
        #print("----")
        #print(nc_fid.variables.keys())
        for k in range(len(sVars)):
            sVar = sVars[k]
            #print(sVar)
            # print(np.nanmean(ensmem[sVar][:]))
            nc_fid[sVar][:] = nc_fid[sVar][:] - wmeans[k][:] + w_c[k][:]
            # print(np.nanmean(ensmem[sVar][:]))
            #print(nc_fid[sVar][:].shape)
            #print(wmeans[k][:].shape)
            #print(w_c[k][:].shape)

        nc_fid.close

    return


def calValue(ensmem, sVar, mems, mem_mean, memno, Npert=20):
    VarValue = ensmem[sVar][:]
    mems.append(VarValue)
    if memno == 0:
        mem_mean = VarValue / float(Npert)
    else:
        mem_mean += VarValue / float(Npert)
    return mem_mean


if __name__ == '__main__':
    import sys

    main()

    print("--Done!")


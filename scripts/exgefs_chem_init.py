#! /usr/bin/env python3

"""

"""

import os
import subprocess
import typing
import shutil
import itertools
import contextlib
import re
from datetime import datetime, timedelta
from functools import partial
from inspect import cleandoc

# Constants
init_path_pattern = "{ges_inout}/init/{member}"
init_file_pattern = "{path}/{kind}.{tile}.nc"
increment_file_pattern = "{path}/fv3_increment.nc"
restart_dest_pattern = "{path}/RESTART/{filename}"

restart_base_pattern = "{com_root}/{net}/{envir}/{run}.%Y%m%d/%H/{component}/restart/{member}"  # Time of previous run
restart_file_pattern = "{restart_base}/%Y%m%d.%H0000.{kind}.{tile}.nc"                          # Time when restart is valid (current run)
restart_core_res_file_pattern = "{restart_base}/%Y%m%d.%H0000.coupler.res"                      # Time when restart is valid (current run)
restart_coupler_file_pattern = "{restart_base}/%Y%m%d.%H0000.fv_core.res.nc"                    # Time when restart is valid (current run)
tracer_list_file_pattern = "{parm_gefs}/gefs_aerosol_tracer_list.parm"
merge_script_pattern = "{ush_gfs}/merge_fv3_chem_tile.py"
n_tiles = 6

analysis_file_pattern = "{com_gfs}/gfs.t%Hz.atmanl.nemsio"
com_base_pattern = "{com_out}/init/{member}"
fcst_file_pattern = "{com_root}/{net}/{envir}/{run}.%Y%m%d/%H/{component}/sfcsig/ge{member}.t%Hz.atmf{forecast_hour:03}.nemsio"
vert_coord_file_pattern = "{fix_gfs}/fix_am/global_hyblev.l{n_levels}.txt"

max_lookback = 1  # Maximum number of cycles backwards to search for files

# Namelist used by regrid
regrid_namelist = cleandoc("""
    &nam_setup
        i_output={n_lon}
        j_output={n_lat}
        input_file="{analysis_file}"
        output_file="{output_file}"
        terrain_file="{terrain_file}"
        vcoord_file="{vert_coord_file}"
    /
    """)

# Namelist used to calculate increment
calcinc_namelist = cleandoc("""
    &setup
        datapath = './'
        analysis_filename = '{analysis_filename}'
        firstguess_filename = '{forecast_filename}'
        increment_filename = '{increment_filename}'
        debug = .false.
        nens = 1
        imp_physics = {imp_physics}
    /

    &zeroinc
        incvars_to_zero = 'delz_inc','clwmr_inc','icmr_inc'
    /
    """)

# End constant settings

# Make sure print statements are flushed immediately, otherwise
#   print statments may be out-of-order with subprocess output
print = partial(print, flush=True)

tiles = list(map(lambda t: "tile{t}".format(t=t), range(1, n_tiles + 1)))


# Main entry point when called as script on command line
def main() -> None:
    # Read in environment variables and make sure they exist
    cdate = get_env_var("CDATE")
    incr = int(get_env_var('gefs_cych'))
    job = get_env_var('job')
    ges_in = get_env_var('GESIN')
    ges_out = get_env_var('GESOUT')
    com_root = get_env_var('COMROOT')
    com_out = get_env_var('COMOUT')
    com_gfs = get_env_var('COMINgfs')
    envir = get_env_var('envir')
    data = get_env_var('DATA')
    fix_gfs = get_env_var('FIXgfs')
    ush_gfs = get_env_var('USHgfs')
    parm_gefs = get_env_var('PARMgefs')
    net = get_env_var('NET')
    run = get_env_var('RUN')
    init_type = get_env_var('AEROSOL_INIT_TYPE')
    regrid_exec = get_env_var('REGRIDEXEC')
    calcinc_exec = get_env_var('CALCINCEXEC')
    regrid_aprun = get_env_var('APRUN_CHGRES')
    calcinc_aprun = get_env_var('APRUN_CALCINC')
    imp_physics = get_env_var('imp_physics')
    resolution = get_env_var("CASE", fail_on_missing=False, default_value="C384")
    send_com = get_env_var("SENDCOM") == "YES"
    send_ecf = get_env_var("SENDECF") == "YES"
    mail_recipients = get_env_var("MAIL_LIST")

    if (init_type not in ["warm", "cold"]):
        print(f"FATAL ERROR in {__file__}: Invalid AEROSOL_INIT_TYPE specified, aborting")
        exit(103)

    os.chdir(data)

    time = datetime.strptime(cdate, "%Y%m%d%H")

    atm_source_path = time.strftime(init_path_pattern.format(ges_inout=ges_in, envir=envir, member="c00"))
    destination_path = time.strftime(init_path_pattern.format(ges_inout=ges_out, envir=envir, member="aer"))

    # Even with exist_ok=True, makedirs sometimes throws a FileExistsError
    with contextlib.suppress(FileExistsError):
        os.makedirs(destination_path, exist_ok=True)

    for file_name in os.listdir(atm_source_path):
        full_file_name = os.path.join(atm_source_path, file_name)
        if os.path.isfile(full_file_name):
            shutil.copy(full_file_name, destination_path)

    if (init_type == "warm"):
        analysis_filename = regrid_analysis(time=time, regrid_aprun=regrid_aprun, regrid_exec=regrid_exec, max_lookback=max_lookback,
                                            com_root=com_root, com_gfs=com_gfs, fix_gfs=fix_gfs, net=net, envir=envir, run=run,
                                            destination_path=destination_path, resolution=resolution, incr=incr)
        prev_fcst_file = get_previous_forecast(time=time, incr=incr, max_lookback=max_lookback, com_root=com_root, net=net, envir=envir, run=run)
        increment_filename = increment_file_pattern.format(path=destination_path)

        sfc_files = get_init_files(path=destination_path, kind="sfc_data")

        restart_files = get_all_restart_files(time=time, incr=incr, max_lookback=max_lookback, com_root=com_root, net=net, envir=envir, run=run)

        files_exist = restart_files is not None and analysis_filename is not None and prev_fcst_file is not None and sfc_files is not None

        if(files_exist):
            # Link restart files
            # Even with exist_ok=True, makedirs sometimes throws a FileExistsError
            with contextlib.suppress(FileExistsError):
                os.makedirs("{path}/RESTART".format(path=destination_path), exist_ok=True)
            for file in restart_files:
                basename = os.path.basename(file)
                link = restart_dest_pattern.format(path=destination_path, filename=basename)
                with contextlib.suppress(FileNotFoundError):
                    os.unlink(link)
                os.symlink(file, link)

            # Calculate increment
            success = calc_increment(calcinc_aprun=calcinc_aprun, calcinc_exec=calcinc_exec, forecast_filename=prev_fcst_file, increment_filename=increment_filename, imp_physics=imp_physics)

            for file in sfc_files:
                tile = re.search('tile(\d)', file).group(0)
                basename = os.path.basename(time.strftime(restart_file_pattern.format(restart_base="", kind="sfcanl_data", tile=tile)))
                link = restart_dest_pattern.format(path=destination_path, filename=basename)
                with contextlib.suppress(FileNotFoundError):
                    os.unlink(link)
                os.symlink(file, link)

        if(not files_exist or not success):
            print("WARNING: Could not calculate increment (previous forecast may be missing), reverting to cold start")
            print("    If this is the first cycle, you can safely ignore this warning.")
            message = f"WARNING: {job} could not calculate the forecast increment to warm start the chem member. Will use cold start, but foreacst may be degraded!"
            subprocess.call(f'echo {message} | mail.py -c {mail_recipients}', shell=True)
            init_type = "cold"

    if(init_type == "cold"):
        merge_script = merge_script_pattern.format(ush_gfs=ush_gfs)
        tracer_list_file = tracer_list_file_pattern.format(parm_gefs=parm_gefs)

        atm_filenames = get_init_files(path=destination_path, kind="gfs_data")
        restart_files = get_tracer_restart_files(time=time, incr=incr, max_lookback=max_lookback, com_root=com_root, net=net, envir=envir, run=run)

        if (restart_files is not None):
            merge_tracers(merge_script, atm_filenames, restart_files, tracer_list_file)

    if(send_com):
        # Copy init files to COM
        com_path = time.strftime(com_base_pattern.format(com_out=com_out, envir=envir, net=net, run=run, member="aer", component="chem"))
        shutil.rmtree(com_path, ignore_errors=True)
        safe_copytree(destination_path, com_path)  # Copy data to COM

    return


# Handle error for missing files due to possibility of dangling symlinks (ignore_dangling_symlinks doesn't work properly; Python Issue 38523)
def safe_copytree(source: str, destination: str) -> None:
    try:
        shutil.copytree(source, destination, ignore_dangling_symlinks=True)
    except shutil.Error as err:
        # shutil.Error from copytree are tuples of src, dest, err
        tuples = [t for t in err.args[0]]
        exceptionStrings = [t[2] for t in tuples]
        # Ignore any no such file errors, raise any others
        if all(e.startswith('[Errno 2] No such file or directory:') for e in exceptionStrings):
            pass
        else:
            raise err


# Retrieve environment variable and exit or print warning if not defined
def get_env_var(varname: str, fail_on_missing: bool=True, default_value: str=None) -> str:
    var = os.environ.get(varname)
    if(var is None):
        if(fail_on_missing is True):
            print(f"FATAL ERROR in {__file__}: Environment variable {varname} not set")
            exit(100)
        elif(default_value is None):
            print(f"Environment variable {varname} not set, continuing using None")
        else:
            print(f"Environment variable {varname} not set, continuing using default of {default_value}")
            var = default_value
    return(var)


# Check if init files from the control exist
def get_init_files(path: str, kind: str) -> typing.List[str]:
    files = list(map(lambda tile: init_file_pattern.format(tile=tile, path=path, kind=kind), tiles))
    for file_name in files:
        print(file_name)
        if(not os.path.isfile(file_name)):
            print(f"FATAL ERROR in {__file__}: Atmosphere file {file_name} not found")
            exit(101)
    return files


# Find the most recent forecast available
def get_previous_forecast(time: datetime, incr: int, max_lookback: int, com_root: str, net: str, envir: str, run: str) -> str:
    for lookback in map(lambda i: incr * (i + 1), range(max_lookback)):
        last_time = time - timedelta(hours=lookback)
        prev_fcst_file = last_time.strftime(fcst_file_pattern.format(com_root=com_root, net=net, envir=envir, run=run, member="aer", component="chem", forecast_hour=lookback))
        found = os.path.isfile(prev_fcst_file)
        if(found):
            return prev_fcst_file
        else:
            print(last_time.strftime("Previous forecast file not found for %Y%m%d_%H"))

    if(not found):
        print("WARNING: Unable to find any previous forecast file, no increment will be produced. Will need to use cold start")
        return None


# Find last cycle with tracer data available via restart files
def get_tracer_restart_files(time: datetime, incr: int, max_lookback: int, com_root: str, net: str, envir: str, run: str) -> typing.List[str]:
    for lookback in map(lambda i: incr * (i + 1), range(max_lookback)):
        last_time = time - timedelta(hours=lookback)
        restart_base = last_time.strftime(restart_base_pattern.format(com_root=com_root, net=net, envir=envir, run=run, member="aer", component="chem"))
        files = list(time.strftime(restart_file_pattern.format(restart_base=restart_base, lookback=lookback, tile=tile, kind="fv_tracer.res")) for tile in tiles)
        found = [file for file in files if os.path.isfile(file)]
        if(found):
            return files
        else:
            print(last_time.strftime("Tracer files not found for %Y%m%d_%H"))

    if(not found):
        print("WARNING: Unable to find tracer files, will use zero fields")
        return None


# Find last cycle with all needed restart files and return a list of those files
def get_all_restart_files(time: datetime, incr: int, max_lookback: int, com_root: str, net: str, envir: str, run: str) -> typing.List[str]:
    for lookback in map(lambda i: incr * (i + 1), range(max_lookback)):
        last_time = time - timedelta(hours=lookback)
        restart_base = last_time.strftime(restart_base_pattern.format(com_root=com_root, net=net, envir=envir, run=run, member="aer", component="chem"))
        kinds = ["fv_tracer.res", "fv_core.res", "fv_srf_wnd.res", "phy_data"]
        files = list(time.strftime(restart_file_pattern.format(restart_base=restart_base, lookback=lookback, tile=tile, kind=kind)) for (tile, kind) in itertools.product(tiles, kinds))
        core_res_file = time.strftime(restart_core_res_file_pattern.format(restart_base=restart_base, lookback=lookback))
        coupler_file = time.strftime(restart_coupler_file_pattern.format(restart_base=restart_base, lookback=lookback))
        files.append(core_res_file)
        files.append(coupler_file)
        found = [file for file in files if os.path.isfile(file)]
        if(found):
            return files
        else:
            print(last_time.strftime("Not all restart files found for %Y%m%d_%H"))
            for file in files:
                print(file + ": " + str(os.path.isfile(file)))

    if(not found):
        print("WARNING: Missing some restart files, try cold start")
        return None


# Merge tracer data into atmospheric data (cold start)
def merge_tracers(merge_script: str, atm_filenames: typing.List[str], restart_files: typing.List[str], tracer_list_file: str) -> None:
    if(len(atm_filenames) != len(restart_files)):
        print(f"FATAL ERROR in {__file__}: atmosphere file list and tracer file list are not the same length")
        exit(102)

    for atm_file, tracer_file in zip(atm_filenames, restart_files):
        subprocess.call([merge_script, atm_file, tracer_file, tracer_list_file])


# Regrid analysis file to ensemble resolution to create warm-start increment
def regrid_analysis(time: datetime, regrid_aprun: str, regrid_exec: str, max_lookback: int, com_root: str, com_gfs: str, fix_gfs: str, net: str, envir: str, run: str, destination_path: str, resolution: str, incr: int) -> str:
    n_lon = int(resolution[1:]) * 4
    n_lat = int(resolution[1:]) * 2

    analysis_file = time.strftime(analysis_file_pattern.format(com_gfs=com_gfs))
    vert_coord_file = vert_coord_file_pattern.format(fix_gfs=fix_gfs, n_levels=64)
    output_file = "atmanl_mem001"
    for lookback in map(lambda i: incr * (i + 1), range(max_lookback)):
        last_time = time - timedelta(hours=lookback)
        terrain_file = last_time.strftime(fcst_file_pattern.format(com_root=com_root, net=net, envir=envir, run=run, member="aer", component="chem", forecast_hour=0))
        if(os.path.isfile(terrain_file)):
            break
        terrain_file = None

    if(terrain_file is None or not os.path.isfile(analysis_file)):
        return False

    namelist_file = open("fort.43", "w")
    namelist_file.write(regrid_namelist.format(n_lon=n_lon, n_lat=n_lat, analysis_file=analysis_file, terrain_file=terrain_file, vert_coord_file=vert_coord_file, output_file=output_file))
    namelist_file.close()
    if (subprocess.call("{regrid_aprun} {regrid_exec}".format(regrid_aprun=regrid_aprun, regrid_exec=regrid_exec), shell=True) == 0):
        print("Regrid analysis successful")
        return output_file
    else:
        print("Regrid analysis failed")
        return None


# Calculate increment for warm-start
def calc_increment(calcinc_aprun: str, calcinc_exec: str, forecast_filename: str, increment_filename: str, imp_physics: str) -> bool:
    print("Calculating increment...")
    analysis_filename = "atmanl"
    forecast_basename = os.path.basename(forecast_filename)
    increment_basename = os.path.basename(increment_filename)
    shutil.copy(forecast_filename, "{basename}_mem001".format(basename=forecast_basename))
    os.symlink(increment_filename, "{basename}_mem001".format(basename=increment_basename))
    namelist_file = open("calc_increment.nml", "w")
    namelist_file.write(calcinc_namelist.format(imp_physics=imp_physics, analysis_filename=analysis_filename, forecast_filename=forecast_basename, increment_filename=increment_basename))
    namelist_file.close()
    if (subprocess.call("{calcinc_aprun} {calcinc_exec}".format(calcinc_aprun=calcinc_aprun, calcinc_exec=calcinc_exec), shell=True) == 0):
        print("Calculate increment completed successfully")
        return True
    else:
        print("Calculate increment failed!")
        return False


if __name__ == "__main__":
    main()
    exit(0)

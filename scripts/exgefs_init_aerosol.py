#! /usr/bin/env python3

"""

"""

import os
import subprocess
import typing
import shutil
from datetime import datetime, timedelta
from functools import partial

# Constants
atm_base_pattern = "{ges_root}/{envir}/gefs.%Y%m%d/%H/{member}"
atm_file_pattern = "{path}/gfs_data.{tile}.nc"
com_base_pattern = "{com_root}/{net}/{envir}/{run}.%Y%m%d/%H/init/{member}"
tracer_base_pattern = "{com_root}/{net}/{envir}/{run}.%Y%m%d/%H/restart/{member}"  # Time of previous run
tracer_file_pattern = "{tracer_base}/%Y%m%d.%H0000.fv_tracer.res.{tile}.nc"           # Time when restart is valid (current run)
tracer_list_file_pattern = "{parm_gefs}/gefs_aerosol_tracer_list.parm"
merge_script_pattern = "{ush_gfs}/merge_fv3_chem_tile.py"
n_tiles = 6
max_lookback = 4

# End configurable settings

# Make sure print statements are flushed immediately, otherwise
#   print statments may be out-of-order with subprocess output
print = partial(print, flush=True)

tiles = list(map(lambda t: "tile{t}".format(t=t), range(1, n_tiles + 1)))


def main() -> None:
	# Read in environment variables and make sure they exist
	cdate = get_env_var("CDATE")
	incr = int(get_env_var('gefs_cych'))
	ges_root = get_env_var('GESROOT')
	com_root = get_env_var('COMROOT')
	envir = get_env_var('envir')
	data = get_env_var('DATA')
	ush_gfs = get_env_var('USHgfs')
	parm_gefs = get_env_var('PARMgefs')
	net = get_env_var('NET')
	run = get_env_var('RUN')

	os.chdir(data)

	merge_script = merge_script_pattern.format(ush_gfs=ush_gfs)
	tracer_list_file = tracer_list_file_pattern.format(parm_gefs=parm_gefs)

	time = datetime.strptime(cdate, "%Y%m%d%H")
	atm_source_path = time.strftime(atm_base_pattern.format(ges_root=ges_root, envir=envir, member="c00"))
	destination_path = time.strftime(atm_base_pattern.format(ges_root=ges_root, envir=envir, member="aer"))
	com_path = time.strftime(com_base_pattern.format(com_root=com_root, envir=envir, net=net, run=run, member="aer"))

	os.makedirs(destination_path, exist_ok=True)
	for file_name in os.listdir(atm_source_path):
		full_file_name = os.path.join(atm_source_path, file_name)
		if os.path.isfile(full_file_name):
			shutil.copy(full_file_name, destination_path)

	atm_files = get_atm_files(destination_path)
	tracer_files = get_tracer_files(time, incr, max_lookback, com_root, net, envir, run)

	if (tracer_files is not None):
		merge_tracers(merge_script, atm_files, tracer_files, tracer_list_file)

	shutil.rmtree(com_path, ignore_errors=True)
	shutil.copytree(destination_path, com_path)  # Copy data to COM

	return


# Retrieve environment variable and exit or print warning if not defined
def get_env_var(varname: str, fail_on_missing: bool=True) -> str:
	var = os.environ.get(varname)
	if(var is None):
		if(fail_on_missing is True):
			print("FATAL: Environment variable {varname} not set".format(varname=varname))
			exit(100)
		else:
			print("WARNING: Environment variable {varname} not set, continuing using None".format(varname=varname))
	return(var)


# Check if atm files exist
def get_atm_files(path: str) -> typing.List[str]:
	files = list(map(lambda tile: atm_file_pattern.format(tile=tile, path=path), tiles))
	for file_name in files:
		print(file_name)
		if(not os.path.isfile(file_name)):
			print("FATAL: Atmosphere file {file_name} not found".format(file_name=file_name))
			exit(101)
	return files


# Find last cycle with tracer data available via restart files
def get_tracer_files(time: datetime, incr: int, max_lookback: int, com_root: str, net: str, envir: str, run: str) -> typing.List[str]:
	for lookback in map(lambda i: incr * (i + 1), range(max_lookback)):
		last_time = time - timedelta(hours=lookback)
		tracer_base = last_time.strftime(tracer_base_pattern.format(com_root=com_root, net=net, envir=envir, run=run, member="aer"))
		files = list(map(lambda tile: time.strftime(tracer_file_pattern.format(tracer_base=tracer_base, lookback=lookback, tile=tile)), tiles))
		found = [file for file in files if os.path.isfile(file)]
		if(found):
			return files
		else:
			print(last_time.strftime("Tracer files not found for %Y%m%d_%H"))

	if(not found):
		print("WARNING: Unable to find tracer files, will use zero fields")
		return None


# Merge tracer data into atmospheric data
def merge_tracers(merge_script: str, atm_files: typing.List[str], tracer_files: typing.List[str], tracer_list_file: str) -> None:
	if(len(atm_files) != len(tracer_files)):
		print("FATAL: atmosphere file list and tracer file list are not the same length")
		exit(102)

	for atm_file, tracer_file in zip(atm_files, tracer_files):
		subprocess.call([merge_script, atm_file, tracer_file, tracer_list_file])


if __name__ == "__main__":
	main()
	exit(0)

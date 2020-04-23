#! /usr/bin/env python3


"""
Archives specified directories of GEFS output to HPSS for long-term storage.

Inputs (via environment variables):
    WORKDIR              : The base GEFS output directory (usually in ptmp)
    HPSS_DIR             : The base HPSS directory in which to store tar files
    DIRS_TO_ARCHIVE_WAVE : A comma-separated list of directories to archive
    PDY                  : Initialization date in YYYYMMDD form
    cyc                  : Initialization hour in HH form

    GEFS output must be located in the WORKDIR com directory as such:
        <WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/wave/<directory>

Outputs:
    For each directory in DIRS_TO_ARCHIVE, a tar file will be created in the following
    location on HPSS:
        <HPSS_DIR>/<YYYY>/<YYYY><MM>/<YYYY><MM><DD>/gefs.<YYYY><MM><DD>_<HH>.wave.<directory>.tar

Error Codes:
    -100 : Required environment variable not defined
    -101 : HPSS error

"""

import os
import subprocess
from datetime import datetime
from datetime import timedelta
from functools import partial

# File patterns
workdir_pattern = "{work_dir}/com/gefs/dev/gefs.%Y%m%d/%H/wave"
destination_pattern = "{hpss_path}/%Y/%Y%m/%Y%m%d"
tarfile_pattern = "{destination_path}/gefs.%Y%m%d_%H.wave.{directory}.tar"
tarfile_pattern_restart = "{destination_path}/gefs.%Y%m%d_%H+{interval:02d}.wave.{directory}.tar"

# Make sure print statements are flushed immediately, otherwise
#   print statments may be out-of-order with subprocess output
print = partial(print, flush=True)

# Read in environment variables and make sure they exist
work_dir = os.environ.get("WORKDIR")
if( work_dir is None ):
    print("FATAL: Environment variable WORKDIR not set")
    quit(-100)

hpss_path = os.environ.get("HPSS_DIR")
if( hpss_path is None ):
    print("FATAL: Environment variable HPSS_DIR not set")
    quit(-100)

dirs_to_archive_string = os.environ.get("DIRS_TO_ARCHIVE_WAVE")
if( dirs_to_archive_string is None ):
    print("FATAL: Environment variable DIRS_TO_ARCHIVE_WAVE not set")
    quit(-100)
# Convert to array using commas and removing whitespace
dirs_to_archive = [x.strip() for x in dirs_to_archive_string.split(',')]

pdy = os.environ.get("PDY")
if( pdy is None ):
    print("FATAL: Environment variable PDY not set")
    quit(-100)

cycle = os.environ.get("cyc")
if( cycle is None ):
    print("FATAL: Environment variable cyc not set")
    quit(-100)

interval = int(os.environ.get("gefs_cych"))
if( interval is None ):
    print("FATAL: Environment variable gefs_cych not set")
    quit(-100)

time = datetime.strptime("{pdy}{cycle}".format(pdy=pdy, cycle=cycle), "%Y%m%d%H")
time_next = time + timedelta(hours=interval)

print("Starting GEFS archive with the following settings:")
print("Source directory              : {work_dir}".format(work_dir=work_dir))
print("Destination Directory on HPSS : {hpss_path}".format(hpss_path=hpss_path))
print("Directories to Archive        : {dirs_to_archive}".format(dirs_to_archive=str(dirs_to_archive)))
print(time.strftime("Date/Cycle                    : %Y%m%d_%H"))

destination_path = time.strftime(destination_pattern.format(hpss_path=hpss_path))

# Create directory on HPSS
err_code = subprocess.run(["hsi", "mkdir", "-p", destination_path]).returncode
if(err_code != 0):
    print("FATAL: Could not create destination directory {destination_path} on HPSS, error code: {err_code}".format(
        destination_path=destination_path,
        err_code=str(err_code)
    ))
    quit(-101)

# Archive directories
for directory in dirs_to_archive:
    if directory == "restart":
        tar_file = time_next.strftime(tarfile_pattern_restart.format(
            destination_path=destination_path,
            directory=directory,
            interval=interval
        ) )
        output_path = time_next.strftime( workdir_pattern.format( work_dir=work_dir ) )
    else:
        tar_file = time.strftime(tarfile_pattern.format(
            destination_path=destination_path,
            directory=directory)
        )
        output_path = time.strftime( workdir_pattern.format( work_dir=work_dir ) )

    output_dir = "{output_path}/{directory}".format(output_path=output_path, directory=directory)
    print("output_dir = {output_dir}".format(output_dir=output_dir))
    if os.path.exists(output_dir):
        os.chdir(output_path)
        print("Creating tar on HPSS for {directory}".format(directory=directory))
        print("    From {output_path}/{directory} to {tar_file}".format(output_path=output_path, directory=directory, tar_file=tar_file))
        err_code = subprocess.call(["htar", "-cvf", tar_file, directory])
        if(err_code != 0):
            print("FATAL: Could not create {tar_file} on HPSS, error code: {err_code}".format(tar_file=tar_file, err_code=str(err_code)))
            quit(-101)
    else:
        print("WARNING: {output_dir} does not exist, skipping".format(output_dir=output_dir))

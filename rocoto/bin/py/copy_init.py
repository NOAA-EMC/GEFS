#! /usr/bin/env python3

"""
Copies specified directories of GEFS output from temporary location to another location.

Inputs (via environment variables):
	WORKDIR         : The base GEFS working directory (usually in ptmp)
	INIT_DIR        : The base directory with the GEFS init files
	MEMBER    		: Ensemble member for this job
	PDY             : Initialization date in YYYYMMDD form
	cyc             : Initialization hour in HH form

GEFS init files must be located in the INIT_DIR as such:
	<INIT_DIR>/<PDY>/<cyc>/<MEMBER>

Outputs:
	Contents of the init directory will be copied to the WORKDIR in the following location:
		<WORKDIR>/nwges/dev/gefs.<PDY>/<cyc>/<MEMBER>

Error Codes:
	-100 : Required environment variable not defined
	-102 : Destination directory already exists and couldn't be deleted

"""

import os
import shutil
from functools import partial

source_pattern = "{init_dir}/{pdy}/{cycle}/atmos/{member}"
destination_base = "{init_dir}/{pdy}/{cycle}/atmos/{member}"
destination_pattern = "{work_dir}/nwges/dev/gefs.{pdy}/{cycle}/{member}"
clobber = True

# Make sure print statements are flushed immediately, otherwise
#   print statments may be out-of-order with subprocess output
print = partial(print, flush=True)

# Read in environment variables and make sure they exist
work_dir = os.environ.get("WORKDIR")
if( work_dir is None ):
	print("FATAL: Environment variable WORKDIR not set")
	quit(-100)

init_dir = os.environ.get("INIT_DIR")
if( init_dir is None ):
	print("FATAL: Environment variable INIT_DIR not set")
	quit(-100)

member = os.environ.get("MEMBER")
if( member is None ):
	print("FATAL: Environment variable MEMBER not set")
	quit(-100)

pdy = os.environ.get("PDY")
if( pdy is None ):
	print("FATAL: Environment variable PDY not set")
	quit(-100)

cycle = os.environ.get("cyc")
if( cycle is None ):
	print("FATAL: Environment variable cyc not set")
	quit(-100)

print("Starting GEFS copy_init with the following settings:")
print("Source Directory      : {init_dir}".format(init_dir=init_dir))
print("Destination Directory : {work_dir}".format(work_dir=work_dir))
print("Member                : {member}".format(member=member))
print("Date/Cycle            : {pdy}_{cycle}".format(pdy=pdy, cycle=cycle))

source_dir = source_pattern.format( init_dir=init_dir, pdy=pdy, cycle=cycle, member=member )
destination_dir = destination_pattern.format( work_dir=work_dir, pdy=pdy, cycle=cycle, member=member )

os.makedirs(destination_base, exist_ok=True)

if ( os.path.exists(destination_dir) ):
	if(clobber):
		shutil.rmtree(destination_dir)
	else:
		print(destination_dir + " already exists and clobber is False, skipping.")
		quit(-102)

print("Copying files for init")
print("    From " + source_dir + " to " + destination_dir)
shutil.copytree(source_dir, destination_dir, symlinks=True)

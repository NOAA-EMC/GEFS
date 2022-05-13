help([[
Load environment for building gefs_anom2_fcst on WCOSS2
]])

intel_ver=os.getenv("intel_ver") or "19.1.3.304"
load(pathJoin("intel", intel_ver))

PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver") or "8.1.0"
load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))

craype_ver=os.getenv("craype_ver") or "2.7.10"
load(pathJoin("craype", craype_ver))


g2_ver=os.getenv("g2_ver") or "3.4.5"
load(pathJoin("g2", g2_ver))

w3nco_ver=os.getenv("w3nco_ver") or "2.4.1"
load(pathJoin("w3nco", w3nco_ver))

bacio_ver=os.getenv("bacio_ver") or "2.4.1"
load(pathJoin("bacio", bacio_ver))


setenv("FCMP","ftn")
setenv("LDFLAGSM","")
setenv("OMPFLAGM","")

whatis("Description: gefs_anom2_fcst build environment")

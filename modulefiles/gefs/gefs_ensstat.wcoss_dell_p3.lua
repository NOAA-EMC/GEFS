help([[
Load environment for building gefs_ensstat on wcoss_dell_p3
]])

ips_ver=os.getenv("ips_ver") or "18.0.1.163"
load(pathJoin("ips", ips_ver))


g2_ver=os.getenv("g2_ver") or "3.1.0"
load(pathJoin("g2", g2_ver))

w3nco_ver=os.getenv("w3nco_ver") or "2.0.6"
load(pathJoin("w3nco", w3nco_ver))

bacio_ver=os.getenv("bacio_ver") or "2.0.2"
load(pathJoin("bacio", bacio_ver))

jasper_ver=os.getenv("jasper_ver") or "1.900.1"
load(pathJoin("jasper", jasper_ver))

libpng_ver=os.getenv("libpng_ver") or "1.2.59"
load(pathJoin("libpng", libpng_ver))

zlib_ver=os.getenv("zlib_ver") or "1.2.11"
load(pathJoin("zlib", zlib_ver))


setenv("FCMP","ifort")
setenv("LDFLAGSM","")
setenv("OMPFLAGM","")

whatis("Description: gefs_ensstat build environment")

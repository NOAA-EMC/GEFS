#ifndef NEMSIO2NC_VARS
#define NEMSIO2NC_VARS
#include <string>
#include <map>

namespace nems2nc {
  // map of variable names to change between NEMSIO and netCDF
  // these are for only when the input name != output name
  const std::map<std::string, std::string> varchanges = {
    { "hgt sfc", "hgtsfc" },
    { "pres sfc", "pressfc" },
    { "vvel mid layer", "dzdt" },
  };
  const std::map<std::string, std::string> varlongnames = {
    { "hgtsfc", "surface geopotential height" },
    { "pressfc", "surface pressure"},
    { "pres", "pressure"},
    { "cld_amt", "cloud amount"},
    { "clwmr", "cloud water mixing ratio"},
    { "delz", "height thickness"},
    { "dpres", "pressure thickness"},
    { "dzdt", "vertical wind"},
    { "grle", "graupel mixing ratio"},
    { "icmr", "cloud ice mixing ratio"},
    { "o3mr", "ozone mixing ratio"},
    { "rwmr", "rain mixing ratio"},
    { "snmr", "snow mixing ratio"},
    { "spfh", "specific humidity"},
    { "tmp", "temperature"},
    { "ugrd", "zonal wind"},
    { "vgrd", "meridional wind"},
  };
  const std::map<std::string, std::string> varunits = {
    { "hgtsfc", "gpm" },
    { "pressfc", "pa"},
    { "pres", "pa"},
    { "cld_amt", "1"},
    { "clwmr", "kg/kg"},
    { "delz", "m"},
    { "dpres", "pa"},
    { "dzdt", "m/sec"},
    { "grle", "graupel mixing ratio"},
    { "icmr", "kg/kg"},
    { "o3mr", "kg/kg"},
    { "rwmr", "kg/kg"},
    { "snmr", "kg/kg"},
    { "spfh", "kg/kg"},
    { "tmp", "K"},
    { "ugrd", "m/sec"},
    { "vgrd", "m/sec"},
  };
}

#endif // NEMSIO2NC_VARS

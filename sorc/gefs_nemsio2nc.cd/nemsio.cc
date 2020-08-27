#include <iostream>
#include <iomanip>
#include <string>
#include <map>
#include <cstdlib>
#include "nemsio.h"
#include "nemsio_interface.h"

namespace nems2nc {

 int nemsio::open(std::string filenamein) {
   std::cout << "Opening " << filenamein << " for reading " << std::endl;
   filename = filenamein;
   // convert filename to fname for use in F90
   char fname[filename.length()];
   int i;
   int fname_len = filename.length();
   for (i = 0; i < sizeof(fname); i++) {
      fname[i] = filename[i];
   }
   // open file using NEMSIO F90 library
   nemsio_open_f90(fname_len, fname);
   // get header info
   nemsio_get_header_f90( idate, fhour, nx, ny, nz, nrec);
   std::cout << "Input NEMSIO file:" << std::endl;
   std::cout << "File info: " << std::endl;
   // print out time M/D/YYYY HH:MM UTC
   std::cout << "Initial time: " << idate[1] << "/" << idate[2] << "/" << idate[0] << " ";
   std::cout << std::setw(2) << std::setfill('0') << idate[3];
   std::cout << ":";
   std::cout << std::setw(2) << std::setfill('0') << idate[4];
   std::cout << " UTC" << std::endl;
   // print out dimension info
   std::cout << "Forecast hour = " << fhour << std::endl;
   std::cout << "nx = " << nx << std::endl;
   std::cout << "ny = " << ny << std::endl;
   std::cout << "nz = " << nz << std::endl;
   // get ak,bk,ntrac
   int npts = nx*ny;
   ak = new double[nz+1];
   bk = new double[nz+1];
   phalf = new double[nz+1];
   pfull = new double[nz];
   lat = new double[npts];
   lon = new double[npts];
   nemsio_get_akbk_latlon_f90(nx, ny, nz, ak, bk, lat, lon, pfull, phalf, ntrac);
   // get list of records
   reclev = new int[nrec];
   char recname_tmp[10*nrec], reclevtyp_tmp[10*nrec];
   nemsio_get_recinfo_f90(nrec, recname_tmp, reclevtyp_tmp, reclev);
   for (i = 0; i < nrec; i++) {
     std::string tmpstr="";
     for (int j = 0; j<10; j++) {
       int k = i*10 + j;
       tmpstr = tmpstr + recname_tmp[k];
     }
     tmpstr.erase(tmpstr.find_last_not_of(" \n\r\t")+1);
     recname.push_back(tmpstr);
     std::string tmpstr2="";
     for (int j = 0; j<10; j++) {
       int k = i*10 + j;
       tmpstr2 = tmpstr2 + reclevtyp_tmp[k];
     }
     tmpstr2.erase(tmpstr2.find_last_not_of(" \n\r\t")+1);
     reclevtype.push_back(tmpstr2);
     recfields.push_back(tmpstr+" "+tmpstr2);
   }
   // count duplicate reccord names to figure out which are 2D/3D
   // Iterate over the vector and store the frequency of each element in map
   for (auto & elem : recfields) {
      auto result = countRecs.insert(std::pair<std::string, int>(elem, 1));
      if (result.second == false)
        result.first->second++;
      }
   return 0;
 }

 int nemsio::read_rec(std::string recname, std::string levtyp, int lev, double* data) {
   int npts = nx*ny;
   int strlen1 = recname.length();
   int strlen2 = levtyp.length();
   nemsio_readrec_f90(recname.c_str(),levtyp.c_str(),strlen1,
                      strlen2,lev, npts, data);
   return 0;
 }
}

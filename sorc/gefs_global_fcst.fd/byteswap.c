#ifdef LINUX
  void byteswap_
         (char *data, int *nbyte, int *nnum) {
#endif
  int  i, j;
  char swap[256];
  int  nb=*nbyte;
  int  nn=*nnum;

  for (j=0; j<nn; j++) {

    for (i=0; i<nb; i++) swap[i] = data[j*nb+i];

    for (i=0; i<nb; i++) data[j*nb+i] = swap[nb-i-1];

  }

}


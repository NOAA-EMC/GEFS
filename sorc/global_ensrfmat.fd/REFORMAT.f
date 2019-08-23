      program reformat
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Program will convert rfc24-uniq-early     
C                       to usa-dlyprcp-$YYMMDD
C     Convert from IBMSP                          
C
C PROGRAM HISTORY LOG:
C   03-03-12  YUEJIAN ZHUL
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
c     parameter (len=9824)                  
c     parameter (len=51485)
      parameter (ntab=80000,nrec=40000)                  
      dimension lat(ntab),lon(ntab)
      dimension rlat(ntab),rlon(ntab)
      dimension prcp (nrec)
      character*8 gageid(nrec)
      character*8 statid(ntab),cymd
      character*255 fname1,fname2,fname3
      namelist /namin/iymd,fname1,fname2,fname3
cccccc
      read (5,namin)
      write(6,namin)
C     read *, iymd
C     write (cymd,1004) iymd
      iunit=10
      junit=11
      kunit=51
C     fname1='/nfsuser/g01/wx20yz/jif_cqpf/data/ingest_nwsli.uniq'
C     fname2(1:21)='/com/ingest/prod/shf.'
C     fname3(1:29)='/ptmp/wx20yz/cvt/usa-dlyprcp-'
C     fname2(22:29)=cymd
C     fname2(30:46)='/rfc24-uniq-early'
C     fname3(30:37)=cymd
      open(unit=iunit,file=fname1,status='OLD',form='FORMATTED',
     & iostat=ios10)
      print *, 'INPUT REFERENCE STATION LIST FILE: ', fname1
      open(unit=junit,file=fname2,status='OLD',form='FORMATTED',
     & iostat=ios11)
      print *, 'INPUT RFC 24 HOURS PRECIPITATION: ', fname2
      open(unit=kunit,file=fname3,status='NEW',form='FORMATTED',
     & iostat=ios51)
      print *, 'OUTPUT RFC 24 HOURS PRECIPITATION: ', fname3

      if(ios10.ne.0) print *, 'failure to open file =', fname1
      if(ios11.ne.0) print *, 'failure to open file =', fname2
      if(ios51.ne.0) print *, 'failure to open file =', fname3
      if (ios10.ne.0) stop 101
      if (ios11.ne.0) stop 102
      if (ios51.ne.0) stop 103

      iii=0
  100 continue
c     do iii = 1, len 
      iii=iii+1
      if ( iii .gt. ntab ) go to 904
      read(iunit,1001,err=901,end=101) statid(iii),rlat(iii),rlon(iii)                  
C     print *, rlat(iii),rlon(iii),statid(iii)
      go to 100
c     enddo
  101 continue
      len=iii-1
      print *,"total stations in directory  ",len
      icnt=0
 1000 continue
      icnt = icnt + 1
      if (icnt .gt. nrec) go to 905
      read(junit,1002,err=902,end=104) gageid(icnt),prcp(icnt)
C     print *, gageid(icnt),prcp(icnt)
      goto 1000
  104 continue
      icnt=icnt-1
      print *, "total record read in  ",icnt
      write (51,1005) iymd
      call sort(prcp,gageid,icnt)
      jcnt=0
      ijk=1200
      do iii = icnt, 1, -1
       do jjj = 1, len
        if (statid(jjj).eq.gageid(iii)) then
c        rlat(jjj) = float(lat(jjj))/100.00
c        rlon(jjj) = float(lon(jjj))/100.00
         jcnt = jcnt + 1
         write (kunit,1003,err=903) 
     &     rlat(jjj),rlon(jjj),prcp(iii),gageid(iii),ijk
         goto 105
        endif
       enddo
  105  continue
      enddo
      print *, "total record write out",jcnt
      stop
 1001 format(a8,f7.2,f7.2)
#1002 format(a8,40x,f8.2)
 1002 format(a8,39x,f9.3)
C1002 format(17x,a8,4x,f8.2)
 1003 format(f6.2,f8.2,f7.2,1x,a8,i6)
 1004 format(i8)
 1005 format(' 24-hr precip reports ending 12Z on ',i8)
 901  continue
      print *, ' error reading file ',iunit
      stop 101
 902  continue
      print *, ' error reading file ',junit
      stop 102
 903  continue
      print *, ' error writing file ',kunit
      stop 103
 904  continue
      print *, ' too many records in file ',iunit
      stop 104
 905  continue
      print *, ' too many records in file ',junit
      stop 105
      end
C===================================================== SORT.FOR
      SUBROUTINE SORT(X,C,N)
C***********************************************************************
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C***********************************************************************
C
C  SORTS THE ARRAY X INTO ASCENDING ORDER
C
C  PARAMETERS OF ROUTINE:
C  X      *IN/OUT* ARRAY OF LENGTH N. CONTAINS THE NUMBERS TO BE SORTED.
C                  ON EXIT, CONTAINS THE SORTED NUMBERS.
C  N      * INPUT* NUMBER OF ELEMENTS TO BE SORTED
C
C  METHOD USED IS SHELL SORT WITH SEQUENCE OF INCREMENTS AS IN
C  D.F.KNUTH (1969) 'THE ART OF COMPUTER PROGRAMMING', VOL.3, P.95
C
      DIMENSION X(N)        
      CHARACTER*8 C(N),CTMP
      IF(N.LE.1)RETURN
      J=4
      DO 10 I=1,100
      J=3*J+1
      IF(J.GE.N)GOTO 20
   10 CONTINUE
   20 CONTINUE
      M=(J/3)
      DO 60 MM=1,100
      M=M/3
      IF(M.EQ.0)RETURN
      DO 50 I=M+1,N
      TEST=X(I)
      CTMP=C(I)
      J=I
      DO 30 JJ=1,100
      J=J-M
      IF(J.LE.0)GOTO 40
      IF(TEST.GE.X(J))GOTO 40
      X(J+M)=X(J)
      C(J+M)=C(J)
   30 CONTINUE
   40 CONTINUE
      X(J+M)=TEST
      C(J+M)=CTMP
   50 CONTINUE
   60 CONTINUE
      END


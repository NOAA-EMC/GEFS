      SUBROUTINE CRHTAB(RHCL,IER)
C---------------------------------------------------------------------
C..  CLD-RH RELATIONS OBTAINED FROM MITCHELL-HAHN PROCEDURE, HERE READ
C     CLD/RH TUNING TABLES FOR DAY 0,1,...,5 AND MERGE INTO 1 FILE..
C                         .............K.A.C.   MAR 93
C     USE ONLY ONE TABLE (DAY 1) FOR ALL FCST HRS....K.A.C. FEB 94
c...    4 cld types .... KAC  FEB96
c...    smooth out last bunch of bins of the tables...KAC AUG97
C    OUTPUT:
C        RHCL - TUNING TABLES FOR ALL FORECAST DAYS
C        IER  - =1 IF TABLES AVAILABLE.. =-1 IF NO TABLES
C--------------------------------------------------------------------
      use machine
      implicit none
!!
      integer mcld,nseal,ida,nbin,nlon,nlat,lon,jl,nc,lat,kcl,nsl
      integer ken,icrit,isat,ibs,nb,kt,it,l,m,j,k,itim,ier,icfq,i
      integer iy,im,n,kd,nbdayi,id,ld
CRH1T PARAMETER (MCLD=3,NSEAL=2,IDA=6,
cmcl3 PARAMETER (MCLD=3,NSEAL=2,IDA=1,
      PARAMETER (MCLD=4,NSEAL=2,IDA=1,
     2           NBIN=100,NLON=2,NLAT=4)
      real (kind=kind_io8) cstem,cfrac,clsat,rhsat,binscl
      real (kind=kind_io8) RHFD(NBIN,NLON,NLAT,MCLD,NSEAL)
      real (kind=kind_io8) RRHFD(NBIN,NLON,NLAT,MCLD,NSEAL)
      real (kind=kind_io8) RTNFFD(NBIN,NLON,NLAT,MCLD,NSEAL)
      real (kind=kind_io8) RRNFFD(NBIN,NLON,NLAT,MCLD,NSEAL)
      real (kind=kind_io8) RHCF(NBIN,NLON,NLAT,MCLD,NSEAL)
      real (kind=kind_io8) RTNFCF(NBIN,NLON,NLAT,MCLD,NSEAL)
      INTEGER KPTS(NLON,NLAT,MCLD,NSEAL)
      INTEGER KKPTS(NLON,NLAT,MCLD,NSEAL)
      real (kind=kind_io8) RHC(NLON,NLAT,MCLD,NSEAL)
      real (kind=kind_io8) RHCL (NBIN,NLON,NLAT,MCLD,NSEAL,IDA)
      real (kind=kind_io8) RHCLA(NBIN,NLON,NLAT,MCLD,NSEAL)
      INTEGER ICDAYS(15),IDATE(4)
      real(kind=kind_io4) fhour
      real(kind=kind_io4) RHFD4(NBIN,NLON,NLAT,MCLD,NSEAL)
      real(kind=kind_io4) RTNFFD4(NBIN,NLON,NLAT,MCLD,NSEAL)
C...........................  BEGIN HERE  ..............
      IER = 1
      DO 8000 ITIM=1,IDA
      ICFQ = 43 + ITIM-1
      REWIND ICFQ
Cmcl3       NCLDS=1,2,3 (L,M,H)..JSL=1,2 (LAND,SEA)
cmcl4       MCLD=1,2,3,4 (BL,L,M,H)
      BINSCL = 1./NBIN
      DO 1000 M=1,NSEAL
       DO 1000 L=1,MCLD
        DO 1000 K=1,NLAT
         DO 1000 J=1,NLON
          DO 1000 I=1,NBIN
           RRHFD(I,J,K,L,M) = 0.
           RRNFFD(I,J,K,L,M) = 0.
 1000 CONTINUE
      DO 1001 M=1,NSEAL
       DO 1001 L=1,MCLD
        DO 1001 K=1,NLAT
         DO 1001 J=1,NLON
          KKPTS(J,K,L,M) = 0
 1001 CONTINUE
C....  READ THE DATA OFF THE ROTATING FILE
      READ (ICFQ,ERR=998,END=999) NBDAYI,ICDAYS
      DO 53 LD=1,NBDAYI
       ID = ICDAYS(LD) / 10000
       IM = (ICDAYS(LD)-ID*10000) / 100
       IY = ICDAYS(LD)-ID*10000-IM*100
   53 CONTINUE
      READ (ICFQ,ERR=998,END=999) FHOUR,IDATE
csela PRINT 3003,IDATE,FHOUR,ITIM
      DO 1300 KD=1,NBDAYI
       READ (ICFQ) RHFD4
       RHFD=RHFD4
       READ (ICFQ) RTNFFD4
       RTNFFD=RTNFFD4
       READ (ICFQ) KPTS
 
       DO 1002 M=1,NSEAL
        DO 1002 L=1,MCLD
         DO 1002 K=1,NLAT
          DO 1002 J=1,NLON
           DO 1002 I=1,NBIN
            RRHFD(I,J,K,L,M) = RRHFD(I,J,K,L,M) + RHFD(I,J,K,L,M)
            RRNFFD(I,J,K,L,M) = RRNFFD(I,J,K,L,M)+RTNFFD(I,J,K,L,M)
 1002  CONTINUE
       DO 1003 M=1,NSEAL
        DO 1003 L=1,MCLD
         DO 1003 K=1,NLAT
          DO 1003 J=1,NLON
           KKPTS(J,K,L,M) = KKPTS(J,K,L,M) + KPTS(J,K,L,M)
 1003  CONTINUE
 1300 CONTINUE
C
      DO 1004 M=1,NSEAL
       DO 1004 L=1,MCLD
        DO 1004 K=1,NLAT
         DO 1004 J=1,NLON
          DO 1004 I=1,NBIN
           RHCF(I,J,K,L,M) = RRHFD(I,J,K,L,M)
           RTNFCF(I,J,K,L,M) = RRNFFD(I,J,K,L,M)
 1004 CONTINUE
      DO 1005 M=1,NSEAL
       DO 1005 L=1,MCLD
        DO 1005 K=1,NLAT
         DO 1005 J=1,NLON
          KPTS(J,K,L,M) = KKPTS(J,K,L,M)
 1005 CONTINUE
C.....  COMPUTE THE CUMULATIVE FREQUENCY DISTRIBUTION..
      DO 200 N=1,NSEAL
       DO 200 K=1,MCLD
        DO 200 L=1,NLAT
         DO 200 J=1,NLON
          DO 190 I=2,NBIN
           RHCF(I,J,L,K,N) = RHCF(I-1,J,L,K,N) + RHCF(I,J,L,K,N)
           RTNFCF(I,J,L,K,N)=RTNFCF(I-1,J,L,K,N) + RTNFCF(I,J,L,K,N)
  190     CONTINUE
  200 CONTINUE
      DO 300 N=1,NSEAL
       DO 300 L=1,NLAT
        DO 300 J=1,NLON
         DO 300 K=1,MCLD
          DO 300 I=1,NBIN
           IF (KPTS(J,L,K,N).GT.0) THEN
            RHCF(I,J,L,K,N) = RHCF(I,J,L,K,N) / KPTS(J,L,K,N)
            RTNFCF(I,J,L,K,N) = RTNFCF(I,J,L,K,N) / KPTS(J,L,K,N)
c...  cause we mix calculations of rh retune with cray and ibm words
c      the last value of rhcf is close to but ne 1.0,
c      so we reset it in order that the 360 loop gives compleat tabl
c...  rtnfcf caused couple of problems, seems to be ever so slightly
c      gt 1.0
            IF (I.EQ.NBIN) THEN
             RHCF(I,J,L,K,N) = 1.0
            END IF
            IF (RTNFCF(I,J,L,K,N).GE.1.0) THEN
             RTNFCF(I,J,L,K,N) = 1.0
            END IF
           ELSE
            RHCF(I,J,L,K,N) = -0.1
            RTNFCF(I,J,L,K,N) = -0.1
           END IF
  300 CONTINUE
      DO 255 NSL=1,NSEAL
       DO 255 KCL=1,MCLD
csela   PRINT 264,KCL,NSL
csela   PRINT 265,((KPTS(I,L,KCL,NSL),I=1,NLON),L=1,NLAT)
  255 CONTINUE
      DO 360 NSL=1,NSEAL
       DO 360 K=1,MCLD
        DO 360 L=1,NLAT
         DO 360 J=1,NLON
          IF (KPTS(J,L,K,NSL).LE.0) GO TO 317
          DO 320 I=1,NBIN
           ICRIT = I
           IF (RHCF(I,J,L,K,NSL).GE.RTNFCF(1,J,L,K,NSL)) GO TO 350
  320     CONTINUE
C... NO CRITICAL RH
  317     ICRIT=-1
  350     RHC(J,L,K,NSL) = ICRIT * BINSCL
  360 CONTINUE
csela DO 1210 NSL=1,NSEAL
csela  DO 1210 K=1,MCLD
csela   PRINT 1221,K,NSL
csela   DO 1210 L=1,NLAT
csela    PRINT 211,(RHC(J,L,K,NSL),J=1,NLON)
csela  1210 CONTINUE
      DO 450 NSL=1,NSEAL
       DO 450 KEN=1,MCLD
        DO 450 L=1,NLAT
         DO 450 JL=1,NLON
          DO 400 I=1,NBIN
           RHCL(I,JL,L,KEN,NSL,ITIM) = -0.1
  400     CONTINUE
  450 CONTINUE
      DO 751 NSL=1,NSEAL
       DO 751 KEN=1,MCLD
        DO 751 L=1,NLAT
         DO 751 JL=1,NLON
          IF (KPTS(JL,L,KEN,NSL).LE.0) GO TO 751
          DO 753 I=1,NBIN
           DO 755 J=1,NBIN
            IF (RHCF(J,JL,L,KEN,NSL).GE.RTNFCF(I,JL,L,KEN,NSL)) THEN
             RHCL(I,JL,L,KEN,NSL,ITIM) = J*BINSCL
             GO TO 753
            END IF
  755      CONTINUE
  753     CONTINUE
  751 CONTINUE
      DO 3000 LON=1,NLON
       DO 3000 LAT=1,NLAT
        DO 3000 NC=1,MCLD
         DO 3000 NSL=1,NSEAL
         ISAT = 0
         DO 67 IT=1,NBIN
          CFRAC = BINSCL * (IT-1)
          IF (RHCL(IT,LON,LAT,NC,NSL,ITIM).LT.0.) THEN
           PRINT 1941,IT,NSL,NC,LAT,LON
           STOP
          END IF
          IF (IT.LT.NBIN.AND.RTNFCF(IT,LON,LAT,NC,NSL).GE.1.) THEN
           IF (ISAT.LE.0) THEN
            ISAT = IT
            RHSAT = RHCL(IT,LON,LAT,NC,NSL,ITIM)
            CLSAT = CFRAC
           END IF
           RHCL(IT,LON,LAT,NC,NSL,ITIM) =
     1               RHSAT + (1.-RHSAT)*(CFRAC-CLSAT)/(1.-CLSAT)
          END IF
          IF (IT.EQ.NBIN) RHCL(IT,LON,LAT,NC,NSL,ITIM) = 1.
   67    CONTINUE
 3000 CONTINUE
c... smooth out the table as it reaches rh=1.0, via linear interpolation
c      between location of rh ge .98 and the NBIN bin (where rh=1.0)
c... previously rh=1.0 occurred for many of the latter bins in the
c      table, thereby giving a cloud value of less then 1.0 for rh=1.0
      nb=nbin-2
      DO 4000 LON=1,NLON
       DO 4000 LAT=1,NLAT
        DO 4000 NC=1,MCLD
         DO 4000 NSL=1,NSEAL
         do 4167 it=1,nbin
          RHCLA(IT,LON,LAT,NC,NSL)=RHCL(IT,LON,LAT,NC,NSL,ITIM)
 4167    continue
         DO 4067 IT=1,nb
          ibs=it
          cfrac=binscl*ibs
          IF (RHCL(IT,LON,LAT,NC,NSL,ITIM).ge..98) THEN
CC           Print 4011,nsl,nc,lat,lon,ibs,nbin
CC 4011      format (1h ,'nsl,nc,lat,lon,ibs,nbin=',6i4)
           do 4068 kt=ibs,nbin
            cstem=binscl*kt
            RHCLA(kt,LON,LAT,NC,NSL) =
     1       rhcl(ibs,LON,LAT,NC,NSL,ITIM)+
     2       (rhcl(nbin,LON,LAT,NC,NSL,ITIM)
     3                      -rhcl(ibs,LON,LAT,NC,NSL,ITIM))*
     3       (cstem-cfrac)/(1.-cfrac)
c           if (nc.eq.2.and.lat.eq.2.and.lo.eq.1.and.nsl.eq.2) then
c            print 4012,kt,cstem,cfrac,rhcl(ibs,LON,LAT,NC,NSL,ITIM),
c    1                RHCLA(kt,LON,LAT,NC,NSL)
c  4012 format(1h ,'kt,cs,cf,rhibs,rhcla=',i5,4f12.8)
c           end if
 4068     continue
          go to 4000
          end if
 4067    CONTINUE
 4000 CONTINUE
c... restore table data to preferred location..
      DO 4200 LON=1,NLON
       DO 4200 LAT=1,NLAT
        DO 4200 NC=1,MCLD
         DO 4200 NSL=1,NSEAL
          DO 4200 IT=1,NBIN
           RHCL(IT,LON,LAT,NC,NSL,ITIM)= RHCLA(IT,LON,LAT,NC,NSL)
 4200 CONTINUE
 8000 CONTINUE
      DO 8001 KEN=1,IDA
       ICFQ = 42 + KEN
       REWIND ICFQ
 8001 CONTINUE
      RETURN
  998 PRINT 988,ITIM
      IER = -1
      RETURN
  999 PRINT 989,ITIM
      IER = -1
      RETURN
   11 FORMAT(1H ,'  from crhtab DAYS ON FILE =',I5)
   51 FORMAT(1H ,'  from crhtab ARCHV DATA FROM DA,MO,YR=',3I4)
  202 FORMAT(1H0,' MODEL RH ',' OBS RTCLD')
  203 FORMAT(2F10.2)
  210 FORMAT(1H ,' NO CRIT RH FOR LAT=',I3,' AND LON BAND=',I3,
     1           ' LAND(=1) SEA=',I3)
  211 FORMAT(1H ,15F6.2)
  264 FORMAT(1H ,' NUMBER OF GG POINTS USED IN EACH AREA..BY LATITUDE',
     1           '..FOR CLOUD TYPE=',I4,'SEALAND=',I2)
  265 FORMAT(1H ,15I8)
  988 FORMAT(1H ,'from crhtab ERROR READING TABLES FOR TIME=',I4)
  989 FORMAT(1H ,'from crhtab E.O.F READING TABLES FOR TIME=',I4)
 1221 FORMAT(1H0,' CRITICAL RH FOR LON,LAT ARRAYS FOR CLD TYPE=',I3,
     1           ' LAND(=1) SEA=',I3)
 1941 FORMAT(1H ,' NEG RHCL FOR IT,NSL,NC,LAT,LON=',5I4,'...STOPPP..')
 3003 FORMAT(5X,'...LAST DATE/TIME AND CURRENT ITIM',/,10X,
     1       4I15,F7.1,I6)
      END

      PROGRAM VRFYGEN
C
C PROGRAM HISTORY LOG:
C  96-09-10  MARK BALDWIN
C  02-09-20  YUEJIAN ZHU MODIFIED FOR IMPLEMENTATION
C
C  GENERAL PRECIP VERF CODE, USES M IRDELL'S IPLIB TO INTERPOLATE
C   CODE IS MORE OR LESS GENERAL, DRIVEN BY THE ANALYSIS GDS
C    BUT DIMENSIONS, ETC ARE SET UP TO GO TO GRID 211 (80km LMBC)
C    PARTICULARLY FOR THE REGIONAL MASKS
C
C  1. READ IN PRECIP ANALYSIS
C  2. READ IN PRECIP FORECAST
C  3. INTERPOLATE FORECAST TO ANALYSIS GRID
C  4. DO VERIFICATION
C  5. OUTPUT STATS, FORECAST ON ANALYSIS GRID FOR ARCHIVE
C
C  INPUT UNIT 5
C   CONTROLS THE INPUT
C LINE
C  #  
C  1   PATH TO PCP ANALYSIS FILE
C  2   PATH TO PCP OBS FILE
C  
C  3   MODEL NAME, SEE GETARCH FOR PROPER SPELLING AND PATH TO ARCHIVE
C  4   GRID NUMBER TO PULL FROM FCSTS, -1 MEANS USE THE FIRST ONE FOUND
C  5   NUMBER OF CYCLES AVAILABLE=N  (ex, 1 for ecmwf, possibly 4 for avn)
C  5+1  CYCLE #1 (eg, 0)
C  5+2  CYCLE #1 (eg, 12)
C ....
C  5+N  CYCLE #N
C  6+N OUTPUT FREQUENCY IN HOURS (ex, 3 for meso, 12 for eta, 24 for ecmwf)
C  7+N FORECAST DURATIONIN HOURS (ex, 36 (not 33) for meso, 72 for ecmwf)
C  ...
C  ...REPEAT LINES 2-7+N FOR ALL MODELS YOU WANT TO VERIFY
C  ...
C LAST  'done'
C
c     parameter(ji=445*365,jo=445*365,numreg=10,numthr=12) 
      parameter(ji=445*365,jo=445*365,numreg=10,numthr=9) 

      integer IPOPT(20)
      integer kpdsa(25),kgdsa(22),KPDSF(25),KGDSF(22),kpds1(25)
      integer jpds(25),jgds(22)
      integer KGDSF2(22),kpdso(25),KGDSO(25),kgds80(22)
      integer ICYC(8),mnth(12),jstat(32),mlana(ji)
      integer maskreg(jo,numreg),MASKNAT(ji),mask80(ji)

      logical*1 lfci(ji),lana(jo),lfco(jo),LNAT(ji),FIRST,FIRCT
      logical*1 l80(ji),lfc80(ji),LPCPDA

      DIMENSION rlat(jo),rlon(jo),smask(jo)
      DIMENSION fci(ji),sumfc(ji),fc2(ji),fco(jo),sum80(ji)
      DIMENSION ana(jo),thresh(numthr),ananat(ji),anan80(ji)
      DIMENSION thresh2(numthr),fcold(ji)

      character gdso(400),FNAME*80,fnamei*80,MDLNAM*10,FMASK(numreg)*4
      character datestr*15,month(12)*3,mdlverf*10
      character datcmd*18,fname2*80,fname2i*80
      CHARACTER*80 CPGBA,CPGIA,CPGBF,CPGIF,PCPDA,CMASK,DMASK,CTMPD

      data thresh/0.2,2.,5.,10.,15.,25.,35.,50.,75./
c     data thresh/.1,.25,1.,2.5,5.,10.,20.,25.,30.,40.,50.,75./
      data thresh2/0.01,0.10,0.25,0.50,0.75,1.0,1.5,2.0,3.0/
      data mnth/31,28,31,30,31,30,31,31,30,31,30,31/
      data month/'jan','feb','mar','apr','may','jun',
     &           'jul','aug','sep','oct','nov','dec'/
      data FMASK/ 'Cusa','Appl','Npln','Spln',
     &           'East','Gulf','West','Rkys','Midw','Fntr'/
      data kgds80/201,12902,1,182,210113,136,92,141,577,538,64,
     &            0,0,0,0,0,0,0,0,255,0,0/
C
C  ASSIGN THE UNITS
      KGDSF  =  0
      KPDSF  =  0
      LUMASK = 13
      LPCDAT = 15
      MDLINF = 17
      LUNOUT = 51
      LOANA  = 61
      LSTOUT = 82

C
C  SET UP READING CART DATA FORMAT
 80   FORMAT(A80)
 88   FORMAT(A10)
 98   FORMAT(A40)
 8810 FORMAT(8A10)

C
C  ASSIGN AND OPEN STATS OUTPUT FILE 
      OPEN (UNIT=LSTOUT,FILE='stat.out',FORM='FORMATTED')
      OPEN (UNIT=LOANA,FILE='obs_box.dat',FORM='UNFORMATTED',
     *      STATUS='new')

C
C  READ IN MODEL INFO FILE NAME AND OPENED
      READ   (5,80) FNAME
      WRITE  (6,*) 'MODEL  INFORMATION FILE: ',FNAME(1:40)
      CLOSE  (MDLINF)
      OPEN   (UNIT=MDLINF,FILE=FNAME,FORM='FORMATTED')
 99   REWIND (MDLINF)

C
C  READ IN CONUS and US REGIONAL DATA FILE                   
      READ   (5,80,END=9000) CMASK 
      WRITE  (6,*) 'CON US AND REGIONAL MASK: ',CMASK(1:40) 
      LCMASK=LEN_TRIM(CMASK)

C
C  READ IN FOR TEMP DIRECTORY                                
      READ   (5,80,END=9000) CTMPD 
      WRITE  (6,*) 'FOR TEMP DIRECTORY: ',CTMPD(1:40) 
      LCTMPD=LEN_TRIM(CTMPD)

C
C  READ IN GRIB ANALYSIS PRECIPITATION FILE NAME AND OPENED
      READ   (5,80,END=9000) CPGBA  
      WRITE  (6,*) 'GRIB PRECIPITATION FILE: ',CPGBA(1:40) 
      LPGB=LEN_TRIM(CPGBA)
      CPGIA(1:LPGB)=CPGBA    
      CPGIA(LPGB+1:LPGB+6)='.index'
      WRITE  (6,*) 'GRIB PRECIPITATION FILE: ',CPGIA(1:40)  
      LPGI=LEN_TRIM(CPGIA)
      CALL BAOPENR(11,CPGBA(1:LPGB),IER11)
      CALL BAOPENR(12,CPGIA(1:LPGI),IER12)
      IERRS = IER11 + IER12
      IF (IERRS.NE.0) THEN
       WRITE(6,*) 'GRIB:BAOPEN ERR FOR DATA ',CPGBA           
       WRITE(6,*) 'GRIB:BAOPEN ERR FOR DATA ',CPGIA           
       WRITE(6,*) 'PLEASE CHECK DATA AVAILABLE OR NOT !!!'        
       GOTO 9000 
      ENDIF

C
C  READ IN OBSERVATION PRECIPITATION DATA FILE NAME AND OPENED
      READ   (5,80) PCPDA
      WRITE  (6,*) 'OBS  PRECIPITATION FILE: ',PCPDA(1:40)
      IF (PCPDA(1:4).NE.'NONE') THEN
       LPCPDA=.TRUE.
       CLOSE(LPCDAT)
       OPEN(UNIT=LPCDAT,FILE=PCPDA,FORM='FORMATTED')
      ELSE
       LPCPDA=.FALSE.
      ENDIF
C
C  READ IN MRF FACTOR FOR BIAS CORECTION ( Default:1.0 )
      READ   (5,*) FMRF
      WRITE  (6,*) 'MRF FACTER IS ',FMRF
C
C  READ IN PRECIP ANALYSIS AND FORECAST ( GRIB FORMAT )
      MDATA=20000
      J   =-1
      JPDS=-1
      JGDS=-1
      JPDS(5)=59
      CALL GETGB(11,12,MDATA,J,JPDS,JGDS,
     .           KF,K,KPDSA,KGDSA,LANA,ANA,IRET)
      IF (IRET.EQ.0) THEN
       CALL GRANGE(KF,LANA,ANA,DMIN,DMAX)
       WRITE(*,886)
       WRITE(*,888) K,(KPDSA(I),I=5,11),KPDSA(14),KF,DMIN,DMAX
       KSTHR=KPDSA(11)
       KEDHR=KPDSA(15)
      ELSE
       WRITE(6,*) 'GETGB PROBLEM FOR ANALYSIS : IRET=',IRET
       GOTO 9000
      ENDIF

      WRITE(6,*) ' MAKING ANALYSIS ON GRID 211 '
      CALL MAKGDS(211,KGDSA,GDSO,LENGDS,IRET)
      IMA = KGDSA(2)
      JMA = KGDSA(3)

      REWIND(LPCDAT)
      CALL ANNATV(LPCDAT,IMA,JMA,211,KPDSA,KGDSA,ANA,MLANA)

      DO J=1,IMA*JMA
       LANA(J)=.FALSE.
       IF (MLANA(J).EQ.1) LANA(J)=.TRUE.
      ENDDO

      IPOPT=0
C
      WRITE(6,*) ' KGDSA-211 = ',KGDSA
      IG    = KPDSA(3)
      IMA   = KGDSA(2)
      JMA   = KGDSA(3)
      JSIZE = JMA*IMA
      IF (KGDSA(1).EQ.3) KGDSA(14)=KGDSA(12)

C
C  READ IN REGIONAL MASKS FOR THIS GRID
      DO NN=1,NUMREG
       WRITE(FNAME,881) IG,FMASK(NN)
       DMASK=CMASK(1:LCMASK) // FNAME(1:8)
       OPEN(UNIT=LUMASK,FILE=DMASK,FORM='UNFORMATTED')
       REWIND(LUMASK)
       WRITE (6,*) ' START TO READ: ',DMASK(1:60)
       READ(LUMASK) (SMASK(KK),KK=1,JSIZE)
       CLOSE(LUMASK)

       IC = 0

       DO KK=1,JSIZE
        IC=IC+NINT(SMASK(KK))
        MASKREG(KK,NN)=NINT(SMASK(KK))
       ENDDO
      ENDDO
C--------+---------+---------+---------+---------+---------+---------+---------+
C881   FORMAT('/nfsuser/g01/wx20yz/rvrfy/data/pcpmask',i3.3,'.',a4)
 881   FORMAT(i3.3,'.',a4)
 886   FORMAT('  Irec  pds5 pds6 pds7 pds8 pds9 pd10 pd11 pd14',
     .       '  ndata  Minimun    Maximum')
 888   FORMAT (i4,2x,8i5,i8,2g12.4)


C
C  SET UP DATES FOR VERIFICATION VALID TIME
      WRITE (6,*) 'kpds15=',KPDSA(15),'kpds11=',KPDSA(11)
      IACC = KPDSA(15)
      IVYR = (KPDSA(21)-1)*100+KPDSA(8)
      IVMN = KPDSA(9)
      IF (MOD(IVYR,100).NE.0.AND.MOD(IVYR,4).EQ.0) MNTH(2)=29
      IF (MOD(IVYR,400).EQ.0) MNTH(2)=29
      IVDA = KPDSA(10)
      IVHR = KPDSA(11)+KPDSA(15)
      DO WHILE (IVHR.GT.23)
       IVDA = IVDA+1
       IVHR = IVHR-24
      ENDDO
      IF (IVDA.GT.MNTH(IVMN)) THEN
       IVDA = IVDA-MNTH(IVMN)
       IVMN = IVMN+1
      ENDIF 
      IF (IVMN.GT.12) THEN
       IVMN = 1
       IVYR = IVYR+1
      ENDIF
      WRITE(6,*) 'VERIFICATION DATE: ',ivyr,ivmn,ivda,ivhr
C
C  MAIN LOOP, MAIN LOOP, MAIN LOOP
C  LOOP THROUGH MODELS TO VERIFY
C
C  READ MODEL NAME  
C
      READ(MDLINF,88,END=99) MDLNAM
      DO WHILE (MDLNAM.NE.'done      ')
       FIRST=.TRUE.
       FIRCT=.TRUE.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      READ IN GRID NUM          - NGRID        C
C      NUM OF START TIMES        - NSTART       C
C      MODEL START TIME CYCLES   - ICYC         C
C      OUTPUT FREQUENCY          - IFREQ        C
C      FORECAST DURATION         - IFDUR        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       READ(MDLINF,*) NGRID
       READ(MDLINF,*) NSTART
       DO N=1,NSTART
        READ(MDLINF,*) ICYC(N)
       ENDDO
       READ(MDLINF,*) IFREQ
       READ(MDLINF,*) IFDUR
C
CMEB PROBABLY WANT TO CHANGE THIS
C
       KDAT = INDEX(MDLNAM,' ') -1
       IF (KDAT.LE.0) KDAT = LEN(MDLNAM)

C  SET UP DATES FOR FORECAST START TIME, GIVEN VALID TIME
C
       INUMF  = IACC/IFREQ
       IFREQ2 = IFREQ
       IF (MDLNAM.EQ.'dwd       ') IFREQ2 = 12
       IF (MDLNAM.EQ.'ecmwf     ') INUMF  = 2
C
C  GET THE FORECASTS THAT ARE VALID AT THE ANALYSIS DATE/TIME
C  AND COVER THE SAME ACCUMULATION PERIOD AS THE ANALYSIS
C
       DO KTIME=IACC,IFDUR,IFREQ2
        ismn=ivmn
        isyr=ivyr
        isda=ivda
c       ishr=ivhr-ktime - ivhr
c       ishr=ivhr-ktime
        ishr=KSTHR-ktime
        do while (ishr.lt.0)
         isda=isda-1
         ishr=ishr+24
        ENDDO
        do while (isda.lt.1) 
         ismn=ismn-1
         do while (ismn.lt.1)
          isyr=isyr-1
          ismn=ismn+12
         ENDDO
         isda=isda+mnth(ismn)
        ENDDO
        WRITE(6,*) 'STARTED      TIME: ',isyr,ismn,isda,ishr
 
C  LOOP OVER FORECAST START CYCLEs
C
        DO n=1,nstart
         if (ishr.eq.ICYC(n)) THEN
          fc2   = 0.
          sumfc = 0.
          IFACC = 0
c
c loop over the number of forecasts we need to get an iacc h accum
c
          do kn=1,inumf
           ifhr=ktime + ivhr - KSTHR - (kn-1)*ifreq
c          ifhr=ktime + ivhr - (kn-1)*ifreq
           ifhr1=ifhr - ifreq
           if (MDLNAM.eq.'ecmwf     ') THEN
            ifhr1=ifhr
            ifhr=0
           ENDIF
           CLOSE(21)
           CLOSE(22)
CCC Modified by Yuejian Zhu (12/14/98)
           if (ifhr1.lt.100.and.ifhr.lt.100) THEN
c           WRITE (datcmd,120) mod(isyr,100),ismn,isda,ishr,ifhr1,ifhr
            WRITE (datcmd,120) isyr,ismn,isda,ishr,ifhr1,ifhr
           elseif (ifhr1.lt.100.and.ifhr.ge.100) THEN
c           WRITE (datcmd,121) mod(isyr,100),ismn,isda,ishr,ifhr1,ifhr
            WRITE (datcmd,121) isyr,ismn,isda,ishr,ifhr1,ifhr
           else
c           WRITE (datcmd,122) mod(isyr,100),ismn,isda,ishr,ifhr1,ifhr
            WRITE (datcmd,122) isyr,ismn,isda,ishr,ifhr1,ifhr
           ENDIF
           IF ((MDLNAM.eq.'meso      ').and.
     &         (kn.lt.inumf.or.ishr.eq.0)) THEN
            WRITE (datcmd,120) mod(isyr,100),ismn,isda,
     &             ishr+3,ifhr1-3,ifhr-3
           ENDIF
 120       format(i4.4,3i2.2,'_',i2.2,'_',i2.2)
 121       format(i4.4,3i2.2,'_',i2.2,'_',i3.3)
 122       format(i4.4,3i2.2,'_',i3.3,'_',i3.3)

C          CPGBF='/ptmp/wx20yz/' // MDLNAM(1:KDAT) // '/' //
           CPGBF=CTMPD(1:LCTMPD) // '/' //
     .        MDLNAM(1:KDAT) // '_' // datcmd 
           LPGB=LEN_TRIM(CPGBF)
           CPGIF(1:LPGB)=CPGBF(1:LPGB)
           CPGIF(LPGB+1:LPGB+6)='.index'         
C
C        CALL FUNCTION STAT TO FIND NUMBER OF BYTES IN FILE
C
           WRITE  (6,*) '==============================================' 
           WRITE  (6,*) 'FORECAST DATA NAME: ',CPGBF(1:60)
           WRITE  (6,*) 'FORECAST DATA NAME: ',CPGIF(1:60)

           LPGB=LEN_TRIM(CPGBF)
           LPGI=LEN_TRIM(CPGIF)
           CALL BAOPENR(21,CPGBF(1:LPGB),IER21)
           CALL BAOPENR(22,CPGIF(1:LPGI),IER22)
           IERRS = IER21 + IER22
           IF (IERRS.NE.0) THEN
            WRITE(6,*) 'GRIB:BAOPEN ERR FOR DATA ',CPGBF           
            WRITE(6,*) 'GRIB:BAOPEN ERR FOR DATA ',CPGIF           
            WRITE(6,*) 'PLEASE CHECK DATA AVAILABLE OR NOT'        
            GOTO 9100 
           ENDIF

C
C  READ IN PRECIP FORECAST, AND SUM UP IF NEEDED
           MDATA=20000
           J   =-1
           JPDS=-1
           JGDS=-1
           JPDS(5)=59
           CALL GETGB(21,22,MDATA,J,JPDS,JGDS,
     .                KF,K,KPDSF,KGDSF,LFCI,FCI,IRET)
           IF (IRET.EQ.0) THEN
            CALL GRANGE(KF,LFCI,FCI,DMIN,DMAX)
            WRITE(*,886)
            WRITE(*,888) K,(KPDSF(I),I=5,11),KPDSF(14),KF,DMIN,DMAX
           ENDIF
           IF (KPDSF(5).EQ.61.OR.KPDSF(5).EQ.228
     .        .OR.KPDSF(5).EQ.50.OR.KPDSF(5).EQ.59) THEN
c
            IF (NGRID.EQ.-1) NGRID=KPDSF(3)
            IF (KPDSF(3).EQ.NGRID) THEN
               
             IF (KPDSF(5).EQ.228) FCTR=1000.
c  ecmwf has units of m and 228 is total forecast accum 24,48,72, etc.
             IF (KPDSF(5).eq.61.or.KPDSF(5).eq.50) FCTR=1.
             IF (MOD(KPDSF(14),12).ne.0) KPDSF(14)=KPDSF(14)+256
             IF (MOD(KPDSF(15),12).ne.0) KPDSF(15)=KPDSF(15)+256
             IF (KPDSF(5).eq.59) FCTR=(KPDSF(15)-KPDSF(14))*3600.
          WRITE(6,*) 'FCTR=',FCTR,'PDS14=',KPDSF(14),'PDS15=',KPDSF(15)
C  global has units of mm/s
C
C  SUM UP ACCUMULATION PRECIPITATION
             IF (KPDSF(5).EQ.228) THEN
              IF(KN.EQ.1) THEN
               IFACC=KPDSF(14)
               DO IJ=1,KF
                IF (LFCI(ij).AND.FCI(IJ).GT.0.0)
     &           SUMFC(IJ)=FCI(IJ)*FCTR*FMRF
               ENDDO
              ELSE
               IFACC=IFACC-KPDSF(14)
               DO IJ=1,KF
                IF (LFCI(IJ).AND.FCI(IJ).GT.0.0)
     &           SUMFC(IJ)=SUMFC(IJ)-FCI(IJ)*FCTR*FMRF
               ENDDO
              ENDIF
             ELSE
              DO IJ=1,KF
               IF (LFCI(IJ).AND.FCI(IJ).GT.0.0) 
     &          SUMFC(IJ)=SUMFC(IJ)+FCI(IJ)*FCTR*FMRF
              ENDDO
              IFACC=IFACC+KPDSF(15)-KPDSF(14)
             ENDIF
            ENDIF
           ENDIF
          ENDDO
C
C  if we've found enough fcst precip to match the obs acc period...
C
          WRITE (6,*) 'IFACC,IACC=',IFACC,IACC
          IF (IFACC.EQ.IACC) THEN
           IF (LPCPDA) THEN
C
C   DO NATIVE GRID ANALYSIS AND VERIFICATION
C
            IF (FIRST) THEN
             IMF   = KGDSF(2)
             JMF   = KGDSF(3)
             IMJMF = IMF*JMF
             IF (KGDSF(1).EQ.201) THEN
              IMF   = KGDSF(7)
              JMF   = KGDSF(8)
              IMJMF = IMF*JMF-JMF/2
             ENDIF
             IGRIDF = KPDSF(3)
             IGMDL  = KPDSF(3)
             REWIND(LPCDAT)
C--------+---------+---------+---------+---------+---------+---------+---------+
             CALL                                                           
     .       ANNATV(LPCDAT,IMF,JMF,IGRIDF,KPDS1,KGDSF,ANANAT,MASKNAT)
             III=0
             DO KNA=1,IMJMF
              IF (MASKNAT(KNA).EQ.1) THEN
               LNAT(KNA)=.TRUE.
c              WRITE(*,'(i5,f10.3)') KNA,ANANAT(KNA)
               III=III+1
              ELSE
               LNAT(KNA)=.FALSE.
              ENDIF
             ENDDO
C
C   WRITE OUT THE OBSERVATION ANALYSIS AT EACH GRID POINTS
C
             WRITE(LOANA) (MASKNAT(KNA),KNA=1,10512)
             WRITE(LOANA) (ANANAT(KNA),KNA=1,10512)

             WRITE(6,*) 'NUM. OF PTS IN NAT GRID ANALYSIS = ',III
             IF (MDLNAM.EQ.'eta       '.OR.
     .           MDLNAM.EQ.'para32    '.OR.
     .           MDLNAM.EQ.'meso      ') THEN
              REWIND(LPCDAT)
              CALL ANNATV(LPCDAT,92,141,90,KPDS1,KGDS80,ANAN80,MASK80)
              DO KNA=1,12902
               IF (MASK80(KNA).EQ.1) THEN
                l80(KNA)=.TRUE.
               ELSE
                l80(KNA)=.FALSE.
               ENDIF
              ENDDO
             ENDIF
             FIRST=.FALSE.
            ENDIF
            CHKSUM=0.
            DO KNA=1,IMJMF
             IF(LNAT(KNA)) CHKSUM=CHKSUM+SUMFC(KNA)
            ENDDO

            WRITE(6,*) 'CHKSUM= ',CHKSUM
            IF (CHKSUM.GT.0.) THEN
c            CALL VERF(SUMFC,ANANAT,LFCI,LNAT,MASKNAT,IMJMF,1,12,THRESH,
c    .                 1,IGRIDF,IGMDL,1,FMASK,ISYR,ISMN,ISDA,ISHR,
c    .                 IVYR,IVMN,IVDA,IVHR,KTIME,IACC,MDLNAM,LSTOUT)
             CALL VERF(SUMFC,ANANAT,LFCI,LNAT,MASKNAT,IMJMF,1,8,THRESH2,
     .                 2,IGRIDF,IGMDL,1,FMASK,ISYR,ISMN,ISDA,ISHR,
     .                 IVYR,IVMN,IVDA,IVHR,KTIME,IACC,MDLNAM,LSTOUT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     FCST    - FORECAST GRID                                             C
C     ANA     - ANALYSIS GRID                                             C
C     LFC     - BIT MAP FOR FORECAST GRID                                 C
C     LAN     - BIT MAP FOR ANALYSIS GRID TO DETERMINE VERF DOMAIN        C
C     MASKREG - INTEGER MAP FOR TO DETERMINE REGIONAL VERF DOMAINS        C
C     JO      - SIZE OF FCST,ANA,LFC,LAN                                  C
C     NUMREG  - NUMBER OF REGIONS                                         C
C     NUMTHR  - NUMBER OF THRESHOLDS                                      C
C     THRESH  - VERIFICATION THRESHOLDS                                   C
C     TNAME   - STRING VERSION OF THESHOLDS                               C
C     IGRID   - GRID NUMBER                                               C
C     IANATYP - ANALYSIS TYPE (1=NATV, 2=MPCP)                            C
C     FMASK   - NAMES OF THE REGIONAL MASKS                               C
C     IYR     - YEAR                                                      C
C     IMN     - MONTH                                                     C
C     IDA     - DAY                                                       C
C     IHR     - HOUR                                                      C
C     IFHR    - FCST HOUR                                                 C
C     IACC    - LENGTH OF ACCUMULATION                                    C
C     MDLNAM  - NAME OF MODEL                                             C
C     IOUNIT  - UNIT TO WRITE STATS OUT TO                                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C    if eta or meso, do cetlih and verify on 80km grid
C
C      we don't use this option
C            IF (MDLNAM.eq.'eta       '.or.MDLNAM.eq.'para32    '
C    &          .or.MDLNAM.eq.'meso      ') THEN
C             CALL CETLIH(igmdl,firct,sumfc,sum80,lfci,lfc80)
C             CALL VERF(SUM80,ANAN80,LFC80,L80,MASK80,12902,1,8,THRESH2,
C    &                  2,90,igmdl,1,fmask,isyr,ismn,isda,ishr,
C    &                  ivyr,ivmn,ivda,ivhr,ktime,iacc,MDLNAM,LSTOUT)
C             firct=.false.
C            ENDIF

            ENDIF
           ENDIF
 
C  put this on the filled grid if unfilled
C
           if (KGDSF(1).eq.201) THEN
            call ipxetas(1,ji,ji,1,KGDSF,sumfc,KGDSF2,fc2,IRET)
            IMF2=KGDSF2(7) 
            JMF2=KGDSF2(8) 
            kf=IMF2*JMF2
            if(IRET.ne.0) call exit(IRET)
            lfci=.TRUE.
           else
            do ij=1,kf
             fc2(ij)=sumfc(ij)
            ENDDO
            KGDSF2=KGDSF
           ENDIF
 
           amax=0.
           do ij=1,kf
            if (fc2(ij).gt.amax) amax=fc2(ij)
           ENDDO
          IF (amax.gt.0.) THEN
 
C  INTERPOLATE PRECIP FORECAST TO ANALYSIS GRID ( Default: 211 grid )
C
           call makgds(ig,KGDSO,gdso,lengds,IRET)
           if(IRET.ne.0) call exit(IRET)
           IPOPT(1)=-1
           IPOPT(2)=-1
           CALL IPOLATES(6,IPOPT,KGDSF2,KGDSO,ji,jo,1,1,lfci,fc2,
     .                   ko,rlat,rlon,ibo,lfco,fco,IRET)
           if(IRET.ne.0) CALL exit(IRET)
 
           AMAXX=0.
           do ij=1,ko
            if (fco(ij).gt.AMAXX) AMAXX=fco(ij)
           ENDDO
 
C  MAIN VERIFICATION PROGRAM CALL
C
            IGMDL=KPDSF(3)
            CALL VERF(FCO,ANA,LFCO,LANA,MASKREG,JO,NUMREG,NUMTHR,THRESH,
     .                1,ig,igmdl,2,fmask,isyr,ismn,isda,ishr,
     .                ivyr,ivmn,ivda,ivhr,ktime,iacc,MDLNAM,LSTOUT)
           ENDIF
          ENDIF
         ENDIF
        ENDDO
 9100  CONTINUE
       ENDDO    ! DO KTIME=IACC,IFDUR,IFREQ2
C
C  READ NEXT MODEL NAME  
C
      READ(MDLINF,88,END=99) MDLNAM
      ENDDO    ! DO WHILE (MDLNAM.NE.'done      ')

 9000 STOP
      END


      PROGRAM VRFYGEN 
! 
! PROGRAM HISTORY LOG:
!  96-09-10  MARK BALDWIN
!  02-09-20  YUEJIAN ZHU MODIFIED FOR IMPLEMENTATION 
!  14-11-10  YAN LUO MODIFIED TO CONVERT I/O FROM GRIB1 TO GRIB2 
! 
!  ABSTRACT: 
!  GENERAL PRECIP VERF CODE, USES M IRDELL'S IPLIB TO INTERPOLATE
!   CODE IS MORE OR LESS GENERAL, DRIVEN BY THE ANALYSIS GDS
!    BUT DIMENSIONS, ETC ARE SET UP TO GO TO GRID 211 (80km LMBC)
!    PARTICULARLY FOR THE REGIONAL MASKS
!
!  1. READ IN PRECIP ANALYSIS
!  2. READ IN PRECIP FORECAST
!  3. INTERPOLATE FORECAST TO ANALYSIS GRID
!  4. DO VERIFICATION
!  5. OUTPUT STATS, FORECAST ON ANALYSIS GRID FOR ARCHIVE
!
!  INPUT UNIT 5
!   CONTROLS THE INPUT
! LINE
!  #  
!  1   PATH TO PCP ANALYSIS FILE
!  2   PATH TO PCP OBS FILE
!  
!  3   MODEL NAME, SEE GETARCH FOR PROPER SPELLING AND PATH TO ARCHIVE
!  4   GRID NUMBER TO PULL FROM FCSTS, -1 MEANS USE THE FIRST ONE FOUND
!  5   NUMBER OF CYCLES AVAILABLE=N  (ex, 1 for ecmwf, possibly 4 for avn)
!  5+1  CYCLE #1 (eg, 0)
!  5+2  CYCLE #1 (eg, 12)
! ....
!  5+N  CYCLE #N
!  6+N OUTPUT FREQUENCY IN HOURS (ex, 3 for meso, 12 for eta, 24 for ecmwf)
!  7+N FORECAST DURATIONIN HOURS (ex, 36 (not 33) for meso, 72 for ecmwf)
!  ...
!  ...REPEAT LINES 2-7+N FOR ALL MODELS YOU WANT TO VERIFY
!  ...
! LAST  'done'
!
! USAGE: 
! 
!   INPUT FILE: 
!      
!     UNIT 05 -    : CONTROL INPUT FOR RUNNING THE CODE 
!     UNIT 11 -    : PRECIP ANALYSIS IN GRIB2  
!     UNIT 13 -    : REGIONAL MASKS FOR THIS GRID IN BINARY 
!     UNIT 15 -    : OBS PRECIP ANALYSIS IN ASCII
!     UNIT 17 -    : MODEL INFO (READ IN GRID NUM, NUM OF START TIMES 
!                    MODEL START TIME CYCLES, OUTPUT FREQUENCY, FORECAST 
!                    DURATION, ETC.)IN ASCII 
!     UNIT 21 -    : PRECIP FORECAST IN GRIB2  
! 
!   OUTPUT FILE:  
!     UNIT 61 -    : INTERPOLATED MASKS AND OBS PRECIP ANALYSIS IN BINARY 
!     UNIT 82 -    : STATS OUTPUT IN ASCII 
! 
! PROGRAMS CALLED: 
!    
!   BAOPENR          GRIB I/O 
!   BACLOSE          GRIB I/O 
!   GETGB2           GRIB2 READER 
!   PUTGB2           GRIB2 WRITER 
!   GF_FREE          FREE UP MEMORY FOR GRIB2  
!   INIT_PARM        DEFINE GRID DEFINITION AND PRODUCT DEFINITION 
!   PRINTINFR        PRINT GRIB2 DATA INFORMATION 
!   MAKGDS           MAKE OR BREAK A GRID DESCRIPTION SECTION   
!   ANNATV           DO A BOX AVERAGE ANALYSIS TO THE GRID DEFINED BY KGDS
!   IPOLATES         INTERPOLATE SCALAR FIELDS FROM ANY GRID TO ANY GRID
!   VRFY             PERFORM PRECIP VERIFICATION 
! 
! ATTRIBUTES: 
!   LANGUAGE: FORTRAN 90 
! 
!$$$ 

      use grib_mod 
      use params

!     parameter(ji=445*365,jo=445*365,numreg=10,numthr=12) 
      parameter(ji=445*365,jo=445*365,numreg=10,numthr=9) 

      integer IPOPT(20)
      integer kpds1(25) 
      integer jpds(25),jgds(22) 
      integer KGDSF2(22),kpdso(25),KGDSO(25),kgds80(22)
      integer ICYC(8),mnth(12),jstat(32),mlana(ji)
      integer maskreg(jo,numreg),MASKNAT(ji),mask80(ji)

      logical*1 lfco(jo),LNAT(ji),FIRCT
      logical*1 l80(ji),lfc80(ji),LPCPDA

      integer :: igdsa(5)=(/0,0,0,0,0/) 
      integer :: kpdsa(25),kgdsa(22),kproba(2)

      integer :: igdsf(5)=(/0,0,0,0,0/)  
      integer :: kpdsf(25),kgdsf(22),kprobf(2) 

      integer ipd1,ipd2,ipd10,ipd11,ipd12 
      integer,dimension(200) :: jids,jpdt,jgdt,iids,ipdt,igdt 
      common /param/jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt 

      type(gribfield) :: gfld,gfldo 
      integer :: currlen=0 
      logical :: unpack=.true. 
      logical :: expand=.false. 

      logical :: first=.true. 
! VAIABLE: APCP 

      data ipd1 /1/ 
      data ipd2 /8/ 
      data ipd10/1/ 
      data ipd11/0/ 
      data ipd12/0/ 

      real,allocatable :: fgrid(:),fci(:),ana(:) 
      logical*1,allocatable :: lfci(:),lana(:)

      DIMENSION rlat(jo),rlon(jo)
      real*4 smask(jo)
      DIMENSION sumfc(ji),fc2(ji),fco(jo),sum80(ji)
      DIMENSION thresh(numthr),ananat(ji),anan80(ji)
      DIMENSION thresh2(numthr),fcold(ji)

      character gdso(400),FNAME*80,fnamei*80,MDLNAM*10,FMASK(numreg)*4
      character datestr*15,month(12)*3,mdlverf*10
      character datcmd*18,fname2*80,fname2i*80
      CHARACTER*255 CPGBA,CPGBF,CPGIF,PCPDA,CMASK,DMASK,CTMPD

      data thresh/0.2,2.,5.,10.,15.,25.,35.,50.,75./
!     data thresh/.1,.25,1.,2.5,5.,10.,20.,25.,30.,40.,50.,75./
      data thresh2/0.01,0.10,0.25,0.50,0.75,1.0,1.5,2.0,3.0/
      data mnth/31,28,31,30,31,30,31,31,30,31,30,31/
      data month/'jan','feb','mar','apr','may','jun',&
                 'jul','aug','sep','oct','nov','dec'/
      data FMASK/ 'Cusa','Appl','Npln','Spln',&
                 'East','Gulf','West','Rkys','Midw','Fntr'/
      data kgds80/201,12902,1,182,210113,136,92,141,577,538,64,&
                  0,0,0,0,0,0,0,0,255,0,0/
!
!  ASSIGN THE UNITS
      KGDSF  =  0
      KPDSF  =  0
      LUMASK = 13
      LPCDAT = 15
      MDLINF = 17
      LUNOUT = 51
      LOANA  = 61
      LSTOUT = 82

!
!  SET UP READING CART DATA FORMAT
 80   FORMAT(A80)
 88   FORMAT(A10)
 98   FORMAT(A40)
 8810 FORMAT(8A10)

!
!  ASSIGN AND OPEN STATS OUTPUT FILE 
      OPEN (UNIT=LSTOUT,FILE='stat.out',FORM='FORMATTED')
      OPEN (UNIT=LOANA,FILE='obs_box.dat',FORM='UNFORMATTED',&
            STATUS='new')

!
!  READ IN MODEL INFO FILE NAME AND OPENED
      READ   (5,80) FNAME
      WRITE  (6,*) 'MODEL  INFORMATION FILE: ',FNAME(1:40)
      CLOSE  (MDLINF)
      OPEN   (UNIT=MDLINF,FILE=FNAME,FORM='FORMATTED')
 99   REWIND (MDLINF)

!
!  READ IN CONUS and US REGIONAL DATA FILE                   
      READ   (5,80,END=9000) CMASK 
      WRITE  (6,*) 'CON US AND REGIONAL MASK: ',CMASK(1:40) 
      LCMASK=LEN_TRIM(CMASK)

!
!  READ IN FOR TEMP DIRECTORY                                
      READ   (5,80,END=9000) CTMPD 
      WRITE  (6,*) 'FOR TEMP DIRECTORY: ',CTMPD(1:40) 
      LCTMPD=LEN_TRIM(CTMPD)

!
!  READ IN GRIB ANALYSIS PRECIPITATION FILE NAME AND OPENED
      READ   (5,80,END=9000) CPGBA  
      WRITE  (6,*) 'GRIB PRECIPITATION FILE: ',CPGBA(1:40) 
      LPGB=LEN_TRIM(CPGBA)
      CALL BAOPENR(11,CPGBA(1:LPGB),IER11)
      IERRS = IER11
      IF (IERRS.NE.0) THEN
       WRITE(6,*) 'GRIB:BAOPEN ERR FOR DATA ',CPGBA            
       WRITE(6,*) 'PLEASE CHECK DATA AVAILABLE OR NOT !!!'        
       GOTO 9000 
      ENDIF

!
!  READ IN OBSERVATION PRECIPITATION DATA FILE NAME AND OPENED
      READ   (5,80) PCPDA
      WRITE  (6,*) 'OBS  PRECIPITATION FILE: ',PCPDA(1:40)
      IF (PCPDA(1:4).NE.'NONE') THEN
       LPCPDA=.TRUE.
       CLOSE(LPCDAT)
       OPEN(UNIT=LPCDAT,FILE=PCPDA,FORM='FORMATTED')
      ELSE
       LPCPDA=.FALSE.
      ENDIF
!
!  READ IN MRF FACTOR FOR BIAS CORECTION ( Default:1.0 )
      READ   (5,*) FMRF
      WRITE  (6,*) 'MRF FACTER IS ',FMRF
!
!  READ IN PRECIP ANALYSIS AND FORECAST ( GRIB FORMAT )
!  Find GRIB MESSAGE 

      iids=-9999;ipdt=-9999; igdt=-9999 
      idisc=-1;  ipdtn=-1;   igdtn=-1 
      call init_parm(ipdtn,ipdt,igdtn,igdt,idisc,iids) 
      call getgb2(11,0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,& 
                  unpack,jskp,gfld,iret) 
      if (iret.eq.0) then      
      call printinfr(gfld,1)
      maxgrd=gfld%ngrdpts  
     allocate (fgrid(maxgrd),fci(maxgrd),ana(maxgrd)) 
     allocate (lfci(maxgrd),lana(maxgrd)) 

         ! 
         !   Construct GDS 
         ! 
         igdsa(1)=gfld%griddef 
         igdsa(2)=gfld%ngrdpts 
         igdsa(3)=gfld%numoct_opt 
         igdsa(4)=gfld%interp_opt 
         igdsa(5)=gfld%igdtnum 
        if ( .NOT. associated(gfld%list_opt) )& 
                 allocate(gfld%list_opt(1))
         call gdt2gds(igdsa,gfld%igdtmpl,gfld%num_opt,gfld%list_opt,& 
                     kgdsa,igrid,iret) 
         if (iret.ne.0) then 
           print *,'cnv21: could not create gds' 
         endif 
         print *,' SAGT: NCEP GRID: ',igrid
         KSTHR=gfld%idsect(9) 
         KEDHR=gfld%ipdtmpl(9)+gfld%ipdtmpl(29)*gfld%ipdtmpl(30) 
      else 
       write(6,*) 'GETGB PROBLEM FOR ANALYSIS : IRET=',iret 
       goto 9000 
      endif 

      WRITE(6,*) ' MAKING ANALYSIS ON GRID 211 '
      CALL MAKGDS(211,KGDSA,GDSO,LENGDS,IRET)
      KPDSA(3)=igrid
      IMA = KGDSA(2)
      JMA = KGDSA(3)  

      REWIND(LPCDAT)
      CALL ANNATV(LPCDAT,IMA,JMA,211,KPDSA,KGDSA,ANA,MLANA)

      DO J=1,IMA*JMA
       LANA(J)=.FALSE.
       IF (MLANA(J).EQ.1) LANA(J)=.TRUE.
      ENDDO

      IPOPT=0
!
      WRITE(6,*) ' KGDSA-211 = ',KGDSA
      IG    = KPDSA(3)
      IMA   = KGDSA(2)
      JMA   = KGDSA(3)
      JSIZE = JMA*IMA
      IF (KGDSA(1).EQ.3) KGDSA(14)=KGDSA(12)

!
!  READ IN REGIONAL MASKS FOR THIS GRID
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
!--------+---------+---------+---------+---------+---------+---------+---------+
!881   FORMAT('/nfsuser/g01/wx20yz/rvrfy/data/pcpmask',i3.3,'.',a4)
 881   FORMAT(i3.3,'.',a4)
 886   FORMAT('  Irec  pds5 pds6 pds7 pds8 pds9 pd10 pd11 pd14',&
              '  ndata  Minimun    Maximum')
 888   FORMAT (i4,2x,8i5,i8,2g12.4)


!
!  SET UP DATES FOR VERIFICATION VALID TIME
!      write (6,*) 'YEAR MN DY HR  =', (gfld%idsect(i),i=6,9) 
      write (6,*)  'gfld%ipdtmpl(9)=',gfld%ipdtmpl(9),& 
                   'gfld%idsect(9)=', gfld%idsect(9),&
                   'gfld%ipdtmpl(30)=',gfld%ipdtmpl(30) 
      iacc = gfld%ipdtmpl(30)*12 
      ivyr = gfld%idsect(6) 
      ivmn = gfld%idsect(7) 
      if (mod(ivyr,100).ne.0.and.mod(ivyr,4).eq.0) mnth(2)=29 
      if (mod(ivyr,400).eq.0) mnth(2)=29 
      ivda = gfld%idsect(8) - 1 
      ivhr = gfld%idsect(9) + gfld%ipdtmpl(22) + &
             gfld%ipdtmpl(29)*gfld%ipdtmpl(30) 
      do while (ivhr.gt.23) 
      ivda = ivda + 1  
      ivhr = ivhr - 24 
      enddo 
      if (ivda.gt.mnth(ivmn)) then 
       ivda = ivda - mnth (ivmn) 
       ivmn = ivmn + 1 
       endif  
       if (ivmn.gt.12) then 
       ivmn = 1 
       ivyr = ivyr + 1 
       endif 
      write(6,*) 'VERIFICATION DATE: ',ivyr,ivmn,ivda,ivhr
       call gf_free(gfld) 
       call baclose(11,ier11)
!
!  MAIN LOOP, MAIN LOOP, MAIN LOOP
!  LOOP THROUGH MODELS TO VERIFY
!
!  READ MODEL NAME  
!
      READ(MDLINF,88,END=99) MDLNAM
      DO WHILE (MDLNAM.NE.'done      ')
       FIRST=.TRUE. 
       FIRCT=.TRUE.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C      READ IN GRID NUM          - NGRID        C
!C      NUM OF START TIMES        - NSTART       C
!C      MODEL START TIME CYCLES   - ICYC         C
!C      OUTPUT FREQUENCY          - IFREQ        C
!C      FORECAST DURATION         - IFDUR        C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       READ(MDLINF,*) NGRID
       READ(MDLINF,*) NSTART
       DO N=1,NSTART
        READ(MDLINF,*) ICYC(N)
       ENDDO
       READ(MDLINF,*) IFREQ
       READ(MDLINF,*) IFDUR
!
!MEB PROBABLY WANT TO CHANGE THIS
!
       KDAT = INDEX(MDLNAM,' ') -1
       IF (KDAT.LE.0) KDAT = LEN(MDLNAM)

!  SET UP DATES FOR FORECAST START TIME, GIVEN VALID TIME
!
       INUMF  = IACC/IFREQ
       IFREQ2 = IFREQ
       IF (MDLNAM.EQ.'dwd       ') IFREQ2 = 12
       IF (MDLNAM.EQ.'ecmwf     ') INUMF  = 2
!
!  GET THE FORECASTS THAT ARE VALID AT THE ANALYSIS DATE/TIME
!  AND COVER THE SAME ACCUMULATION PERIOD AS THE ANALYSIS
!
       DO KTIME=IACC,IFDUR,IFREQ2
        ismn=ivmn
        isyr=ivyr
        isda=ivda
!       ishr=ivhr-ktime - ivhr
!       ishr=ivhr-ktime
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
 
!  LOOP OVER FORECAST START CYCLEs
!
        DO n=1,nstart
         if (ishr.eq.ICYC(n)) THEN
          fc2   = 0.
          sumfc = 0.
          IFACC = 0
!
! loop over the number of forecasts we need to get an iacc h accum
!
          do kn=1,inumf
           ifhr=ktime + ivhr - KSTHR - (kn-1)*ifreq
!          ifhr=ktime + ivhr - (kn-1)*ifreq
           ifhr1=ifhr - ifreq
           if (MDLNAM.eq.'ecmwf     ') THEN
            ifhr1=ifhr
            ifhr=0
           ENDIF
           CLOSE(21)
           CLOSE(22)
!CC Modified by Yuejian Zhu (12/14/98)
           if (ifhr1.lt.100.and.ifhr.lt.100) THEN
!           WRITE (datcmd,120) mod(isyr,100),ismn,isda,ishr,ifhr1,ifhr
            WRITE (datcmd,120) isyr,ismn,isda,ishr,ifhr1,ifhr
           elseif (ifhr1.lt.100.and.ifhr.ge.100) THEN
!           WRITE (datcmd,121) mod(isyr,100),ismn,isda,ishr,ifhr1,ifhr
            WRITE (datcmd,121) isyr,ismn,isda,ishr,ifhr1,ifhr
           else
!           WRITE (datcmd,122) mod(isyr,100),ismn,isda,ishr,ifhr1,ifhr
            WRITE (datcmd,122) isyr,ismn,isda,ishr,ifhr1,ifhr
           ENDIF
           IF ((MDLNAM.eq.'meso      ').and.&
              (kn.lt.inumf.or.ishr.eq.0)) THEN
            WRITE (datcmd,120) mod(isyr,100),ismn,isda,&
                   ishr+3,ifhr1-3,ifhr-3
           ENDIF
 120       format(i4.4,3i2.2,'_',i2.2,'_',i2.2)
 121       format(i4.4,3i2.2,'_',i2.2,'_',i3.3)
 122       format(i4.4,3i2.2,'_',i3.3,'_',i3.3)

!          CPGBF='/ptmp/wx20yz/' // MDLNAM(1:KDAT) // '/' //
           CPGBF=CTMPD(1:LCTMPD) // '/' //&
              MDLNAM(1:KDAT) // '_' // datcmd 
           LPGB=LEN_TRIM(CPGBF)
             
!
!        CALL FUNCTION STAT TO FIND NUMBER OF BYTES IN FILE
!
           WRITE  (6,*) '==============================================' 
           WRITE  (6,*) 'FORECAST DATA NAME: ',CPGBF(1:60)

           LPGB=LEN_TRIM(CPGBF)
           CALL BAOPENR(21,CPGBF(1:LPGB),IER21)
           IERRS = IER21 
           IF (IERRS.NE.0) THEN
            WRITE(6,*) 'GRIB:BAOPEN ERR FOR DATA ',CPGBF                  
            WRITE(6,*) 'PLEASE CHECK DATA AVAILABLE OR NOT'        
            GOTO 9100 
           ENDIF

!
!  READ IN PRECIP FORECAST, AND SUM UP IF NEEDED
      iids=-9999;ipdt=-9999; igdt=-9999 
      idisc=-1;  ipdtn=-1;   igdtn=-1 

      ipdt(1)=ipd1 
      ipdt(2)=7 
      ipdt(10)=ipd10 
      ipdt(11)=ipd11 
      ipdt(12)=ipd12 

      ipdtn=11; igdtn=-1 
      call init_parm(ipdtn,ipdt,igdtn,igdt,idisc,iids) 
      call getgb2(21,0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,& 
                  unpack,jskp,gfldo,iret) 
      if (iret.eq.0) then 
       fci(1:maxgrd) = gfldo%fld(1:maxgrd)  
!       lfci(1:maxgrd) = gfldo%bmap(1:maxgrd) 
       call printinfr(gfldo,1) 
 
         !  
         !   Construct GDS  
         !  
         igdsf(1)=gfldo%griddef  
         igdsf(2)=gfldo%ngrdpts  
         igdsf(3)=gfldo%numoct_opt  
         igdsf(4)=gfldo%interp_opt  
         igdsf(5)=gfldo%igdtnum  
        if ( .NOT. associated(gfldo%list_opt) )&  
                 allocate(gfldo%list_opt(1)) 
         call gdt2gds(igdsf,gfldo%igdtmpl,gfldo%num_opt,gfldo%list_opt,&  
                     kgdsf,igrid,iret)  
         if (iret.ne.0) then  
           print *,'cnv21: could not create gds'  
         endif  
         print *,' SAGT: NCEP GRID: ',igrid
      else 
       write(6,*) 'GETGB PROBLEM FOR ANALYSIS : IRET=',iret 
       goto 9000 
      endif 

           IF (IPDT(2).EQ.8.OR.IPDT(2).EQ.228 &
              .OR.IPDT(2).EQ.50.OR.IPDT(2).EQ.7) THEN
!
            IF (NGRID.EQ.-1) NGRID=igrid
            IF (igrid.EQ.NGRID) THEN

             IF (IPDT(2).EQ.228) FCTR=1000.
!  ecmwf has units of m and 228 is total forecast accum 24,48,72, etc.
             IF (ipdt(2).eq.8.or.ipdt(2).eq.50) FCTR=1.
!             IF (MOD(KPDSF(14),12).ne.0) KPDSF(14)=KPDSF(14)+256
!             IF (MOD(KPDSF(15),12).ne.0) KPDSF(15)=KPDSF(15)+256
             IF (ipdt(2).eq.7) FCTR=(ifhr-ifhr1)*3600.
          WRITE(6,*) 'FCTR=',FCTR,'ifhr1=',ifhr1,'ifhr=',ifhr
!  global has units of mm/s
!
!  SUM UP ACCUMULATION PRECIPITATION
             LFCI=.TRUE.
             KF=KGDSF(2)*KGDSF(3)
             IF (ipdt(2).EQ.228) THEN
              IF(KN.EQ.1) THEN
               IFACC=gfldo%ipdtmpl(9)
               DO IJ=1,KF
                IF (LFCI(ij).AND.FCI(IJ).GT.0.0) &              
                 SUMFC(IJ)=FCI(IJ)*FCTR*FMRF
               ENDDO
              ELSE
               IFACC=IFACC-gfldo%ipdtmpl(9)
               DO IJ=1,KF
                IF (LFCI(IJ).AND.FCI(IJ).GT.0.0) &
                SUMFC(IJ)=SUMFC(IJ)-FCI(IJ)*FCTR*FMRF
              ENDDO
              ENDIF
             ELSE
              DO IJ=1,KF
               IF (LFCI(IJ).AND.FCI(IJ).GT.0.0) &
                SUMFC(IJ)=SUMFC(IJ)+FCI(IJ)*FCTR*FMRF
              ENDDO
              IFACC=IFACC+gfldo%ipdtmpl(30)*12
             ENDIF
            ENDIF
           ENDIF
          ENDDO

!
!  if we've found enough fcst precip to match the obs acc period...
!
          WRITE (6,*) 'IFACC,IACC=',IFACC,IACC
          IF (IFACC.EQ.IACC) THEN
           IF (LPCPDA) THEN
!
!   DO NATIVE GRID ANALYSIS AND VERIFICATION
!
            IF (FIRST) THEN
             IMF   = KGDSF(2)
             JMF   = KGDSF(3)
             IMJMF = IMF*JMF
             IF (KGDSF(1).EQ.201) THEN
              IMF   = KGDSF(7)
              JMF   = KGDSF(8)
              IMJMF = IMF*JMF-JMF/2
             ENDIF
             IGRIDF = igrid
             IGMDL  = igrid
             REWIND(LPCDAT)
!--------+---------+---------+---------+---------+---------+---------+---------+
             CALL &                                                          
             ANNATV(LPCDAT,IMF,JMF,IGRIDF,KPDS1,KGDSF,ANANAT,MASKNAT)
             III=0
             DO KNA=1,IMJMF
              IF (MASKNAT(KNA).EQ.1) THEN
               LNAT(KNA)=.TRUE.
!              WRITE(*,'(i5,f10.3)') KNA,ANANAT(KNA)
               III=III+1
              ELSE
               LNAT(KNA)=.FALSE.
              ENDIF
             ENDDO
!
!   WRITE OUT THE OBSERVATION ANALYSIS AT EACH GRID POINTS
!
             WRITE(LOANA) (MASKNAT(KNA),KNA=1,10512)
             WRITE(LOANA) (ANANAT(KNA),KNA=1,10512)

             WRITE(6,*) 'NUM. OF PTS IN NAT GRID ANALYSIS = ',III
             IF (MDLNAM.EQ.'eta       '.OR.&
                 MDLNAM.EQ.'para32    '.OR.&
                 MDLNAM.EQ.'meso      ') THEN
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
!            CALL VERF(SUMFC,ANANAT,LFCI,LNAT,MASKNAT,IMJMF,1,12,THRESH,
!    .                 1,IGRIDF,IGMDL,1,FMASK,ISYR,ISMN,ISDA,ISHR,
!    .                 IVYR,IVMN,IVDA,IVHR,KTIME,IACC,MDLNAM,LSTOUT)
             CALL VERF(SUMFC,ANANAT,LFCI,LNAT,MASKNAT,IMJMF,1,8,THRESH2,&
                      2,IGRIDF,IGMDL,1,FMASK,ISYR,ISMN,ISDA,ISHR,&
                      IVYR,IVMN,IVDA,IVHR,KTIME,IACC,MDLNAM,LSTOUT)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C     FCST    - FORECAST GRID                                             C
!C     ANA     - ANALYSIS GRID                                             C
!C     LFC     - BIT MAP FOR FORECAST GRID                                 C
!C     LAN     - BIT MAP FOR ANALYSIS GRID TO DETERMINE VERF DOMAIN        C
!C     MASKREG - INTEGER MAP FOR TO DETERMINE REGIONAL VERF DOMAINS        C
!C     JO      - SIZE OF FCST,ANA,LFC,LAN                                  C
!C     NUMREG  - NUMBER OF REGIONS                                         C
!C     NUMTHR  - NUMBER OF THRESHOLDS                                      C
!C     THRESH  - VERIFICATION THRESHOLDS                                   C
!C     TNAME   - STRING VERSION OF THESHOLDS                               C
!C     IGRID   - GRID NUMBER                                               C
!C     IANATYP - ANALYSIS TYPE (1=NATV, 2=MPCP)                            C
!C     FMASK   - NAMES OF THE REGIONAL MASKS                               C
!C     IYR     - YEAR                                                      C
!C     IMN     - MONTH                                                     C
!C     IDA     - DAY                                                       C
!C     IHR     - HOUR                                                      C
!C     IFHR    - FCST HOUR                                                 C
!C     IACC    - LENGTH OF ACCUMULATION                                    C
!C     MDLNAM  - NAME OF MODEL                                             C
!C     IOUNIT  - UNIT TO WRITE STATS OUT TO                                C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!    if eta or meso, do cetlih and verify on 80km grid
!
!      we don't use this option
!            IF (MDLNAM.eq.'eta       '.or.MDLNAM.eq.'para32    '
!    &          .or.MDLNAM.eq.'meso      ') THEN
!             CALL CETLIH(igmdl,firct,sumfc,sum80,lfci,lfc80)
!             CALL VERF(SUM80,ANAN80,LFC80,L80,MASK80,12902,1,8,THRESH2,
!    &                  2,90,igmdl,1,fmask,isyr,ismn,isda,ishr,
!    &                  ivyr,ivmn,ivda,ivhr,ktime,iacc,MDLNAM,LSTOUT)
!             firct=.false.
!            ENDIF

            ENDIF
           ENDIF
 
!  put this on the filled grid if unfilled
!
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
 
!  INTERPOLATE PRECIP FORECAST TO ANALYSIS GRID ( Default: 211 grid )
!
           call makgds(ig,KGDSO,gdso,lengds,IRET)
           if(IRET.ne.0) call exit(IRET)
           IPOPT(1)=-1
           IPOPT(2)=-1
           CALL IPOLATES(6,IPOPT,KGDSF2,KGDSO,ji,jo,1,1,lfci,fc2,&
                         ko,rlat,rlon,ibo,lfco,fco,IRET)
           if(IRET.ne.0) CALL exit(IRET)
 
           AMAXX=0.
           do ij=1,ko
            if (fco(ij).gt.AMAXX) AMAXX=fco(ij)
           ENDDO
 
!  MAIN VERIFICATION PROGRAM CALL
!
            IGMDL=igrid
            CALL VERF(FCO,ANA,LFCO,LANA,MASKREG,JO,NUMREG,NUMTHR,THRESH,&
                     1,ig,igmdl,2,fmask,isyr,ismn,isda,ishr,&
                     ivyr,ivmn,ivda,ivhr,ktime,iacc,MDLNAM,LSTOUT)
           ENDIF
          ENDIF
         ENDIF
        ENDDO
 9100  CONTINUE
       ENDDO    ! DO KTIME=IACC,IFDUR,IFREQ2
!
!  READ NEXT MODEL NAME  
!
      READ(MDLINF,88,END=99) MDLNAM
      ENDDO    ! DO WHILE (MDLNAM.NE.'done      ')

       deallocate (fgrid,fci,ana) 
       deallocate (lfci,lana)

 9000 STOP
      END


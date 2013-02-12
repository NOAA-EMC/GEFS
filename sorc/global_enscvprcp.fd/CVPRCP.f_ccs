C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: PRCPCV
C   PRGMMR: ZHU              ORG: NP23        DATE: 1999-08-31
C
C ABSTRACT: THIS PROGRAM WILL CONVERT 6 HOURS PERIOD PRECIP.  
C           TO 12/24 HOURS ACCUMULATION, CONVERT PRECIP. RATE
C           TO ACCUMULATION PRECIPITATION AND ETC.
C
C PROGRAM HISTORY LOG:
C   97-03-17   YUEJIAN ZHU (WD20YZ)
C   99-07-27   YUEJIAN ZHU (WX20YZ) MODIFY TO IBM-SP
C   00-10-30   YUEJIAN ZHU (WX20YZ) MODIFY TO MATCH AVN PROCESS
C   00-12-12   YUEJIAN ZHU (WX20YZ) MODIFY TO MATCH T12Z AVN PROCESS
C   03-08-29   YUEJIAN ZHU (WX20YZ) MODIFY TO MATCH NEW IMPLEMENTATION
C   06-02-06   YUEJIAN ZHU (WX20YZ) MODIFY TO MATCH NEW IMPLEMENTATION
C   09-18-06   YUEJIAN ZHU (WX20YZ) MODIFY TO CORRECT THE PROBLEM AFTER
C              FORECAST LEAD TIME EXCEEDING 252 HOURS.
C
C USAGE:
C
C   INPUT FILES:
C     UNIT  11  PRECIPITATION GRIB FILE ( 144*73 )
C     UNIT  21  PRECIPITATION GRIB INDEX FILE
C
C   OUTPUT FILES:
C     UNIT  51  PRECIPITATION GRIB FILE ( 144*73 )
C
C   SUBPROGRAMS CALLED:
C     GETGBE -- W3LIB ROUTINE
C     PUTGBE -- W3LIB ROUTINE
C     CHECK1 -- LOCAL ROUTINE ( included after main program )
C     GRANGE -- LOCAL ROUTINE ( included after main program )
C
C   EXIT STATES:
C     COND =  0 - SUCCESSFUL RUN
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      program prcpcv                                            
      parameter(jf=10512,iensem=16)
      dimension f(jf),ff(jf),pp(jf)
      dimension itind(20)
      dimension jpds(25),jgds(22),jens(5)             
      dimension kpds(25),kgds(22),kens(5)
      dimension jens2(iensem),jens3(iensem)
      character*80 cpgb,cpgi,cpge
      logical*1 lb(jf)
      namelist /namin/ cpgb,cpgi,cpge,ini,ipr,isp,itu
C     ini-intial time
C     ipr-average period
C     isp-avaerage step
C     itu-output time unit
C     data jens2/1,1,2,3,2,3,2,3,2,3,2,3/
C     data jens3/1,2,1,1,2,2,3,3,4,4,5,5/
      data jens2/1,1,3,3,3,3,3,3,3,3,3, 3, 3, 3, 3, 3/
      data jens3/1,2,1,2,3,4,5,6,7,8,9,10,11,12,13,14/
c
      CALL W3TAGB('PRCPCV',2003,0243,0069,'NP23')
c
      read (5,namin,end=1000)
      write(6,namin)
 1000 continue

      itind     = 0
      itind(1)  = 1      ! pds(13) = 1,  unit=hour
      itind(2)  = 24     ! pds(13) = 2,  unit=day
      itind(12) = 12     ! pds(13) = 12, unit=12 hrs
      itind(11) = 6      ! pds(13) = 11, unit=6 hrs
      itind(10) = 3      ! pds(13) = 10, unit=3 hrs
      lpgb=len_trim(cpgb)
      lpgi=len_trim(cpgi)
      lpge=len_trim(cpge)
      print *, cpgb(1:lpgb+1),cpgi(1:lpgi+1),cpge(1:lpge+1)
      call baopenr(11,cpgb(1:lpgb),iretb)
      call baopenr(21,cpgi(1:lpgi),ireti)
      call baopen (51,cpge(1:lpge),irete)

      do jj = 1, iensem ! total ensemble members
       jpds15=0
       do ii = 1, 31   ! need to set up
        print *, "jj=",jj,",ii=",ii
        icnt=0
        ncnt=0
        ff=0.0
        pp=0.0
        f =0.0
        ip2m1=0
        do n = 1, 100
         j=-1
         jpds=-1
         jgds=-1
         jens=-1
         jpds(23) = 2
         jens(2)=jens2(jj)
         jens(3)=jens3(jj)
         if (jj.eq.1) then
          if (n.eq.1) then
           if (ii.le.15) then
            jpds(14)=(ii-1)*12
            jpds(15)=jpds(14)+6
           elseif (ii.gt.15.and.ii.le.21) then
            jpds(14)=(ii-1)*12
            jpds(15)=jpds(14)+12
           else
            jpds(14)=(ii-1)
            jpds(15)=jpds(14)+1
           endif
          else
           if (jpds15.lt.180) then
            jpds(14)=jpds15
            jpds(15)=jpds15+6
           elseif (jpds15.ge.180.and.jpds15.lt.252) then
            jpds(14)=jpds15
            jpds(15)=jpds15+12   
           else
            jpds(14)=jpds15/12
            jpds(15)=jpds15/12+1
           endif
          endif
         else
          if (n.eq.1) then
           if (ii.le.21) then
            jpds(14)=(ii-1)*12
            jpds(15)=jpds(14)+6
           else
            jpds(14)=(ii-1)*2
            jpds(15)=jpds(14)+1
           endif
          else
           if (jpds15.lt.252) then
            jpds(14)=jpds15
            jpds(15)=jpds15+6
           else
            jpds(14)=jpds15/6
            jpds(15)=jpds15/6+1
           endif
          endif
         endif
         call getgbe(11,21,jf,j,jpds,jgds,jens,
     &               kf,k,kpds,kgds,kens,lb,f,iret)
         if(iret.eq.0) then
          icnt = icnt + 1
          call grange(kf,lb,f,dmin,dmax)
          print '(i4,i3,2i3,4i3,i3,2i4,2i2,i6,2g12.4)',
     &           n,(kpds(i),i=5,11),(kpds(i),i=13,15),
     &           kens(2),kens(3),kf,dmin,dmax
          ip1=kpds(14)*itind(kpds(13))
          ip2=kpds(15)*itind(kpds(13))
          if (icnt.eq.1) ip1st=ip1
          ip2m1=ip2m1 + (ip2-ip1)
          do ij=1, 10512
           ff(ij)=ff(ij) + f(ij)
          enddo
          jpds15=kpds(15)*itind(kpds(13))
          if (ip2m1.eq.ipr) goto 101
         else
          ncnt=ncnt+1
          if ( ncnt.le.1 ) then
           print *,' n=',n,' iret=',iret
          endif
         endif
        enddo
 101    continue
        if (ncnt.ne.0) goto 102
c       kpds(13) = 12
c       kpds(14) = jpds15/12 - 2
        kpds(13) = itu
        kpds(14) = jpds15/12 - 24/itind(kpds(13))
        kpds(15) = jpds15/12     
        do ij=1, 10512
         if (ff(ij).lt.0.0000001) then
          ff(ij)=0.0                  
         endif
        enddo
        call grange(kf,lb,ff,dmin,dmax)
        print *, "=== write out ==="
        print '(i4,i3,2i3,4i3,i3,2i4,2i2,i6,2g12.4)',
     &         n,(kpds(i),i=5,11),(kpds(i),i=13,15),
     &         kens(2),kens(3),kf,dmin,dmax
        call putgbe(51,kf,kpds,kgds,kens,lb,ff,iret)
       enddo
 102   continue
      enddo 
c
      call baclose(11,iretb)
      call baclose(21,ireti)
      call baclose(51,irete)

      CALL W3TAGE('PRCPCV')

      stop    
      end
      subroutine grange(n,ld,d,dmin,dmax)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: GRANGE(N,LD,D,DMIN,DMAX)          
C   PRGMMR: YUEJIAN ZHU       ORG:NP23           DATE: 97-03-17
C
C ABSTRACT: THIS SUBROUTINE WILL ALCULATE THE MAXIMUM AND      
C           MINIMUM OF A ARRAY
C
C PROGRAM HISTORY LOG:
C   97-03-17   YUEJIAN ZHU (WD20YZ)
C
C USAGE:
C
C   INPUT ARGUMENTS:
C     N        -- INTEGER              
C     LD(N)    -- LOGICAL OF DIMENSION N
C     D(N)     -- REAL ARRAY OF DIMENSION N
C
C   OUTPUT ARGUMENTS:
C     DMIN     -- REAL NUMBER ( MINIMUM )
C     DMAX     -- REAL NUMBER ( MAXIMUM )
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      logical*1 ld
      dimension ld(n),d(n)
      dmin=1.e40
      dmax=-1.e40
      do i=1,n
        if(ld(i)) then
          dmin=min(dmin,d(i))
          dmax=max(dmax,d(i))
        endif
      enddo
      return
      end
      subroutine check1(kpds,kens,kkf,f1,f2,ictl)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: CHECK1(KPDS,KENS,KKF,F1,F2,ICT)          
C   PRGMMR: YUEJIAN ZHU       ORG:NP23           DATE: 97-03-17
C
C ABSTRACT: THIS SUBROUTINE WILL CHECK THE PRECIPITATION FIELDS
C           ARRANGEMENT AND CONVERT TO 12 HOURS PERIOD, OR 
C           CONVERT PRATE TO ACCUMULATION AMOUNT.
C
C PROGRAM HISTORY LOG:
C   97-03-17   YUEJIAN ZHU (WD20YZ)
C   98-05-13   YUEJIAN ZHU (WD20YZ) -- REMOVE 24 HOURS CONVERSION
C   00-10-30   YUEJIAN ZHU (WX20YZ) -- MODIFIED TO MATCH AVN PROCESS
C   00-12-12   YUEJIAN ZHU (WX20YZ) -- MODIFIED TO MATCH T12Z AVN PROCESS
C
C USAGE:
C   SUBROUTINE CHECK1(KPDS,KENS,KKF,F1,F2,ICT)
C
C   INPUT ARGUMENTS:
C     KPDS(25) -- GRIB PDS MESSGAE
C     KENS(5)  -- GRIB ENSEMBLE MESSAGE 
C     F1(KKF)  -- FIELD OF DIMENSION KKF
C     F2(KKF)  -- FIELD OF DIMENSION KKF
C     KKF      -- INTEGER NUMBER
C
C   OUTPUT ARGUMENTS:
C     F2(KKF)  -- FIELD OF DIMENSION KKF
C     ICTL     -- INTEGER TO CONTROL CONVERSION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      dimension f1(kkf),f2(kkf)
      dimension kpds(25),kens(5)        
ccc
      ictl=1
      if (kpds(13).eq.1)  ifact=1 
      if (kpds(13).eq.11) ifact=6 
      if (kpds(13).eq.12) ifact=12
      
      ifs=kpds(14)*ifact
      ife=kpds(15)*ifact
      idiff=ife-ifs

      if (kens(2).eq.1.and.kens(3).eq.1) then
ccc....
ccc   ###for T12Z forecasting up to 72 hours  ( AVN running )
ccc   ###for T12Z forecasting up to 84 hours  ( AVN running )
ccc   ###for T12Z forecasting up to 120 hours  ( AVN running )
ccc   ###for T12Z forecasting up to 180 hours  ( AVN running )
ccc   ###for GFS forecasting up to 180 hours  ( AVN running )
ccc....
c      if (kpds(14).le.66) then
c      if (kpds(14).le.78) then
c      if (kpds(14).le.114) then
       if (kpds(14).le.174) then
        if (mod(kpds(15),12).eq.0) then
         do i=1,kkf
          f2(i) = f2(i) + f1(i)
          if (f2(i).lt.0.0) then
           f2(i) = 0.0
          endif
         enddo
         kpds(14) = kpds(14) - 6
         kpds(5)  = 61
         kpds(14) = kpds(14)
         kpds(15) = kpds(14)+12 
         kpds(13) = 1
         kpds(16) = 4
        elseif (idiff.ne.12) then
         ictl=0
        endif
       endif
      elseif (idiff.ne.12) then
       ictl=0
      endif
c
      if (kpds(13).eq.1) then
       kpds(13) = 12
       kpds(14) = kpds(14)/12
       kpds(15) = kpds(15)/12
      endif
c     print *, 'diff=',idiff,' ictl=',ictl
c
      return
      end

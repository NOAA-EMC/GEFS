C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: PQPF          PROBABILISTIC QUANTITATIVE SNOW FORECAST
C   PRGMMR: YUEJIAN ZHU       ORG:NP23           DATE: 01-09-21
C
C ABSTRACT: THIS PROGRAM WILL CALCULATE ENSEMBLE BASED 
C           PROBABILISTIC QUANTITATIVE PRECIPITATION FORECAST (PQPF)
C           PROBABILISTIC QUANTITATIVE RAIN FORECAST          (PQRF)
C           PROBABILISTIC QUANTITATIVE SNOW FORECAST          (PQSF)
C           PROBABILISTIC QUANTITATIVE ICE PELLETS FORECAST   (PQIF)
C           PROBABILISTIC QUANTITATIVE FREEZING RAIN FORECAST (PQFF)
C
C PROGRAM HISTORY LOG:
C   01-09-21   YUEJIAN ZHU (WX20YZ)
C   03-05-05   YUEJIAN ZHU: calculate for 6-hour intevals. 
C   02-08-06   YUEJIAN ZHU: Modify for new implementation. 
C
C USAGE:
C
C   INPUT FILES:
C     UNIT  11  PRECIPITATION GRIB FILE ( 144*73 )
C     UNIT  12  CATEGORICAL RAIN          (1=RAIN, 0=NOT) GRIB FILE (144*73)
C     UNIT  13  CATEGORICAL FREEZING RAIN (1=FRAIN,0=NOT) GRIB FILE (144*73)
C     UNIT  14  CATEGORICAL ICE PELLETS   (1=ICE,  0=NOT) GRIB FILE (144*73)
C     UNIT  15  CATEGORICAL SNOW          (1=SNOW, 0=NOT) GRIB FILE (144*73)
C     UNIT  21  PRECIPITATION GRIB INDEX FILE
C     UNIT  22  CATEGORICAL RAIN INDEX    (1=RAIN, 0=NOT) INDEX FILE (144*73)
C     UNIT  23  CATEGORICAL FREEZING RAIN (1=FRAIN,0=NOT) INDEX FILE (144*73)
C     UNIT  24  CATEGORICAL ICE PELLETS   (1=ICE,  0=NOT) INDEX FILE (144*73)
C     UNIT  25  CATEGORICAL SNOW          (1=SNOW, 0=NOT) INDEX FILE (144*73)
C
C   OUTPUT FILES:
C     UNIT  51  PQPF GRIB FILE ( 144*73 )
C     UNIT  52  PQRF GRIB FILE ( 144*73 )
C     UNIT  53  PQFF GRIB FILE ( 144*73 )
C     UNIT  54  PQIF GRIB FILE ( 144*73 )
C     UNIT  55  PQSF GRIB FILE ( 144*73 )
C
C   SUBPROGRAMS CALLED:
C     GETGBE -- W3LIB ROUTINE
C     PUTGBEX-- W3LIB ROUTINE
C     GRANGE -- LOCAL ROUTINE ( included after main program )
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      program pqpf                                              
c     parameter(jf=15000,len=30,mem=23)
c     parameter(jf=15000,len=30,mem=12)
c     parameter(jf=15000,len=64,mem=11,ijd=10512)
      parameter(jf=15000,len=64,mem=15,ijd=10512)
      dimension f(jf),ff(ijd,mem),pp(ijd,mem),ss(ijd,mem),aa(ijd)
      dimension xprob(2),imembr(80)
      dimension rk(10,5),kpds5(5)    
      dimension ipds(25),igds(22),iens(5),iprob(2),iclust(16)             
      dimension jpds(25),jgds(22),jens(5)             
      dimension kpds(25),kgds(22),kens(5)
      dimension kens2(mem),kens3(mem)
      logical   lb(jf)
      character*80 cpgb,cpgi,cpge
      character*80 crain,craini,craino
      character*80 cfrzr,cfrzri,cfrzro
      character*80 cicep,cicepi,cicepo
      character*80 csnow,csnowi,csnowo
      namelist /namin/icyc,cpgb,cpgi,cpge,                   
     2               crain,craini,craino,
     3               cfrzr,cfrzri,cfrzro,
     4               cicep,cicepi,cicepo,
     5               csnow,csnowi,csnowo
      data kens2/1,3,3,3,3,3,3,3,3,3, 3, 3, 3, 3, 3/
      data kens3/2,1,2,3,4,5,6,7,8,9,10,11,12,13,14/
      data rk/0.254,1.00,2.54,5.00,6.35,10.00,12.7,20.0,25.4,50.8,
     2        0.254,1.00,2.54,5.00,6.35,10.00,12.7,20.0,25.4,50.8,
     3        0.254,1.00,2.54,5.00,6.35,10.00,12.7,20.0,25.4,50.8,
     4        0.254,1.00,2.54,5.00,6.35,10.00,12.7,20.0,25.4,50.8,
     5        0.254,1.00,2.54,5.00,6.35,10.00,12.7,20.0,25.4,50.8/
      data kpds5/59,140,141,142,143/        
ccc
      CALL W3TAGB('PQPF',2000,0110,0073,'NP20   ')

      read (5,namin)
      lpgb   = len_trim(cpgb)
      lpgi   = len_trim(cpgi)
      lpge   = len_trim(cpge)
      lrain  = len_trim(crain)
      lraini = len_trim(craini)
      lraino = len_trim(craino)
      lfrzr  = len_trim(cfrzr)
      lfrzri = len_trim(cfrzri)
      lfrzro = len_trim(cfrzro)
      licep  = len_trim(cicep)
      licepi = len_trim(cicepi)
      licepo = len_trim(cicepo)
      lsnow  = len_trim(csnow)
      lsnowi = len_trim(csnowi)
      lsnowo = len_trim(csnowo)
      print *, cpgb(1:lpgb)
      print *, cpgi(1:lpgi)
      print *, cpge(1:lpge)
      call baopenr(11,cpgb(1:lpgb),iretb)
      call baopenr(21,cpgi(1:lpgi),ireti)
      call baopen (51,cpge(1:lpge),irete)

      print *, crain(1:lrain)
      print *, craini(1:lraini)
      print *, craino(1:lraino)
      call baopenr(12,crain(1:lrain),irerb)
      call baopenr(22,craini(1:lraini),ireri)
      call baopen (52,craino(1:lraino),irere)

      print *, cfrzr(1:lfrzr)
      print *, cfrzri(1:lfrzri)
      print *, cfrzro(1:lfrzro)
      call baopenr(13,cfrzr(1:lfrzr),irefb)
      call baopenr(23,cfrzri(1:lfrzri),irefi)
      call baopen (53,cfrzro(1:lfrzro),irefe)

      print *, cicep(1:licep)
      print *, cicepi(1:licepi)
      print *, cicepo(1:licepo)
      call baopenr(14,cicep(1:licep),ireib)
      call baopenr(24,cicepi(1:licepi),ireii)
      call baopen (54,cicepo(1:licepo),ireie)

      print *, csnow(1:lsnow)
      print *, csnowi(1:lsnowi)
      print *, csnowo(1:lsnowo)
      call baopenr(15,csnow(1:lsnow),iresb)
      call baopenr(25,csnowi(1:lsnowi),iresi)
      call baopen (55,csnowo(1:lsnowo),irese)

      ncnt=0
      iprob=0
      xprob=0.0
      iclust=0
      imembr=0

      do n = 1, len         !### 16 (days) * 4 = 64 (6-hr)        
       do l = 1, 5          !### 5 categorical
ccc
CCC Part I: get ctl + 10  ensemble members precipitation data
ccc
        icnt=0
        pp=0.0
        imems=1
        imem=15
        do m = imems, imem
         j=n-1
         jpds=-1
         jgds=-1
         jens=-1
         jpds(23)=2
         jpds(11)=icyc
CCC
CCC  PDS(13) --> time unit
CCC  PDS(14) --> start hours
CCC  PDS(15) --> end hours
CCC  There is time unit change:
CCC      hour <= 252  time unit=1, (hour)
CCC      hour >  252  time unit=12, (12-hour)
CCC      hour >  252  time unit=11, (6-hour)
CCC      -- Yuejian Zhu (05/05/2003)
CCC
C        if (l.ne.1.and.n.le.20) then
         if (n.le.42) then
          jpds(13)=1
          jpds(14)=(n-1)*6
          jpds(15)=(n+0)*6
         else 
          jpds(13)=11
          jpds(14)=n-1
          jpds(15)=n+0
         endif
         jens(2)=kens2(m)
         jens(3)=kens3(m)
         call getgbe(10+l,20+l,jf,j,jpds,jgds,jens,
     &               kf,k,kpds,kgds,kens,lb,f,iret)
         if(iret.eq.0) then
          icnt=icnt + 1
          call grange(kf,lb,f,dmin,dmax)
          print '(i4,i3,2i5,4i3,i4,4i2,i4,i7,2g12.4)',
     &     n,(kpds(i),i=5,11),kpds(14),kens,kf,dmin,dmax
c
          do ii=1,ijd   
           if (l.eq.1) then
            ff(ii,icnt)=f(ii)
           else
            pp(ii,icnt)=f(ii)
           endif
          enddo
CCC
CCC  pass PDS, GDS and ENS extended message to output
CCC
          do ii = 1, 25
           ipds(ii)=kpds(ii)
          enddo
          do ii = 1, 22
           igds(ii)=kgds(ii)
          enddo
          do ii = 1, 5 
           iens(ii)=kens(ii)
          enddo 
         else
          ncnt=ncnt+1
          if ( ncnt.le.1 ) then
           print *,' n=',n,' iret=',iret
          endif
         endif
        enddo   !### for m = imems, imem
ccc
CCC PART II: to calculate the probability scores
CCC          icnt is a real read in members of ensemble
CCC          l=1, for tatol precipitation
CCC          l>1, for all categorical precipitation
ccc
        do k = 1, 10
         aa=0.0
         if (l.eq.1) then
          do ii = 1, ijd   
           do mm = 1, icnt
c           if (mod(n,2).eq.0) then
c            bb=ff(ii,mm)-ss(ii,mm)   !### substract to get 6-hour prcp
c           else
             bb=ff(ii,mm)
c           endif
            if (bb.ge.rk(k,l)) then
             aa(ii) = aa(ii) + 1.0
            endif
           enddo
          enddo
          do ii = 1, ijd  
           aa(ii) = aa(ii)*100.0/float(icnt)
           if (aa(ii).ge.99.0) then
            aa(ii) = 100.0
           endif
          enddo
         else
          do ii = 1, ijd   
           do mm = 1, icnt
c           if (mod(n,2).eq.0) then
c            bb=ff(ii,mm)-ss(ii,mm)   !### substract to get 6-hour prcp
c           else
             bb=ff(ii,mm)
c           endif
            cc=pp(ii,mm)
            if (cc.eq.1.0) then
             if (bb.ge.rk(k,l)) then
              aa(ii) = aa(ii) + 1.0
             endif
            endif
           enddo
          enddo
          do ii = 1, ijd  
           aa(ii) = aa(ii)*100.0/float(icnt)
           if (aa(ii).ge.99.0) then
            aa(ii) = 100.0
           endif
          enddo
         endif
CCC 
CCC     testing print
CCC
ccc      1250-1259 (70N, 115W-95W)
c        if (n.eq.1.and.k.eq.2) then 
          write(*,999) l,n,(aa(ii),ii=1250,1259)
c        endif
 999     format (2i3,10f8.1)
 
c        ipds(5)=kpds5(l)    !: OCT 9
         ipds(5)=191         !: OCT 9
c        ipds(13)=12         !: Time unit = 12 hours
         ipds(13)=11         !: Time unit = 6 hours
         ipds(14)=n-1
         ipds(15)=n
         iens(2)=5           !: OCT 42
         iens(3)=0           !: OCT 43
         iens(4)=0           !: OCT 44
         iprob(1)=61         !: OCT 46
         iprob(2)=2          !: OCT 47
         xprob(1)=0.0        !: OCT 48-51
         xprob(2)=rk(k,l)    !: OCT 52-55
         iclust(1)=icnt      !: OCT 61
         call putgbex(50+l,ijd,ipds,igds,iens,iprob,xprob,
     &                iclust,imembr,lb,aa,iret)

        enddo    !### for k = 1, 10
       enddo    !### for l=1,5
c      ss=ff    !### save ff to ss for using of next step
      enddo     !### for n = 1, len

      call baclose(11,iretb)
      call baclose(21,ireti)
      call baclose(51,irete)
      call baclose(12,iretb)
      call baclose(22,ireti)
      call baclose(52,irete)
      call baclose(13,iretb)
      call baclose(23,ireti)
      call baclose(53,irete)
      call baclose(14,iretb)
      call baclose(24,ireti)
      call baclose(54,irete)
      call baclose(15,iretb)
      call baclose(25,ireti)
      call baclose(55,irete)

      CALL W3TAGE('PQPF')

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
      logical ld
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


      program CQPF_BIAS                                        
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC   PQPF - Probabilitistic Quatitative Precipitation Forecast     CCCC
CCC   CPQPF - Calibrated PQPF by using reduced bias method          CCCC
CCC                                                                 CCCC
CCC   This program will read in 23 ensember members and multiply    CCCC
CCC        by the coefficents for different threshold amount        CCCC
CCC        of every individual members. The coefficents will be     CCCC
CCC        two sets: one for MRF high resolution, another is        CCCC
CCC        low resolution control which based on past month/season  CCCC
CCC        statistics.                                              CCCC
CCC                                                                 CCCC
CCC    By using:                                                    CCCC
CCC        STPINT interpolation program                             CCCC
CCC                                                                 CCCC
CCC    Program:  Yuejian Zhu 03/22/2001 IBM-ASP                     CCCC
CCC              Yuejian Zhu 09/25/2001 IBM-ASP modefied            CCCC
CCC              Yuejian Zhu 02/09/2004 IBM-frost implememtation    CCCC
CCC              Yuejian Zhu 02/06/2006 For new configuration       CCCC
CCC                                                                 CCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
ccc
ccc   Parameters:
ccc     1. jpoint ---> model resolution or total grid points ( 10512 )
ccc     2. iensem ---> numbers of ensember members ( 16 )
ccc     3. idays  ---> total numbers of days ( # - 1 )
ccc     4. indexs ---> the index number according global data set
ccc
ccc   Unit:
ccc     10. input table to convert global to region or other way
ccc     12. input statitic numbers for MRF                         
ccc     13. input statitic numbers for T62 (control)              
ccc     11. input GRIB file
ccc     21. input GRIB index file
ccc     51. output bias-free calibrated precipitation forecast, grib format
ccc     52. output bias-free CPQPF, grib format      
C--------+---------+---------+---------+---------+---------+---------+--
      parameter(jpoint=10512,iensem=16,ihdays=31,indexs=264,nsites=677)
      parameter(istd=10)
      dimension fin(iensem),finr(iensem),rainn(800)
      dimension f(jpoint),ff(jpoint,iensem)
      dimension rk(istd)
      dimension rti(9),rob(9),rft(9),rmrf(9),rctl(9)
      dimension rti_r(9),rmrf_r(9),rctl_r(9)
      dimension fhri(2),fhro(2)
      dimension rinc(5),rains(800)
      dimension ipds(25),igds(22),iens(5)             
      dimension jpds(25),jgds(22),jens(5)             
      dimension kpds(25),kgds(22),kens(5)
      dimension xprob(2),imembr(80),iprob(2),iclust(16)
      dimension kens2(iensem),kens3(iensem)
      dimension xmom(5,nsites+1,indexs),len(nsites+1)
      dimension dn(nsites+1),dm(nsites+1),idn(nsites+1)
      dimension idat(8),jdat(8)

      logical*1 lb(jpoint)
      character*80 cindx,cpgrb,cpgri,clmrf,clctl,copts,coptm
      character*80 fnobs(20)                        
      character*10 names(nsites+1)
      namelist /namin/cindx,cpgrb,cpgri,clmrf,clctl,copts,coptm,icyc

      data kens2/1,1,3,3,3,3,3,3,3,3,3, 3, 3, 3, 3, 3/
      data kens3/1,2,1,2,3,4,5,6,7,8,9,10,11,12,13,14/
      data rk/0.254,2.54,6.35,12.7,25.4,50.8,1.00,5.00,10.00,25.00/
      data rti/75.0,50.0,35.0,25.0,15.0,10.0, 5.0, 2.0, 0.2/
 
      read (5,namin)
      write(6,namin)

      lpgb=len_trim(cpgrb)
      lpgi=len_trim(cpgri)
      lpgs=len_trim(copts)
      lpgm=len_trim(coptm)
      call baopenr(11,cpgrb(1:lpgb),iretb)
      call baopenr(21,cpgri(1:lpgi),ireti)
      call baopen(51,copts(1:lpgs),irets)
      call baopen(52,coptm(1:lpgm),iretm)

      open(unit=10,file=cindx,status='old',err=1001)
      open(unit=12,file=clmrf,status='old',err=1002)
      open(unit=13,file=clctl,status='old',err=1003)

      ncnt   = 0
      kctl   = 0

ccc
ccc   Step 1: read in the data on GRIB 2 ( 144*73 ) of global
ccc
      do n = 1, ihdays      ! ihdays = # of half days - 1

       print *, " ***********************************"
       print *, " ***      HALF DAY = ",n,"        ***"
       print *, " ***********************************"

       kctl = 0
       do m = 1, iensem   ! iensem = # of ensemble ( default = 12/11 )
        j    = n-1
        jpds = -1
        jgds = -1
        jens = -1
        jpds(23) = 2
ccc
ccc     12 members for T00Z, 11 member for T12Z cycle          
ccc
        if (icyc.eq.0) then
         jpds(11) = 00
         jpds(13) = 12
         jpds(14) = n - 1
         jpds(15) = n + 1
         jensem   = 16
        else
         jpds(11) = 12
         jpds(13) = 12
         jpds(14) = n - 1
         jpds(15) = n + 1
         jensem   = 16
        endif
        jens(2)   = kens2(m)
        jens(3)   = kens3(m)
        call getgbe(11,21,jpoint,j,jpds,jgds,jens,
     &                kf,k,kpds,kgds,kens,lb,f,iret)
        if (iret.eq.0) then
         call grange(kf,lb,f,dmin,dmax)
         print '(i4,i3,2i5,4i3,i4,i4,4i2,i4,i7,2g12.4)',
     &    n,(kpds(i),i=5,11),kpds(14),kpds(15),kens,kf,dmin,dmax
         if (m.eq.1) then
          if (kpds(8).ne.100) then
           iymd=(kpds(21)-1)*100000000 +
     *          kpds(8)*1000000+kpds(9)*10000+kpds(10)*100+kpds(11)
          else
           iymd=kpds(21)*100000000 +
     *          kpds(9)*10000+kpds(10)*100+kpds(11)
          endif
          ifh1=kpds(14)-12
          ifh2=ifh1 + 24   
         endif
         kctl=kctl+1
         do ii = 1, jpoint
          ff(ii,kctl) = f(ii)
         enddo
ccc
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
         if ( ncnt.ge.1 ) then
          print *,' n=',n,' iret=',iret
         endif
        endif  ! if (iret.eq.0)
       enddo   ! for m = 1, iensem iensem = # of ensemble ( default = 23 )

ccc
ccc    Step 2: get statistical coefficents and multiply by it
ccc    
ccc    for t00z cycle
ccc    n=1, 00-24 hours period
ccc    n=2, 12-36 hours period-good statistics
ccc    n=3, 24-48 hours period
ccc    n=5, 36-60 hours period-good statistics
ccc    n=6, 48-72 hours period
ccc    ......
ccc    for t12z cycle
ccc    n=1, 00-24 hours period-good statistics, but 12-36 hrs lead
ccc    n=2, 12-36 hours period
ccc    n=3, 24-48 hours period-good statistics, but 36-60 hrs lead
ccc    n=5, 36-60 hours period
ccc    n=6, 48-72 hours period
ccc    ......
ccc
       if (mod(n+1,2).eq.0.and.n.ne.31) then
       
       print *, " *** Start to read in statistics: n=",n," ***"
       izero= 0
       isame= 0

       do ii = 1, 7
        read(12,'(1x)')
       enddo
       read(12,994) (rft(ii),ii=9,1,-1)                     
       read(12,994) (rob(ii),ii=9,1,-1)                     
       read(12,'(1x)')
       read(12,'(1x)')
       call cpcoef(rti,rob,rft,rmrf,9,2)

       do ii = 1, 7
        read(13,'(1x)')
       enddo
       read(13,994) (rft(ii),ii=9,1,-1)                     
       read(13,994) (rob(ii),ii=9,1,-1)                     
       read(13,'(1x)')
       read(13,'(1x)')
       call cpcoef(rti,rob,rft,rctl,9,2)

       do ii = 1, 9
        rti_r(ii)  = rti(9-ii+1)
        rmrf_r(ii) = rmrf(9-ii+1)
        rctl_r(ii) = rctl(9-ii+1)
       enddo

       endif ! if (mod(n+1,2).eq.0) then

ccc    print out ratio/coefficences for mrf and ctl
       write (*,'(8x,9f7.2)') (rmrf_r(ii),ii=9,1,-1)
       write (*,'(8x,9f7.2)') (rctl_r(ii),ii=9,1,-1)

ccc    main loop for each grid points ( 264 points )
ccc    =============================================
ccc    the new program didn't use 264 boxes -Y. ZHU (06/30/2003)
ccc    calibration apply to globally.
ccc
ccc      add 2 12 hours forecasts --> to 24 hours accumulation
ccc
       do ii = 1, jpoint
        if (ii.eq.4000) then
         write(*,'("example of first 10 ens value at point 4000")')
         write(*,899) (ff(ii,jj),jj=1,10)
        endif
CCC
CCC     Notes:
CCC     rti_r(n) is thread amount of precipitation
CCC     rtmrf_r(n) is the ratio/coefficences of rti_r(n) for mrf forecast
CCC     rtctl_r(n) is the ratio/coefficences of rti_r(n) for ctl forecast
CCC   
CCC     for stpint:
CCC     input: rti_r(n)
CCC     input: rtmrf_r(n)
CCC     input request: aaa -> single value
CCC     output for aaa: bbb-> single value (ratio value for aaa )
CCC     calibrated precipitation = aaa*bbb
ccc

        do jj = 1, jensem
         aaa = ff(ii,jj)
         if (aaa.eq.0.0) then
          ff(ii,jj) = 0.0
         else
          if (jj.eq.1) then
           call stpint(rti_r,rmrf_r,9,2,aaa,bbb,1,aux,naux)
           ff(ii,jj) = aaa*bbb
          else
           call stpint(rti_r,rctl_r,9,2,aaa,bbb,1,aux,naux)
           ff(ii,jj) = aaa*bbb
          endif
         endif
        enddo

        if (ii.eq.4000) then
         write(*,899) (ff(ii,jj),jj=1,10)
        endif

 899   format(10f7.2)

       enddo     ! for ind loop  / ii loop          

ccc
ccc    write out the results
ccc
       do jj = 1, jensem

        if (icyc.eq.0) then
         jjj=jj
        elseif (icyc.eq.12) then
         jjj=jj+1
         if (jj.eq.1) jjj=jj
        else
         print *, "ICYC=",icyc," is not acceptable! program will stop!"
         stop 16  
        endif
        iens(2)  = kens2(jjj)
        iens(3)  = kens3(jjj)

c
c       we need to set up a lower limit, for example: ff = 0.01 mm
c

        do ii = 1, jpoint
         if (ff(ii,jj).lt.0.01) then
          f(ii) = 0.0
         else
          f(ii) = ff(ii,jj)
         endif
        enddo

c       ipds(5) = 59
        if (icyc.eq.0) then
         ipds(11) = 00
         ipds(14) = n - 1
         ipds(15) = n + 1
        else
         ipds(11) = 12
         ipds(14) = n - 1   
         ipds(15) = n + 1    
        endif
        call putgbe(51,jpoint,ipds,igds,iens,lb,f,iret)
       enddo
ccc
ccc    calculate the CPQPF
ccc
       do k = 1, istd
        f   = 0.0
        do ii = 1, jpoint
ccccc to exclude GFS/AVN high resolution forecast
ccccc    do jj = 1, jensem
         do jj = 2, jensem
          if (ff(ii,jj).ge.rk(k)) then
           f(ii) = f(ii) + 1.0
          endif
         enddo
ccccc    f(ii) = f(ii)*100.00/float(jensem)
         f(ii) = f(ii)*100.00/float(jensem-1)
         if (f(ii).ge.99.0) then
          f(ii) = 100.0
         endif
        enddo
        if (icyc.eq.0) then
         ipds(11) = 00
         ipds(14) = n - 1
         ipds(15) = n + 1
        else
         ipds(11) = 12
         ipds(14) = n - 1
         ipds(15) = n + 1  
        endif
        ipds(5)=191         !: OCT 9
        ipds(13)=12         !: Time unit = 12 hours
        iens(2)=5           !: OCT 42
        iens(3)=0           !: OCT 43
        iens(4)=0           !: OCT 44
        iprob(1)=61         !: OCT 46
        iprob(2)=2          !: OCT 47
        xprob(1)=0.0        !: OCT 48-51
        xprob(2)=rk(k)      !: OCT 52-55
        iclust(1)=icnt      !: OCT 61
        call putgbex(52,jpoint,ipds,igds,iens,iprob,xprob,
     &               iclust,imembr,lb,f,iret)
       enddo
      enddo     ! for n loop
 990  format (69x)
 991  format (6x,i6,57x)
 994  format(5x,9f7.0)                                  
 995  format(i4,2x,a8,2x,2f8.4,i4,f6.2,4f10.4)
c995  format(i4,2x,f8.4,5f10.3)
 996  format(17f5.1)
 997  format('-- ',i2,' numbers mean = ',f10.4,' --')
 998  format(i4,i3,f5.2,800f5.2)
 999  format(3f6.1,10f5.1)

      call baclose (11,iretb)
      call baclose (21,ireti)

      close (10)
      close (12)
      close (52)
      close (53)

      stop
  800 print *, ' end of reading file - past archive'
      stop
 1001 print *, ' there is a problem to open unit 10'
      stop
 1002 print *, ' there is a problem to open unit 12'
      stop
 1003 print *, ' there is a problem to open unit 13'
      stop
 1004 print *, ' there is a problem to open unit 52'
      stop    
 1005 print *, ' there is a problem to open unit 30-49'
      stop    
      end
C--------+---------+---------+---------+---------+---------+---------+---------+
      subroutine grange(n,ld,d,dmin,dmax)
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

      subroutine CPCOEF(x,y,z,r,n,ictl)
C
C     This program will calculate the precipitation calibration
C     coefficence by using statistical distributions
C
C     Program: IBM-ASP     By: Yuejian Zhu ( 03/22/2001 )
C              Modified    By: Yuejian Zhu ( 03/21/2004 )
C
C     Input:    x(n)-vector for preciptation threshold amount
C               y(n)-vector for observation numbers at x(n)
C               z(n)-vector for forecasting numbers at x(n)
C               n   -length of the vector
C               ictl-to control the interpolation bases
C                    1 - standard
C                    2 - logrithm
C     Output:   r(n)-coefficents/ratio of each threshold amount x(n)
C               which will apply to particular precipitation amount
C               multiply the ratio at this point (linear interpolation)
C
      dimension x(n),y(n),z(n),r(n),a(n),b(n)             
      dimension rnti(n-2),rnto(n-2)

C     Safty check of x-axis (dimension y)
C      Repeating to confirm the x-axis is ascending
      do i = 1, n-1
       do j = 1, n-2
        if (y(i).eq.y(i+1)) then
         y(i) = y(i) + 1.0  
        endif
       enddo
      enddo

      if ( ictl.eq.1) then

C
C   input: y(n) as abscissas (x-axis)
C   input: x(n) as ordinates (y-axis) 
C   input: z(n) as request abscissas values ( x-axis )
C   output: b(n) as cooresponding values of z(n)
C           similar to the thread amount, but shifted

       call stpint(y,x,n,2,z,r,n,aux,naux)

       write(*,991) (y(i)/1000.0,i=1,n)
       write(*,993) (x(i),i=1,n)
       write(*,992) (z(i)/1000.0,i=1,n)
       write(*,994) (r(i),i=1,n)
       write(*,995) (r(i)/x(i),i=1,n)
  
c      do i = 1, n-2
c       rnti(i) = x(n-i)
c       rnto(i) = r(n-i)/x(n-i)
c      enddo
c      call stpint(rnti,rnto,n-2,2,x,r,n,aux,naux)
c      write(*,993) (r(i),i=1,n)
c      if (r(n).lt.0.0.and.r(n-1).gt.0.0) then
c       r(n) = r(n-1)*r(n-2)
c      endif
c      write(*,993) (r(i),i=1,n)

      else
C
C  Tested both of log and log10, log is better
C
C   input: y(n) as abscissas (x-axis)
C   input: x(n) as ordinates (y-axis) -- using logrithem a(n) instead
C   input: z(n) as request abscissas values ( x-axis )
C   output: b(n) as cooresponding values of z(n)
C           similar to the thread amount, but shifted

       do i = 1, n
        a(i) = alog(x(i))
c       a(i) = log10(x(i))
       enddo

       call stpint(y,a,n,2,z,b,n,aux,naux)

       do i = 1, n
        r(i) = exp(b(i))
c       r(i) = exp(b(i)*log(10.0))
        if (r(i).gt.100.0.or.r(i).le.100.0) then
        else
         print *, "i=",i,"  r(i)=",r(i)," problem, use default"
         r(i) = x(i)
        endif
       enddo

       write(*,991) (y(i)/1000.0,i=1,n)
       write(*,993) (x(i),i=1,n)
       write(*,992) (z(i)/1000.0,i=1,n)
       write(*,994) (r(i),i=1,n)
       write(*,995) (r(i)/x(i),i=1,n)
   
c      do i = 1, n-2
c       rnti(i) = x(n-i)
c       rnto(i) = exp(b(n-i))/x(n-i)
c       rnto(i) = exp(b(n-i)*log(10.0))/x(n-i)
c      enddo
c      call stpint(rnti,rnto,n-2,2,x,r,n,aux,naux)
c      write(*,993) (r(i),i=1,n)
c      if (r(n).lt.0.0.and.r(n-1).gt.0.0) then
c       r(n) = r(n-1)*r(n-2)
c      endif
c      write(*,993) (r(i),i=1,n)
      endif

      do i = 1, n
       r(i) = r(i)/x(i)
      enddo

 991  format ('input = ',9f7.2,' OBS/1000')
 992  format ('input = ',9f7.2,' FST/1000')
 993  format ('input = ',9f7.2,' thrd    ')
 994  format ('output= ',9f7.2,' thrd_FST')
 995  format ('ratio = ',9f7.2,' tFST/thrd')
      return
      end

C           ==== using p50 results ====
Cinput =   75.00  50.00  35.00  25.00  15.00  10.00   5.00   2.00    .20
Coutput=   78.57  78.57  57.14  30.83  15.71   9.86   4.41   1.07  -3.19
Cratio =    1.05   1.57   1.63   1.23   1.05    .99    .88    .54 -15.95

C           ==== using t62 results ====
Cinput =   75.00  50.00  35.00  25.00  15.00  10.00   5.00   2.00    .20
Coutput=   78.57  67.86  41.32  25.83  14.36   9.27   4.17   1.19  -2.42
Cratio =    1.05   1.36   1.18   1.03    .96    .93    .83    .59 -12.10
Cratio =    1.65   1.36   1.18   1.03    .96    .93    .83    .59    .45



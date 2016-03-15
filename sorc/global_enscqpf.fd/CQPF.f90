      program CQPF_BIAS                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCC   PQPF - Probabilitistic Quatitative Precipitation Forecast     CCCC
!CCC   CPQPF - Calibrated PQPF by using reduced bias method          CCCC
!CCC                                                                 CCCC
!CCC   This program will read in 23 ensember members and multiply    CCCC
!CCC        by the coefficents for different threshold amount        CCCC
!CCC        of every individual members. The coefficents will be     CCCC
!CCC        two sets: one for MRF high resolution, another is        CCCC
!CCC        low resolution control which based on past month/season  CCCC
!CCC        statistics.                                              CCCC
!CCC                                                                 CCCC
!CCC    By using:                                                    CCCC
!CCC        QTPINT interpolation program                             CCCC
!CCC                                                                 CCCC
!CCC    Program:  Yuejian Zhu 03/22/2001 IBM-ASP                     CCCC
!CCC              Yuejian Zhu 09/25/2001 IBM-ASP modefied            CCCC
!CCC              Yuejian Zhu 02/09/2004 IBM-frost implememtation    CCCC
!CCC              Yuejian Zhu 02/06/2006 For new configuration       CCCC
!CCC              Yan Luo     11/12/2014 For grib2 conversion        CCCC
!CCC                                                                 CCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!ccc
!ccc   Parameters:
!ccc     1. jpoint ---> model resolution or total grid points ( 10512 )
!ccc     2. iensem ---> numbers of ensember members ( 16 )
!ccc     3. idays  ---> total numbers of days ( # - 1 )
!ccc     4. indexs ---> the index number according global data set
!ccc
!ccc   Unit:
!ccc     10. input table to convert global to region or other way
!ccc     12. input statitic numbers for MRF                         
!ccc     13. input statitic numbers for T62 (control)              
!ccc     11. input GRIB file
!ccc     51. output bias-free calibrated precipitation forecast, grib format
!ccc     52. output bias-free CPQPF, grib format      
!C--------+---------+---------+---------+---------+---------+---------+--

use grib_mod 
use params

      parameter(jpoint=10512,iensem=22,ihdays=31,indexs=264,nsites=677)
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
      integer e16(iensem),e17(iensem) 
      integer ee16(iensem-1),ee17(iensem-1)
      integer temp(200)

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
      data e16/0,1,3,3,3,3,3,3,3,3,3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3/
      data e17/0,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20/
      data ee16/1,3,3,3,3,3,3,3,3,3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3/
      data ee17/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20/

      character*100 cindx,cpgrb,clmrf,clctl,copts,coptm
      character*80 fnobs(20)                        
      character*10 names(nsites+1)
      namelist /namin/cindx,cpgrb,clmrf,clctl,copts,coptm,icyc

      data rk/0.254,2.54,6.35,12.7,25.4,50.8,1.00,5.00,10.00,25.00/
      data rti/75.0,50.0,35.0,25.0,15.0,10.0, 5.0, 2.0, 0.2/
 
      read (5,namin)
      write(6,namin)

      lpgb=len_trim(cpgrb)
      lpgs=len_trim(copts)
      lpgm=len_trim(coptm)
      call baopenr(11,cpgrb(1:lpgb),iretb)
      call baopenw(51,copts(1:lpgs),irets)
      call baopenw(52,coptm(1:lpgm),iretm)

      open(unit=10,file=cindx,status='old',err=1001)
      open(unit=12,file=clmrf,status='old',err=1002)
      open(unit=13,file=clctl,status='old',err=1003)

      ncnt   = 0
      kctl   = 0

!cc
!cc   Step 1: read in the data on GRIB 2 ( 144*73 ) of global
!cc
      do n = 1, ihdays      ! ihdays = # of half days - 1

       print *, " ***********************************"
       print *, " ***      HALF DAY = ",n,"        ***"
       print *, " ***********************************"

       kctl = 0
      if (n.le.15) then
       jensem=iensem
      else
       jensem=iensem-1
      endif
       do m = 1, jensem   ! jensem = # of ensemble ( default = 12/11 )
         iids=-9999;ipdt=-9999; igdt=-9999 
         idisc=-1;  ipdtn=-1;   igdtn=-1 
         ipdt(1)=ipd1 
         ipdt(2)=ipd2 
         ipdt(10)=ipd10 
         ipdt(11)=ipd11 
         ipdt(12)=ipd12 
!cc
!cc     12 members for T00Z, 11 member for T12Z cycle          
!cc
        if (icyc.eq.0) then
         iids(9) = 00
         ipdt(8) =  12
         ipdt(9) = n - 1
         ipdt(29) = 12
         ipdt(30) = 2
         if (n.le.15) then
          ipdt(16)=e16(m)
          ipdt(17)=e17(m)
         else
          ipdt(16)=ee16(m)
          ipdt(17)=ee17(m)     
         endif   
        else
         iids(9) = 12
         ipdt(8) =  12
         ipdt(9) = n - 1 
         ipdt(29) = 12
         ipdt(30) = 2
         if (n.le.15) then 
          ipdt(16)=e16(m)
          ipdt(17)=e17(m)
         else 
          ipdt(16)=ee16(m)
          ipdt(17)=ee17(m)
         endif 
        endif
         ipdtn=11; igdtn=-1 
         call init_parm(ipdtn,ipdt,igdtn,igdt,idisc,iids) 
         call getgb2(11,0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,& 
                  unpack,jskp,gfld,iret) 
        if (iret.eq.0) then
         f(1:jpoint) = gfld%fld(1:jpoint)
         call printinfr(gfld,m)
         if (m.eq.1) then
          iymd=gfld%idsect(6)*1000000+gfld%idsect(7)*10000+ &
               gfld%idsect(8)*100+gfld%idsect(9)
          ifh1=gfld%ipdtmpl(9)-12
          ifh2=ifh1 + 24   
         endif
         kctl=kctl+1
         do ii = 1, jpoint
          ff(ii,kctl) = f(ii)
         enddo
!cc
         gfldo=gfld
        else
         ncnt=ncnt+1
         if ( ncnt.ge.1 ) then
          print *,' n=',n,' iret=',iret
         endif
        endif  ! if (iret.eq.0)
       enddo   ! for m = 1, jensem jensem = # of ensemble ( default = 23 )

!cc
!cc    Step 2: get statistical coefficents and multiply by it
!cc    
!cc    for t00z cycle
!cc    n=1, 00-24 hours period
!cc    n=2, 12-36 hours period-good statistics
!cc    n=3, 24-48 hours period
!cc    n=5, 36-60 hours period-good statistics
!cc    n=6, 48-72 hours period
!cc    ......
!cc    for t12z cycle
!cc    n=1, 00-24 hours period-good statistics, but 12-36 hrs lead
!cc    n=2, 12-36 hours period
!cc    n=3, 24-48 hours period-good statistics, but 36-60 hrs lead
!cc    n=5, 36-60 hours period
!cc    n=6, 48-72 hours period
!cc    ......
!cc
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

!cc    print out ratio/coefficences for mrf and ctl
       write (*,'(8x,9f7.2)') (rmrf_r(ii),ii=9,1,-1)
       write (*,'(8x,9f7.2)') (rctl_r(ii),ii=9,1,-1)

!cc    main loop for each grid points ( 264 points )
!cc    =============================================
!cc    the new program didn't use 264 boxes -Y. ZHU (06/30/2003)
!cc    calibration apply to globally.
!cc
!cc      add 2 12 hours forecasts --> to 24 hours accumulation
!cc
       do ii = 1, jpoint
        if (ii.eq.4000) then
         write(*,'("example of first 10 ens value at point 4000")')
         write(*,899) (ff(ii,jj),jj=1,10)
        endif
!CC
!CC     Notes:
!CC     rti_r(n) is thread amount of precipitation
!CC     rtmrf_r(n) is the ratio/coefficences of rti_r(n) for mrf forecast
!CC     rtctl_r(n) is the ratio/coefficences of rti_r(n) for ctl forecast
!CC   
!CC     for stpint:
!CC     input: rti_r(n)
!CC     input: rtmrf_r(n)
!CC     input request: aaa -> single value
!CC     output for aaa: bbb-> single value (ratio value for aaa )
!CC     calibrated precipitation = aaa*bbb
!cc

        do jj = 1, jensem
         aaa = ff(ii,jj)
         if (aaa.eq.0.0) then
          ff(ii,jj) = 0.0
         else
          if (jj.eq.1.and.n.le.15) then
!           call stpint(rti_r,rmrf_r,9,2,aaa,bbb,1,aux,naux)
           call qtpint(rti_r,rmrf_r,9,2,aaa,bbb,1)
           ff(ii,jj) = aaa*bbb
          else
!           call stpint(rti_r,rctl_r,9,2,aaa,bbb,1,aux,naux)
           call qtpint(rti_r,rctl_r,9,2,aaa,bbb,1)
           ff(ii,jj) = aaa*bbb
          endif
         endif
        enddo

        if (ii.eq.4000) then
         write(*,899) (ff(ii,jj),jj=1,10)
        endif

 899   format(10f7.2)

       enddo     ! for ind loop  / ii loop          

!cc
!cc    write out the results
!cc
       do jj = 1, jensem

        if (icyc.eq.0) then
         jjj=jj
        elseif (icyc.eq.12) then
         jjj=jj+1
         if (jj.eq.1) jjj=jj
        else
         print *, "ICYC=",icyc," is not acceptable! program will stop!"
         stop 
        endif
        if (n.le.15) then
          gfldo%ipdtmpl(16) = e16(jj)
          gfldo%ipdtmpl(17) = e17(jj)
        else
          gfldo%ipdtmpl(16) = ee16(jj)
          gfldo%ipdtmpl(17) = ee17(jj)
        endif
        gfldo%ipdtmpl(3) = 11              ! code table 4.3, Bias corrected ensemble forecast 
        gfldo%idsect(13) = 4

!
!       we need to set up a lower limit, for example: ff = 0.01 mm
!

        do ii = 1, jpoint
         if (ff(ii,jj).lt.0.01) then
          f(ii) = 0.0
         else
          f(ii) = ff(ii,jj)
         endif
        enddo

!       ipds(5) = 59
        if (icyc.eq.0) then
         gfldo%idsect(9) = 00
         gfldo%ipdtmpl(8) =  12
         gfldo%ipdtmpl(9) = n - 1
         gfldo%ipdtmpl(29) = 12
         gfldo%ipdtmpl(30) = 2
        else
         gfldo%idsect(9) = 12
         gfldo%ipdtmpl(8) =  12
         gfldo%ipdtmpl(9) = n - 1
         gfldo%ipdtmpl(29) = 12
         gfldo%ipdtmpl(30) = 2
        endif
        gfldo%fld(1:jpoint)=f(1:jpoint)
        call putgb2(51,gfldo,iret)
       enddo
       call gf_free(gfld)
!cc
!cc    calculate the CPQPF
!cc
      ! change grib2 pdt message for new ensemble products

      gfldo%idsect(2)=2  ! Identification of originating/generating subcenter
                         ! 2: NCEP Ensemble Products

      gfldo%idsect(13)=5 ! Type of processed data in this GRIB message       
                         ! 5: Control and Perturbed Forecast Products

      temp=-9999 

!     print *, 'gfldo%ipdtlen=',gfldo%ipdtlen 
!     print *, 'gfldo%ipdtmpl=',gfldo%ipdtmpl 

      temp(1:gfldo%ipdtlen)=gfldo%ipdtmpl(1:gfldo%ipdtlen) 

      temp(3)=11              ! code table 4.3, Bias corrected ensemble forecast

      deallocate (gfldo%ipdtmpl)
                              ! 5: Probability Forecast 
      gfldo%ipdtnum=9         ! Probability forecasts from ensemble 
      if(gfldo%ipdtnum.eq.9) gfldo%ipdtlen=36
      if(gfldo%ipdtnum.eq.9) allocate (gfldo%ipdtmpl(gfldo%ipdtlen))

      gfldo%ipdtmpl(1:15)=temp(1:15)

      gfldo%ipdtmpl(1)=1      ! Parameter category : 1 Moisture
      gfldo%ipdtmpl(2)=8      ! Parameter number : 8 Total Precipitation(APCP)

      gfldo%ipdtmpl(16)=0     ! Forecast probability number 
      gfldo%ipdtmpl(17)= iensem-1  ! Total number of forecast probabilities
      gfldo%ipdtmpl(18)=1     ! Probability Type
                              ! 1: Probability of event above upper limit
      gfldo%ipdtmpl(19)=0     ! Scale factor of lower limit
      gfldo%ipdtmpl(20)=0     ! Scaled value of lower limit
      gfldo%ipdtmpl(21)=3     ! Scale factor of upper limit
 
      ! gfldo%ipdtmpl(22) will be set below 

      gfldo%ipdtmpl(23:36)=temp(19:32) 

      ! gfldo%ipdtmpl(34): Length of the time range over which processing done

        if (icyc.eq.0) then 
         gfldo%idsect(9) = 00 
         gfldo%ipdtmpl(8) =  12 
         gfldo%ipdtmpl(9) = n - 1 
         gfldo%ipdtmpl(34) = 2 
        else 
         gfldo%idsect(9) = 12 
         gfldo%ipdtmpl(8) =  12 
         gfldo%ipdtmpl(9) = n - 1 
         gfldo%ipdtmpl(34) = 2 
        endif 

       do k = 1, istd 
        f   = 0.0 
        do ii = 1, jpoint 
!cccc to exclude GFS/AVN high resolution forecast 
!cccc    do jj = 1, iensem 
        if (n.le.15) then
         istart=2
         iend=iensem
        else
         istart=1
         iend=iensem-1
        endif
         do jj = istart, iend 
          if (ff(ii,jj).ge.rk(k)) then 
           f(ii) = f(ii) + 1.0 
          endif 
         enddo 
!cccc    f(ii) = f(ii)*100.00/float(jensem) 
         f(ii) = f(ii)*100.00/float(iend-istart+1) 
         if (f(ii).ge.99.0) then 
          f(ii) = 100.0 
         endif 
        enddo 
 
     ! gfldo%ipdtmpl(22): Scaled value of upper limit

      gfldo%ipdtmpl(22)=rk(k)*(10**gfldo%ipdtmpl(21))

      gfldo%fld(1:jpoint)=f(1:jpoint)

      call putgb2(52,gfldo,iret)
       enddo    ! for k = 1, istd
      call gf_free(gfldo)
      enddo     ! for n loop
 990  format (69x)
 991  format (6x,i6,57x)
 994  format(5x,9f7.0)                                  
 995  format(i4,2x,a8,2x,2f8.4,i4,f6.2,4f10.4)
!995  format(i4,2x,f8.4,5f10.3)
 996  format(17f5.1)
 997  format('-- ',i2,' numbers mean = ',f10.4,' --')
 998  format(i4,i3,f5.2,800f5.2)
 999  format(3f6.1,10f5.1)

      call baclose (11,iretb)

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
!--------+---------+---------+---------+---------+---------+---------+---------+
      subroutine CPCOEF(x,y,z,r,n,ictl)
!
!     This program will calculate the precipitation calibration
!     coefficence by using statistical distributions
!
!     Program: IBM-ASP     By: Yuejian Zhu ( 03/22/2001 )
!              Modified    By: Yuejian Zhu ( 03/21/2004 )
!
!     Input:    x(n)-vector for preciptation threshold amount
!               y(n)-vector for observation numbers at x(n)
!               z(n)-vector for forecasting numbers at x(n)
!               n   -length of the vector
!               ictl-to control the interpolation bases
!                    1 - standard
!                    2 - logrithm
!     Output:   r(n)-coefficents/ratio of each threshold amount x(n)
!               which will apply to particular precipitation amount
!               multiply the ratio at this point (linear interpolation)
!
      dimension x(n),y(n),z(n),r(n),a(n),b(n)             
      dimension rnti(n-2),rnto(n-2)

!     Safty check of x-axis (dimension y)
!      Repeating to confirm the x-axis is ascending
      do i = 1, n-1
       do j = 1, n-2
        if (y(i).eq.y(i+1)) then
         y(i) = y(i) + 1.0  
        endif
       enddo
      enddo

      if ( ictl.eq.1) then

!
!   input: y(n) as abscissas (x-axis)
!   input: x(n) as ordinates (y-axis) 
!   input: z(n) as request abscissas values ( x-axis )
!   output: b(n) as cooresponding values of z(n)
!           similar to the thread amount, but shifted

!       call stpint(y,x,n,2,z,r,n,aux,naux)
       call qtpint(y,x,n,2,z,r,n)
       write(*,991) (y(i)/1000.0,i=1,n)
       write(*,993) (x(i),i=1,n)
       write(*,992) (z(i)/1000.0,i=1,n)
       write(*,994) (r(i),i=1,n)
       write(*,995) (r(i)/x(i),i=1,n)
  
!      do i = 1, n-2
!       rnti(i) = x(n-i)
!       rnto(i) = r(n-i)/x(n-i)
!      enddo
!      call stpint(rnti,rnto,n-2,2,x,r,n,aux,naux)
!      write(*,993) (r(i),i=1,n)
!      if (r(n).lt.0.0.and.r(n-1).gt.0.0) then
!       r(n) = r(n-1)*r(n-2)
!      endif
!      write(*,993) (r(i),i=1,n)

      else
!
!  Tested both of log and log10, log is better
!
!   input: y(n) as abscissas (x-axis)
!   input: x(n) as ordinates (y-axis) -- using logrithem a(n) instead
!   input: z(n) as request abscissas values ( x-axis )
!   output: b(n) as cooresponding values of z(n)
!           similar to the thread amount, but shifted

       do i = 1, n
        a(i) = alog(x(i))
!       a(i) = log10(x(i))
       enddo

!       call stpint(y,a,n,2,z,b,n,aux,naux)
       call qtpint(y,a,n,2,z,b,n)
       do i = 1, n
        r(i) = exp(b(i))
!       r(i) = exp(b(i)*log(10.0))
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
   
!      do i = 1, n-2
!       rnti(i) = x(n-i)
!       rnto(i) = exp(b(n-i))/x(n-i)
!       rnto(i) = exp(b(n-i)*log(10.0))/x(n-i)
!      enddo
!      call stpint(rnti,rnto,n-2,2,x,r,n,aux,naux)
!      write(*,993) (r(i),i=1,n)
!      if (r(n).lt.0.0.and.r(n-1).gt.0.0) then
!       r(n) = r(n-1)*r(n-2)
!      endif
!      write(*,993) (r(i),i=1,n)
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

!           ==== using p50 results ====
!input =   75.00  50.00  35.00  25.00  15.00  10.00   5.00   2.00    .20
!output=   78.57  78.57  57.14  30.83  15.71   9.86   4.41   1.07  -3.19
!ratio =    1.05   1.57   1.63   1.23   1.05    .99    .88    .54 -15.95

!           ==== using t62 results ====
!input =   75.00  50.00  35.00  25.00  15.00  10.00   5.00   2.00    .20
!output=   78.57  67.86  41.32  25.83  14.36   9.27   4.17   1.19  -2.42
!ratio =    1.05   1.36   1.18   1.03    .96    .93    .83    .59 -12.10
!ratio =    1.65   1.36   1.18   1.03    .96    .93    .83    .59    .45



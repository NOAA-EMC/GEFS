      program CVT24H
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCC   This program will convert input global ensemble precipitation CCCC
!CCC   forecast to 24 hrs accumulation and ready for verification    CCCC
!CCC   ( output gfs and ensemble ctl only )                          CCCC
!CCC                                                                 CCCC
!CCC    Notes:                                                       CCCC
!CCC      for ensemble precipitation calibration using only          CCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! PROGRAM HISTORY LOG:
!   04-02-09  YUEJIAN ZHU 
!   06-02-06  YUEJIAN ZHU MODIFYINF FOR NEW CONFIGURATION 
!   14-11-06  YAN LUO MODIFY TO CONVERT I/O FROM GRIB1 TO GRIB2
!
!
!   Parameters:
!     1. jf--->    model resolution or total grid points ( default 10512 )
!     2. iem-->    numbers of ensember members
!     3. len-->    the length of 12 hours interval                 
!
!--------+---------+---------+---------+---------+---------+---------+--

      use grib_mod
      use params

      parameter(jf=10512,iem=22,len=34)
      dimension f(jf),ff(jf,iem),pp(jf,iem),fr(jf),aa(jf)
      dimension trf(jf),ctl(jf)
      integer e16(iem),e17(iem)

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
      data e16/0,1,3,3,3,3,3,3,3,3,3, 3, 3, 3, 3, 3, 3, 3,&
               3, 3, 3, 3/
      data e17/0,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,&
              17,18,19,20/

      character*255 cpgb,pgb71,pgb72

      namelist /namin/ cpgb,pgb71,pgb72

      read (5,namin)
      write(6,namin)

      lpgb=len_trim(cpgb)
      print *, cpgb(1:lpgb)
      call baopenr(11,cpgb(1:lpgb),iretb)
 
      lg71=len_trim(pgb71)
      call baopenw(71,pgb71(1:lg71),irt71)
      lg72=len_trim(pgb72)
      call baopenw(72,pgb72(1:lg72),irt72)

      ncnt=0
!     do n=1,len  
      do n=2,len  
!     do m=1,iem
      do m=1,2  
      iids=-9999;ipdt=-9999; igdt=-9999
      idisc=-1;  ipdtn=-1;   igdtn=-1
      ipdt(1)=ipd1
      ipdt(2)=ipd2
      ipdt(10)=ipd10
      ipdt(11)=ipd11
      ipdt(12)=ipd12
      ipdt(16)=e16(m)
      ipdt(17)=e17(m)
      ipdt(9)=n-1 
      ipdt(30)=2
      ipdtn=11; igdtn=-1
      call init_parm(ipdtn,ipdt,igdtn,igdt,idisc,iids)
      if (n.le.30) then
       call getgb2(11,0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,&
                  unpack,jskp,gfld,iret)
        if (iret.eq.0) then
         f(1:jf) = gfld%fld(1:jf)
!  Find GRIB MESSAGE
         call printinfr(gfld,n)
         do ii=1,jf    
         ff(ii,m)=f(ii)
         enddo
        else
          ncnt=ncnt+1
          if ( ncnt.le.1 ) then
           print *,' n=',n,' iret=',iret
          endif
        endif
      endif
      enddo
       if (n.ge.2.and.mod(n,2).eq.0) then        
        gfldo=gfld
        gfldo%idsect(13)=5 ! Type of processed data in this GRIB message       
                          ! 5: Control and Perturbed Forecast Products
        gfldo%ipdtmpl(1) = ipd1
        gfldo%ipdtmpl(2) = 7       ! Precipitation Rate
        gfldo%idrtmpl(3) = 6       ! GRIB2 DRT 5.40 decimal scale factor
        gfldo%ipdtmpl(8) =  1
        gfldo%ipdtmpl(9) = (n-1)*12
        gfldo%ipdtmpl(29) = 12
        gfldo%ipdtmpl(30)= 2
        do ii = 1, jf
!        trf(ii) = (ff(ii,1)+pp(ii,1))/3600.00/24.00
!        ctl(ii) = (ff(ii,2)+pp(ii,2))/3600.00/24.00
         trf(ii) = ff(ii,1)/3600.00/24.00
         ctl(ii) = ff(ii,2)/3600.00/24.00
        enddo
        print *, '==== To write out the precip. of gfs run'
        gfldo%ipdtmpl(16) = e16(1)
        gfldo%ipdtmpl(17) = e17(1)
        gfldo%fld(1:jf)=trf(1:jf)
        call putgb2(71,gfldo,iret)
        print *, '==== To write out the precip. of ctl run'
        gfldo%ipdtmpl(16) = e16(2)
        gfldo%ipdtmpl(17) = e17(2)
        gfldo%fld(1:jf)=ctl(1:jf)
        call putgb2(72,gfldo,iret)
 
       endif          ! if (n.ge.4.and.mod(n,2).eq.0)

       do ii = 1, jf
        do k = 1, iem
         pp(ii,k) = ff(ii,k)
        enddo
       enddo
      enddo           ! for n loop
      call gf_free(gfld)   
      call gf_free(gfldo)

      call baclose(11,iretb)
      call baclose(71,iret)
      call baclose(72,iret)
!
 666  format (10(1pe8.1))
      stop    
      end

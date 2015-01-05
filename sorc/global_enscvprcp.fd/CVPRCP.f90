!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: PRCPCV
!   PRGMMR: ZHU              ORG: NP23        DATE: 1999-08-31
!
! PROGRAM HISTORY LOG:
!   97-03-17   YUEJIAN ZHU (WD20YZ)
!   99-07-27   YUEJIAN ZHU (WX20YZ) MODIFY TO IBM-SP
!   00-10-30   YUEJIAN ZHU (WX20YZ) MODIFY TO MATCH AVN PROCESS
!   00-12-12   YUEJIAN ZHU (WX20YZ) MODIFY TO MATCH T12Z AVN PROCESS
!   03-08-29   YUEJIAN ZHU (WX20YZ) MODIFY TO MATCH NEW IMPLEMENTATION
!   06-02-06   YUEJIAN ZHU (WX20YZ) MODIFY TO MATCH NEW IMPLEMENTATION
!   09-18-06   YUEJIAN ZHU (WX20YZ) MODIFY TO CORRECT THE PROBLEM AFTER
!              FORECAST LEAD TIME EXCEEDING 252 HOURS.
!   14-11-06   YAN LUO (WX22LU) MODIFY TO CONVERT I/O FROM GRIB1 TO GRIB2
!
! ABSTRACT: THIS PROGRAM WILL CONVERT 6 HOURS PERIOD PRECIP.  
!           TO 12/24 HOURS ACCUMULATION, CONVERT PRECIP. RATE
!           TO ACCUMULATION PRECIPITATION AND ETC.
!
! USAGE:
!
!   INPUT FILES:
!     UNIT  11  PRECIPITATION GRIB FILE ( 144*73 ) IN GRIB2
!
!   OUTPUT FILES:
!     UNIT  51  PRECIPITATION GRIB FILE ( 144*73 ) IN GRIB2
!
!   SUBPROGRAMS CALLED:
!   
!   BAOPENR          GRIB I/O
!   BACLOSE          GRIB I/O
!   GETGB2           GRIB2 READER
!   PUTGB2           GRIB2 WRITER
!   GF_FREE          FREE UP MEMORY FOR GRIB2 
!   INIT_PARM        DEFINE GRID DEFINITION AND PRODUCT DEFINITION
!   PRINTINFR        PRINT GRIB2 DATA INFORMATION
!
!   EXIT STATES:
!     COND =  0 - SUCCESSFUL RUN
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      program prcpcv

      use grib_mod
      use params
                                            
      parameter(jf=10512,iensem=22)
      dimension f(jf),ff(jf),pp(jf)
      dimension itind(20)
      integer e16(iensem),e17(iensem)

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

      character*80 cpgb,cpge

      namelist /namin/ cpgb,cpge,ini,ipr,isp,itu
!     ini-intial time
!     ipr-average period
!     isp-avaerage step
!     itu-output time unit
!
      CALL W3TAGB('PRCPCV',2003,0243,0069,'NP23')
!
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
      lpge=len_trim(cpge)
      print *, cpgb(1:lpgb+1),cpge(1:lpge+1)
      call baopenr(11,cpgb(1:lpgb),iretb)
      call baopenw (51,cpge(1:lpge),irete)
      
!  READ IN PRECIP FORECAST ( GRIB FORMAT )

      do jj = 2, iensem ! total ensemble members
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
      iids=-9999;ipdt=-9999; igdt=-9999
      idisc=-1;  ipdtn=-1;   igdtn=-1
      ipdt(1)=ipd1
      ipdt(2)=ipd2
      ipdt(10)=ipd10
      ipdt(11)=ipd11
      ipdt(12)=ipd12
      ipdt(16)=e16(jj)
      ipdt(17)=e17(jj)
         if (jj.eq.1) then
          if (n.eq.1) then
           if (ii.le.15) then
            ipdt(9)=(ii-1)*12
            ipdt(30)=6
            jpds13=1
           elseif (ii.gt.15.and.ii.le.21) then
            ipdt(9)=(ii-1)*12
            ipdt(30)=12
            jpds13=1
           else
            ipdt(9)=(ii-1)
            ipdt(30)=1
            jpds13=12
           endif
          else
           if (jpds15.lt.180) then
            ipdt(9)=jpds15
            ipdt(30)=6
            jpds13=1
           elseif (jpds15.ge.180.and.jpds15.lt.252) then
            ipdt(9)=jpds15
            ipdt(30)=12
            jpds13=1
           else
            ipdt(9)=jpds15/12
            ipdt(30)=1
            jpds13=12
           endif
          endif
         else
          if (n.eq.1) then
            ipdt(9)=(ii-1)*12
            ipdt(30)=6
            jpds13=1
          else
            ipdt(9)=jpds15
            ipdt(30)=6
            jpds13=1
          endif
         endif
      ipdtn=11; igdtn=-1
      call init_parm(ipdtn,ipdt,igdtn,igdt,idisc,iids)
      call getgb2(11,0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,&
                  unpack,jskp,gfld,iret)
      if (iret.eq.0) then
       f(1:jf) = gfld%fld(1:jf)
!  Find GRIB MESSAGE
       icnt = icnt + 1
       call printinfr(gfld,n)
          ip1=ipdt(9)*itind(jpds13)
          ip2=(ipdt(9)+ipdt(30))*itind(jpds13)
          if (icnt.eq.1) ip1st=ip1
          ip2m1=ip2m1 + (ip2-ip1)
          do ij=1, 10512
           ff(ij)=ff(ij) + f(ij)
          enddo
          jpds15=(ipdt(9)+ipdt(30))*itind(jpds13)
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
        jpds13 = itu
        gfldo=gfld
        gfldo%ipdtmpl(8) =  12
        gfldo%ipdtmpl(9) = jpds15/12 - 24/itind(jpds13)
        gfldo%ipdtmpl(29)= 12
        gfldo%ipdtmpl(30)= 2
        gfldo%ipdtmpl(16) = e16(jj)
        gfldo%ipdtmpl(17) = e17(jj)
        do ij=1, 10512
         if (ff(ij).lt.0.0000001) then
          ff(ij)=0.0
         endif
        enddo
        print *, "=== write out ==="
        gfldo%fld(1:jf)=ff(1:jf)
        call putgb2(51,gfldo,iret)
        call printinfr(gfldo,n)
       enddo
 102   continue
        call gf_free(gfld) 
        call gf_free(gfldo)
      enddo

      call baclose(11,iretb)
      call baclose(51,irete)

      CALL W3TAGE('PRCPCV')

      stop    
      end

!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: ENSPPF        ENNSEMBLE PRECIP. PROBABILITY 
!   PRGMMR: YUEJIAN ZHU       ORG:NP23           DATE: 97-03-17
!
! ABSTRACT: THIS PROGRAM WILL CALCULATE ENSEMBLE BASED PRECIP.
!           PROBABILITY FORECAST (PPF)
!
! PROGRAM HISTORY LOG:
!   97-03-17   YUEJIAN ZHU (WD20YZ)
!   99-07-26   YUEJIAN ZHU (WX20YZ) MODITY TO IBM-SP
!   00-04-17   YUEJIAN ZHU (WX20YZ) INCREASE MEMBERS FROM 17 TO 23
!   03-09-09   YUEJIAN ZHU (WX20YZ) REDUCE   MEMBERS FROM 23 TO 11
!              TO MAKE COMPARRABLE RESOLUTIONS AND SKILLS
!   04-02-10   YUEJIAN ZHU (WX20YZ) RESOLVE A BUG WHICH PRODUCE A
!              INCORRECT DATA MESSAGE
!   06-01-25   YUEJIAN ZHU (WX20YZ) CHANGE FOR NEW IMPLEMANTED ENSEMBLE
!              PRODUCTION (14+1+1 MEMBERS AND ONE SIDE ENSMBLE)
!   14-11-06   BO Cui: Modify for grib2 encode/decode
!                      Change for full ensemble member, 
!                      production(20+1 members  and one side ensemble)
!   18-05-10   Xianwu Xue : Add 'npert' to calculate 'mem'
!
!
! USAGE:
!
!   INPUT FILES:
!     UNIT  11  PRECIPITATION GRIB FILE ( 144*73 )
!
!   OUTPUT FILES:
!     UNIT  51  PPF GRIB FILE ( 144*73 )
!
!   SUBPROGRAMS CALLED:
!     GETGB2 -- W3LIB ROUTINE
!     PUTGB2 -- W3LIB ROUTINE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN
!
program ppf                                               

use grib_mod
use params

implicit none

type(gribfield) :: gfld,gfldo

integer :: currlen=0
logical :: unpack=.true.
logical :: expand=.false.

integer,dimension(200) :: kids,kpdt,kgdt
integer kskp,kdisc,kpdtn,kgdtn,i

integer,dimension(200) :: jids,jpdt,jgdt,iids,ipdt,igdt
integer jskp,jdisc,jpdtn,jgdtn,idisc,ipdtn,igdtn
common /param/jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt

integer   len,mem,icyc,ijd,ncnt,n,l,icnt,m,ii,k
integer   lpgb,lpge,iretb,irete,imem,iret 
integer   lpgi,ireti,jj
integer   temp(200),ipdt9,P2,iunit2
real      bb,cc

parameter(len=60)

integer :: npert = 20

real, allocatable :: ff(:,:),pp(:,:),ss(:,:),aa(:),gg(:,:),hh(:,:)

character*255 cpgb,cpgi,cpge

namelist /namin/ cpgb,cpge,npert

real rk(9)
data rk/0.254,1.00,2.54,5.00,6.35,10.00,12.7,25.4,50.8/

CALL W3TAGB('ENSPPF',2000,0110,0073,'NP20   ')

read (5,namin,end=1020)
!write (6, namin)
!print *, npert

mem = npert + 1

lpgb=len_trim(cpgb)
lpge=len_trim(cpge)
print *, cpgb(1:lpgb)
print *, cpge(1:lpge)
call baopenr(11,cpgb(1:lpgb),iretb)
call baopenw(51,cpge(1:lpge),irete)

print*,'     '

! find grib message

iids=-9999;ipdt=-9999; igdt=-9999
idisc=-1;  ipdtn=-1;   igdtn=-1
call init_parm(ipdtn,ipdt,igdtn,igdt,idisc,iids)
call getgb2(11,0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,unpack,jskp,gfld,iret)

if(iret.eq.0) then

  ijd=gfld%ngrdpts

  ! check Indicator of unit of time ranga 
  ! gfld%ipdtmpl(8): indicator of unit of time range
  ! 11: 6 hours; 1: hour; 10: 3 hour; 12: 12 hours

  if(gfld%ipdtmpl(8).eq.1) then
     print *, ' Unit of time range is 1 hour '; print *, ' '
  elseif(gfld%ipdtmpl(8).eq.11) then
     print *, ' Unit of time range is 6 hours '; print *, ' '
  else
     print *, 'Unit of time range is not 1 hour or 6 hours '; print *, ' '
  endif

  ! check gfld%ipdtmpl(30) for PDT number 4.11
  ! length of the time range in units defined by the previous octet

  if(gfld%ipdtmpl(30).eq.6) then
     print *, ' Length of time range is 6 hours '; print *, ' '
  else
     print *, ' Length of time range is not 6 hours '; print *, ' '
  endif

  call gf_free(gfld)

  allocate(ff(ijd,mem),pp(ijd,mem),ss(ijd,mem),aa(ijd))
  allocate(gg(ijd,mem),hh(ijd,mem))

  ncnt=0

  do n = 1, len         !### len=steps

  ! Part I: get ctl + 20 ensemble members precipitation data

    icnt=0
    ff=0.0
    do m=1, mem

      iids=-9999;ipdt=-9999; igdt=-9999
      idisc=-1;  ipdtn=-1;   igdtn=-1

      ! read and process input ensemble member

      ! read in control member for m=1

      if(m.eq.1) then
        ipdt(16)=1           ! type of ensemble forecast
        ipdt(17)=0           ! perturbation number
      else
        ipdt(16)=3           ! type of ensemble forecast
        ipdt(17)=m-1           ! perturbation number
      endif

  !   ipdt(8)=1            ! time unit: 1 hour  - kpds(13) in grib1

      ! gfld%ipdtmpl(9): forecast time in units

      ipdt(9)=int((n-1)*6)

      ! gfld%ipdtmpl(29): indicator of unit of time ranga for process
      ! 11: 6 hours; 1: hour; 10: 3 hour; 12: 12 hours

      ipdt(29)=1           ! time unit: 1 for 1 hour

      ! check gfld%ipdtmpl(30) for PDT number 4.11
      ! length of the time range in units defined by the previous octet

      ipdt(30)=6           ! length of the time range is 6 hours

      ipdtn=11; igdtn=-1

      call init_parm(ipdtn,ipdt,igdtn,igdt,idisc,iids)
      call getgb2(11,0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,unpack,jskp,gfld,iret)

      if(iret.ne.0) print *, 'there is no varibale for member ',m

      if(iret.eq.0) then
        icnt=icnt + 1
        print *, '----- Input Data for Current Time ------'
        call printinfr(gfld,m)
        do ii=1,ijd
          ff(ii,icnt)=gfld%fld(ii)
        enddo
      else
        ncnt=ncnt+1
        if(ncnt.le.1) then
          print *,' n=',n,' iret=',iret
        endif
      endif  ! end of iret.eq.0

      if(icnt.eq.1) then
        if (n.ge.4.and.mod(n,2).eq.0) then
          gfldo=gfld
        else
          call gf_free(gfld)
        endif
      else
        call gf_free(gfld)
      endif

    enddo   !### for m = 1, mem
    !
    !
    !   PART II: to calculate the probability scores
    !
    !   skip n=1-3 and other odd steps, when you calculate 24-hour interval

    if (n.ge.4.and.mod(n,2).eq.0) then

      ! change grib2 pdt message for new ensemble products

      gfldo%idsect(2)=2  ! Identification of originating/generating subcenter
                         ! 2: NCEP Ensemble Products

      gfldo%idsect(13)=5 ! Type of processed data in this GRIB message       
                         ! 5: Control and Perturbed Forecast Products

      temp=-9999

!     print *, 'gfldo%ipdtlen=',gfldo%ipdtlen
!     print *, 'gfldo%ipdtmpl=',gfldo%ipdtmpl

      temp(1:gfldo%ipdtlen)=gfldo%ipdtmpl(1:gfldo%ipdtlen)

      temp(3)=5               ! Type of generating process 
                              ! 5: Probability Forecast

      deallocate (gfldo%ipdtmpl) 

      gfldo%ipdtnum=9         ! Probability forecasts from ensemble 
      if(gfldo%ipdtnum.eq.9) gfldo%ipdtlen=36
      if(gfldo%ipdtnum.eq.9) allocate (gfldo%ipdtmpl(gfldo%ipdtlen))

      gfldo%ipdtmpl(1:15)=temp(1:15)

      gfldo%ipdtmpl(1)=1      ! Parameter category : 1 Moisture
      gfldo%ipdtmpl(2)=8      ! Parameter number : 8 Total Precipitation(APCP)

!     unit of time range(time interval) is 6 hours for gefs
!     for ipdt 4.11, unit of time range: gfldo%ipdtmpl(29)
!                    forecast time p1: gfldo%ipdtmpl(9)
!     this is for ipdt 4.11 only

      if(temp(8).eq.1) then
        if(temp(29).eq.1) then
          iunit2=1
          P2=abs(temp(30)*iunit2)
          ipdt9=int(temp(9)+P2)
          gfldo%ipdtmpl(9)=int(ipdt9-24)  ! Forecast time in units
        endif
      endif

      gfldo%ipdtmpl(16)=0     ! Forecast probability number 
      gfldo%ipdtmpl(17)=mem   ! Total number of forecast probabilities
      gfldo%ipdtmpl(18)=1     ! Probability Type
                              ! 1: Probability of event above upper limit

      gfldo%ipdtmpl(19)=0     ! Scale factor of lower limit
      gfldo%ipdtmpl(20)=0     ! Scaled value of lower limit
      gfldo%ipdtmpl(21)=3     ! Scale factor of upper limit

      ! gfldo%ipdtmpl(22) will be set below

      gfldo%ipdtmpl(23:36)=temp(19:32)

      ! gfldo%ipdtmpl(34): Length of the time range over which processing done

      if(gfldo%ipdtmpl(8).eq.1) then
        gfldo%ipdtmpl(34)=24  ! Length of the time range 
      endif

      ! start to calculate the probability scores

      do k = 1, 9
        aa=0.0
        do ii = 1, ijd  
          do m = 1, icnt
            bb=(ff(ii,m)+pp(ii,m)+gg(ii,m)+hh(ii,m))
            if (bb.ge.rk(k)) then
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

        !
        !       testing print
        !
        !        if (n.eq.2.and.k.eq.2) then
        !         write(*,999) (aa(ii),ii=1001,1100)
        !        endif
        !999     format (10f8.1)

        print *, '----- Output for Current Time ------'

        ! gfldo%ipdtmpl(22): Scaled value of upper limit

        gfldo%ipdtmpl(22)=rk(k)*(10**gfldo%ipdtmpl(21))

        gfldo%fld(1:ijd)=aa(1:ijd)

        call printinfr(gfldo,k)
        call putgb2(51,gfldo,iret)

      enddo    !### for k = 1, 9

      call gf_free(gfldo)

    endif

    if (icnt.gt.0) then
      do ii = 1, ijd
        do jj = 1, mem
          hh(ii,jj)=gg(ii,jj)
          gg(ii,jj)=pp(ii,jj)
          pp(ii,jj)=ff(ii,jj)
        enddo
      enddo
    endif

  enddo     !### for n = 1, len

  call baclose(11,iretb)
  call baclose(51,irete)

  CALL W3TAGE('ENSPPF')

  stop
else
  print*,' getgbeh, cannot get maxgrd '
endif

1020 Continue

print *,'Wrong Data Input, Output or Wrong Message Input'

stop

end



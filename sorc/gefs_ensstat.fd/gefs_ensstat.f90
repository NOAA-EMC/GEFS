program ens_avgspr_g2
!
! main program: ens_avgspr_g2          
!
! prgmmr: Bo Cui           org: np/wx20        date: 2013-10-01
!
! abstract: calculate ensemble mean & spread 
! 
! usage:
!
!   input file: ncep/cmc/fnmoc ensemble forecast                                          
!
!   output file: ensemble mean and spread

! programs called:
!   baopenr          grib i/o
!   baopenw          grib i/o
!   baclose          grib i/o
!   getgb2           grib reader
!   putgb2           grib writer
!   init_parm        define grid definition and product definition
!   printinfr        print grib2 data information
!   change_template4 change data values for specified Product Definition Template

! exit states:
!   cond =   0 - successful run
!   cond =   1 - I/O abort
!
! attributes:
!   language: fortran 90
! modified by:
!   Xianwu Xue 05/11/2018
!      added 'navg_min' from namelist to determine the minimum members
!$$$

use grib_mod
use params

!implicit none

type(gribfield) :: gfld,gfldo
integer :: currlen=0
logical :: unpack=.true.
logical :: expand=.false.

integer,dimension(200) :: kids,kpdt,kgdt,temp
integer kskp,kdisc,kpdtn,kgdtn
integer firstfile

integer,dimension(200) :: jids,jpdt,jgdt,iids,ipdt,igdt
integer jskp,jdisc,jpdtn,jgdtn,idisc,ipdtn,igdtn
common /param/jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt

integer     nmemd,nmvar,nvar,ivar,im,imem,n,inum 
parameter   (nmemd=62,nmvar=50)

real, allocatable :: fgrid(:,:),fst(:),ens_avg(:),ens_spr(:)
real  weight(nmemd)

integer     maxgrd,iret,jret,icount,i,ipdtnum_out

integer     ipd1,ipd2,ipd3,ipd10,ipd11,ipd12,ipdn

integer     iunit,lfipg(nmemd),icfipg(nmemd)
integer     nfiles,nenspost,iskip(nmemd),tfiles,ifile
integer     lfopg1,lfopg2
integer     icfopg1,icfopg2

character*255 cfipg(nmemd)
character*255 cfopg1,cfopg2

real    gmin,gmax
integer nbit

integer :: navg_min = 10

namelist /namens/nfiles,nenspost,cfipg,iskip,cfopg1,cfopg2,navg_min
 
read (5,namens)
!write (6,namens)
!print *, navg_min

print *, 'Input variables include '

! stop this program if there is no enough files put in 

print *, ' '; print *, 'Input files size ', nfiles                  

if(nfiles.gt.2) then

  ! set the fort.* of intput file, open forecast files

  print *, '   '
  print *, 'Input files include '

  iunit=9

  tfiles=nfiles

  do ifile=1,nfiles
    iunit=iunit+1
    icfipg(ifile)=iunit
    lfipg(ifile)=len_trim(cfipg(ifile))
    print '(a4,i3,a98)', 'fort.',icfipg(ifile), cfipg(ifile)(1:lfipg(ifile))
    call baopenr(icfipg(ifile),cfipg(ifile)(1:lfipg(ifile)),iret)
    if ( iret .ne. 0 ) then
      print *,'there is no NAEFS forecast, ifile,iret = ',cfipg(ifile)(1:lfipg(ifile)),iret
      tfiles=nfiles-1
      iskip(ifile)=1
    endif
  enddo

  ! set the fort.* of output file

  print *, '   '
  print *, 'Output files include '

  iunit=iunit+1
  icfopg1=iunit
  lfopg1=len_trim(cfopg1)
  call baopenwa(icfopg1,cfopg1(1:lfopg1),iret)
  print *, 'fort.',icfopg1, cfopg1(1:lfopg1)
  if(iret.ne.0) then
    print *,'there is no output ensemble average =  ',cfopg1(1:lfopg1),iret
  endif

  iunit=iunit+1
  icfopg2=iunit
  lfopg2=len_trim(cfopg2)
  call baopenwa(icfopg2,cfopg2(1:lfopg2),iret)
  print *, 'fort.',icfopg2, cfopg2(1:lfopg2)
  if(iret.ne.0) then
    print *,'there is no output ensemble spread  =  ',cfopg2(1:lfopg2),iret
  endif

  ! find grib message, maxgrd: number of grid points in the defined grid

  !do ifile=1,tfiles
  do ifile=1,nfiles
    if(iskip(ifile).eq.0) then
      iids=-9999;ipdt=-9999; igdt=-9999
      idisc=-1;  ipdtn=-1;   igdtn=-1
      call init_parm(ipdtn,ipdt,igdtn,igdt,idisc,iids)
      call getgb2(icfipg(ifile),0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,unpack,jskp,gfld,iret)
      maxgrd=gfld%ngrdpts
      call gf_free(gfld)
      firstfile=ifile
      if(iret.eq.0) exit
    endif
  enddo

  if(iret.eq.0) then

    allocate(fgrid(maxgrd,tfiles),fst(tfiles),ens_avg(maxgrd),ens_spr(maxgrd))

    print *, '   '

    ! loop over variables

    kids=-9999;kpdt=-9999; kgdt=-9999
    kdisc=-1;  kpdtn=-1;   kgdtn=-1

    icount=0
    kskp=0
    do

      inum=0
      fgrid=-9999.9999

      print *, '----- Start Read Ensemble Forecast ------'
      print *, '   '

      call getgb2(icfipg(firstfile),0,kskp,kdisc,kids,kpdtn,kpdt,kgdtn,kgdt,unpack,kskp,gfldo,iret)

      if(iret.ne.0) then
        if(iret.eq.99 ) exit
        print *,' getgb2 error = ',iret
        cycle
        !call errexit(17)
      endif

      icount=icount+1

      if(iret.eq.0) then
        inum=inum+1
        call printinfr(gfldo,icount)
        fgrid(1:maxgrd,inum)=gfldo%fld(1:maxgrd)
      else
        print*, 'there is no fcst for ens member 1'
      endif

      ! save parameter message for other members

      ipd1=gfldo%ipdtmpl(1)
      ipd2=gfldo%ipdtmpl(2)
      ipd3=gfldo%ipdtmpl(3)
      ipd10=gfldo%ipdtmpl(10)
      ipd11=gfldo%ipdtmpl(11)
      ipd12=gfldo%ipdtmpl(12)
      ipdn=gfldo%ipdtnum

      ! loop over NAEFS members, get operational ensemble forecast

      print *, '----- Ensemble Forecast for Member ------'
      print *, '   '

    ! do imem=2,nfiles
      do imem=firstfile+1,nfiles

        if(iskip(ifile).eq.0) then

          iids=-9999;ipdt=-9999; igdt=-9999
          idisc=-1;  ipdtn=-1;   igdtn=-1
          ipdt(1)=ipd1; ipdt(2)=ipd2; ipdt(10)=ipd10; ipdt(11)=ipd11; ipdt(12)=ipd12
          igdtn=-1; ipdtn=ipdn
          call init_parm(ipdtn,ipdt,igdtn,igdt,idisc,iids)
          call getgb2(icfipg(imem),0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,unpack,jskp,gfld,iret)

          ! print GEFS data message

          if(iret.eq.0) then
            inum=inum+1
            call printinfr(gfld,icount)
            fgrid(1:maxgrd,inum)=gfld%fld(1:maxgrd)
          else
            print*, 'there is no fcst for ens member',imem
          endif

          call gf_free(gfld)

        endif

      enddo          ! end of imem loop

      ! end of imem loop, calculate ensemble mean and spread

      print *, '   '; print *,' variable has member',inum; print *, '   '
      if(inum.gt.navg_min) then

        print *, '   '; print *,  ' Combined Ensemble Data Example at Point 8601 '
        write (*,'(10f8.1)') (fgrid(8601,i),i=1,inum)
        print *, '   '

        do n=1,maxgrd

          fst(1:inum)=fgrid(n,1:inum)

          do i=1,inum
            weight(i)=1/float(inum)
          enddo

          ens_avg(n)=epdf(fst,weight,inum,1.0,0)
          ens_spr(n)=epdf(fst,weight,inum,2.0,0)

        enddo

        print *, 'ens_avg(8601)= ',ens_avg(8601)
        print *, 'ens_spr(8601)= ',ens_spr(8601)

        print *, '   '
        print *, '----- Output ensemble average and spread for Current Time ------'
        print *, '   '

        ! fnmoc tmax and tmin have message different from ncep, modify them as ncep

        if(gfldo%ipdtnum.eq.11) then
          if(gfldo%ipdtmpl(1).eq.0.and.gfldo%ipdtmpl(2).eq.0.and.gfldo%ipdtmpl(27).eq.2) then
            gfldo%ipdtmpl(1)=0
            gfldo%ipdtmpl(2)=4
          endif
          if(gfldo%ipdtmpl(1).eq.0.and.gfldo%ipdtmpl(2).eq.0.and.gfldo%ipdtmpl(27).eq.3) then
            gfldo%ipdtmpl(1)=0
            gfldo%ipdtmpl(2)=5
          endif
        endif

        ! when product difinition template 4.1/4.11 chenge to 4.2/4.12
        ! ipdtlen aslo change, need do modification for output
        ! code table 4.0, 2=derived forecast

        if(gfldo%ipdtnum.eq.1) then

          temp=-9999
          temp(1:gfldo%ipdtlen)=gfldo%ipdtmpl(1:gfldo%ipdtlen)

          deallocate (gfldo%ipdtmpl)

          gfldo%ipdtnum=2
          if(gfldo%ipdtnum.eq.2) gfldo%ipdtlen=17
          if(gfldo%ipdtnum.eq.2) allocate (gfldo%ipdtmpl(gfldo%ipdtlen))

          gfldo%ipdtmpl(1:15)=temp(1:15)
          gfldo%ipdtmpl(17)=temp(18)

        elseif(gfldo%ipdtnum.eq.11) then

          temp=-9999
          temp(1:gfldo%ipdtlen)=gfldo%ipdtmpl(1:gfldo%ipdtlen)

          deallocate (gfldo%ipdtmpl)

          gfldo%ipdtnum=12
          if(gfldo%ipdtnum.eq.12) gfldo%ipdtlen=31
          if(gfldo%ipdtnum.eq.12) allocate (gfldo%ipdtmpl(gfldo%ipdtlen))

          gfldo%ipdtmpl(1:15)=temp(1:15)
          gfldo%ipdtmpl(17:31)=temp(18:32)

        endif

      ! if(gfldo%ipdtnum.eq.1) ipdtnum_out=2
      ! if(gfldo%ipdtnum.eq.11) ipdtnum_out=12
      ! call change_template4(gfldo%ipdtnum,ipdtnum_out,gfldo%ipdtmpl,gfldo%ipdtlen)

        ! extensions for ensemble mean

        gfldo%ipdtmpl(16)=0      ! code table 4.7, 0=unweighted mean of all Members
        gfldo%ipdtmpl(17)=inum   ! template 4.2, number of forecast in the ensemble

        ! get the number of bits
        ! gfldo%idrtmpl(3) : GRIB2 DRT 5.40 decimal scale factor

        write(6,*) 'gfldo%idrtmpl(3)=',gfldo%idrtmpl(3)

        call gtbits(0,gfldo%idrtmpl(3),maxgrd,0,ens_avg,gmin,gmax,nbit)

      ! gfldo%idrtmpl(4) : GRIB2 DRT 5.40 number of bits

        gfldo%idrtmpl(4)=nbit

        gfldo%fld(1:maxgrd)=ens_avg(1:maxgrd)

        print *, '-----  Ensemble Average for Current Time ------'
        call putgb2(icfopg1,gfldo,jret)
        call printinfr(gfldo,icount)

        ! extensions for ensemble spread

        gfldo%ipdtmpl(16)=2        ! code table 4.7, 2=standard deviation w.r.t cluster mean
        gfldo%ipdtmpl(17)=inum     ! template 4.2, number of forecast in the ensemble

        ! get the number of bits
        ! gfldo%idrtmpl(3) : GRIB2 DRT 5.40 decimal scale factor

        call gtbits(0,gfldo%idrtmpl(3),maxgrd,0,ens_spr,gmin,gmax,nbit)

      ! gfldo%idrtmpl(4) : GRIB2 DRT 5.40 number of bits

        gfldo%idrtmpl(4)=nbit

        gfldo%fld(1:maxgrd)=ens_spr(1:maxgrd)

        print *, '-----  Ensemble Spread for Current Time ------'
        call putgb2(icfopg2,gfldo,jret)
        call printinfr(gfldo,icount)

        ! end of probability forecast calculation

      endif

      call gf_free(gfldo)

    enddo

    ! end of ivar loop

    ! close files

    do ifile=1,nfiles
      call baclose(icfipg(ifile),iret)
    enddo

    call baclose(icfopg1,iret)
    call baclose(icfopg2,iret)

    print *,'Probability Calculation Successfully Complete'

    stop

  else  !if(iret.ne.0) then
    print *,'there is no maxgrd information'
  endif !if(iret.eq.0) then
endif !if(nfiles.gt.2) then

print *, 'There is not Enough Files Input, Stop!'
call errmsg('There is not Enough Files Input, Stop!')
!call errexit(1)

stop
end


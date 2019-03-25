      program gefs_6h_ave_1mem
! main program: gefs_6h_ave_1mem
! Author: Hong Guan :2018-12-17
! REF: Eric Sinsky, Wei Li, Yali Mao, Bo Cui
! Purpose: calculate accumulation or avrage (03Z,06Z)
! 
! usage: daily_ave_acc.exe yyyymmdd
!
!   input file: control f00 and f03 reanalysis data and ensemble forecast (master file)                                      
!   output file: accumulation

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
!
!$$$

      use grib_mod
      use params
      implicit none

      type(gribfield) :: gfld,gfldo

!type pdt_t0 
!    integer :: npdt0   ! Product Definition Template Number
!    integer :: icat0   ! Parameter Category by Product Discipline 
!    integer :: iprm0   ! Parameter Number by Product Discipline and Parameter Category
!    integer :: igp0    ! Type of Generating Process
!    integer :: iffs0   ! Type of first fixed surface 
!    integer :: isf0    ! Scale factor of first fixed surface
!    integer :: isv0    !Scaled value of first fixed surface
!end type pdt_t0
!
!! PDT parameters in the input GRIB2 file (template 4 number, category, parameter, type of level)
!type(pdt_t0), parameter :: &
!in pgrb2a
!    pdt_hgt500   = pdt_t0(1, 3, 5, 4, 100, 0, 50000), &
!    pdt_hgt200   = pdt_t0(1, 3, 5, 4, 100, 0, 20000), &
!    pdt_ugrd200  = pdt_t0(1, 2, 2, 4, 100, 0, 20000), &
!    pdt_ugrd850  = pdt_t0(1, 2, 2, 4, 100, 0, 85000), &
!    pdt_vgrd200  = pdt_t0(1, 2, 3, 4, 100, 0, 20000), &
!    pdt_vgrd850  = pdt_t0(1, 2, 3, 4, 100, 0, 85000), &
!    pdt_t2m      = pdt_t0(1, 0, 0, 4, 103, 0, 2), &
!    pdt_apcp     = pdt_t0(11,1, 8, 4,   1, 0, 0), &
!    pdt_ulwrf_top= pdt_t0(11,5,193,4,   8, 0, 0), &
!now in pgrb2a
!    pdt_tsfc     = pdt_t0(1, 0, 0, 4, 1,   0, 0)

      integer :: nfield,nfield1
      parameter(nfield=45)
      parameter(nfield1=42)

      type pdt_t
          integer,dimension(nfield):: npdt  ! Product Definition Template Number
          integer,dimension(nfield):: icat  ! Parameter Category by Product Discipline
          integer,dimension(nfield):: iprm  ! Parameter Number by Product Discipline and Parameter Category
          integer,dimension(nfield):: igp   ! Type of Generating Process
          integer,dimension(nfield):: iffs  ! Type of first fixed surface 
          integer,dimension(nfield):: isf   ! Scale factor of first fixed surface
          integer,dimension(nfield):: isv   !Scaled value of first fixed surface
      end type pdt_t

      type(pdt_t) :: pdt

      data pdt%npdt/8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11/ ! Product Definition Template Number
      data pdt%icat/5,5,4,4,4,5,2,2,0,0,1,1,19,6,6,6,6,6,0,3,3,5,5,4,4,4,5,2,2,0,0,1,1,19,6,6,6,6,6,0,3,3,1,1,1/  ! Parameter Category by Product Discipline
      data pdt%iprm/3,4,7,8,8,4,17,18,11,10,7,37,1,1,1,1,1,1,10,16,17,192,193,192,193,193,193,17,18,11,10,7,196,1,1,1,1,1,1,193,194,195,8,10,9/  ! Parameter Number by Product Discipline and Parameter Category
      data pdt%iffs/1,1,1,1,8,8,1,1,1,1,1,1,1,1,211,234,224,214,1,1,1,1,1,1,1,8,8,1,1,1,1,1,1,1,10,211,234,224,214,1,1,1,1,1,1/

      integer,dimension(nfield)::disc
      data disc/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0/


      integer :: n_time
      parameter(n_time=3) !from fhr 00 to 06
      integer :: j,jpdtn,jgdtn
      integer,dimension(200) :: jids,jpdt,jgdt
      logical :: unpack=.true.
      integer :: jdisc      ! discipline#, table 0.0(met:0 hydro:1 land:2)
      integer :: day,month,year,hour,fhour
      character(len=256) :: datafile(n_time),outfile,file_dir,out_dir,outfile03,outfile006
      character(len=8) :: file_date
      character(len=5) :: ens_mem
      integer :: unit, ifid,ifid1,nx,ny,iret,jret,i,k,ifh,nfi,maxgrd,ifd,ind,ens_id
      character(len=3) ::sfh
      character(len=2) ::sdy
      character(len=2) ::smonth
      character(len=4) ::syr
      character(len=8) :: pabbrev
      character(len=30) :: labbrev
      character(len=30) :: labbrev_short

      real, allocatable :: var_save(:,:) ! (maxgrd,nfi)
      real, allocatable :: var_save_acc(:) ! (maxgrd)
      real, allocatable :: apcp_6h(:) ! (maxgrd)
      real, allocatable :: acpcp_6h(:) ! (maxgrd)
      real, allocatable :: apcp_3h(:) ! (maxgrd)
      real, allocatable :: acpcp_3h(:) ! (maxgrd)
      real, allocatable :: dpcp(:) ! (maxgrd)


      call GET_COMMAND_ARGUMENT(1, file_date)
      call get_environment_variable("file_dir", file_dir)
      call get_environment_variable("out_dir", out_dir)
      call get_environment_variable("ens_mem", ens_mem)

      print *,'file dir:',trim(file_dir)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!open files for output grib2 (f03 and f006)
       outfile03=trim(out_dir)//ens_mem//'.t00z.'//'pgrb2af03'
       print *,outfile03
       call baopenwa(300,outfile03,iret) ! for add more than 1 members

       outfile006=trim(out_dir)//ens_mem//'.t00z.'//'pgrb2af006'
       print *,outfile006
       call baopenwa(200,outfile006,iret) ! for add more than 1 members
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      do ifid=1,nfield1/2
      nfi=0
      do ifh=000,006,3  !foercast hours

       nfi=nfi+1
       write(sfh,'(i3.3)') ifh

!search in pgrb2a

!        datafile(nfi)=trim(file_dir)//'/gefs.'//trim(file_date)//'/00/'//trim(ens_mem)//'.t00z.pgrb2af'//trim(sfh)
        if(ifh == 000 .or. ifh == 003 ) then 
           datafile(nfi)=trim(file_dir)//'gec00.t00z.master.grb2f'//trim(sfh) ! only control data avail
        else
           datafile(nfi)=trim(file_dir)//trim(ens_mem)//'.t00z.master.grb2f'//trim(sfh)
        endif


      unit=100
       call BAOPENR(unit, datafile(nfi), iret)
       if(iret /= 0 ) then
        write(*,*) "there is no GEFS forecast",datafile(nfi)
       end if
       write(*,*) unit, datafile(nfi), iret

       j = 0
       jids=-9999
       jids=-9999;jpdt=-9999; jgdt=-9999
       jdisc=-1; jgdtn=-1

       if (nfi /= 3 ) then
          jpdtn = pdt%npdt(ifid)  !template version num. 
          jpdt(1)  = pdt%icat(ifid)
          jpdt(2)  = pdt%iprm(ifid)
          jpdt(10) = pdt%iffs(ifid)
          jdisc   = disc(ifid)
       else
          ifid1=ifid + nfield1/2
          jpdtn = pdt%npdt(ifid1)  !template version num.
          jpdt(1)  = pdt%icat(ifid1)
          jpdt(2)  = pdt%iprm(ifid1)
          jpdt(10) = pdt%iffs(ifid1)
          jdisc   = disc(ifid1)
       endif

!       j=ifid-1
!
       call getgb2(unit,0,j,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,unpack,j,gfld,iret)
       if (iret /= 0) then
        write(*,*) "reading file iret=", iret
        stop
       end if
       call baclose(unit, iret)
       write(*,*) 'after getgb2, iret=', iret

       maxgrd=gfld%ngrdpts
       print*,maxgrd
       nx = gfld%igdtmpl(8)
       ny = gfld%igdtmpl(9)
! assign gfldo same arribute for gfld
       gfldo=gfld

       print*,nfi,' ',ifh

      if(.not. allocated(apcp_3h)) allocate(apcp_3h(maxgrd))
      if(.not. allocated(acpcp_3h)) allocate(acpcp_3h(maxgrd))
      if(.not. allocated(apcp_6h)) allocate(apcp_6h(maxgrd))
      if(.not. allocated(acpcp_6h)) allocate(acpcp_6h(maxgrd))
      if(.not. allocated(dpcp)) allocate(dpcp(maxgrd))
      if(.not. allocated(var_save)) allocate(var_save(maxgrd,3))

       var_save(:,nfi) = gfld%fld
       year  = gfld%idsect(6)
       month = gfld%idsect(7)
       day   = gfld%idsect(8)
       hour  = gfld%idsect(9)

       write(syr,'(i4.4)') year
       write(smonth,'(i2.2)') month
       write(sdy,'(i2.2)') day

!- forecast hour
       fhour=gfld%ipdtmpl(9)
       ens_id=gfld%ipdtmpl(17)
       pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1),gfld%ipdtmpl(2))
       call prlevel(gfld%ipdtnum,gfld%ipdtmpl,labbrev)

       write(*,*) 'nfi=',nfi
       call gf_free(gfld)
       
  201 end do !ifh

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      labbrev_short=trim(labbrev)
      print *,pabbrev,trim(labbrev_short(1:4))

      if(.not. allocated(var_save_acc)) allocate(var_save_acc(maxgrd))
!      gfldo%fld=-999999.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!calculate the 00-6h ave/acc

       if(pabbrev=='PRATE' .or. pabbrev=='CPRAT') then
         var_save(:,1)=var_save(:,1)*6.
         var_save(:,2)=var_save(:,2)*9.


            dpcp(:)=(var_save(:,2)-var_save(:,1))
         do i=1,maxgrd
           if(dpcp(i).lt.0.0) then
             dpcp(i)=0.0
           endif
           dpcp(i)=dpcp(i)*3600.
         enddo

         if(pabbrev=='PRATE') then
            apcp_6h(:)=dpcp(:)+var_save(:,3)*6.*3600.
            var_save_acc(:)=apcp_6h(:)/6./3600.
         else
            acpcp_6h(:)=dpcp(:)+var_save(:,3)*6.*3600.
            var_save_acc(:)=acpcp_6h(:)/6./3600.
         endif
         print*,'For 0-6h precip. and precip. rate'
         print*,''

            
        else
!         var_save_acc(:)=(var_save(:,2)+var_save(:,3))/2.
!        rate for 6, 12, 18,... hours is accumulated values/(current time - diagnosis time)
         var_save_acc(:)=(var_save(:,2)+var_save(:,3)*6./3.)/2.
        end if

!output 6h variables

!reasign gfldo%fld
       gfldo%fld=var_save_acc(:)
!reassign the ipdtmpl
       gfldo%ipdtmpl(17)=ens_id !!!! ensemble member: iens=0 ->CTL
       gfldo%ipdtmpl(9)=0 !forecast time
!       gfldo%discipline=disc(ifid) !forecast time
       jret=0
       call putgb2(200,gfldo,jret)
       write(*,*) '200 put',jret,gfldo%discipline,gfldo%ipdtmpl

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       write(*,'(a10,a20,4f15.2)') pabbrev,trim(labbrev),var_save(100,1),var_save(100,2),var_save(100,3),var_save_acc(100)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!calculate the 3h ave/acc
!3h accumulation 00-03z
       if(pabbrev=='PRATE' .or. pabbrev=='CPRAT') then

        var_save_acc(:)=dpcp(:)/3./3600.

        print*,'For 0-3h precip. rate'
        print*,''

        if(pabbrev=='PRATE') then
           apcp_3h(:)=dpcp(:)
        else
           acpcp_3h(:)=dpcp(:)
        endif
        print*,'For 0-3h precip.'
        print*,''

       else
       
        var_save_acc(:)=var_save(:,2)

       end if

!output 3-h variables

!reasign gfldo%fld 
       gfldo%fld=var_save_acc(:)
!reassign the ipdtmpl
       gfldo%ipdtmpl(17)=ens_id !!!! ensemble member: iens=0 ->CTL
       gfldo%ipdtmpl(9)=0 !forecast time
       gfldo%ipdtmpl(22)=3 !forecast time
       gfldo%ipdtmpl(30)=3 !forecast time
       jret=0
       call putgb2(300,gfldo,jret)
       write(*,*) '300 put',jret,gfldo%ipdtmpl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      call gf_free(gfldo)

      deallocate(var_save_acc)
      deallocate(var_save)

      enddo


!      output accumulated precipitations

       unit=100
       call BAOPENR(unit, datafile(3), iret)
       if(iret /= 0 ) then
        write(*,*) "there is no GEFS forecast",datafile(nfi)
       end if

      do ifid=43,45
       j = 0
       jids=-9999
       jids=-9999;jpdt=-9999; jgdt=-9999
       jdisc=-1; jgdtn=-1

       jpdtn = pdt%npdt(ifid)  !template version num.
       jpdt(1)  = pdt%icat(ifid)
       jpdt(2)  = pdt%iprm(ifid)
       jpdt(10) = pdt%iffs(ifid)
!
        write(*,*) unit,datafile(3),jpdt(1),jpdt(2),jpdt(10)
       call getgb2(unit,0,j,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,unpack,j,gfld,iret)
       if (iret /= 0) then
        write(*,*) "reading file iret=", iret
        stop
       end if

       gfldo=gfld
       fhour=gfld%ipdtmpl(9)
       ens_id=gfld%ipdtmpl(17)
       pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1),gfld%ipdtmpl(2))
       call prlevel(gfld%ipdtnum,gfld%ipdtmpl,labbrev)



! assign gfldo same arribute for gfld
       if(ifid.eq.43) then

         gfldo%ipdtmpl(22)=6 !forecast time
         gfldo%ipdtmpl(30)=6 !forecast time
         gfldo%fld=apcp_6h(:)
         jret=0
         call putgb2(200,gfldo,jret)
         write(*,*) '0-6h apcp put',jret

         gfldo%fld=apcp_3h(:)
         gfldo%ipdtmpl(22)=3 !forecast time
         gfldo%ipdtmpl(30)=3 !forecast time
         jret=0
         call putgb2(300,gfldo,jret)
         write(*,*) '0-3h apcp put',jret

       endif

       if(ifid.eq.44) then

         gfldo%ipdtmpl(22)=6 !forecast time
         gfldo%ipdtmpl(30)=6 !forecast time
         gfldo%fld=acpcp_6h(:)
         jret=0
         call putgb2(200,gfldo,jret)
         write(*,*) '0-6h acpcp put',jret

         gfldo%ipdtmpl(22)=3 !forecast time
         gfldo%ipdtmpl(30)=3 !forecast time
         gfldo%fld=acpcp_3h(:)
         jret=0
         call putgb2(300,gfldo,jret)
         write(*,*) '0-3h acpcp put',jret

       endif

       if(ifid.eq.45) then
         gfldo%ipdtmpl(22)=6 !forecast time
         gfldo%ipdtmpl(30)=6 !forecast time
         gfldo%fld=apcp_6h(:)-acpcp_6h(:)
        do i=1,maxgrd
          if(gfldo%fld(i).lt.0.0) then
            gfldo%fld(i)=0.0
          endif
        enddo
         jret=0
         call putgb2(200,gfldo,jret)
         write(*,*) '0-6h ncpcp put',jret

         gfldo%ipdtmpl(22)=3 !forecast time
         gfldo%ipdtmpl(30)=3 !forecast time
         gfldo%fld=apcp_3h(:)-acpcp_3h(:)
        do i=1,maxgrd
          if(gfldo%fld(i).lt.0.0) then
            gfldo%fld(i)=0.0
          endif
        enddo
         jret=0
         call putgb2(300,gfldo,jret)
         write(*,*) '0-3h ncpcp put',jret


       endif

      enddo
!       gfldo%ipdtmpl(17)=ens_id !!!! ensemble member: iens=0 ->CTL
!       gfldo%ipdtmpl(9)=0 !forecast time


      deallocate(apcp_6h)
      deallocate(acpcp_6h)
      deallocate(apcp_3h)
      deallocate(acpcp_3h)
      deallocate(dpcp)

      call baclose(100,iret)
      call baclose(200,iret)
      call baclose(300,iret)

      end program gefs_6h_ave_1mem

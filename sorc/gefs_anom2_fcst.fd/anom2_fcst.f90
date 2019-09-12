      program anom_fcst
! uses persistence with growing weight towards CFSv2 as lead time increases 
! NCEP grid T126 (Gaussian 384x190)
 
      implicit none

      integer,     parameter :: maxgrd=1038240, ndays=35
      integer,     parameter :: iunit=51,ounit=61 
      real,        parameter :: undef=10e+20
      character*250 fn_rawfc,fn_anom_fc,kpdsfile
      character*3  un_climm,un_climo,un_rtgan,un_HFcfs
      real         fgrid(maxgrd) 
      real         rawfc(maxgrd,ndays)
      real*4       climo(maxgrd), climo_0(maxgrd)
      real*4       climm(maxgrd)
      real*4       rtgan(maxgrd)
      real*4       pers(maxgrd)
      logical(1)   lbms(maxgrd)
      real         alpha
     
      integer      index,mskp,n,kskp,j,jf,k,kg,kf,ihr,jhr
      integer      kpds(25),kgds(22),jpds(25),jgds(22)
      integer      kpds8, kpds9, kpds10, kpds21
      integer      res,unit_climm,unit_climo,unit_rtgan
      real         dmin,dmax
      integer      iday,iret,tret,ii,HFcfs

      call getarg(1,fn_rawfc)
      call getarg(2,un_climm)
      call getarg(3,un_climo)
      call getarg(4,un_rtgan)
      call getarg(5,fn_anom_fc)
      call getarg(6,kpdsfile)
      call getarg(7,un_HFcfs)
      fn_rawfc = trim(adjustl(fn_rawfc))
      print *,'Raw forecast filename:',fn_rawfc
      read(un_climm,'(i3)')unit_climm
      print *,'Unit file of model climate',unit_climm
      read(un_climo,'(i3)')unit_climo
      print *,'Unit file of CFSR climate',unit_climo
      read(un_rtgan,'(i3)')unit_rtgan
      print *,'Unit file of RTG analysis',unit_rtgan
      fn_anom_fc = trim(adjustl(fn_anom_fc))
      print *,'Output forecast filename:',fn_anom_fc
      read(un_HFcfs,'(i3)')HFcfs
      print *, HFcfs, ' Days before'
!
      iret = 0
      index = 0
      n = 0
      jpds = -1
      jpds(5) = 81
      jpds(6) = 1
      jpds(7) = 0
      jf = maxgrd

!     reads raw forecast as a grib array, each lead time at a time
      iret = 0
      call baopenr(iunit,fn_rawfc,iret)
      if(iret.ne.0) then
          print *,'error opening input unit'
          call exit(iret)
      endif
!
      iret = 0
      call baopenw(ounit,fn_anom_fc,iret)
      if(iret.ne.0) then
          print *,'error opening output unit'
          call exit(iret)
      endif
!
!     read the list of kpds values
      open(300,file=kpdsfile,form='formatted')

      index = 0
      mskp =  0   !messages to skip
      jpds = -1
      jgds = -1
      call getgbh(iunit,index,mskp,jpds,jgds,kg,kf,k,kpds,kgds,iret)
      if(iret.ne.0) then
          print *,'error get header=',iunit
          call exit(iret)
      endif
      
   

!     read analysis initial 
      read(unit_rtgan)rtgan !analysis
      close(unit_rtgan)

      fgrid=undef
      climo_0=undef
      do iday = 0,ndays
!       read climm and climo as binary arrays
        read(unit_climm)climm
        read(unit_climo)climo
        if (iday.eq.0) climo_0 = climo
!
        index = 0
        n = 0
        jpds = -1
        kpds=-1
        kgds=-1
        jpds(5) = 11
        jpds(6) = 1
        jpds(7) = 0
        jpds(21) = kpds(21)
        jf = maxgrd
        jgds=-1
       
        if (iday.eq.0) then
            jhr=0
        else
            jhr =HFcfs + 24*iday
        endif
        print *, "hour: ", jhr
        jpds(14) = jhr
        call getgb(iunit,index,jf,n,jpds,jgds,kf,k,kpds,kgds,lbms,fgrid,iret)
        if(iret.ne.0) then
          print *,'error reading=',iunit
          call exit(iret)
        endif
   
       print*,kpds    
!WWLL20180103
        print *, 'kgds=',kgds
!
!        alpha = iday/real(ndays)*0.01
        alpha = iday/real(ndays)
!        alpha = 1.
        pers=rtgan + climo - climo_0
        print *, "changing?",iday,rtgan(4613),climm(4613),climo(4613),climo_0(4613),fgrid(4613)
        fgrid = (1.0-alpha)*pers + alpha*(fgrid-1.*(climm-climo))
        print *,"to",fgrid(4613)
!       print *, alpha,rtgan(4613),fgrid(4613)
!       write out as an analysis time series 
        read(300,*)kpds8,kpds9,kpds10,kpds21
!        print*,kpds8
!        print*,kpds9
!        print*,kpds10
!        print*,kpds21
        kpds(8)=  kpds8    !year
!        if(kpds8.eq.0)then
!         kpds(8)=100
!        endif 
        kpds(9)=  kpds9    !month
        kpds(10)= kpds10   !day of the month
        kpds(14) = 24*iday !0
        kpds(21) = kpds21
        call putgb(ounit,maxgrd,kpds,kgds,lbms,fgrid,iret)
!        print *,(kpds(j),j=8,11)
!
        if (iret.ne.0) then
            print *, 'error reading file=', iunit
            call exit(iret)
        endif
      enddo
      close(unit_climm)
      call baclose(iunit,iret)
      if(iret.ne.0) then
          print *,'error closing file=',iunit
          call exit(iret)
      endif
      call baclose(ounit,iret)
      if(iret.ne.0) then
          print *,'error closing file=',ounit
          call exit(iret)
      endif
      close(300)
!
800   format(2(2x,i4),2x,i6,2(2x,f8.3))
101   format(a50,2(i5),5(f10.4))
100   stop
      end


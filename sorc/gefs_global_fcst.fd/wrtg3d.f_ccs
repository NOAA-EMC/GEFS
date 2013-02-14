      SUBROUTINE WRTG3D(IOPROC,nog3d,nblck,ZHOUR,FHOUR,IDATE,colat1,
     &                 global_lats_r,lonsperlar,pl_coeff,secswr,sl,si)
      use resol_def
      use layout1
      use sig_io
      use namelist_def
      use d3d_def
      implicit none
!
!2008/10/08 HO-Chun Huang   Output only 3D fileds Specific for GFS-GOCART 
!                           Including DQ3DT, upd_mf, dwn_mf, det_mf, dkh, rnp
!
!!
      integer IOPROC, pl_coeff
      integer IDQ3(5+pl_coeff), idq3a(7), idq3b(9), ICC
      integer icmf(3), idkh, irnp
      data    idq3a/249, 243, 245, 217, 141, 142, 143/
      data    idq3b/249, 243, 245, 217, 141, 142, 143,139,140/
! rnp use large scale precipate = 62,  dkh use Velocity potential = 36
! dwn_mf=230, upd_mf=231, det_mf=236
!      data    icmf/230,231,236/, idkh /36/, irnp /62/, icc /213/
! table 133 dwn_mf=209, upd_mf=202, det_mf=219, rnp=134
! table 182 dkh   =182
      data    icmf/202,209,219/, idkh /182/, irnp /134/, icc /213/
!
      save idq3a, idq3b, icmf, idkh, irnp, icc
!
      integer   isglev, iavg, ifhour
      parameter(isglev=109, IAVG=3, IFHOUR=1)
      real(kind=kind_io4) wrkga(lonr*latr),wrkgb(lonr*latr)
!
      LOGICAL(1) LBM(LONR*LATR)
      CHARACTER G(200+LONR*LATR*(16+1)/8)
      INTEGER     IDATE(4), IDS(255)
      integer nv,il1,il2,j,i,k,itop,ibot,k4,l,nog3d
      integer ilpds,iyr,imo,ida,ihr,ifhr,ithr,lg,ierr
      real (kind=kind_io8) colat1,cl1,secswr,zhour,fhour,rtime,rtimsw
!
      real (kind=kind_io8)  SL(LEVS), si(levs+1)
C
      real (kind=kind_io8)   glolal(lonr,LATS_NODE_R)
      real (kind=kind_io8)   buffo(lonr,LATS_NODE_R)
      real (kind=kind_io4)   buff1(lonr,latr)
!!
      integer kmsk0(lonr*latr),nblck
      real (kind=kind_io8)   glolal1(lonr*latr)
      INTEGER  LATS_NODES_R(NODES)
      INTEGER  GLOBAL_LATS_R(LATR)
      INTEGER  LONSPERLAR(LATR)
      integer  lan,lat,lons_lat,iblk,il,lon,NJEFF
!
      if (pl_coeff .eq. 2) then
        idq3(1:7) = idq3a(1:7)
      else
        idq3(1:9) = idq3b(1:9)
      endif
!
      IDS=0
      G=' '
!
      kmsk0=0
      CALL IDSDEF(1,IDS)
!                     For the Ozone diagnostics
      ids(139) = 13
      ids(140) = 15
      ids(141) = 13
      ids(142) = 13
      ids(143) = 13
!
      IDS(217) = 10   ! This is for moisture change due to large-scale
!
      ids(202) = 5    ! Cumulus updraft massflux
      ids(209) = 5    ! Cumulus downdraft massflux
      ids(219) = 5    ! Cumulus detrainment massflux
!
      IDS(idkh) = 7   ! vertical diffusivity
      IDS(irnp) = 7   ! rain production rate
!
      ILPDS=28
      IF(ICEN2.EQ.2) ILPDS=45
      IENS(1)=1
      IENS(2)=IENST
      IENS(3)=IENSI
      IENS(4)=1
      IENS(5)=255
      IYR=IDATE(4)
      IMO=IDATE(2)
      IDA=IDATE(3)
      IHR=IDATE(1)
      IFHR=NINT(ZHOUR)
      ITHR=NINT(FHOUR)
      IF(FHOUR.GT.ZHOUR) THEN
        RTIME=1./(3600.*(FHOUR-ZHOUR))
      ELSE
        RTIME=0.
      ENDIF
      WRITE(*,'(''wrtg3d   : CHECK RTIME = '', E12.2, TR2, ''FHOUR = '',
     & F10.2, TR2, ''ZHOUR = '', F10.2)') RTIME, FHOUR, ZHOUR
      IF(SECSWR.GT.0.) THEN
        RTIMSW=1./SECSWR
      ELSE
        RTIMSW=1.
      ENDIF
      CL1=colat1
!..........................................................
!
!     Moisture tendencies -- use id 249
!     GFS-GOCART NEED THE SUM OF FIRST 4 - 249,243,245,217
!
!      do nv=1,5+pl_coeff
       do k=1,levs
         il1 = nint(sl(k) * 10000.0)
         il2 = il1
        glolal=0.
        do lan=1,LATS_NODE_R
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         iblk=0
         il=1
         DO lon=1,lons_lat,NGPTC
          NJEFF=MIN(NGPTC,lons_lat-lon+1)
          iblk=iblk+1
          do i=1,NJEFF
           do nv=1,4
             glolal(il,lan)=glolal(il,lan)+dq3dt(i,k,nv,iblk,lan)*rtime
           enddo
           il=il+1
          enddo
         enddo
        enddo
      CALL uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
         if(me.eq.ioproc) then
         call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IDQ3(1),ISGLEV,il1,il2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IDQ3(1)),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
         if(ierr.ne.0)print*,'wrtg3d gribit ierr=',ierr,'  ',
     &  ' K=',K
!    x '01)Moisture tendency (G/KG/s) '
         endif
         IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(nog3d,LG,G)
       enddo
!      enddo
!
!     Cloud Cover
!
      do k=1,levs
        il1 = nint(sl(k) * 10000.0)
        il2 = il1
        do j=1,LATS_NODE_R
          do i=1,lonr
!            glolal(i,j) = cldcov(k,i,j) * (rtimsw * 100.0)
            glolal(i,j) = cldcov(k,i,j) * rtimsw
          enddo
        enddo
      CALL uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICC,ISGLEV,il1,il2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICC),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrgt3d gribit ierr=',ierr,'  ',' K=',K
!    x '02) 3D cloud cover (fraction)                                 '
        endif

        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(nog3d,LG,G)
      enddo
!
! hchuang 03/07/08 Correct the icmf sequence icmf(1) is updraft
!                                            icmf(2) is downdraft
!
!     Convective Updraft Massflux    Table 133 parameter ID 202
!
       do k=1,levs
        il1 = nint(si(k) * 10000.0)
        il2 = il1
        glolal=0.
        do lan=1,LATS_NODE_R
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         iblk=0
         il=1
         DO lon=1,lons_lat,NGPTC
          NJEFF=MIN(NGPTC,lons_lat-lon+1)
          iblk=iblk+1
          do i=1,NJEFF
            glolal(il,lan) = upd_mf(i,k,iblk,lan)* RTIME
             il=il+1
          enddo
         enddo
        enddo
      CALL uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,133,ICEN,IGEN,
     &            0,ICMF(1),ISGLEV,il1,il2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(icmf(1)),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtg3d gribit ierr=',ierr,'  ',' K=',K
!    x '04)Convective Updraft Massflux (Pa/s) '
        endif
        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(nog3d,LG,G)
      enddo
!
!     Convective Downdraft Massflux     Table 133 parameter ID 209
!
       do k=1,levs
        il1 = nint(si(k) * 10000.0)
        il2 = il1
        glolal=0.
        do lan=1,LATS_NODE_R
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         iblk=0
         il=1
         DO lon=1,lons_lat,NGPTC
          NJEFF=MIN(NGPTC,lons_lat-lon+1)
          iblk=iblk+1
          do i=1,NJEFF
            glolal(il,lan) = dwn_mf(i,k,iblk,lan)* RTIME
             il=il+1
          enddo
         enddo
        enddo
      CALL uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,133,ICEN,IGEN,
     &            0,ICMF(2),ISGLEV,il1,il2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(icmf(2)),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtg3d gribit ierr=',ierr,'  ',' K=',K
!    x '03)Convective Downdraft Massflux (Pa/s) '
        endif

        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(nog3d,LG,G)
      enddo
!
!     Convective Detrainment Massflux      Table 133 parameter ID 219
!
       do k=1,levs
        il1 = nint(sl(k) * 10000.0)
        il2 = il1
        glolal=0.
        do lan=1,LATS_NODE_R
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         iblk=0
         il=1
         DO lon=1,lons_lat,NGPTC
          NJEFF=MIN(NGPTC,lons_lat-lon+1)
          iblk=iblk+1
          do i=1,NJEFF
            glolal(il,lan) = det_mf(i,k,iblk,lan)* RTIME
             il=il+1
          enddo
         enddo
        enddo
      CALL uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,133,ICEN,IGEN,
     &            0,ICMF(3),ISGLEV,il1,il2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(icmf(3)),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtg3d gribit ierr=',ierr,'  ',' K=',K
!    x '05)Convective Detrainment Massflux (Pa/s) '
        endif

        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(nog3d,LG,G)
      enddo
!
!     Vertical Diffusion Diffusivity      Table 129 parameter ID 182
!
       do k=1,levs
        il1 = nint(sl(k) * 10000.0)
        il2 = il1
        glolal=0.
        do lan=1,LATS_NODE_R
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         iblk=0
         il=1
         DO lon=1,lons_lat,NGPTC
          NJEFF=MIN(NGPTC,lons_lat-lon+1)
          iblk=iblk+1
          do i=1,NJEFF
            glolal(il,lan) = dkh(i,k,iblk,lan)* RTIME
             il=il+1
          enddo
         enddo
        enddo
      CALL uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,129,ICEN,IGEN,
     &            0,idkh,ISGLEV,il1,il2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(idkh),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtg3d gribit ierr=',ierr,'  ',' K=',K
!    x '06)Vertical Diffusion Diffusivity (m**2/s) '
        endif

        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(nog3d,LG,G)
      enddo
!
!     large-scale rain production rate in kg/kg/s
!     Table 133 parameter ID 134
!
       do k=1,levs
        il1 = nint(sl(k) * 10000.0)
        il2 = il1
        glolal=0.
        do lan=1,LATS_NODE_R
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         iblk=0
         il=1
         DO lon=1,lons_lat,NGPTC
          NJEFF=MIN(NGPTC,lons_lat-lon+1)
          iblk=iblk+1
          do i=1,NJEFF
            glolal(il,lan) = rnp(i,k,iblk,lan)* RTIME   ! unit in kg/kg/s
            if ( glolal(il,lan) < 0. ) glolal(il,lan) = 0.
             il=il+1
          enddo
         enddo
        enddo
      CALL uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,133,ICEN,IGEN,
     &            0,irnp,ISGLEV,il1,il2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(irnp),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtg3d gribit ierr=',ierr,'  ',' K=',K
!    x '07)3D LargeG-Scale Precip rate (kg/kg/s)'
        endif

        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(nog3d,LG,G)
      enddo
!
      RETURN
      END

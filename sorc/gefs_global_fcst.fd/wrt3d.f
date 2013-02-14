      SUBROUTINE WRT3D(IOPROC,no3d,ZHOUR,FHOUR,IDATE,colat1,
     &                 global_lats_r,lonsperlar,pl_coeff,
     &                 SECSWR,SECLWR,sl,si,slmsk,psurf)
      use resol_def
      use layout1
      use sig_io
      use namelist_def
      use d3d_def
      implicit none
!!
      integer IOPROC, pl_coeff
      integer IDT3(6), IDQ3(5+pl_coeff), ICC, npes, IDU3(4), IDV3(4)
     &,       idq3a(7), idq3b(9), icmf(3)
      data    idt3/251, 250, 246, 242, 244, 241/
      data    idq3a/249, 243, 245, 173, 174, 175, 188/
      data    idq3b/249, 243, 245, 173, 174, 175, 188,139,239/
      data    idu3/247, 181, 183, 196/
      data    idv3/248, 182, 184, 197/
      data    icc/213/, icmf/202,209,219/
!
      save idt3, idq3a,idq3b, icc, idu3, idv3, icmf
!
      integer   IPRS,ITEMP,IZNLW,IMERW,ISPHUM,IPWAT,
     $          IPCPR,ISNOWD,ICLDF,ICCLDF,
     $          ISLMSK,IZORL,IALBDO,ISOILM,ICEMSK,
     $          ILHFLX,ISHFLX,IZWS,IMWS,IGHFLX,
     $          IUSWFC,IDSWFC,IULWFC,IDLWFC,
     $          INSWFC,INLWFC,
     $          IDSWVB,IDSWVD,IDSWNB,IDSWND,
     $          ITMX,ITMN,IRNOF,IEP,
     &          ICLDWK,IZGW,IMGW,IHPBL,
     $          IDSWF,IDLWF,IUSWF,IULWF,ICPCPR,
     $          ISFC,ITOA,IELEV,
     $          ISGLEV,IDBLS,I2DBLS,ICOLMN,
     $          IBLBL,IBLTL,IBLLYR,
     $          ILCBL,ILCTL,ILCLYR,
     $          IMCBL,IMCTL,IMCLYR,
     $          IHCBL,IHCTL,IHCLYR,
     $          ICVBL,ICVTL,ICVLYR,
     $          INST,IWIN,IAVG,IACC,
     $          IFHOUR,IFDAY,
     $          latd
!
      PARAMETER(IPRS=1,ITEMP=11,IZNLW=33,IMERW=34,ISPHUM=51,IPWAT=54,
     $          IPCPR=59,ISNOWD=65,ICLDF=71,ICCLDF=72,
     $          ISLMSK=81,IZORL=83,IALBDO=84,ISOILM=144,ICEMSK=91,
     $          ILHFLX=121,ISHFLX=122,IZWS=124,IMWS=125,IGHFLX=155,
     $          IUSWFC=160,IDSWFC=161,IULWFC=162,IDLWFC=163,
     $          INSWFC=164,INLWFC=165,
     $          IDSWVB=166,IDSWVD=167,IDSWNB=168,IDSWND=169,
     $          ITMX=15,ITMN=16,IRNOF=90,IEP=145,
     &          ICLDWK=146,IZGW=147,IMGW=148,IHPBL=221,
     $          IDSWF=204,IDLWF=205,IUSWF=211,IULWF=212,ICPCPR=214)
      PARAMETER(ISFC=1,ITOA=8,IELEV=105,
     $          ISGLEV=107,IDBLS=111,I2DBLS=112,ICOLMN=200,
     $          IBLBL=209,IBLTL=210,IBLLYR=211,
     $          ILCBL=212,ILCTL=213,ILCLYR=214,
     $          IMCBL=222,IMCTL=223,IMCLYR=224,
     $          IHCBL=232,IHCTL=233,IHCLYR=234,
     $          ICVBL=242,ICVTL=243,ICVLYR=244)
      PARAMETER(INST=10,IWIN=2,IAVG=3,IACC=4)
      PARAMETER(IFHOUR=1,IFDAY=2)
      real(kind=kind_io4) wrkga(lonr*latr),wrkgb(lonr*latr)
!     real(kind=kind_io8) slmskful(lonr*latr)
      LOGICAL(1) LBM(LONR*LATR)
      CHARACTER G(200+LONR*LATR*(16+1)/8)
      INTEGER     IDATE(4), IDS(255)
!     INTEGER     IDATE(4), IDS(255),IENS(5)
         integer iens(5)
      integer nv,il1,il2,j,i,k,itop,ibot,k4,l,no3d
       integer ilpds,iyr,imo,ida,ihr,ifhr,ithr,lg,ierr
       REAL (kind=kind_io8) rtime,rtimsw,rtimlw
       real (kind=kind_io8) colat1
       real (kind=kind_io8) cl1,secswr,zhour,fhour,seclwr
!
      real (kind=kind_io8)  SL(LEVS), si(levs+1)
!
      real (kind=kind_io8)   glolal(lonr,LATS_NODE_R)
      real (kind=kind_io8)   buffo(lonr,LATS_NODE_R)
      real (kind=kind_io4)   buff1(lonr,latr)
!!
      integer kmsk0(lonr*latr)
!
      real (kind=kind_io8) psurf(LONR,LATS_NODE_R)
      real (kind=kind_io8) slmsk(LONR,LATS_NODE_R)
!
      real (kind=kind_io8)   glolal1(lonr*latr)
      INTEGER               LATS_NODES_R(NODES)
      INTEGER              GLOBAL_LATS_R(LATR)
      INTEGER                 LONSPERLAR(LATR)
      integer lan,lat,lons_lat,iblk,il,lon,njeff,gtn
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
!
      CALL IDSDEF(1,IDS)
!                     For the Ozone diagnostics
      ids(139) = 13
      ids(140) = 15
      ids(141) = 13
      ids(142) = 13
      ids(143) = 13
!                     For momentum diagnostics
      ids(181) = 7
      ids(182) = 7
      ids(183) = 7
      ids(184) = 7
!
      ids(196) = 7
      ids(197) = 7

!
      IDS(217) = 10   ! This is for moisture change due to large-scale
!
      ids(202) = 5    ! Cumulus updraft massflux
      ids(209) = 5    ! Cumulus downdraft massflux
      ids(219) = 5    ! Cumulus detrainment massflux
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
      IF(SECSWR.GT.0.) THEN
        RTIMSW=1./SECSWR
      ELSE
        RTIMSW=1.
      ENDIF
      IF(SECLWR.GT.0.) THEN
        RTIMLW=1./SECLWR
      ELSE
        RTIMLW=1.
      ENDIF
      CL1=colat1
!..........................................................
!     Temperature tendencies
!
      do nv=1,6
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
             glolal(il,lan)=dt3dt(i,k,nv,iblk,lan)* RTIME
             il=il+1
          enddo
         enddo
        enddo
      CALL uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
         if(me.eq.ioproc) then
         call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IDT3(nv),ISGLEV,il1,il2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IDT3(nv)),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
         if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' nv=',nv
     &,  ' K=',K
!    x '01)Temperature tendency (K/s) '
         endif
         IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(no3d,LG,G)
       enddo
      enddo
!
!     Moisture tendencies
!
      do nv=1,5+pl_coeff
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
             glolal(il,lan)=dq3dt(i,k,nv,iblk,lan)* RTIME
             il=il+1
          enddo
         enddo
        enddo
      CALL uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
         if(me.eq.ioproc) then
         gtn = 2
         if (nv > 3) gtn = 133
         call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,gtn,ICEN,IGEN,
     &            0,IDQ3(nv),ISGLEV,il1,il2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IDQ3(nv)),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
         if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' nv=',nv
     &,  ' K=',K
!    x '01)Moisture tendency (G/KG/s) '
         endif
         IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(no3d,LG,G)
       enddo
      enddo
!
!     zonal velocity (U)  tendencies
!
      do nv=1,4
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
             glolal(il,lan)=du3dt(i,k,nv,iblk,lan)* RTIME
             il=il+1
          enddo
         enddo
        enddo
      CALL uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
 
         if(me.eq.ioproc) then
           gtn = 2
           if (nv > 1) gtn = 133
         call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,gtn,ICEN,IGEN,
     &            0,IDU3(nv),ISGLEV,il1,il2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IDU3(nv)),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
         if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' nv=',nv
     &,  ' K=',K
!    x '01)Zonal compt of momentum tendency (m/s**2) '
         endif
         IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(no3d,LG,G)
       enddo
      enddo
!
!     meridional velocity (V)  tendencies
!
      do nv=1,4
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
             glolal(il,lan)=dv3dt(i,k,nv,iblk,lan)* RTIME
             il=il+1
          enddo
         enddo
        enddo
      CALL uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
         if(me.eq.ioproc) then
           gtn = 2
           if (nv > 1) gtn = 133
         call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,gtn,ICEN,IGEN,
     &            0,IDV3(nv),ISGLEV,il1,il2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IDV3(nv)),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
         if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' nv=',nv
     &,  ' K=',K
!    x '01)Meridional compt of momentum tendency (m/s**2) '
         endif
         IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(no3d,LG,G)
       enddo
      enddo
!
!     Cloud Cover
!
      do k=1,levs
        il1 = nint(sl(k) * 10000.0)
        il2 = il1
         do j=1,LATS_NODE_R
           do i=1,lonr
            glolal(i,j) = cldcov(k,i,j) * (RTIMSW * 100.0)
          enddo
        enddo
      CALL uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICC,ISGLEV,il1,il2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICC),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' K=',K
!    x '01)Zonal compt of momentum flux (N/m**2) land and sea surface '
        endif
 
        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(no3d,LG,G)
      enddo
!
!     Surface Pressure
!
      glolal=PSURF*1.E3
      CALL uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc)then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IPRS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IPRS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '38)Pressure (Pa) land and sea surface                         '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(no3d,LG,G)
!
      if (ras) then
!
!     Convective Updraft Massflux
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
        if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' K=',K
!    x '01)Convective Updraft Massflux (Kg/m**2) '
        endif

        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(no3d,LG,G)
      enddo
!
!     Convective Downdraft Massflux
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
        if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' K=',K
!    x '01)Convective Downdraft Massflux (Kg/m**2) '
        endif

        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(no3d,LG,G)
      enddo
!
!     Convective Detrainment Massflux
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
        if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' K=',K
!    x '01)Convective Detrainment Massflux (Kg/m**2) '
        endif

        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(no3d,LG,G)
      enddo
      endif
!
      RETURN
      END

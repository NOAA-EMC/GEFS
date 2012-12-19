      subroutine wrt3d_hyb(ioproc,no3d,zhour,fhour,idate,colat1,
     &                     global_lats_r,lonsperlar,pl_coeff,
     &                     secswr,seclwr,slmsk,psurf)
      use resol_def
      use layout1
      use sig_io
      use namelist_def
      use d3d_def
      implicit none
!!
      integer ioproc, pl_coeff
      integer idt3(6), idq3(5+pl_coeff), icc, npes, idu3(4), idv3(4)
     &,       idq3a(7), idq3b(9), icmf(3)
      data    idt3/251, 250, 246, 242, 244, 241/
      data    idq3a/249, 243, 245, 173, 174, 175, 188/
      data    idq3b/249, 243, 245, 173, 174, 175, 188,139,239/
      data    idu3/247, 181, 183, 196/
      data    idv3/248, 182, 184, 197/
      data    icc/213/, icmf/202,209,219/
!
      save idt3, idq3a, idq3b, icc, idu3, idv3, icmf
!
      integer   iprs,itemp,iznlw,imerw,isphum,ipwat,
     $          ipcpr,isnowd,icldf,iccldf,
     $          islmsk,izorl,ialbdo,isoilm,icemsk,
     $          ilhflx,ishflx,izws,imws,ighflx,
     $          iuswfc,idswfc,iulwfc,idlwfc,
     $          inswfc,inlwfc,
     $          idswvb,idswvd,idswnb,idswnd,
     $          itmx,itmn,irnof,iep,
     &          icldwk,izgw,imgw,ihpbl,
     $          idswf,idlwf,iuswf,iulwf,icpcpr,
     $          isfc,itoa,ielev,
     $          isglev,idbls,i2dbls,icolmn,
     $          iblbl,ibltl,ibllyr,
     $          ilcbl,ilctl,ilclyr,
     $          imcbl,imctl,imclyr,
     $          ihcbl,ihctl,ihclyr,
     $          icvbl,icvtl,icvlyr,
     $          inst,iwin,iavg,iacc,
     $          ifhour,ifday,
     $          latd
      parameter(iprs=1,itemp=11,iznlw=33,imerw=34,isphum=51,ipwat=54,
     $          ipcpr=59,isnowd=65,icldf=71,iccldf=72,
     $          islmsk=81,izorl=83,ialbdo=84,isoilm=144,icemsk=91,
     $          ilhflx=121,ishflx=122,izws=124,imws=125,ighflx=155,
     $          iuswfc=160,idswfc=161,iulwfc=162,idlwfc=163,
     $          inswfc=164,inlwfc=165,
     $          idswvb=166,idswvd=167,idswnb=168,idswnd=169,
     $          itmx=15,itmn=16,irnof=90,iep=145,
     &          icldwk=146,izgw=147,imgw=148,ihpbl=221,
     $          idswf=204,idlwf=205,iuswf=211,iulwf=212,icpcpr=214)
      parameter(isfc=1,itoa=8,ielev=105,
     $          isglev=109,idbls=111,i2dbls=112,icolmn=200,
     $          iblbl=209,ibltl=210,ibllyr=211,
     $          ilcbl=212,ilctl=213,ilclyr=214,
     $          imcbl=222,imctl=223,imclyr=224,
     $          ihcbl=232,ihctl=233,ihclyr=234,
     $          icvbl=242,icvtl=243,icvlyr=244)
      parameter(inst=10,iwin=2,iavg=3,iacc=4)
      parameter(ifhour=1,ifday=2)
      real(kind=kind_io4) wrkga(lonr*latr),wrkgb(lonr*latr)
!     real(kind=kind_io8) slmskful(lonr*latr)
      logical(1) lbm(lonr*latr)
      character g(200+lonr*latr*(16+1)/8)
      integer     idate(4), ids(255)
!     integer     idate(4), ids(255),iens(5)
      integer nv,il1,il2,j,i,k,itop,ibot,k4,l,no3d
       integer ilpds,iyr,imo,ida,ihr,ifhr,ithr,lg,ierr
       real (kind=kind_io8) rtime,rtimsw,rtimlw
       real (kind=kind_io8) colat1
       real (kind=kind_io8) cl1,secswr,zhour,fhour,seclwr
      real (kind=kind_io8)   glolal(lonr,lats_node_r)
      real (kind=kind_io8)   buffo(lonr,lats_node_r)
      real (kind=kind_io4)   buff1(lonr,latr)
!!
      real (kind=kind_io8) psurf(lonr,lats_node_r)
      real (kind=kind_io8) slmsk(lonr,lats_node_r)
      real (kind=kind_io8)   glolal1(lonr*latr)
      integer kmsk0(lonr*latr)
      integer               lats_nodes_r(nodes)
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
      integer lan,lat,lons_lat,iblk,il,lon,njeff,gtn
!
      if (pl_coeff .eq. 2) then
        idq3(1:7) = idq3a(1:7)
      else
        idq3(1:9) = idq3b(1:9)
      endif
!
      ids=0
      g=' '
!
      kmsk0=0
!
      call idsdef(1,ids)
!                     for the ozone diagnostics
      ids(139) = 13
      ids(140) = 15
      ids(141) = 13
      ids(142) = 13
      ids(143) = 13
!                     for momentum diagnostics
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
      ilpds=28
      if(icen2.eq.2) ilpds=45
      iens(1)=1
      iens(2)=ienst
      iens(3)=iensi
      iens(4)=1
      iens(5)=255
      iyr=idate(4)
      imo=idate(2)
      ida=idate(3)
      ihr=idate(1)
      ifhr=nint(zhour)
      ithr=nint(fhour)
      if(fhour.gt.zhour) then
        rtime=1./(3600.*(fhour-zhour))
      else
        rtime=0.
      endif
      if(secswr.gt.0.) then
        rtimsw=1./secswr
      else
        rtimsw=1.
      endif
      if(seclwr.gt.0.) then
        rtimlw=1./seclwr
      else
        rtimlw=1.
      endif
      cl1=colat1
!..........................................................
!     temperature tendencies
!
      do nv=1,6
       do k=1,levs
        il1 = k
        il2 = il1
        glolal=0.
        do lan=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         iblk=0
         il=1
         do lon=1,lons_lat,ngptc
          njeff=min(ngptc,lons_lat-lon+1)
          iblk=iblk+1
          do i=1,njeff
             glolal(il,lan)=dt3dt(i,k,nv,iblk,lan)* rtime
             il=il+1
          enddo
         enddo
        enddo
      call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
         if(me.eq.ioproc) then
         call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,idt3(nv),isglev,il1,il2,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(idt3(nv)),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
         if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' nv=',nv
     &,  ' k=',k
!    x '01)temperature tendency (k/s) '
         endif
         if(ierr.eq.0 .and. me.eq.ioproc) call wryte(no3d,lg,g)
       enddo
      enddo
!
!     moisture tendencies
!
      do nv=1,5+pl_coeff
       do k=1,levs
         il1 = k
         il2 = il1
        glolal=0.
        do lan=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         iblk=0
         il=1
         do lon=1,lons_lat,ngptc
          njeff=min(ngptc,lons_lat-lon+1)
          iblk=iblk+1
          do i=1,njeff
             glolal(il,lan)=dq3dt(i,k,nv,iblk,lan)* rtime
             il=il+1
          enddo
         enddo
        enddo
      call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
         if(me.eq.ioproc) then
         gtn = 2
         if (nv > 3) gtn = 133
         call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,gtn,icen,igen,
     &            0,idq3(nv),isglev,il1,il2,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(idq3(nv)),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
         if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' nv=',nv
     &,  ' k=',k
!    x '01)moisture tendency (g/kg/s) '
         endif
         if(ierr.eq.0 .and. me.eq.ioproc) call wryte(no3d,lg,g)
       enddo
      enddo
!
!     zonal velocity (u)  tendencies
!
      do nv=1,4
       do k=1,levs
        il1 = k
        il2 = il1
        glolal=0.
        do lan=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         iblk=0
         il=1
         do lon=1,lons_lat,ngptc
          njeff=min(ngptc,lons_lat-lon+1)
          iblk=iblk+1
          do i=1,njeff
             glolal(il,lan)=du3dt(i,k,nv,iblk,lan)* rtime
             il=il+1
          enddo
         enddo
        enddo
      call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
 
         if(me.eq.ioproc) then
           gtn = 2
           if (nv > 1) gtn = 133
         call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,gtn,icen,igen,
     &            0,idu3(nv),isglev,il1,il2,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(idu3(nv)),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
         if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' nv=',nv
     &,  ' k=',k
!    x '01)zonal compt of momentum tendency (m/s**2) '
         endif
         if(ierr.eq.0 .and. me.eq.ioproc) call wryte(no3d,lg,g)
       enddo
      enddo
!
!     meridional velocity (v)  tendencies
!
      do nv=1,4
       do k=1,levs
        il1 = k
        il2 = il1
        glolal=0.
        do lan=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         iblk=0
         il=1
         do lon=1,lons_lat,ngptc
          njeff=min(ngptc,lons_lat-lon+1)
          iblk=iblk+1
          do i=1,njeff
             glolal(il,lan)=dv3dt(i,k,nv,iblk,lan)* rtime
             il=il+1
          enddo
         enddo
        enddo
      call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
         if(me.eq.ioproc) then
           gtn = 2
           if (nv > 1) gtn = 133
         call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,gtn,icen,igen,
     &            0,idv3(nv),isglev,il1,il2,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(idv3(nv)),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
         if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' nv=',nv
     &,  ' k=',k
!    x '01)meridional compt of momentum tendency (m/s**2) '
         endif
         if(ierr.eq.0 .and. me.eq.ioproc) call wryte(no3d,lg,g)
       enddo
      enddo
!
!     cloud cover
!
      do k=1,levs
        il1 = k
        il2 = il1
         do j=1,lats_node_r
           do i=1,lonr
            glolal(i,j) = cldcov(k,i,j) * (rtimsw * 100.0)
          enddo
        enddo
      call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me.eq.ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,icc,isglev,il1,il2,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icc),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
        if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' k=',k
!    x '01)zonal compt of momentum flux (n/m**2) land and sea surface '
        endif
 
        if(ierr.eq.0 .and. me.eq.ioproc) call wryte(no3d,lg,g)
      enddo
!
!     surface pressure
!
      glolal=psurf*1.e3
      call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc)then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,iprs,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(iprs),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '38)pressure (pa) land and sea surface                         '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(no3d,lg,g)
!
      if (ras) then
!
!     Convective Updraft Massflux
!
       do k=1,levs
        il1 = k
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
        il1 = k
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
        il1 = k
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
      return
      end

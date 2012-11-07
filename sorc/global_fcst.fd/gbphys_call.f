      subroutine gbphys_call(solhr,clstp,nsphys,
     x     syn_gr_r_2,dyn_gr_r_2,anl_gr_r_2,                            ! hmhj
     &     dtp,dtf,xkt2, sfc_fld, flx_fld, nsst_fld, sfalb ,
                                 ! nsst_fld contians  sub-layer cooling and
                                 ! diurnal warming in ocean (InOut), XL Jun07
     &     acv,acvb,acvt,
     &     swh,hlw,hprime,slag,sdec,cdec,ozplout,
     &     phy_f3d, phy_f2d,nblck,kdt,
     &     njeff,istrt,iblk,lat,lon_dim,lons_lat,lan,
     &     xlon, xlat)
!!
      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use coordinate_def                                                ! hmhj
      use tracer_const	                                                ! hmhj
      use module_ras , only : ras_init
      use physcons, fv => con_fvirt
     &,             RD => con_RD, RVRDM1 => con_FVirt, G=>con_g       !cpl insertion
      use ozne_def
      use Sfc_Flx_ESMFMod
      use Nsstm_ESMFMod
      use d3d_def

!-> Coupling insertion
      USE SURFACE_cc
!<- Coupling insertion

      implicit none
      include 'mpif.h'
!
      TYPE(Sfc_Var_Data)        :: sfc_fld
      TYPE(Flx_Var_Data)        :: flx_fld
      TYPE(Nsst_Var_Data)       :: nsst_fld
!
      real, PARAMETER:: RLAPSE=0.65E-2
!
      integer njeff,istrt,lon,iblk,kdt,nblck
!!
      real(kind=kind_phys)    prsl(ngptc,levs)
!Moor real(kind=kind_phys)    prsl(ngptc,levs),prdel(ngptc,levs)
      real(kind=kind_phys)   prslk(ngptc,levs),dpshc(ngptc)
!     real(kind=kind_phys)   prslk(ngptc,levs),prsshc    ! pry
      real(kind=kind_phys)    prsi(ngptc,levs+1),phii(ngptc,levs+1)
      real(kind=kind_phys)   prsik(ngptc,levs+1),phil(ngptc,levs)
!!
      real (kind=kind_rad) ugr(ngptc,levs),vgr(ngptc,levs)
      real (kind=kind_rad) gphi(ngptc),glam(ngptc)
      real (kind=kind_rad) gq(ngptc),gt(ngptc,levs)
      real (kind=kind_rad) gtv(ngptc,levs)                              ! hmhj
      real (kind=kind_rad) gtvx(ngptc,levs),gtvy(ngptc,levs)            ! hmhj
      real (kind=kind_rad) sumq(ngptc,levs),xcp(ngptc,levs)             ! hmhj
      real (kind=kind_rad) gr(ngptc,levs,ntrac)
      real (kind=kind_rad) gd(ngptc,levs)
      real (kind=kind_rad) adt(ngptc,levs),adr(ngptc,levs,ntrac)
      real (kind=kind_rad) adu(ngptc,levs),adv(ngptc,levs)
!!
      real (kind=kind_evod) slag,sdec,cdec
      real (kind=kind_rad) xlon(lonr,lats_node_r)
     &,                    xlat(lonr,lats_node_r)
     &,    hprime(nmtvr,lonr,lats_node_r), sfalb(lonr,lats_node_r)

      real (kind=kind_rad)  swh(ngptc,levs,nblck,lats_node_r)
      real (kind=kind_rad)  hlw(ngptc,levs,nblck,lats_node_r)
!!
      real (kind=kind_phys)
     &     phy_f3d(ngptc,levs,nblck,lats_node_r,num_p3d),
     &     phy_f2d(lonr,lats_node_r,num_p2d)
!
      real (kind=kind_phys) xkt2(lonr,lats_node_r,nrcm),
     &                      vvel(ngptc,levs)
      real (kind=kind_phys) dtp,dtf
!!
cc
c$$$      integer                lots,lota
c$$$cc
c$$$      parameter            ( lots = 5*levs+1*levh+3 )
c$$$      parameter            ( lotd = 6*levs+2*levh+0 )       ! hmhj
c$$$      parameter            ( lota = 3*levs+1*levh+1 )
cc
      real(kind=kind_evod) syn_gr_r_2(lonrx*lots,lats_dim_r)
      real(kind=kind_evod) dyn_gr_r_2(lonrx*lotd,lats_dim_r)    ! hmhj
      real(kind=kind_evod) anl_gr_r_2(lonrx*lota,lats_dim_r)
cc
      integer              i,j,k,kap,kar,kat,kau,kav
      integer              ksd,ksq,ksr,kst,ksplam,kspphi
      integer              kdtphi,kdtlam,kss                    ! hmhj
      integer              ksu,ksv,ksz,l,lan,lat,lmax,locl
      integer              lon_dim,lons_lat,n,node
      integer nsphys, iz
!
      real(kind=kind_evod) solhr,clstp
cc
c$$$      parameter(ksq     =0*levs+0*levh+1,
c$$$     x          ksplam  =0*levs+0*levh+2,
c$$$     x          kspphi  =0*levs+0*levh+3,
c$$$     x          ksu     =0*levs+0*levh+4,
c$$$     x          ksv     =1*levs+0*levh+4,
c$$$     x          ksz     =2*levs+0*levh+4,
c$$$     x          ksd     =3*levs+0*levh+4,
c$$$     x          kst     =4*levs+0*levh+4,
c$$$     x          ksr     =5*levs+0*levh+4)
c$$$cc
c$$$      parameter(kdtphi  =0*levs+0*levh+1,
c$$$     x          kdrphi  =1*levs+0*levh+1,
c$$$     x          kdtlam  =1*levs+1*levh+1,
c$$$     x          kdrlam  =2*levs+1*levh+1,
c$$$     x          kdulam  =2*levs+2*levh+1,
c$$$     x          kdvlam  =3*levs+2*levh+1,
c$$$     x          kduphi  =4*levs+2*levh+1,
c$$$     x          kdvphi  =5*levs+2*levh+1)
c$$$cc
c$$$      parameter(kau     =0*levs+0*levh+1,
c$$$     x          kav     =1*levs+0*levh+1,
c$$$     x          kat     =2*levs+0*levh+1,
c$$$     x          kar     =3*levs+0*levh+1,
c$$$     x          kap     =3*levs+1*levh+1)
c$$$cc
c$$$cc
c$$$      integer   p_gz,p_zem,p_dim,p_tem,p_rm,p_qm
c$$$      integer   p_ze,p_di,p_te,p_rq,p_q,p_dlam,p_dphi,p_uln,p_vln
c$$$      integer   p_w,p_x,p_y,p_rt,p_zq
c$$$cc
c$$$cc                                               old common /comfspec/
c$$$      parameter(p_gz  = 0*levs+0*levh+1,  !      gze/o(lnte/od,2),
c$$$     x          p_zem = 0*levs+0*levh+2,  !     zeme/o(lnte/od,2,levs),
c$$$     x          p_dim = 1*levs+0*levh+2,  !     dime/o(lnte/od,2,levs),
c$$$     x          p_tem = 2*levs+0*levh+2,  !     teme/o(lnte/od,2,levs),
c$$$     x          p_rm  = 3*levs+0*levh+2,  !      rme/o(lnte/od,2,levh),
c$$$     x          p_qm  = 3*levs+1*levh+2,  !      qme/o(lnte/od,2),
c$$$     x          p_ze  = 3*levs+1*levh+3,  !      zee/o(lnte/od,2,levs),
c$$$     x          p_di  = 4*levs+1*levh+3,  !      die/o(lnte/od,2,levs),
c$$$     x          p_te  = 5*levs+1*levh+3,  !      tee/o(lnte/od,2,levs),
c$$$     x          p_rq  = 6*levs+1*levh+3,  !      rqe/o(lnte/od,2,levh),
c$$$     x          p_q   = 6*levs+2*levh+3,  !       qe/o(lnte/od,2),
c$$$     x          p_dlam= 6*levs+2*levh+4,  !  dpdlame/o(lnte/od,2),
c$$$     x          p_dphi= 6*levs+2*levh+5,  !  dpdphie/o(lnte/od,2),
c$$$     x          p_uln = 6*levs+2*levh+6,  !     ulne/o(lnte/od,2,levs),
c$$$     x          p_vln = 7*levs+2*levh+6,  !     vlne/o(lnte/od,2,levs),
c$$$     x          p_w   = 8*levs+2*levh+6,  !       we/o(lnte/od,2,levs),
c$$$     x          p_x   = 9*levs+2*levh+6,  !       xe/o(lnte/od,2,levs),
c$$$     x          p_y   =10*levs+2*levh+6,  !       ye/o(lnte/od,2,levs),
c$$$     x          p_rt  =11*levs+2*levh+6,  !      rte/o(lnte/od,2,levh),
c$$$     x          p_zq  =11*levs+3*levh+6)  !      zqe/o(lnte/od,2)
!!
      integer nlons_v(ngptc)
      real(kind=kind_evod) sinlat_v(ngptc),coslat_v(ngptc),rcs2_v(ngptc)
      real(kind=kind_evod) smc_v(ngptc,lsoil),stc_v(ngptc,lsoil)
!lu [+1L]: add slc_v
     +,                    slc_v(ngptc,lsoil)
      real(kind=kind_evod) hprime_v(ngptc,nmtvr)
      real(kind=kind_evod) phy_f3dv(ngptc,LEVS,num_p3d),
     &                     phy_f2dv(ngptc,num_p2d)
     &,                    rannum(ngptc,nrcm)
!!
!!
      logical, parameter :: flipv = .true.
!!
      integer ncrnd,kk
      real ozplout(levozp,lats_node_r,pl_coeff),
     &     ozplout_v(ngptc,levozp,pl_coeff)
!!
        real(kind=kind_phys)  acv(lonr,lats_node_r)
        real(kind=kind_phys) acvb(lonr,lats_node_r)
        real(kind=kind_phys) acvt(lonr,lats_node_r)
!!mjr
      real (kind=kind_rad) work1, qmin, tem
      parameter (qmin=1.0e-10)
!!mjr
!
!     print *,' in gbphys.call vertcoord_id =',vertcoord_id
!
      ksq     =0*levs+0*levh+1
      ksplam  =0*levs+0*levh+2
      kspphi  =0*levs+0*levh+3
      ksu     =0*levs+0*levh+4
      ksv     =1*levs+0*levh+4
      ksz     =2*levs+0*levh+4
      ksd     =3*levs+0*levh+4
      kst     =4*levs+0*levh+4
      ksr     =5*levs+0*levh+4

      kdtphi  =0*levs+0*levh+1                  ! hmhj
      kdtlam  =1*levs+1*levh+1                  ! hmhj

      kau     =0*levs+0*levh+1
      kav     =1*levs+0*levh+1
      kat     =2*levs+0*levh+1
      kar     =3*levs+0*levh+1
      kap     =3*levs+1*levh+1
!
! p in cb by finite difference from henry juang not ln(p)               ! hmhj
      if(.not.gen_coord_hybrid) then                                    ! hmhj
        do j=1,njeff
              syn_gr_r_2(istrt+j-1+(ksq-1)*lon_dim,lan)=
     &    exp(syn_gr_r_2(istrt+j-1+(ksq-1)*lon_dim,lan))
        enddo

      endif     ! .not.gen_coord_hybrid                                 ! hmhj

!!
! for omega in gen_coord_hybrid                                         ! hmhj
! the same variables for thermodyn_id=3 for enthalpy                    ! hmhj
      if( gen_coord_hybrid ) then                                       ! hmhj
      do k=1,levs                                                       ! hmhj
      do j=1,njeff                                                      ! hmhj
            gtv(j,k)=                                                   ! hmhj
     &      syn_gr_r_2(istrt+j-1+(kst-1)*lon_dim+lon_dim*(k-1),lan)     ! hmhj
      enddo                                                             ! hmhj
      enddo                                                             ! hmhj
! ------
      if( vertcoord_id.eq.3. ) then
      do k=1,levs                                                       ! hmhj
      do j=1,njeff                                                      ! hmhj
            gtvx(j,k)=                                                  ! hmhj
     &      dyn_gr_r_2(istrt+j-1+(kdtlam-1)*lon_dim+lon_dim*(k-1),lan)  ! hmhj
            gtvy(j,k)=                                                  ! hmhj
     &      dyn_gr_r_2(istrt+j-1+(kdtphi-1)*lon_dim+lon_dim*(k-1),lan)  ! hmhj
      enddo                                                             ! hmhj
      enddo                                                             ! hmhj
      endif     ! vertcoord_id=3
! ------

      endif     ! gen_coord_hybrid                                      ! hmhj
!!
!
      if( gen_coord_hybrid .and. thermodyn_id.eq.3 ) then		! hmhj

! get dry temperature from enthalpy					! hmhj
        sumq=0.0							! hmhj
        xcp=0.0								! hmhj
        do i=1,ntrac							! hmhj
         if( cpi(i).ne.0.0 ) then					! hmhj
         kss=ksr+(i-1)*levs						! hmhj
         do k=1,levs							! hmhj
          do j=1,njeff							! hmhj
            work1=							! hmhj 
     &      syn_gr_r_2(istrt+j-1+(kss-1)*lon_dim+lon_dim*(k-1),lan)   	! hmhj
            sumq(j,k)=sumq(j,k)+work1					! hmhj
            xcp(j,k)=xcp(j,k)+cpi(i)*work1				! hmhj
          enddo								! hmhj
         enddo								! hmhj
         endif								! hmhj
        enddo								! hmhj
        do k=1,levs							! hmhj
         do j=1,njeff							! hmhj
           work1=(1.-sumq(j,k))*cpi(0)+xcp(j,k)				! hmhj
           syn_gr_r_2(istrt+j-1+(kst-1)*lon_dim+lon_dim*(k-1),lan)=	! hmhj
     &     syn_gr_r_2(istrt+j-1+(kst-1)*lon_dim+lon_dim*(k-1),lan)/work1! hmhj
         enddo								! hmhj
        enddo								! hmhj
        							
      else								! hmhj

! get dry temperture from virtual temperature				! hmhj
      do k=1,levs
      do j=1,njeff
           work1 = 1.0 + fv * 
     & max(syn_gr_r_2(istrt+j-1+(ksr-1)*lon_dim+lon_dim*(k-1),lan),qmin)
!!
           syn_gr_r_2(istrt+j-1+(kst-1)*lon_dim+lon_dim*(k-1),lan)=
     &     syn_gr_r_2(istrt+j-1+(kst-1)*lon_dim+lon_dim*(k-1),lan)/work1
      enddo
      enddo

      endif								! hmhj
!!mjr
c$$$      do lan=1,lats_node_r
c$$$
c$$$         lat = global_lats_r(ipt_lats_node_r-1+lan)
c$$$
c$$$         lon_dim = lon_dims_r(lan)
c$$$
c$$$         lons_lat = lonsperlar(lat)
c$$$         pwatp=0.
c$$$!$omp parallel do private(lon,
c$$$!$omp+reduction(+:pwatp)
c$$$         do lon=1,lons_lat,ngptc
c$$$
c$$$          njeff=min(ngptc,lons_lat-lon+1) 
c$$$          istrt=lon
c$$$          if (ngptc.ne.1) then
c$$$            iblk=lon/ngptc+1
c$$$          else
c$$$            iblk=lon
c$$$          endif
          do k=1,levs
          do j=1,njeff
            ugr(j,k)=
     &      syn_gr_r_2(istrt+j-1+(ksu-1)*lon_dim+lon_dim*(k-1),lan)
            vgr(j,k)=
     &      syn_gr_r_2(istrt+j-1+(ksv-1)*lon_dim+lon_dim*(k-1),lan)
            gd(j,k)=
     &      syn_gr_r_2(istrt+j-1+(ksd-1)*lon_dim+lon_dim*(k-1),lan)
            gt(j,k)=
     &      syn_gr_r_2(istrt+j-1+(kst-1)*lon_dim+lon_dim*(k-1),lan)

          enddo
          enddo
          do kk=1,ntrac
          do k=1,levs
          do j=1,njeff

            gr(j,k,kk)=
     &  syn_gr_r_2(istrt+j-1+(ksr+(kk-1)*levs-1)*lon_dim
     &             +lon_dim*(k-1),lan)

          enddo
          enddo
          enddo

!!
          do j=1,njeff
            gq(j)=syn_gr_r_2(istrt+j-1+(ksq-1)*lon_dim,lan)
            gphi(j)=syn_gr_r_2(istrt+j-1+(kspphi-1)*lon_dim,lan)
            glam(j)=syn_gr_r_2(istrt+j-1+(ksplam-1)*lon_dim,lan)
          enddo
!!
!                       hmhj for gen_coord_hybrid
      if( gen_coord_hybrid ) then                                   ! hmhj
        call hyb2press_gc(njeff,ngptc,gq, gtv, prsi,prsl            ! hmhj
     &,                                          prsik, prslk)
        call omegtes_gc(njeff,ngptc,rcs2_r(min(lat,latr-lat+1)),    ! hmhj
     &               gq,gphi,glam,gtv,gtvx,gtvy,gd,ugr,vgr,vvel)    ! hmhj
      else if( hybrid )then                                         ! hmhj
        call  hyb2press(njeff,ngptc,gq, prsi, prsl,prsik,prslk)
        call omegtes(njeff,ngptc,rcs2_r(min(lat,latr-lat+1)),
     &               gq,gphi,glam,gd,ugr,vgr,vvel)
      else
        call  sig2press(njeff,ngptc,gq,sl,si,slk,sik,
     &                                    prsi,prsl,prsik,prslk)
        call omegast3(njeff,ngptc,levs,
     &              gphi,glam,ugr,vgr,gd,del,
     &              rcs2_r(min(lat,latr-lat+1)),vvel,gq,sl)
      endif
!!
!     dpshc=40.
!     dpshc=30.
!     prsshc=70.                         ! for pry
      do i=1,ngptc
        phil(i,levs)  = 0.0 ! will force calculation of geopotential in gbphys.
 !      prslk(i,1)    = 0.0 ! will force calculation of geopotential in gbphys.
 !      prsik(i,1)    = 0.0 ! will force calculation of geopotential in gbphys.
      enddo
      if (gen_coord_hybrid .and. thermodyn_id == 3) then
        do i=1,ngptc
          prslk(i,1) = 0.0 ! will force calculation of geopotential in gbphys.
          prsik(i,1) = 0.0 ! will force calculation of geopotential in gbphys.
        enddo
      endif
      do i=1,njeff
        dpshc(i) = 0.3 * prsi(i,1)
!       dpshc(i) = 0.40 * prsi(i,1)
      enddo
      nlons_v  = lons_lat
      sinlat_v = sinlat_r(lat)
      coslat_v = coslat_r(lat)
      rcs2_v   = rcs2_r(min(lat,latr-lat+1))

      if (ntoz .gt. 0) then
        do j=1,pl_coeff
          do k=1,levozp
            do i=1,ngptc
              ozplout_v(i,k,j) = ozplout(k,lan,j)
            enddo
          enddo
        enddo
      endif

      do k=1,lsoil
        do i=1,njeff
          smc_v(i,k) = sfc_fld%smc(k,istrt+i-1,lan)
          stc_v(i,k) = sfc_fld%stc(k,istrt+i-1,lan)
          slc_v(i,k) = sfc_fld%slc(k,istrt+i-1,lan)
        enddo
      enddo
      do k=1,nmtvr
        do i=1,njeff
          hprime_v(i,k) = hprime(k,istrt+i-1,lan)
        enddo
      enddo
!!
      do j=1,num_p3d
        do k=1,levs
          do i=1,njeff
            phy_f3dv(i,k,j) = phy_f3d(i,k,iblk,lan,j)
          enddo
        enddo
      enddo
      do j=1,num_p2d
        do i=1,njeff
          phy_f2dv(i,j) = phy_f2d(istrt+i-1,lan,j)
        enddo
      enddo
      do j=1,nrcm
        do i=1,njeff
          rannum(i,j) = xkt2(istrt+i-1,lan,j)
        enddo
      enddo
!
!     if (me .eq. 0)
!    & print *,' calling gbphys njeff=',njeff,' ntrac=',ntrac,' ncld=',
!    & ncld,' ntoz=',ntoz,' ntcw=',ntcw,' me=',me, ' lan=',lan
!    &,' TREF in gbphys_call=',nsst_fld%tref(istrt,lan)
!    &,'trans_trac=',trans_trac
!    &,' prsig=',prsi(1,1:5)
!    &,' kdt=',kdt,' lat=',lat
!    &,' xlon=',(xlon(istrt+i-1,lan),i=1,njeff),' kdt=',kdt
!     if (me .eq. 1) print *,' slope_main2=',slope(61,1),' slmsk='
!    &,slmsk(61,1)
!     if (me .eq. 18) print *,' istrt=',istrt,' sfalb=',
!    &  sfalb(istrt,lan),' lan=',lan

            call gbphys(njeff,ngptc,levs,lsoil,lsm,ntrac,ncld,
     &      ntoz,ntcw,nmtvr,lonr,latr,jcap,ras,nlons_v,rannum,nrcm,
     &      pre_rad,
!    &      ntoz,ntcw,nmtvr,lonr,latr,jcap,ras,nlons_v,xkt2(istrt,lan), 
     &      ugr,vgr,gq,gt,gr,vvel,
     &      adt,adr,adu,adv,
     &      sinlat_v,coslat_v,rcs2_v,
     &      prsi,prsl,prslk,prsik,phii,phil,dpshc,fhour,lssav,solhr,
!    &      prsi,prsl,prslk,prsik,phii,phil,prsshc,fhour,lssav,solhr,  ! for pry
     &      lsfwd,clstp,dtp,dtf,
     &      pl_pres,ozplout_v,levozp,pl_coeff,

! The beginning for sub-layer and diurnal warming in ocean (InOut), XL Jun07
     &      nsst_active,
     &      nsst_fld%ifd(istrt,lan),      nsst_fld%time_old(istrt,lan),
     &      nsst_fld%time_ins(istrt,lan), nsst_fld%I_Sw(istrt,lan),
     &      nsst_fld%I_Q(istrt,lan),      nsst_fld%I_Qrain(istrt,lan),
     &      nsst_fld%I_M(istrt,lan),      nsst_fld%I_Tau(istrt,lan),
     &      nsst_fld%I_Sw_Zw(istrt,lan),  nsst_fld%I_Q_Ts(istrt,lan),
     &      nsst_fld%I_M_Ts(istrt,lan),   nsst_fld%Tref(istrt,lan),
     &      nsst_fld%dt_cool(istrt,lan),  nsst_fld%z_c(istrt,lan),
     &      nsst_fld%dt_warm(istrt,lan),  nsst_fld%z_w(istrt,lan),
     &      nsst_fld%c_0(istrt,lan),      nsst_fld%c_d(istrt,lan),
     &      nsst_fld%w_0(istrt,lan),      nsst_fld%w_d(istrt,lan),
! The end for sub-layer and diurnal warming in ocean (InOut), XL Jun07

     &      sfc_fld%hice(istrt,lan),sfc_fld%fice(istrt,lan),
     &      sfc_fld%tisfc(istrt,lan),flx_fld%sfcdsw(istrt,lan),        ! SEA-ICE - Nov04  !cpl: add dswsfc
!lu [+3L]: add (tprcp,srflag),(slc_v,snwdph,slope,shdmin,shdmax,snoalb),sfalb
     +      sfc_fld%tprcp(istrt,lan),sfc_fld%srflag(istrt,lan),
     +      slc_v,sfc_fld%snwdph(istrt,lan),sfc_fld%slope(istrt,lan),
     &       sfc_fld%shdmin(istrt,lan),
     +      sfc_fld%shdmax(istrt,lan),sfc_fld%snoalb(istrt,lan),
     &      sfalb(istrt,lan),
!wei added 10/24/2006
     +     flx_fld%chh(istrt,lan),flx_fld%cmm(istrt,lan),
     +     flx_fld%epi(istrt,lan),flx_fld%dlwsfci(istrt,lan),
     +     flx_fld%ulwsfci(istrt,lan),flx_fld%uswsfci(istrt,lan),
     +     flx_fld%dswsfci(istrt,lan),flx_fld%dtsfci(istrt,lan),
     +     flx_fld%dqsfci(istrt,lan),flx_fld%gfluxi(istrt,lan),
     &     flx_fld%srunoff(istrt,lan),
     +     flx_fld%t1(istrt,lan),flx_fld%q1(istrt,lan),
     +     flx_fld%u1(istrt,lan),flx_fld%v1(istrt,lan),
     +     flx_fld%zlvl(istrt,lan),flx_fld%evbsa(istrt,lan),
     +     flx_fld%evcwa(istrt,lan),flx_fld%transa(istrt,lan),
     +     flx_fld%sbsnoa(istrt,lan),flx_fld%snowca(istrt,lan),
     +     flx_fld%soilm(istrt,lan), flx_fld%snohfa(istrt,lan),
     +     flx_fld%smcwlt2(istrt,lan),flx_fld%smcref2(istrt,lan),

!hchuang code change [+3L] 11/12/2007 : add 2D
     &     flx_fld%gsoil(istrt,lan),  flx_fld%gtmp2m(istrt,lan),
     &     flx_fld%gustar(istrt,lan), flx_fld%gpblh(istrt,lan),
     &     flx_fld%gu10m(istrt,lan),  flx_fld%gv10m(istrt,lan),
     &     flx_fld%gzorl(istrt,lan),  flx_fld%goro(istrt,lan),

     &     sfc_fld%tsea(istrt,lan),sfc_fld%sheleg(istrt,lan),
     &     sfc_fld%sncovr(istrt,lan),sfc_fld%tg3(istrt,lan),
     &     sfc_fld%zorl(istrt,lan),sfc_fld%cv(istrt,lan),
     &     sfc_fld%cvb(istrt,lan),sfc_fld%cvt(istrt,lan),
     &     sfc_fld%slmsk(istrt,lan),sfc_fld%vfrac(istrt,lan),
     &     sfc_fld%canopy(istrt,lan),
     &     sfc_fld%f10m(istrt,lan),sfc_fld%vtype(istrt,lan),
     &     sfc_fld%stype(istrt,lan),sfc_fld%uustar(istrt,lan),
     &     sfc_fld%ffmm(istrt,lan),sfc_fld%ffhh(istrt,lan),
     &     flx_fld%tmpmin(istrt,lan),flx_fld%tmpmax(istrt,lan),
!jwang add spfhmin/spfhmax
     &     flx_fld%spfhmin(istrt,lan),flx_fld%spfhmax(istrt,lan),
     &     flx_fld%geshem(istrt,lan),
     &     flx_fld%dusfc(istrt,lan) ,flx_fld%dvsfc(istrt,lan) ,
     &     flx_fld%dtsfc(istrt,lan) ,
     &     flx_fld%dqsfc(istrt,lan),flx_fld%dlwsfc(istrt,lan),
!yth add suntim as output. 3/08
!    &     flx_fld%ulwsfc(istrt,lan),
     &     flx_fld%ulwsfc(istrt,lan),flx_fld%suntim(istrt,lan),
     &     flx_fld%gflux(istrt,lan),flx_fld%runoff(istrt,lan),
     &     flx_fld%ep(istrt,lan),
     &     flx_fld%cldwrk(istrt,lan),flx_fld%dugwd(istrt,lan),
     &     flx_fld%dvgwd(istrt,lan),flx_fld%psmean(istrt,lan),
     &     flx_fld%bengsh(istrt,lan),
     &     xlon(istrt,lan),flx_fld%coszen(istrt,lan),
     &     flx_fld%sfcnsw(istrt,lan),
Clu_q2m [+1L]: add xlat
     +     xlat(istrt,lan),
     &     flx_fld%sfcdlw(istrt,lan),flx_fld%tsflw(istrt,lan) ,
     &     flx_fld%psurf(istrt,lan),flx_fld%u10m(istrt,lan),
     &     flx_fld%v10m(istrt,lan),
     &     sfc_fld%t2m(istrt,lan),sfc_fld%q2m(istrt,lan),
     &     flx_fld%hpbl(istrt,lan),
     &     flx_fld%pwat(istrt,lan),swh(1,1,iblk,lan),hlw(1,1,iblk,lan),
     &     smc_v,stc_v,hprime_v,
     &     slag,sdec,cdec,
     &     acv(istrt,lan),acvb(istrt,lan),acvt(istrt,lan),
     &     phy_f3dv,phy_f2dv,num_p3d,num_p2d,flgmin,

     &     dt3dt(1,1,1,iblk,lan), dq3dt(1,1,1,iblk,lan),
     &     du3dt(1,1,1,iblk,lan), dv3dt(1,1,1,iblk,lan),
     &     upd_mf(1,1,iblk,lan),  dwn_mf(1,1,iblk,lan),
     &     det_mf(1,1,iblk,lan),  dkh(1,1,iblk,lan),
     &     rnp(1,1,iblk,lan),     ldiag3d, lggfs3d,

     &     flipv,me,kdt,lat,sfc_fld%oro(istrt,lan),
! Coupling deletion->
!    &     crtrh, ncw, old_monin)                                     ! hmhj
!<-Coupling deletion
! Coupling insertion->
     &     crtrh, ncw,  old_monin, cnvgwd, ccwf, ctei_rm,
     &     sashal, newsas, mom4ice, mstrat, trans_trac, cal_pre,
     > lssav_cc,
     > DLWSFC_cc(istrt,lan),ULWSFC_cc(istrt,lan),SWSFC_cc(istrt,lan),
     > XMU_cc(istrt,lan),DLW_cc(istrt,lan),DSW_cc(istrt,lan),         !cpl insertion dlw_cc
     > SNW_cc(istrt,lan),LPREC_cc(istrt,lan),         !cpl insertion dlw_cc
     > DUSFC_cc(istrt,lan),DVSFC_cc(istrt,lan),DTSFC_cc(istrt,lan),
     > DQSFC_cc(istrt,lan),PRECR_cc(istrt,lan))
!<-Coupling insertion
!           gen_coord_hybrid)           				! hmhj
!    &      flipv,me,kdt,lat,psthk)
!    &       rannum(1,nsphys),flipv,ncrnd,me,kdt)
c$$$     &       rannum(1,nsphys),flipv,ncrnd,me,kdt,lat,istrt)

!!
!<-- cpl insertion: instantanious variables
      U_BOT_cc(istrt:istrt+njeff-1,lan)= adu(1:njeff,1)
      V_BOT_cc(istrt:istrt+njeff-1,lan)= adv(1:njeff,1)
      Q_BOT_cc(istrt:istrt+njeff-1,lan)= adr(1:njeff,1,1)
      P_BOT_cc(istrt:istrt+njeff-1,lan)= prsl(1:njeff,1)*1000.
      P_SURF_cc(istrt:istrt+njeff-1,lan)= prsi(1:njeff,1)*1000.

      do iz=1,njeff
        T_BOT_cc(istrt+iz-1,lan)= adt(iz,1)
        tem                     = adt(iz,1)*(1+RVRDM1*adr(iz,1,1))
        Z_BOT_cc(istrt+iz-1,lan)= -(RD/G)*tem*LOG(prsl(iz,1)/prsi(iz,1))
!
        ffmm_cc(istrt+iz-1,lan)  = sfc_fld%ffmm(istrt+iz-1,lan)
        ffhh_cc(istrt+iz-1,lan)  = sfc_fld%ffhh(istrt+iz-1,lan)
        if (sfc_fld%SLMSK(istrt+iz-1,lan) .lt. 0.01) then
          T_SFC_cc(istrt+iz-1,lan) = sfc_fld%tsea(istrt+iz-1,lan)
     &                             + sfc_fld%oro(istrt+iz-1,lan)*RLAPSE
        else
          T_SFC_cc(istrt+iz-1,lan) = sfc_fld%tsea(istrt+iz-1,lan)
        end if
        FICE_SFC_cc(istrt+iz-1,lan) = sfc_fld%fice(istrt+iz-1,lan)
        HICE_SFC_cc(istrt+iz-1,lan) = sfc_fld%hice(istrt+iz-1,lan)
     &                              * sfc_fld%fice(istrt+iz-1,lan)
      enddo
!     do i=istrt,istrt+njeff-1
!        if (ffmm_cc(i,lan).LT.1.0) print *,'ffmm_cc<1',ffmm_cc(i,lan)
!        if (ffhh_cc(i,lan).LT.1.0) print *,'ffhh_cc<1',ffmm_cc(i,lan)
!     enddo
!      if (me .eq. 0) then
!          call atm_maxmin(njeff,1,LPREC_cc(istrt,lan),
!     >    'in gbphys_call, LPREC_cc')
!          print *,'after cpl,istrt=',istrt,'istrt+njeff-1=',
!     >      istrt+njeff-1,'lan=',lan
!       endif
!--> cpl insertion
!
!!

      if( gen_coord_hybrid .and. thermodyn_id.eq.3 ) then		! hmhj

! convert dry temperature to enthalpy					! hmhj
        sumq=0.0							! hmhj
        xcp=0.0								! hmhj
        do i=1,ntrac							! hmhj
         if( cpi(i).ne.0.0 ) then					! hmhj
         do k=1,levs							! hmhj
          do j=1,njeff							! hmhj
            work1=adr(j,k,i)						! hmhj
            sumq(j,k)=sumq(j,k)+work1					! hmhj
            xcp(j,k)=xcp(j,k)+cpi(i)*work1				! hmhj
          enddo								! hmhj
         enddo								! hmhj
         endif								! hmhj
        enddo								! hmhj
        do k=1,levs							! hmhj
         do j=1,njeff							! hmhj
           work1    = (1.-sumq(j,k))*cpi(0)+xcp(j,k)			! hmhj
           adt(j,k) = adt(j,k)*work1					! hmhj
         enddo								! hmhj
        enddo								! hmhj
        							
      else								! hmhj

! convert dry temperture to virtual temperature				! hmhj
      do k=1,levs							! hmhj
      do j=1,njeff							! hmhj
           work1    = 1.0 + fv * max(adr(j,k,1),qmin)			! hmhj
           adt(j,k) = adt(j,k)*work1					! hmhj
      enddo								! hmhj
      enddo								! hmhj

      endif								! hmhj

      if( gen_coord_hybrid .and. vertcoord_id.eq.3. ) then              ! hmhj
        if( thermodyn_id.eq.3. ) then              			! hmhj
          call gbphys_adv_h(njeff,ngptc,dtf,gtv,ugr,vgr,gr , gq,        ! hmhj
     &                              adt,adu,adv,adr,prsi )              ! hmhj
        else								! hmhj
          call gbphys_adv(njeff,ngptc,dtf,gtv,ugr,vgr,gr , gq,          ! hmhj
     &                              adt,adu,adv,adr,prsi )              ! hmhj
        endif								! hmhj
      endif                                                             ! hmhj

!!

      CALL dscal(LEVS*ngptc,rcs2_v,adu,1)
      CALL dscal(LEVS*ngptc,rcs2_v,adv,1)
!!
      do k=1,lsoil
        do i=1,njeff
          sfc_fld%smc(k,istrt+i-1,lan) = smc_v(i,k)
          sfc_fld%stc(k,istrt+i-1,lan) = stc_v(i,k)
          sfc_fld%slc(k,istrt+i-1,lan) = slc_v(i,k)
        enddo
      enddo
!!
      do j=1,num_p3d
        do k=1,levs
          do i=1,njeff
            phy_f3d(i,k,iblk,lan,j) = phy_f3dv(i,k,j)
          enddo
        enddo
      enddo
      do j=1,num_p2d
        do i=1,njeff
          phy_f2d(istrt+i-1,lan,j) = phy_f2dv(i,j)
        enddo
      enddo
!
!!
c$$$          write(2800000+1000*lat+istrt,
c$$$     .'("adt",T16,"adu",T31,"adv",T46,"adr 1",T61,"adr 2",T76,"adr 3")')
c$$$      do k=1,levs
c$$$         write(2800000+1000*lat+istrt,
c$$$     .         '(e10.3,T16,e10.3,T31,e10.3,T46,e10.3,T61,
c$$$     .           e10.3,T76,e10.3)')
c$$$     .    adt(1,k),adu(1,k),adv(1,k),adr(1,k,1),adr(1,k,2),adr(1,k,3)
c$$$      enddo
c$$$      close (2800000+1000*lat+istrt)
          do k=1,levs
          do j=1,njeff
            anl_gr_r_2(istrt+j-1+(kat-1)*lon_dim+lon_dim*(k-1),lan)
     &    =adt(j,k)

            anl_gr_r_2(istrt+j-1+(kau-1)*lon_dim+lon_dim*(k-1),lan)
     &    =adu(j,k)

            anl_gr_r_2(istrt+j-1+(kav-1)*lon_dim+lon_dim*(k-1),lan)
     &    =adv(j,k)
          enddo
          enddo
!!
          do kk=1,ntrac
          do k=1,levs
          do j=1,njeff

          anl_gr_r_2(istrt+j-1+(kar+(kk-1)*levs-1)*lon_dim
     &               +lon_dim*(k-1),lan)
     &    =adr(j,k,kk)

          enddo
          enddo
          enddo
!sela      do i=1,njeff
!sela        pwatp     = pwatp + pwat(istrt+i-1,lan)*(grav/1.e3)
!sela      enddo

c$$$!!
c$$$          enddo   !lon
c$$$!
c$$$         ptotj(lan)=0.
c$$$         do j=1,lons_lat
c$$$            ptotj(lan)=ptotj(lan)+syn_gr_r_2(j+(ksq   -1)*lon_dim,lan)
c$$$         enddo
c$$$        pwatj(lan)=pwatp/(2.*lonsperlar(lat))
c$$$cjfe    pwatg=pwatg+wgt_r(min(lat,latr-lat+1))*pwatj
c$$$        ptotj(lan)=ptotj(lan)/(2.*lonsperlar(lat))
c$$$cjfe    ptotg=ptotg+wgt_r(min(lat,latr-lat+1))*ptotj
c$$$!!
c$$$c$$$        if (kdt.eq.1) then
c$$$c$$$        do j=1,lons_lat
c$$$c$$$        do i=1,levs
c$$$c$$$        write(8700+lat,*)
c$$$c$$$     &    anl_gr_r_2(j+(kat-1)*lon_dim+lon_dim*(i-1),lan),i,j
c$$$c$$$        write(8800+lat,*)
c$$$c$$$     &    anl_gr_r_2(j+(kar-1)*lon_dim+lon_dim*(i-1),lan),i,j
c$$$c$$$        write(8900+lat,*)
c$$$c$$$     &    anl_gr_r_2(j+(kau-1)*lon_dim+lon_dim*(i-1),lan),i,j
c$$$c$$$        write(8100+lat,*)
c$$$c$$$     &    anl_gr_r_2(j+(kav-1)*lon_dim+lon_dim*(i-1),lan),i,j
c$$$c$$$        write(8200+lat,*)
c$$$c$$$     &   anl_gr_r_2(j+(kar+levs-1)*lon_dim+lon_dim*(i-1),lan),i,j
c$$$c$$$        write(8300+lat,*)
c$$$c$$$     &  anl_gr_r_2(j+(kar+2*levs-1)*lon_dim+lon_dim*(i-1),lan),i,j
c$$$c$$$        enddo
c$$$c$$$        enddo
c$$$c$$$        endif
c$$$!!
c$$$      enddo   !lan


         return
         end

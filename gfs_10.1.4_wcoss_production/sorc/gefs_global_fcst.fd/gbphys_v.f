      SUBROUTINE GBPHYS(IM,IX,levs,lsoil,lsm,ntrac,ncld,
     & ntoz,ntcw,nmtvr,lonf,latg,jcap,ras,nlons,xkt2,nrcm,pre_rad,
     & UGRS,VGRS,PGR,TGRS,QGRS,vvel,
     & GT0,GQ0,GU0,GV0,sinlat,coslat,rcs2,
     & prsi,prsl,prslk,prsik,phii,phil,dpshc,fhour,lssav,solhr,
!    & prsi,prsl,prslk,prsik,phii,phil,prsshc,fhour,lssav,solhr,
     & lsfwd,clstp,dtp,dtf,poz,prdout,ko3,pl_coeff,
! The beginning for oceanic sub-layer and diurnal models. XL, Dec, 2007
!    & nr_ocn,nf_ocn,nsst_active,ifd,time_old,time_ins,I_Sw,I_Q,I_Qrain,
     & nsst_active,ifd,time_old,time_ins,I_Sw,I_Q,I_Qrain,
     & I_M,I_Tau,I_Sw_Zw,I_Q_Ts,I_M_Ts,
     & Tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d,
! The end for oceanic sub-layer and diurnal models. XL, Dec, 2007
     & HICE,FICE,TISFC,SFCDSW,                          ! FOR SEA-ICE - XW Nov04
Clu [+2L]: add (tprcp,srflag),(slc,snwdph,slope,shdmin,shdmax,snoalb),sfalb
     & TPRCP, SRFLAG,
     & SLC   ,SNWDPH,SLOPE ,SHDMIN,SHDMAX,SNOALB,SFALB ,
Cwei added 10/24/2006
     & CHH,CMM,EPI,DLWSFCI,ULWSFCI,USWSFCI,DSWSFCI,DTSFCI,
     & DQSFCI,GFLUXI,SRUNOFF,T1,Q1,U1,V1,ZLVL,EVBSA,EVCWA,
     & TRANSA,SBSNOA,SNOWCA,SOILM,SNOHFA,SMCWLT2,SMCREF2,

!hchuang code change 
     & gsoil,gtmp2m,gustar,gpblh,gu10m,gv10m,gzorl,goro,

     & TSEA  ,SHELEG,SNCOVR, TG3   ,ZORL  ,CV    ,CVB   ,CVT   ,
     & SLMSK ,VFRAC ,CANOPY,F10M  ,VTYPE ,STYPE ,UUSTAR,FFMM  ,FFHH  ,
     & TMPMIN,TMPMAX,
!jwang add spfhmax/spfhmin
     & SPFHMIN,SPFHMAX,
     & GESHEM,DUSFC ,DVSFC ,DTSFC ,DQSFC ,DLWSFC,ULWSFC,
     & suntim,
     & GFLUX ,RUNOFF,EP    ,CLDWRK,DUGWD ,DVGWD ,PSMEAN,BENGSH,XLON  ,
     & COSZEN,SFCNSW,XLAT  ,
     & SFCDLW,TSFLW ,PSURF ,U10M  ,V10M  ,T2M   ,Q2M   ,
!    & COSZEN,SFCNSW,SFCDLW,TSFLW ,PSURF ,U10M  ,V10M  ,T2M   ,Q2M   ,
     & HPBL  ,PWAT  ,SWH,HLW,SMC,STC,HPRIME,slag,sdec,cdec,
     & acv,acvb,acvt,
     & phy_f3d, phy_f2d, num_p3d, num_p2d, flgmin,
     & DT3DT, DQ3DT, DU3DT, DV3DT, upd_mf, dwn_mf, det_mf,
     & dkh,   rnp,  LDIAG3D, lggfs3d,
!    & DT3DT, DQ3DT, DU3DT, DV3DT, LDIAG3D,
!
! Coupling deletion->
!    & flipv, me,kdt,lat,oro, crtrh, ncw, old_monin)
!<-Coupling deletion
! Coupling insertion->
     & flipv, me,kdt,lat,oro, crtrh, ncw, old_monin,cnvgwd,ccwf,ctei_rm,
     & sashal,newsas,mom4ice,mstrat,trans_trac,cal_pre,
     > lssav_cc_dummy,DLWSFC_cc_dummy,ULWSFC_cc_dummy,SWSFC_cc_dummy,
     > XMU_cc_dummy,
     > DLW_cc_dummy,DSW_cc_dummy,SNW_cc_dummy,LPREC_cc_dummy,     !cpl insertion
     > DUSFC_cc_dummy,DVSFC_cc_dummy,DTSFC_cc_dummy,DQSFC_cc_dummy,
     > PRECR_cc_dummy)

!!    USE SURFACE_cc, ONLY: JCAL_cc
!<-Coupling insertion
!
      USE MACHINE , ONLY : kind_phys
      USE PHYSCONS, ROCP  => con_rocp,  CP => con_cp, FV => con_fvirt
     &,             grav  => con_g,     RD => con_RD
     &,             RV    => con_RV,    HVAP => con_HVAP
     &,             HFUS  => con_HFUS
     &,             rerth => con_rerth, pi => con_pi
      implicit none
!
      integer  levs,lsoil,lsm,ix,im,ntrac,ncld,ntoz,ntcw,nmtvr,lonf,
     &         latg,jcap,nlons(im),num_p3d,num_p2d,nrcm,lat
     &,        pl_coeff
!
      real, parameter :: hocp=hvap/cp
      LOGICAL lssav,lsfwd, old_monin, cnvgwd
      integer levshc(im), levshcm     ! Needed for pry version
      integer ncw(2)
!     real(kind=kind_phys) dtp,dtf,FHOUR,solhr, prsshc
      real(kind=kind_phys) dtp,dtf,FHOUR,solhr, dpshc(im), crtrh(3)
      real(kind=kind_phys) flgmin(2), ccwf, ctei_rm
      real, parameter :: fhourpr=0.0

! Coupling insertion->
      logical lssav_cc_dummy
      real(kind=kind_phys),dimension(IM)::
     > DLWSFC_cc_dummy,ULWSFC_cc_dummy,SWSFC_cc_dummy,XMU_cc_dummy,
     > DLW_cc_dummy,DSW_cc_dummy,SNW_cc_dummy,LPREC_cc_dummy,
     > DUSFC_cc_dummy,DVSFC_cc_dummy,DTSFC_cc_dummy,DQSFC_cc_dummy,
     > PRECR_cc_dummy

!!     real(kind=kind_phys),parameter:: CONVRAD_cc=JCAL_cc*1.E4/60.
!<-Coupling insertion

      real(kind=kind_phys) UGRS(IX,LEVS),      VGRS(IX,LEVS),
     &                     TGRS(IX,LEVS),      qgrs(IX,levs,ntrac),
     &                     VVEL(IX,LEVS),
!
     &                     GT0(IX,LEVS),       GU0(IX,LEVS),
     &                     GV0(IX,LEVS),       gq0(IX,levs,ntrac),
!
     &                     DEL(IX,LEVS),       PRSI(IX,LEVS+1),
     &                     PRSL(IX,LEVS),      PRSLK(IX,LEVS),
     &                     PRSIK(IX,LEVS+1),   PHII(IX,LEVS+1),
     &                     PHIL(IX,LEVS),
     &                     PGR(IM),            XKT2(IX,nrcm)
     &,                    ccwfac(im)
     &,                    xcosz(im), suntim(im)    ! yth mar/08 cosz for compute suntim

      real(kind=kind_phys) RCS2(IM), SINLAT(IM), COSLAT(IM),clstp

      real(kind=kind_phys) SMC(IX,LSOIL), STC(IX,LSOIL),    SWH(IX,LEVS)
     &,                    HICE(IM),      FICE(IM),         SFCDSW(IM) ! SEA-ICE
     &,                    TISFC(IM)
     &,                    HLW(IX,LEVS),  HPRIME(IX,NMTVR), TSEA(IM)
     &,                    SHELEG(IM),    TG3(IM),          ZORL(IM)
     &,                    SNCOVR(IM)
     &,                    CV(IM),        CVB(IM),          CVT(IM)
     &,                    COSZEN(IM),    PWAT(IM),         SLMSK(IM)
     &,                    VFRAC(IM),     CANOPY(IM),       F10M(IM)
     &,                    VTYPE(IM),     STYPE(IM),        UUSTAR(IM)
     &,                    FFMM(IM),      FFHH(IM),         TMPMIN(IM)
     &,                    TMPMAX(IM),    GESHEM(IM),       DUSFC(IM)
     &,                    DVSFC(IM),     DTSFC(IM),        DQSFC(IM)
     &,                    DLWSFC(IM),    ULWSFC(IM),       GFLUX(IM)
     &,                    RUNOFF(IM),    EP(IM),           CLDWRK(IM)
     &,                    DUGWD(IM),     DVGWD(IM),        PSMEAN(IM)
     &,                    BENGSH(IM),    XLON(IM),         SFCNSW(IM)
     &,                    SFCDLW(IM),    TSFLW(IM),        PSURF(IM)
     &,                    U10M(IM),      V10M(IM),         T2M(IM)
     &,                    Q2M(IM),       HPBL(IM),         xlat(IM)
!jwang add spfhmax/spfhmin
     &,                    SPFHMIN(IM),   SPFHMAX(IM)
Clu [+5L]: add (tprcp,srflag),(slc,snwdph,shdmin,shdmax,snoalb,slope),sfalb,(fm10,fh2)
     &,                    TPRCP(IM),     SRFLAG(IM)
     &,                    SLC(IX,LSOIL)
     &,                    SNWDPH(IM),    SHDMIN(IM),      SHDMAX(IM)
     &,                    SNOALB(IM),    SLOPE(IM),       SFALB(IM)
Cwei added 10/24/2006
     &, CHH(IM),CMM(IM),EPI(IM),DLWSFCI(IM),ULWSFCI(IM),USWSFCI(IM)
     &, DSWSFCI(IM),DTSFCI(IM),DQSFCI(IM),GFLUXI(IM),SRUNOFF(IM),T1(IM)
     &, Q1(IM),U1(IM),V1(IM),ZLVL(IM)
     &, EVBSA(IM),EVCWA(IM),TRANSA(IM),SBSNOA(IM),SNOWCA(IM),SOILM(IM)
!hchuang code change 11/12/2007 [+2L]
     &, gsoil(im), gtmp2m(im), gustar(im), gpblh(im), gu10m(im)
     &, gv10m(im), gzorl(im), goro(im)
     &, SNOHFA(IM),tseal(im)
!
     &,                    phy_f3d(IX,LEVS,num_p3d), phy_f2d(IX,num_p2d)
     &,                    acv(IM),       acvb(IM), acvt(IM)
     &,                    oro(im)

! li added for oceanic components
!     integer nr_ocn,nf_ocn
!     real(kind=kind_phys) ocnr(im,nr_ocn)
!     real(kind=kind_phys) ocnf(im,nf_ocn)
      real(kind=kind_phys) ifd(im),time_old(im),time_ins(im),I_Sw(im),
     &                     I_Q(im),I_Qrain(im),I_M(im),I_Tau(im),
     &                     I_Sw_Zw(im),I_Q_Ts(im),I_M_Ts(im),Tref(im),
     &                     dt_cool(im),z_c(im),dt_warm(im),z_w(im),
     &                     c_0(im),c_d(im),w_0(im),w_d(im)
       logical nsst_active

      real(kind=kind_phys) slag,sdec,cdec
!
      real(kind=kind_phys) dt3dt(IX,levs,6),  dq3dt(IX,levs,5+pl_coeff),
     &                     du3dt(IX,levs,4),  dv3dt(IX,levs,4)
!    &,                    cumflx(ix,levs)
!hchuang code change [+3L]
     &,                    dkh(IX,LEVS),    rnp(ix,levs)
     &,                    upd_mf(ix,levs), dwn_mf(ix,levs)
     &,                    det_mf(ix,levs)
!
      integer me, kdt
      logical RAS,LDIAG3D,pre_rad,sashal,newsas,mom4ice,mstrat
     &,       trans_trac,lggfs3d,cal_pre
!
!     In CLW, the first two varaibles are cloud water and ice. 
!     From third to ntrac are convective transportable tracers,
!     third being the ozone, when ntrac=3 (valid only with RAS)
!
      real(kind=kind_phys), allocatable :: CLW(:,:,:)
      real(kind=kind_phys) garea(im), dlength(im)
!     real(kind=kind_phys) CLW(IX,LEVS,ntrac)
!    &,                    garea(im), dlength(im)
!
      integer KO3
      real(kind=kind_phys) poz(KO3), prdout(IX,ko3,pl_coeff)
!
      real(kind=kind_phys) RHC(IM,LEVS), SR(IM,levs)
     &,                    xncw(IM),  rhbbot, rhbtop, rhpbl
     &,                    dxmax,     dxmin,  dxinv
     &,                    dkt(IM,LEVS-1), rainp(im,levs)
     &,                    ud_mf(im,levs), dd_mf(im,levs)
     &,                    dt_mf(im,levs)
      logical flipv
!
      real, PARAMETER:: RLAPSE=0.65E-2
      real(kind=kind_phys), parameter :: rhc_max=0.9999      ! 20060512
!     real(kind=kind_phys), parameter :: rhc_max=0.999       ! for pry
!
      real (kind=kind_phys), parameter ::  qmin=1.0e-10, p850=85.0

!
!
!     parameter (dxmax=log(1.0/7200.0), dxmin=log(1.0/192.0)
!     parameter (dxmax=-8.8818363, dxmin=-5.2574954
!    &,          dxinv=1.0/(dxmax-dxmin))
!     parameter (dxmax=ln(1.0/14000.0), dxmin=ln(1.0/192.0)
!     parameter (dxmax=-9.5468126, dxmin=-5.2574954
!    &,          dxinv=1.0/(dxmax-dxmin))
!
!     parameter (dxmax=ln(1.0/(14000.0*7000.0)), dxmin=ln(1.0/(192.0*94.0))
!
!     parameter (dxmax=-18.40047804, dxmin=-9.800790154
!    &,          dxinv=1.0/(dxmax-dxmin))
!
!
!     parameter (dxmax=ln(1.0/(4000.0*2000.0)), dxmin=ln(1.0/(192.0*94.0))
!
!     parameter (dxmax=-15.8949521, dxmin=-9.800790154
!    &,          dxinv=1.0/(dxmax-dxmin))
!
!     parameter (dxmax=ln(1.0/(3000.0*1500.0)), dxmin=ln(1.0/(192.0*94.0))
!
!     parameter (dxmax=-15.31958795, dxmin=-9.800790154
!    &,          dxinv=1.0/(dxmax-dxmin))
!
!     parameter (dxmax=ln(1.0/(2500.0*1250.0)), dxmin=ln(1.0/(192.0*94.0))
!
!     parameter (dxmax=-14.95494484, dxmin=-9.800790154
!    &,          dxinv=1.0/(dxmax-dxmin))
!
!     parameter (dxmax=ln(1.0/(2000.0*1000.0)), dxmin=ln(1.0/(192.0*94.0))
!     parameter (dxmax=-14.50865774, dxmin=-9.800790154
!    &,          dxinv=1.0/(dxmax-dxmin))
!
!     parameter (dxmax=ln(1.0/(5000.0*2500.0)), dxmin=ln(1.0/(192.0*94.0))
      parameter (dxmax=-16.118095651, dxmin=-9.800790154
     &,          dxinv=1.0/(dxmax-dxmin))
!
      real (kind=kind_phys), parameter :: cb2mb=10.0
c
c     Local variables 
c     ---------------
      real(kind=kind_phys) DTDT(IM,LEVS),       DQDT(IM,LEVS,ntrac),
     &                     DUDT(IM,LEVS),       DVDT(IM,LEVS),
     &                     GWDCU(IM,LEVS),      GWDCV(IM,LEVS),
     &                     DIAGN1(IM,LEVS),     DIAGN2(IM,LEVS)
     &,                    cuhr(im,levs)
       real(kind=kind_phys) cumchr(IM,levs),cumabs(IM)
       real(kind=kind_phys) qmax(IM)
!
Clu [-3L/+1L]: add slsoil; comment out ai, bi, cci, rhsmc, zsoil
Cwei [+1L]: uncomment and add slsoil because of adding OSU LSM
      real(kind=kind_phys) SMSOIL(IM,LSOIL),    STSOIL(IM,LSOIL),
     &                     AI(IM,LSOIL),        BI(IM,LSOIL),
     &                     CCI(IM,LSOIL),       RHSMC(IM,LSOIL),
     &                     ZSOIL(IM,LSOIL),
     +                     SLSOIL(IM,LSOIL)
!c-- XW: FOR SEA-ICE Nov04
      real(kind=kind_phys) CICE(IM),    DSWSFC(IM),  ZICE(IM)
     &,                    TICE(IM)
!c-- XW: END SEA-ICE
!
      real(kind=kind_phys) GFLX(IM),    RAIN(IM),    RAINC(IM),
     &                     RAINL(IM),   RAIN1(IM),   EVAPC(IM),
     &                     SNOWMT(IM),  CD(IM),      CDQ(IM),
     &                     QSS(IM),     radsl(IM),   DUSFCG(IM),
     &                     DVSFCG(IM),  DUSFC1(IM),  DVSFC1(IM),
     &                     DTSFC1(IM),  DQSFC1(IM),  DLWSF1(IM),
     &                     ULWSF1(IM),  RB(IM),      RHSCNPY(IM),
     &                     DRAIN(IM),   CLD1D(IM),   RAINCS(IM),
     &                     EVAP(IM),    HFLX(IM),    STRESS(IM),
!    &                     EVAP(IM),    HFLX(IM),    RNET(IM),
     &                     T850(IM),
     &                     EP1D(IM),    GAMT(IM),    GAMQ(IM),
     &                     sigmaf(IM),
     &                     rcl(IM),     rcs(IM),
     &                     oc(IM),      oa4(IM,4),   clx(IM,4),
     &                     theta(IM),gamma(IM),sigma(IM),elvmax(IM),
     &                     wind(IM),    work1(IM),   work2(IM),
     &                     runof(IM),   xmu(IM)
     &,                    qr_col(im,levs), fc_ice(im,levs)
     &,                    oro_land(im)
Cwei added 10/24/2006
     &,   EVBS(im),EVCW(im),TRANS(im),SBSNO(im),SNOWC(im),SNOHF(im)
     &,   SMCWLT2(IM),SMCREF2(IM)
!
Clu [+1L]: add (fm10,fh2)
     &,                    FM10(IM),        FH2(IM)
Clu_q2m_iter [+1L]: add tsurf
     &,                    tsurf(im)
Clu_q2m_iter [+1L]: add flag_iter and flag_guess
      logical              flag_iter(im), flag_guess(im)
Clu_q2m [+1L]: add wrk1, wrk2
      real(kind=kind_phys)  wrk1,   wrk2
!
!
      REAL(kind=kind_phys), PARAMETER ::    EPSQ=1.E-20, HSUB=HVAP+HFUS
      integer               KBOT(IM),    KTOP(IM),    kcnv(im),
     &                      soiltyp(IM), vegtype(IM), kpbl(IM)
Clu [+1L]: add slopetyp
     &,                    SLOPETYP(IM)
      integer i, nvdiff, kk, ic, k, n, kinver(im), ipr, lv, k1
     &,       lmh(im), tracers, trc_shft, tottracer
Clu_q2m_iter [+1L]: add iter
     +,        iter
!
      logical lprnt, invrsn(im)
      real(kind=kind_phys) frain,  tem, tem1, qi, qw, qr, wc
     &,                    f_rain, f_ice, tem0d, tx1(im), tx2(im)
     &,                    tem2,   ctei_r(im), flgmin_l(im)
!    &,                    sumq, sumr, tem2
!    &,                    pwato(im), raino(im), evapo(im), sume
!
! HCHUANG add temporary arrays
      real(kind=kind_phys), dimension(im) :: DOMR,DOMZR,DOMIP,DOMS
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!     lprnt = .true.
      lprnt = .false.
!     ipr = 1
!     lprnt = kdt .gt. 0 .and. ilon .eq.  1
!     do i=1,im
!       work1(1) = xlon(i) * 57.29578
!       if (work1(1) .ge. 180.0) work1(1) = work1(1) - 360.0
!       work2(1) = xlat(i) * 57.29578
!     print *,' me=',me,' work1=',work1(1),' work2=',work2(1),' i=',i
!       lprnt = kdt .gt. 4320
!       lprnt = kdt .gt. 0
!    &         .and. abs(work1(1)-110.3) .lt. 0.5
!    &         .and. abs(work2(1)-2.0) .lt. 0.5
!       lprnt = kdt .ge. 14 .and. lat .eq. 43 
!       lprnt = kdt .ge. 0
!    &         .and. abs(xlon(i)*57.29578-180.0) .lt. 0.201
!    &         .and. abs(xlat(i)*57.29578-4.76) .lt. 0.201
!    &         .and. abs(xlon(i)*57.29578-110.3) .lt. 0.201
!    &         .and. abs(xlat(i)*57.29578-2.0) .lt. 0.201
!     print *,' i=',i,' xlon=',xlon(i)*57.29578
!    &,               ' xlat=',xlat(i)*57.29578
!    &,' i=',i,' me=',me
!       if (lprnt) then
!          ipr = i
!          exit
!       endif
!     enddo

!     lprnt = .false.
!     if(lprnt) then
!       print *,' IM=',IM,' IX=',IX,' levs=',levs,' lsoil=',lsoil
!    *,' ntrac=',ntrac,' ntoz=',ntoz,' ntcw=',ntcw,' me=',me
!    *,' xlat=',xlat(ipr),' kdt=',kdt,' slmsk=',slmsk(ipr)
!    &,' tsea=',tsea(ipr),' tref=',tref(ipr),' dt_cool='
!    &,dt_cool(ipr),' dt_warm=',dt_warm(ipr)
!    *,' nrcm=',nrcm
!    &,' xlon=',xlon(ipr),' sfalb=',sfalb(ipr),' kdt=',kdt
!       print *,' pgr=',pgr(ipr),' kdt=',kdt,' ipr=',ipr
!       print *,' ipr=',ipr,' phy_f2d=',phy_f2d(ipr,1:num_p2d)
!       print *,' ugrs=',ugrs(ipr,:)
!       print *,' vgrs=',vgrs(ipr,:)
!       print *,' tgrs=',tgrs(ipr,:),' kdt=',kdt,' ipr=',ipr
!    &,' xlon=',xlon(ipr),' xlat=',xlat(ipr)
!       print *,' qgrs=',qgrs(ipr,:,1)
!       print *,' ozg=',qgrs(ipr,:,2)
!       print *,' clw=',qgrs(ipr,:,3)
!    &,' xlon=',xlon(ipr),' xlat=',xlat(ipr)
!     endif
!
!
      lmh(:) = levs
      NVDIFF = ntrac    ! vertical diffusion of all tracers!
!
!     Figure out how many extra tracers are there
!
      tottracer = 0            ! no convective transport of tracers
      if (trans_trac) then
        if (ntcw > 0) then
          if (ntoz < ntcw) then
            trc_shft = ntcw + ncld - 1
          else
            trc_shft = ntoz
          endif
        elseif (ntoz > 0) then
          trc_shft = ntoz
        else
          trc_shft = 1
        endif

        tracers   = ntrac - trc_shft
        tottracer = tracers
        if (ntoz > 0) tottracer = tottracer + 1  ! ozone is added separately
      endif
      allocate (clw(ix,levs,tottracer+2))

!     if (lprnt) print *,' trans_trac=',trans_trac,' tottracer=',
!    & tottracer,' trc_shft=',trc_shft,' kdt=',kdt
!
      if (ras) then
        if (ccwf >= 0.0) then
          ccwfac = ccwf
        else
          ccwfac = -999.0
        endif
      endif
!
!
      CALL GET_PRS(im,ix,levs,ntrac,TGRS,QGRS
     &,            prsi,prsik,prsl,prslk,phii,phil,del)
!..........................................................................
!
      rhbbot = crtrh(1)
      rhpbl  = crtrh(2)
      rhbtop = crtrh(3)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     do i=1,im
!       PWATo(i) = 0.
!       evapo(i) = dqsfc(i)
!       raino(i) = geshem(i)
!     enddo
!     DO K=1,LEVS
!       do i=1,im
!         work1(i) = 0.0
!       enddo
!       if (ncld .gt. 0) then
!         do ic=ntcw, ntcw+ncld-1
!           do i=1,im
!             work1(i) = work1(i) + qgrs(i,k,ic)
!           enddo
!         enddo
!       endif
!       do i=1,im
!         PWATo(i)  = PWATo(i)  + DEL(i,K)*(qgrs(i,K,1)+work1(i))
!       enddo
!     ENDDO
!     do i=1,im
!       PWATo(i)  = PWATo(i)*(1.E3/grav)
!     enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!     print *,' prsi=',prsi(ipr,:)
!     print *,' prsl=',prsl(ipr,:)
!     print *,' del=',del(ipr,:)
!     print *,' phii=',phii(ipr,:)
!     print *,' phil=',phil(ipr,:)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                      For pry version
      do i=1,im
        levshc(i) = 0
      enddo
      do k=2,levs
        do i=1,im
          if (prsi(i,1)-prsi(i,k) .le. dpshc(i)) levshc(i) = k
        enddo
      enddo
      levshcm = 1
      do i=1,im
        levshcm = max(levshcm, levshc(i))
      enddo
      if (mstrat) then
        levshcm = max(levshcm, levs/2)
      else
        levshcm = min(levshcm, levs/2)
      endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  FRAIN=FACTOR FOR CENTERED DIFFERENCE SCHEME CORRECTION OF RAIN AMOUNT.
!
      FRAIN = DTF/DTP

      do i=1,im
        SOILTYP(i)  = INT(STYPE(i)+.5)
        SIGMAF(i)   =  MAX(VFRAC(i),0.01)
!       SIGMAF(i)   =  MAX(VFRAC(i),.3)
        if (lsm == 0) SIGMAF(i)   =  0.5 + VFRAC(i) * 0.5
        VEGTYPE(i)  = INT(VTYPE(i)+.5)
        SLOPETYP(i) = INT(SLOPE(i)+.5)    !! Clu [+1L]: slope -> slopetyp
!       fm1(i)      = ffmm(i)
!       fh1(i)      = ffhh(i)
!       ustar1(i)   = uustar(i)
        IF(SLMSK(i).EQ.2.) THEN
          SOILTYP(i) = 9
          VEGTYPE(i) = 13
          SLOPETYP(i) = 9                 !! Clu [+1L]: QA(slopetyp)
        ENDIF
      enddo
!sela if(vegtype.eq.0 )print*,' vegtyp ilon ilat =',vegtype, ilon,ilat 
!
!  TRANSFER SOIL MOISTURE AND TEMPERATURE FROM GLOBAL TO LOCAL VARIABLES
!
      DO K = 1, LSOIL
        do i=1,im
          SMSOIL(i,K) = SMC(i,k)
          STSOIL(i,K) = STC(i,k)
          SLSOIL(i,K) = SLC(i,k)          !! Clu [+1L]: slc -> slsoil
        enddo
      ENDDO
!
!c-- XW: FOR SEA-ICE Nov04
!
!  TRANSFER ICE THICKNESS & CONCENTRATION FROM GLOBAL TO LOCAL VARIABLES
!
      do i=1,im
          ZICE(i) = HICE(i)
          CICE(i) = FICE(i)
          TICE(i) = TISFC(i)
      enddo
!c-- XW: END SEA-ICE
!
      do k=1,levs
        do i=1,im
          dudt(i,k) = 0.
          dvdt(i,k) = 0.
          dtdt(i,k) = 0.
        enddo
      enddo
      do n=1,ntrac
        do k=1,levs
          do i=1,im
            dqdt(i,k,n) = 0. ! dqdt may be dimensioned (levs,ntrac)
          enddo
        enddo
      enddo
      do i=1,im
        RCL(i)   = RCS2(i)
        RCS(i)   = SQRT(RCL(i))
        psurf(i) = pgr(i)                         
        work1(i) = prsik(i,1) / prslk(i,1)
!       garea(i)   = rerth * rerth * (pi+pi)*pi*coslat(i)/(nlons(i)*latg)
        tem1       = rerth * (pi+pi)*coslat(i)/nlons(i)
        tem2       = rerth * pi/latg
        garea(i)   = tem1 * tem2
        dlength(i) = sqrt(tem1*tem1+tem2*tem2)
      enddo
      if(lssav) then
        do i=1,im
          PSMEAN(i) = PSMEAN(i) + PGR(i)*DTF
        enddo
      endif
!
!  INITIALIZE DTDT WITH HEATING RATE FROM DCYC2 
!
!     if(lprnt) then
!     do ipr=1,im
!     print *,' before DCYC2: IM=',IM,' LSOIL=',LSOIL,' levs=',levs
!    &,' sde=',sdec,' cdec=',cdec,' tsea=',tsea(ipr),' ipr=',ipr
!    &,' lat=',lat,' me=',me,' kdt=',kdt
!    &,' sfcdlw=',sfcdlw(ipr),' sfcnsw=',sfcnsw(ipr)
!       print *,' hlw=',hlw(ipr,:),' me=',me,' lat=',lat,xlon(ipr)
!       print *,' swh=',swh(ipr,:),' me=',me,' lat=',lat,xlon(ipr)
!     enddo
!     endif
!
!
      if(pre_rad)then
        CALL DCYC2t3_pre_rad(IX,IM,LEVS,SOLHR,SLAG,
     &             SINLAT,COSLAT,SDEC,CDEC,
     &             XLON,COSZEN,SFCDLW,SFCNSW,TGRS(1,1),
     &             SFCDSW,DSWSFC,                       ! FOR SEA-ICE - XW Nov04
     &             TSEA,TGRS(1,1),SWH,HLW,
!yth mar/08 add output xcosz
!    &             DLWSF1,ULWSF1,radsl,DTDT,xmu)
     &             DLWSF1,ULWSF1,radsl,DTDT,xmu,xcosz)
      else
        CALL DCYC2t3(IX,IM,LEVS,SOLHR,SLAG,
     &             SINLAT,COSLAT,SDEC,CDEC,
     &             XLON,COSZEN,SFCDLW,SFCNSW,TGRS(1,1),
     &             SFCDSW,DSWSFC,                       ! FOR SEA-ICE - XW Nov04
     &             TSEA,TSFLW,SWH,HLW,
!yth mar/08 add output xcosz
!    &             DLWSF1,ULWSF1,radsl,DTDT,xmu)
     &             DLWSF1,ULWSF1,radsl,DTDT,xmu,xcosz)
      endif

!-> Coupling insertion
      if(lssav_cc_dummy) then
        do i=1,im
          XMU_cc_dummy(I)    = MAX(0.,MIN(1.0,XMU(I)*COSZEN(I)))
          DSW_cc_dummy(I)    = DSWSFC(I)
          DLW_cc_dummy(I)    = DLWSF1(I)
          DLWSFC_cc_dummy(I) = DLWSFC_cc_dummy(I) + DLWSF1(I)           !*DTF (?)
          ULWSFC_cc_dummy(I) = ULWSFC_cc_dummy(I) + ULWSF1(I)            !*DTF (?)
          SWSFC_cc_dummy(I)  = SWSFC_cc_dummy(I)  + radsl(I) + DLWSF1(I) !*DTF (?)
        enddo
!
!*********************************************************************
!!   >     SWSFC_cc_dummy(1:IM)+radsl(1:IM)*CONVRAD_cc+DLWSF1(1:IM)
! <- see progtmr.f, subr. progtm (called below), and
! SUBROUTINE DCYC2T3 just called
!   ** check signs here, esp. for SWR.
! - there are many pecularities here, the whole rad.
!   stuff must be kept an eye on
!       Signs for SWSFC_cc: sign for SWSFC_cc is intended positive
!       upward, as for other fluxes. Downward LWR needs to be subtracted
!       from radsl*const, where the downward LWR is considered positive
!       upward. Since DLWSF1 is downward LWR positive downward,
!       DLWSF1 must be added rather than subtracted. Similarly (see
!       progtmr.f), radsl must be taken with +.
!    >     SWSFC_cc_dummy(1:IM)+radsl(1:IM)+DLWSF1(1:IM)
!*********************************************************************
      end if
!<- Coupling insertion

     
      if(lssav)then

!yth mar/08  compute sundhine duration time that is defined as the length
!            of time (in mdl output interval) that solar radiation falling
!            on a plane perpendicular to the direction of the sun >= 120 w/m2

        do i = 1, im
          if ( xcosz(i) >= 0.0001 ) then   ! zenth angle > 89.994 deg
            tem1 = dswsfc(i) / xcosz(i)
            if ( tem1 >= 120.0 ) then
              suntim(i) = suntim(i) + dtf
            endif
          endif
        enddo
!yth end suntim

        do i=1,im
          DLWSFC(i) = DLWSFC(i) + DLWSF1(i)*DTF
          ULWSFC(i) = ULWSFC(i) + ULWSF1(i)*DTF
        enddo
       IF (LDIAG3D) THEN
         DO K=1,LEVS
           do i=1,im
             DT3DT(i,k,1) = DT3DT(i,k,1) + HLW(i,K)*DTF
             DT3DT(i,k,2) = DT3DT(i,k,2) + SWH(i,K)*DTF*xmu(i)
           enddo
         ENDDO
       ENDIF
      endif
!
!
      do i=1,im
        kcnv(i)   = 0
        kinver(i) = levs
        invrsn(i) = .false.
        tx1(i)    = 0.0
        tx2(i)    = 10.0
        ctei_r(i) = 10.0
      enddo
!     ctei_rm = 0.20
!     ctei_rm = 0.25
!     ctei_rm = 0.50
!     ctei_rm = 0.70
      do k=1,levs/2
        do i=1,im
          if (prsi(i,1)-prsi(i,k+1) .lt. 0.35*prsi(i,1)
     &                            .and. (.not. invrsn(i))) then
             tem = (TGRS(i,K+1) - TGRS(i,K)) / (prsl(i,k) - prsl(i,k+1))
!            if (tem .gt. 0.02 .and. tx1(i) .lt. 0.0) then
!            if (tem .gt. 0.20 .and. tx1(i) .lt. 0.0) then
!!!!         if (tem > 0.10 .and. (tx1(i) < 0.0 .or. tx2(i) < 0.0)) then
!            if (tem .gt. 0.05 .and. tx1(i) .lt. 0.0) then
             if (tem .gt. 0.025 .and. tx1(i) .lt. 0.0) then
               invrsn(i) = .true.
!
               if (qgrs(i,k,1) .ne. qgrs(i,k+1,1) .and.
     &                         .not. sashal) then
                tem1 = (1.0 + hocp*max(qgrs(i,k+1,1),qmin)/tgrs(i,k+1))
                tem2 = (1.0 + hocp*max(qgrs(i,k,1),qmin)/tgrs(i,k))
                tem1 = tem1 * tgrs(i,k+1) / prslk(i,k+1)
     &               - tem2 * tgrs(i,k)   / prslk(i,k)
!               (Cp/L)(delthetae)/(deltotwater) < 0.7
                ctei_r(i) = (1.0/hocp)*tem1/(qgrs(i,k+1,1)-qgrs(i,k,1)
     &                           + qgrs(i,k+1,ntcw)-qgrs(i,k,ntcw))
               else
                ctei_r(i) = 10
               endif
               kinver(i) = k
!!!!           kinver(i) = k + 1
             endif
             tx2(i) = tx1(i)
             tx1(i) = tem
          endif
        enddo
      enddo

!     ipr = 1
!     if(lprnt) then
!       print *,' before PROGTM: IM=',IM,' LSOIL=',LSOIL
!    &,' nvdiff=',nvdiff,' radsl=',radsl(ipr),' dlwsf1=',dlwsf1(ipr)
!    &,' dlwsf1=',dlwsf1(ipr),' tsea2=',tsea(ipr)
!    &,' ipr=',ipr,' me=',me,' lat=',lat,' xlon=',xlon(ipr)
!    &,' kdt=',kdt
!     print *,' dtdth=',dtdt(ipr,:),' kdt=',kdt
!     endif
!     phy_f2d(:,num_p2d) = 0.0  ! disable downdraft effect on evap
!
Clu [-11L/+34L]: Revision starts here ...................................
Clu     CALL PROGTM(IM,LSOIL,PGR,UGRS,VGRS,TGRS,QGRS,
Clu   & SHELEG,TSEA,QSS,
Clu   & SMSOIL,STSOIL,EVAPC,SOILTYP,SIGMAF,
Clu   & vegtype,CANOPY,
Clu   & dlwsf1,
Clu   & radsl,SNOWMT,dtf   ,ZORL,TG3,
Clu   & GFLX,F10M,U10M,V10M,T2M,Q2M,ZSOIL,
Clu   & CD,CDQ,RB,RHSCNPY,RHSMC,AI,BI,CCI,
Clu   & RCL,PRSL(1,1),work1,SLMSK,
Clu   & DRAIN,EVAP,HFLX,STRESS,EP1D,ffmm,ffhh,
Clu   & uustar,WIND,phy_f2d(1,num_p2d))
                                                                                                         
Clu_q2m [+22L]: print selected variables
!      do i=1,im
!       wrk1 = xlon(i) * 57.29578
!       if (wrk1 .ge. 180.0) wrk1 = wrk1 - 360.0
!       wrk2 = xlat(i) * 57.29578
!       lprnt =  abs(wrk1+96.5) .lt. 0.5
!    &     .and. abs(wrk2-39.1) .lt. 0.5
!    &     .and. me .eq. 12
!      if(lprnt) then
!      write(221) LSOIL,PGR(i),UGRS(i,1),VGRS(i,1),
!    & TGRS(i,1),QGRS(i,1,1),
!    & SHELEG(i),TSEA(i),QSS(i),
!    & (SMSOIL(i,k),k=1,LSOIL),(STSOIL(i,k),k=1,LSOIL),
!    & (SLSOIL(i,k),k=1,LSOIL),EVAPC(i),SOILTYP(i),SIGMAF(I),
!    & vegtype(i),CANOPY(i),dlwsf1(i),
!    & radsl(i),dtf   ,ZORL(i),
!    & TG3(i),GFLX(i),U10M(i),
!    & V10M(i),T2M(i),Q2M(i),CD(i),CDQ(i),
!    & RCL(i),PRSL(1,1),work1(i),SLMSK(i),
!    + DRAIN(i),EVAP(i),HFLX(i),EP1D(i),ffmm(i),ffhh(i),
!    & uustar(i),WIND(i)
!      endif
!      enddo
                                                                                                         
Clu_q2m_iter [+5L]: initialize flag_guess, flag_iter, tsurf
       do i=1, im
         tsurf(i)      = tsea(i)
         flag_guess(i) = .False.
         flag_iter(i)  = .True.
         drain(i)      = 0.0
         ep1d(i)       = 0.0
         runof(i)      = 0.0
         hflx(i)       = 0.0
         evap(i)       = 0.0
!
         evbs(i)       = 0.0
         evcw(i)       = 0.0
         trans(i)      = 0.0
         sbsno(i)      = 0.0
         snowc(i)      = 0.0
         snohf(i)      = 0.0
       enddo
                                                                                                         
Clu_q2m_iter [+2L]: add iter-loop over (sfc_diff,sfc_drv,sfc_ocean,sfc_sice)
      do iter = 1, 2                                 !!!!! <---- Clu_q2m_iter
!
!**  surface exchange coefficients
!
        CALL SFC_DIFF(IM,PGR,UGRS,VGRS,TGRS,QGRS,
     &                TSEA,ZORL,CD,CDQ,RB,
     &                RCL,PRSL(1,1),work1,SLMSK,
     &                STRESS,ffmm,ffhh,
Clu_q2m_iter [-1L/+2L]: add tsurf, flag_iter
!*   &                uustar,WIND,phy_f2d(1,num_p2d),fm10,fh2)
     +                uustar,WIND,phy_f2d(1,num_p2d),fm10,fh2,
     +                SIGMAF,VEGTYPE,SHDMAX,
     +                tsurf, flag_iter)

!     if (lprnt) print *,' cdq=',cdq(ipr),' iter=',iter
!    &,' wind=',wind(ipr),'phy_f2d=',phy_f2d(ipr,num_p2d),' ugrs='
!    &,ugrs(ipr,1),' vgrs=',vgrs(ipr,1)
                                                                                                         
Clu_q2m_iter [+4L]: update flag_guess
         do i=1, im
           if((iter.eq.1) .and. (wind(i).lt.2.))
     +        flag_guess(i) = .True.
         enddo
 
!
!** surface energy balance over ocean
!

       if ( nsst_active ) then

         do i=1, im
           if ( SLMSK(i) .eq. 0. ) then
             tseal(i) = tsea(i)  + oro(i) * rlapse
             Tref(i)  = TSEAL(i) - dt_warm(i) + dt_cool(i)
           endif
         enddo

!     if (lprnt) print *,' tseaz1=',tsea(ipr),' tref=',tref(ipr),
!    &' dt_cool=',dt_cool(ipr),' dt_warm=',dt_warm(ipr)
!    &,' tgrs=',tgrs(ipr,1),' prsl=',prsl(ipr,1)
!    &,' work1=',work1(ipr),' kdt=',kdt

         CALL SFC_NSSTAC(IM,LSOIL,PGR,UGRS,VGRS,TGRS,QGRS,
     &                  TSEAL,QSS,EVAPC,GFLX,CD,CDQ,
     &                  RCL,PRSL(1,1),work1,SLMSK,
     &                  xlon,sinlat,stress,dlwsf1,radsl,tprcp,dtf,kdt,
        ! input
     &                  ifd,time_old,time_ins,I_Sw,I_Q,I_Qrain,
     &                  I_M,I_Tau,I_Sw_Zw,I_Q_Ts,I_M_Ts,
        ! inout
     &                  Tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d,
        ! inout
     &                  CMM,CHH,
     &                  EVAP,HFLX,EP1D,phy_f2d(1,num_p2d),flag_iter,
     &                  lprnt, ipr)

!     if (lprnt) print *,' tseaz2=',tseal(ipr),' tref=',tref(ipr),
!    &' dt_cool=',dt_cool(ipr),' dt_warm=',dt_warm(ipr),' kdt=',kdt

         do i=1, im
           if ( SLMSK(i) .eq. 0. ) then
!            TSEA(i) = Tref(i) + dt_warm(i) - dt_cool(i)
!    &               - oro(i) * rlapse
             TSEA(i) = TSEAL(i) - oro(i) * rlapse
           endif
         enddo
!     if (lprnt) print *,' tseaz2=',tsea(ipr),' tref=',tref(ipr),
!    &' dt_cool=',dt_cool(ipr),' dt_warm=',dt_warm(ipr),' kdt=',kdt

       else

         CALL SFC_OCEAN(IM,LSOIL,PGR,UGRS,VGRS,TGRS,QGRS,
     &                  TSEA,QSS,EVAPC,GFLX,CD,CDQ,
     &                  RCL,PRSL(1,1),work1,SLMSK,
     &                  CMM,CHH,
     &                  EVAP,HFLX,EP1D,phy_f2d(1,num_p2d),flag_iter)
       endif
 
!     if (lprnt) print *,' sfalb=',sfalb(ipr),' ipr=',ipr
!    &,' sheleg=',sheleg(ipr),' snwdph=',snwdph(ipr)
!    &,' tprcp=',tprcp(ipr),' kdt=',kdt,' iter=',iter
 
!
!**  surface energy balance over land
!
      if (lsm == 1) then                          ! NOAH LSM CALL
        CALL SFC_DRV(IM,LSOIL,PGR,UGRS,VGRS,TGRS,QGRS,
     &               SHELEG,SNCOVR,SNWDPH,TSEA,QSS,TPRCP,SRFLAG,
     &               SMSOIL,STSOIL,SLSOIL,EVAPC,SOILTYP,SIGMAF,
     &               vegtype,CANOPY,dlwsf1,DSWSFC,
     &               radsl,dtf,TG3,GFLX,CD,CDQ,
     &               RCL,PRSL(1,1),work1,SLMSK,
     &               DRAIN,EVAP,HFLX,EP1D,phy_f2d(1,num_p2d),
Clu_q2m_iter [-1L/+2L]: add tsurf, flag_iter, flag_guess
!*   +               RUNOF,SLOPETYP,SHDMIN,SHDMAX,SNOALB,SFALB)
     +               RUNOF,SLOPETYP,SHDMIN,SHDMAX,SNOALB,SFALB,
Cwei added 10/24/2006
     +               CMM,CHH,ZLVL,EVBS,EVCW,TRANS,SBSNO,
     +               SNOWC,SOILM,SNOHF,SMCWLT2,SMCREF2,
     +               tsurf, flag_iter, flag_guess)
      else                                       ! OSU LSM CALL
        CALL SFC_LAND(IM,LSOIL,PGR,UGRS,VGRS,TGRS,QGRS,
     &  SHELEG,TSEA,QSS,TPRCP,SRFLAG,
     &  SMSOIL,STSOIL,EVAPC,SOILTYP,SIGMAF,
     &  vegtype,CANOPY,dlwsf1,
     &  radsl,SNOWMT,dtf,ZORL,TG3,
     &  GFLX,ZSOIL,
     &  CD,CDQ,RHSCNPY,RHSMC,AI,BI,CCI,
     &  RCL,PRSL(1,1),work1,SLMSK,
     &  DRAIN,EVAP,HFLX,EP1D,phy_f2d(1,num_p2d),
Cwei added 10/24/2006
     +  CMM,CHH,ZLVL,EVBS,EVCW,TRANS,SBSNO,
     +  SNOWC,SOILM,SNOHF,SMCWLT2,SMCREF2,
     &  tsurf,flag_iter, flag_guess)
      endif
!
!     if (lprnt) print *,' tseabeficemodel =',tsea(ipr),' me=',me
!    &,' kdt=',kdt
!
!**  surface energy balance over seaice
!
!<-- cpl insertion
        CALL SFC_SICE(IM,LSOIL,PGR,UGRS,VGRS,TGRS,QGRS,
     &                ZICE,CICE,TICE,DSWSFC,            ! FOR SEA-ICE - XW Nov04
     &                SHELEG,SNWDPH,TSEA,QSS,TPRCP,SRFLAG,STSOIL,EVAPC,
     &                dlwsf1,radsl,SNOWMT,dtf,GFLX,CD,CDQ,
     &                RCL,PRSL(1,1),work1,SLMSK,
Cwei added 10/24/2006
     +                CMM,CHH,ZLVL,
Clu_q2m_iter [-1L/+1L]: add flag_iter
!*   &                EVAP,HFLX,EP1D,phy_f2d(1,num_p2d))
     +                EVAP,HFLX,EP1D,phy_f2d(1,num_p2d),flag_iter,
     &                mom4ice,lsm)
                                                                                                         
Clu_q2m_iter [+7L]: update flag_iter and flag_guess
        do i=1, im
          flag_iter(i)  = .False.
          flag_guess(i) = .False.
          if((slmsk(i) .eq. 1.) .and. (iter .eq. 1)) then
!!!!      if((slmsk(i) < 1.1) .and. (iter .eq. 1)) then
            if(wind(i).lt.2.) flag_iter(i) = .True.
!         elseif (slmsk(i) == 0. .and. iter == 1 .and. nsst_active) then
!           flag_iter(i) = .True.
          endif
        enddo
      enddo                                 !!!!! <---- Clu_q2m_iter
Cwei added 10/24/2006
      do i=1,im
        epi(i)     = ep1d(i)
        dlwsfci(i) = dlwsf1(i)
        ulwsfci(i) = ulwsf1(i)
        uswsfci(i) = sfcnsw(i)*xmu(i) + dswsfc(i)
        dswsfci(i) = dswsfc(i)
        gfluxi(i)  = gflx(i)
        t1(i)      = tgrs(i,1)
        q1(i)      = qgrs(i,1,1)
        u1(i)      = ugrs(i,1)
        v1(i)      = vgrs(i,1)
      enddo
      if (lsm == 0) then                          ! OSU LSM CALL
        do i=1,im
         SNCOVR(i) = 0.0
         if (SHELEG(i) > 0.0) SNCOVR(i) = 1.0
        enddo
      endif

!
!** update near surface fields
!
      CALL SFC_DIAG(IM,LSOIL,PGR,UGRS,VGRS,TGRS,QGRS,
     &              TSEA,QSS,F10M,U10M,V10M,T2M,Q2M,RCL,work1,SLMSK,
     &              EVAP,ffmm,ffhh,fm10,fh2)
!
Clu_q2m [+22L]: print selected variables
!      do i=1,im
!       wrk1 = xlon(i) * 57.29578
!       if (wrk1 .ge. 180.0) wrk1 = wrk1 - 360.0
!       wrk2 = xlat(i) * 57.29578
!       lprnt =  abs(wrk1+96.5) .lt. 0.5
!    &     .and. abs(wrk2-39.1) .lt. 0.5
!    &     .and. me .eq. 12
!      if(lprnt) then
!      write(222) LSOIL,PGR(i),UGRS(i,1),VGRS(i,1),
!    & TGRS(i,1),QGRS(i,1,1),
!    & SHELEG(i),TSEA(i),QSS(i),
!    & (SMSOIL(i,k),k=1,LSOIL),(STSOIL(i,k),k=1,LSOIL),
!    & (SLSOIL(i,k),k=1,LSOIL),EVAPC(i),SOILTYP(i),SIGMAF(I),
!    & vegtype(i),CANOPY(i),dlwsf1(i),
!    & radsl(i),dtf   ,ZORL(i),
!    & TG3(i),GFLX(i),U10M(i),
!    & V10M(i),T2M(i),Q2M(i),CD(i),CDQ(i),
!    & RCL(i),PRSL(1,1),work1(i),SLMSK(i),
!    + DRAIN(i),EVAP(i),HFLX(i),EP1D(i),ffmm(i),ffhh(i),
!    & uustar(i),WIND(i)
!      endif
!      enddo
Clu [-11L/+34L]: Revision ends here .....................................
!
!     if(lprnt) then
!       print *,' hflx=',hflx(ipr)
!       print *,' evap=',evap(ipr)
!       print *,' stress=',stress(ipr)
!     endif
!
       do i=1,im
         phy_f2d(i,num_p2d) = 0.0
       enddo

!     if (lprnt) print *,' tseaim=',tsea(ipr),' me=',me,' kdt=',kdt
!
      if(lssav)then
        do i=1,im
          GFLUX(i)  = GFLUX(i)  + GFLX(i)*DTF
Cwei added 10/24/2006
          EVBSA(I)  = EVBSA(I)  + EVBS(i)*DTF
          EVCWA(I)  = EVCWA(I)  + EVCW(i)*DTF
          TRANSA(I) = TRANSA(I) + TRANS(i)*DTF
          SBSNOA(I) = SBSNOA(I) + SBSNO(i)*DTF
          SNOWCA(I) = SNOWCA(I) + SNOWC(i)*DTF
          SNOHFA(I) = SNOHFA(I) + SNOHF(i)*DTF

          TMPMAX(i) = MAX(TMPMAX(i),T2M(i))
          TMPMIN(i) = MIN(TMPMIN(i),T2M(i))
!jwang [+2L] add SPFHMIN & SPFHMAX
          SPFHMAX(i) = MAX(SPFHMAX(i),Q2M(i))
          SPFHMIN(i) = MIN(SPFHMIN(i),Q2M(i))
!
          EP(i)     = EP(i) + EP1D(i) * DTF
!
!hchuang code change 11/12/2007 [+6]
!hchuang oro is passin variable not derived in this routine
          gtmp2m(i) =  gtmp2m(i) + t2m(i)    * DTF
          gu10m(i)  =  gu10m(i)  + u10m(i)   * DTF
          gv10m(i)  =  gv10m(i)  + v10m(i)   * DTF
          gustar(i) =  gustar(i) + uustar(i) * DTF
          gzorl(i)  =  gzorl(i)  + zorl(i)   * DTF
          goro(i)   =  goro(i)   + slmsk(i)  * DTF

CWei move to the place after calling progt2
Clu [+3L]: compute total runoff
!  TOTAL RUNOFF IS COMPOSED OF DRAINAGE INTO WATER TABLE AND
!  RUNOFF AT THE SURFACE AND IS ACCUMULATED IN UNIT OF METERS
!         RUNOFF(i) = RUNOFF(i) + (DRAIN(i)+RUNOF(i)) * DTF / 1000.0

!         uustar(i) = ustar1(i)  ! should be under lsfwd
!           ffmm(i) = fm1(i)     ! should be under lsfwd
!           ffhh(i) = fh1(i)     ! should be under lsfwd
        enddo
      endif
csela the following is the correct code, the above duplicates oper. code.
csela if(lsfwd)then
csela  uustar(ilon,ilat)=ustar1
csela    ffmm(ilon,ilat)=fm1
csela    ffhh(ilon,ilat)=fh1
csela endif
!
Clu [+11L]: update smc, stc, slc
!
!  RETURN UPDATED SMSOIL AND STSOIL TO GLOBAL ARRAYS
!
!Wei move to the place after calling progt2
!     DO K = 1, LSOIL
!       do i=1,im
!         SMC(i,k) = SMSOIL(i,K)
!         STC(i,k) = STSOIL(i,K)
!         SLC(i,k) = SLSOIL(i,K)
!       enddo
!     ENDDO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Commented by Moorthi on 20050505
!
      do i=1,im
!
!     COMPUTE COEFFICIENT OF EVAPORATION IN EVAPC
!
        IF (EVAPC(i) .GT. 1.0E0) EVAPC(i) = 1.0E0
!
!     OVER SNOW COVER OR ICE OR SEA, COEF OF EVAP =1.0E0
!
        IF((SHELEG(i).GT.0.) .OR. (SLMSK(i).NE.1.0)) EVAPC(i) = 1.0E0
      enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     VERTICAL DIFFUSION
!
!     if (lprnt) print *,' tsea3=',tsea(ipr),' slmsk=',slmsk(ipr)
!    &,' kdt=',kdt,' evap=',evap(ipr)
!     if (lprnt)  print *,' dtdtb=',dtdt(ipr,:)
!
      do i=1,im
        if (slmsk(i) .eq. 0) then
          oro_land(i) = 0.0
        else
          oro_land(i) = oro(i)
        endif
      enddo
      if (OLD_MONIN) then
        if (mstrat) then
          CALL MONINP1(IX,IM,LEVS,nvdiff,DVDT,DUDT,DTDT,DQDT,
     &     UGRS,VGRS,TGRS,QGRS,
     &     prsik(1,1),RB,ffmm,ffhh,TSEA,QSS,HFLX,EVAP,STRESS,WIND,KPBL,
     &     PRSI,DEL,PRSL,PRSLK,PHII,PHIL,RCL,dtp,
     &     DUSFC1,DVSFC1,DTSFC1,DQSFC1,HPBL,GAMT,GAMQ,DKT,
     &     kinver, oro_land)
        else
          CALL MONINP(IX,IM,LEVS,nvdiff,DVDT,DUDT,DTDT,DQDT,
     &     UGRS,VGRS,TGRS,QGRS,
     &     prsik(1,1),RB,ffmm,ffhh,TSEA,QSS,HFLX,EVAP,STRESS,WIND,KPBL,
     &     PRSI,DEL,PRSL,PRSLK,PHII,PHIL,RCL,dtp,
     &     DUSFC1,DVSFC1,DTSFC1,DQSFC1,HPBL,GAMT,GAMQ,DKT)
        endif
      ELSE
!       if (mstrat) then
!         CALL MONINQ1(IX,IM,LEVS,nvdiff,DVDT,DUDT,DTDT,DQDT,
!    &     UGRS,VGRS,TGRS,QGRS,swh,hlw,xmu,
!    &     prsik(1,1),RB,ffmm,ffhh,TSEA,QSS,HFLX,EVAP,STRESS,WIND,KPBL,
!    &     PRSI,DEL,PRSL,PRSLK,PHII,PHIL,RCS,dtp,
!    &     DUSFC1,DVSFC1,DTSFC1,DQSFC1,HPBL,GAMT,GAMQ,DKT,
!    &     kinver)
!       else
          CALL MONINQ(IX,IM,LEVS,nvdiff,DVDT,DUDT,DTDT,DQDT,
     &     UGRS,VGRS,TGRS,QGRS,swh,hlw,xmu,
     &     prsik(1,1),RB,ffmm,ffhh,TSEA,QSS,HFLX,EVAP,STRESS,WIND,KPBL,
     &     PRSI,DEL,PRSL,PRSLK,PHII,PHIL,RCS,dtp,
     &     DUSFC1,DVSFC1,DTSFC1,DQSFC1,HPBL,GAMT,GAMQ,DKT)
!       endif
      ENDIF
!
!     if (ntoz .gt. 0) dqdt(:,:,ntoz) = 0.0
!
!     if (lprnt) then
!       print *,' dusfc1=',dusfc1(ipr)
!       print *,' dtsfc1=',dtsfc1(ipr)
!       print *,' dqsfc1=',dqsfc1(ipr)
!       print *,' dtdt=',dtdt(ipr,:)
!       print *,' dudtm=',dudt(ipr,:)
!     endif

!-> Coupling insertion
      if(lssav_cc_dummy) then
        DUSFC_cc_dummy(1:IM)=DUSFC_cc_dummy(1:IM)+DUSFC1 !*DTF <-na h.(?)
        DVSFC_cc_dummy(1:IM)=DVSFC_cc_dummy(1:IM)+DVSFC1 !*DTF <-na h.(?)
        DTSFC_cc_dummy(1:IM)=DTSFC_cc_dummy(1:IM)+DTSFC1 !*DTF <-na h.(?)
        DQSFC_cc_dummy(1:IM)=DQSFC_cc_dummy(1:IM)+DQSFC1 !*DTF <-na h.(?)
      end if
!<- Coupling insertion

      if(lssav) then
        do i=1,im
          DUSFC(i)  = DUSFC(i) + DUSFC1(i)*DTF
          DVSFC(i)  = DVSFC(i) + DVSFC1(i)*DTF
          DTSFC(i)  = DTSFC(i) + DTSFC1(i)*DTF
          DQSFC(i)  = DQSFC(i) + DQSFC1(i)*DTF
          dtsfci(i) = dtsfc1(i)
          dqsfci(i) = dqsfc1(i)
!hchuang code change 11/12/2007 [+1L]
          gpblh(i)  =  gpblh(i)  + hpbl(i) * DTF
        enddo
        IF (LDIAG3D) THEN
          DO K=1,LEVS
            do i=1,im
              tem1 = rcs(i) * dtf
              tem  = dtdt(i,k) - (hlw(i,k)+swh(i,k)*xmu(i))
              DT3DT(i,k,3) = DT3DT(i,k,3) + tem*DTF
 !            DQ3DT(i,k,1) = DQ3DT(i,k,1) + DQDT(i,K,1)*DTF
              DU3DT(i,k,1) = DU3DT(i,k,1) + DUDT(i,K)*tem1
              DU3DT(i,k,2) = DU3DT(i,k,2) - DUDT(i,K)*tem1
              DV3DT(i,k,1) = DV3DT(i,k,1) + DVDT(i,K)*tem1
              DV3DT(i,k,2) = DV3DT(i,k,2) - DVDT(i,K)*tem1
            enddo
          ENDDO
        ENDIF
        IF ( LDIAG3D .OR. LGGFS3D ) THEN
          DO K=1,LEVS
            do i=1,im
              DQ3DT(i,k,1) = DQ3DT(i,k,1) + DQDT(i,K,1)*DTF
            enddo
          ENDDO
          if (ntoz .gt. 0) then
            DO K=1,LEVS
              do i=1,im
                DQ3DT(i,k,5) = DQ3DT(i,k,5) + DQDT(i,K,ntoz)*DTF
              enddo
            enddo
          endif
        ENDIF
        IF ( LGGFS3D ) THEN
          DO K=1,LEVS-1
            do i=1,im
              dkh(I,K) = dkh(I,K) + dkt(I,K)*dtf
            enddo
          ENDDO
        ENDIF
      endif
!
      if (NMTVR .eq. 6) then
        do i=1,im
          oc(i) = hprime(i,2)
        enddo
        do k = 1, 4
          do i=1,im
            oa4(i,k) = hprime(i,k+2)
            clx(i,k) = 0.0
          enddo
        enddo
      elseif(NMTVR .eq. 10) then
        do i=1,im
          oc(i) = hprime(i,2)
        enddo
        do k = 1, 4
          do i=1,im
            oa4(i,k) = hprime(i,k+2)
            clx(i,k) = hprime(i,k+6)
          enddo
        enddo
! --- lm mb (*j*)
      elseif(NMTVR .eq. 14) then
! --- get the kim fields (until this is changed)
        do i=1,im
          oc(i) = hprime(i,2)
        enddo
        do k = 1, 4
          do i=1,im
            oa4(i,k) = hprime(i,k+2)
            clx(i,k) = hprime(i,k+6)
          enddo
        enddo
          do i=1,im
            theta(i)  = hprime(i,11)
            gamma(i)  = hprime(i,12)
            sigma(i)  = hprime(i,13)
            elvmax(i) = hprime(i,14)
          enddo
      else
        oc  = 0
        oa4 = 0
        clx = 0
        theta = 0
        gamma = 0
        sigma = 0
        elvmax = 0
      endif
!sela also replace lonf2 with cleff in this routine and compute
!sela cleff for all columns once before the loops on lat and lon
!sela do this in gloopb once and for all, better yet, in step1
!
!
!     Call (old) operational gravity-wave drag
!
!     CALL GWDPS(lonf2,LEVS,DVDT,DUDT,UGRS,VGRS,TGRS,QGRS,
!    & PGR,SI,DEL,CL,SL,slk,RCL,dtp,LAT,HPRIME(ilon,ilat,1),
!    & oc,oa4,DUSFCG,DVSFCG)
!
!     if(lssav) then
!       DUGWD(ilon,ilat)=DUGWD(ilon,ilat)+DUSFCG*DTF
!       DVGWD(ilon,ilat)=DVGWD(ilon,ilat)+DVSFCG*DTF
!       IF (LDIAG3D) THEN
!         work1 = rcs * dtf
!         DO K=1,LEVS
!           DU3DT(k,2,ilon,ilat) = DU3DT(k,2,ilon,ilat) + DUDT(K)*work1
!           DV3DT(k,2,ilon,ilat) = DV3DT(k,2,ilon,ilat) + DVDT(K)*work1
!         ENDDO
!       ENDIF
!     endif
!#ifdef NEWGWD
!
!     Call New operational gravity-wave drag
!
!     if(lprnt) then
!       print *,' kdt=',kdt,' rcl=',rcl(ipr),' dtp=',dtp,
!    &' pgr=',pgr(ipr),' kpbl=',kpbl,grav,   CP, RD, RV, LONF
!       print *,' ugrs=',ugrs
!       print *,' vgrs=',vgrs
!       print *,' tgrs=',tgrs(ipr,:)
!       print *,' qgrs=',qgrs
!       print *,' hprime=',hprime(ipr,:)
!       print *,' oa4=',oa4,' oc=',oc,' clx=',clx
!       print *,' si=',si
!       print *,' prsl=',prsl(1,:),' me=',me
!       print *,' del=',del(1,:),' me=',me
!       print *,' prslk=',prslk(1,:),' me=',me
!       print *,' prsik=',prsik(1,:),' me=',me
!     endif
!
      CALL GWDPS(IM, IX, IM,   LEVS,  DVDT, DUDT,
     &           UGRS,   VGRS, TGRS,  QGRS,
     &           KPBL,   PRSI, DEL,   PRSL, PRSLK,
     &           PHII,   PHIL, RCL, DTP,
!!   &           PGR,    KPBL, PRSI,  DEL, PRSL, PRSLK, RCL, DTP,
     &           KDT,    hprime(1,1), oc, oa4, clx,
     &           theta,sigma,gamma,elvmax,DUSFCG, DVSFCG,
     &           grav,  CP, RD, RV, LONF, nmtvr, me, lprnt,ipr)
!
!     if (lprnt)  print *,' dudtg=',dudt(ipr,:)
      if(lssav) then
        do i=1,im
          DUGWD(i) = DUGWD(i) + DUSFCG(i)*DTF
          DVGWD(i) = DVGWD(i) + DVSFCG(i)*DTF
        enddo
!     if (lprnt) print *,' dugwd=',dugwd(ipr),' dusfcg=',dusfcg(ipr)
!     if (lprnt) print *,' dvgwd=',dvgwd(ipr),' dvsfcg=',dvsfcg(ipr)
        IF (LDIAG3D) THEN
          DO K=1,LEVS
            do i=1,im
              tem = rcs(i) * dtf
              DU3DT(i,k,2) = DU3DT(i,k,2) + DUDT(i,K)*tem
              DV3DT(i,k,2) = DV3DT(i,k,2) + DVDT(i,K)*tem
            enddo
          ENDDO
!     if(lprnt) then
!        print *,' gwdu=',du3dt(:,:,2)
!     endif
        ENDIF
      endif
!#endif
!
      DO  K=1,LEVS
        do i=1,im
          GT0(i,K)   = TGRS(i,K)   + DTDT(i,K)   * DTp
          GU0(i,K)   = UGRS(i,K)   + DUDT(i,K)   * DTp
          GV0(i,K)   = VGRS(i,K)   + DVDT(i,K)   * DTp
        enddo
      enddo
!
      DO  N=1,ntrac
        DO  K=1,LEVS
          do i=1,im
            GQ0(i,K,N) = QGRS(i,K,N) + dqdt(i,k,n) * dtp
          enddo
        enddo
      enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     if (me .eq. 0) then
!     sumq = 0.0
!     DO K=1,LEVS
!       do i=1,im
!         sumq = sumq + (dqdt(i,k,1)+dqdt(i,k,ntcw)) * del(i,k)
!       enddo
!     enddo
!     sume = 0.0
!     do i=1,im
!       sume = sume + dqsfc1(i)
!     enddo
!     sumq = sumq * 1000.0 / grav
!     sume = sume / hvap
!     print *,' after MON: sumq=',sumq,' sume=',sume, ' kdt=',kdt
!     endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!  OZONE PHYSICS
!
      if(ntoz .gt. 0 .and. ntrac .ge. ntoz)then
        CALL OZPHYS(IX, IM, LEVS, KO3, DTP, GQ0(1,1,ntoz), GQ0(1,1,ntoz)
     &,             GT0, poz, prsl, prdout, pl_coeff, DEL, LDIAG3D
     &,             LGGFS3D, dq3dt(1,1,6), me)
!hchuang code change [r1L]
!    &,             dq3dt(1,1,6), me)
      endif
!
!     to side-step the ozone physics
!      if(NTRAC .GE. 2) then
!          do k = 1, LEVS
!              GQ0(k,ntoz) = qgrs(k,ntoz)
!          enddo
!      endif
!
!     if (lprnt) then
!     print *,' levs=',levs,' jcap=',jcap,' dtp',dtp
!    *,' slmsk=',slmsk(ilon,ilat),' rcs=',rcs,' kdt=',kdt
!     print *,' xkt2=',xkt2,' ncld=',ncld,' iq=',iq,' lat=',lat
!     print *,' pgr=',pgr
!     print *,' del=',del(1,:)
!     print *,' prsl=',prsl(1,:)
!     print *,' prslk=',prslk(1,:)
!     print *,' xkt2=',xkt2(ipr,1)
!     print *,' GT0=',GT0(ipr,:)
!    &,' kdt=',kdt,' xlon=',xlon(ipr),' xlat=',xlat(ipr)
!     print *,' dtdt=',dtdt(ipr,:)
!     print *,' gu0=',gu0(ipr,:)
!     print *,' gv0=',gv0(ipr,:)
!     print *,' gq0=',(gq0(ipr,k,1),k=1,levs)
!     print *,' gq1=',(gq0(ipr,k,ntcw),k=1,levs)
!     print *,' vvel=',vvel
!     endif
!
      IF (LDIAG3D) THEN
        do k=1,levs
          do i=1,im
            dtdt(i,k)   = GT0(i,k)
!           dqdt(i,k,1) = gq0(i,k,1)
            dudt(i,k)   = GU0(i,k)
            dvdt(i,k)   = GV0(i,k)
          enddo
        enddo
      elseif (cnvgwd) then
        do k=1,levs
          do i=1,im
            dtdt(i,k)   = GT0(i,k)
          enddo
        enddo
      ENDIF
      IF ( LDIAG3D .OR. LGGFS3D ) THEN
        do k=1,levs
          do i=1,im
            dqdt(i,k,1) = gq0(i,k,1)
          enddo
        enddo
      ENDIF
!
      call GET_PHI(im,ix,levs,ntrac,gt0,gq0,
     &             prsi,prsik,prsl,prslk,phii,phil)
!
!
!     if (lprnt) then
!     print *,' phii2=',phii(ipr,:)
!     print *,' phil2=',phil(ipr,:)
!     endif
!
      do k=1,levs
        do i=1,im
          CLW(i,k,1) = 0.0
          CLW(i,k,2) = -999.9
        enddo
      enddo
!     For convective tracer transport (while using RAS)
      if (ras) then
        if (tottracer > 0) then
          if (ntoz > 0) then
            CLW(:,:,3) = GQ0(:,:,ntoz)
            if (tracers > 0) then
              do n=1,tracers
                CLW(:,:,3+n) = GQ0(:,:,n+trc_shft)
              enddo
            endif
          else
            do n=1,tracers
              CLW(:,:,2+n) = GQ0(:,:,n+trc_shft)
            enddo
          endif
        endif
      endif
!
      do i=1,im
        ktop(i)  = 1
        kbot(i)  = levs
      enddo
!
!     Calling precipitation processes
!
      do i=1,im
!pry    work1(i) = (log(1.0 / (rcs(i)*nlons(i))) - dxmin) * dxinv
!       work1(i) = (log(1.0 / (rcs(i)*nlons(i)*latg)) - dxmin) * dxinv
        work1(i) = (log(coslat(i) / (nlons(i)*latg)) - dxmin) * dxinv
        work1(i) = max(0.0, min(1.0,work1(i)))
        work2(i) = 1.0 - work1(i)
      enddo
!
!     Calling convective parameterization
!
      if (ntcw .gt. 0) then
!
        do k=1,levs
          do i=1,im
            rhc(i,k) = rhbbot - (rhbbot-rhbtop) * (1.0-prslk(i,k))
            rhc(i,k) = rhc_max * work1(i) + rhc(i,k) * work2(i)
            rhc(i,k) = max(0.0, min(1.0,rhc(i,k)))
          enddo
        enddo
        if (num_p3d .eq. 3) then    ! Call Brad Ferrier's Microphysics
          do i=1,im
            flgmin_l(i) = flgmin(1)*work1(i) + flgmin(2)*work2(i)
          enddo
!
!***          ALGORITHM TO SEPARATE DIFFERENT HYDROMETEOR SPECIES
!
          DO K=1,LEVS
            do i=1,im
              WC     = GQ0(i,K,ntcw)
              QI     = 0.
              QR     = 0.
              QW     = 0.
              F_ice  = max(0.0, min(1.0, phy_f3d(I,K,1)))
              F_rain = max(0.0, min(1.0, phy_f3d(I,K,2)))
!
              QI = F_ice*WC
              QW = WC-QI
              IF (QW .GT. 0.0) THEN
                QR = F_rain*QW
                QW = QW-QR
              ENDIF
!
!             IF (F_ice .GE. 1.) THEN
!               QI = WC
!             ELSE IF (F_ice .LE. 0.) THEN
!               QW = WC
!             ELSE
!               QI = F_ice*WC
!               QW = WC-QI
!             ENDIF
!
!             IF (QW.GT.0. .AND. F_rain.GT.0.) THEN
!               IF (F_rain .GE. 1.) THEN
!                 QR = QW
!                 QW = 0.
!               ELSE
!                 QR = F_rain*QW
!                 QW = QW-QR
!               ENDIF
!             ENDIF
!
              QR_col(I,K) = QR
!             CLW(I,K)    = QI + QW
              CLW(I,K,1)  = QI
              CLW(I,K,2)  = QW
!
!
!***          ARRAY TO TRACK FRACTION OF "CLOUD" IN THE FORM OF ICE
!
!             IF (QI+QW .GT. EPSQ) THEN
!               FC_ice(I,K) = QI / (QI+QW)
!             ELSE
!               FC_ice(I,K) = 0.
!             ENDIF

            enddo
          ENDDO
        else
          DO K=1,LEVS
            do i=1,im
              clw(i,K,1) = GQ0(i,K,ntcw)
            enddo
          ENDDO
        endif
      else
        rhc(:,:) = 1.0
      endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     if (me .eq. 0) then
!     sumq = 0.0
!     DO K=1,LEVS
!       do i=1,im
!         sumq = sumq - (gq0(i,k,1)+clw(i,k,1)+clw(i,k,2)) * del(i,k)
!       enddo
!     enddo
!     endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      IF (.NOT. RAS) THEN
        if (.not. newsas) then
          CALL SASCNV(IM,IX,LEVS,JCAP,dtp,DEL,PRSL,PGR,PHIL,
     &                CLW,GQ0,GT0,gu0,gv0,rcs,CLD1D,
     &                RAIN1,KBOT,KTOP,kcnv,SLMSK,
     &                VVEL,xkt2,ncld,ud_mf,dd_mf,dt_mf)
!hchuang code change 03/03/08 [r1L] add SAS modification of mass flux
!    &              VVEL,xkt2,ncld)
        else
          CALL SASCNVN(IM,IX,LEVS,JCAP,dtp,DEL,PRSL,PGR,PHIL,
     &                CLW,GQ0,GT0,gu0,gv0,rcs,CLD1D,
     &                RAIN1,KBOT,KTOP,kcnv,SLMSK,
     &                VVEL,ncld,ud_mf,dd_mf,dt_mf)
!hchuang code change 03/03/08 [r1L] add SAS modification of mass flux
!    &                VVEL,xkt2,ncld)
        endif

!     if(lprnt) print *,' rain1=',rain1(ipr),' xkt2=',xkt2(ipr,1)

      ELSE

!     if(lprnt) print *,' calling RAS for kdt=',kdt,' me=',me
!    &,' lprnt=',lprnt

        CALL RASCNV(IM,     IX,    LEVS,   DTP, DTF, xkt2
!    &,             GT0,    GQ0,   GU0,    GV0, clw
     &,             GT0,    GQ0,   GU0,    GV0, clw, tottracer
     &,             prsi,   prsl,   prsik,  prslk, phil,  phii
     &,             KPBL,   CD,     RAIN1,  KBOT,  KTOP,  kcnv
     &,             phy_f2d(1,num_p2d), flipv, cb2mb
     &,             me, garea, lmh, ccwfac, nrcm, rhc
     &,             ud_mf, dd_mf, dt_mf, lprnt, ipr)
!    &,             me, 1, 1, garea, lprnt, ipr)
!
!       do i=1,im
!         if (tsea(i) .gt. 380.0 .or. tsea(i) .lt. 10) then
!           print *,' tsea=', tsea(i),' i=',i,' lat=',lat,
!    &' kdt=',kdt,' xlon=',xlon(i),' xlat=',xlat(i),' slmsk=',
!    &slmsk(i),' me=',me
!           stop
!         endif
!       enddo
!     do k=1,levs
!       do i=1,im
!         if (gt0(i,k) .gt. 330.0 .or. gt0(i,k) .lt. 80) then
!           print *,' gt0=', gt0(i,k),' i=',i,' k=',k,' lat=',lat,
!    &' kdt=',kdt,' xlon=',xlon(i),' xlat=',xlat(i)
!           stop
!         endif
!         if (gq0(i,k,1) .gt. 1.0 ) then
!           print *,' gq0=', gq0(i,k,1),' i=',i,' k=',k,' lat=',lat,
!    &' kdt=',kdt
!           stop
!         endif
!       enddo
!     enddo
!     if(lprnt) print *,' returning from RAS for kdt=', kdt,' me=',me
!    &,' lat=',lat
!
        CLD1D = 0
!
!  Update the tracers due to convective transport
!
        if (tottracer > 0) then
          if (ntoz > 0) then                         ! For ozone
            GQ0(:,:,ntoz) = clw(:,:,3)
            if (tracers > 0) then                    ! For other tracers
              do n=1,tracers
                GQ0(:,:,n+trc_shft) = CLW(:,:,3+n)
              enddo
            endif
          else
            do n=1,tracers
              GQ0(:,:,n+trc_shft) = CLW(:,:,2+n)
            enddo
          endif
        endif
      ENDIF
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     if (me .eq. 0) then
!     DO K=1,LEVS
!       do i=1,im
!         sumq = sumq + (gq0(i,k,1)+clw(i,k,1)+clw(i,k,2)) * del(i,k)
!       enddo
!     enddo
!     sumr = 0.0
!     do i=1,im
!       sumr = sumr + rain1(i)
!     enddo
!     sumq = sumq * 1000.0 / grav
!     sumr = sumr *1000
!     print *,' after RAS: sumq=',sumq,' sumr=',sumr, ' kdt=',kdt
!     endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
      if(lssav) then
        do i=1,im
          CLDWRK(i) = CLDWRK(i) + CLD1D(i) * DTF
        enddo
        IF (LDIAG3D) THEN
          DO K=1,LEVS
            do i=1,im
              tem = rcs(i) * FRAIN
              DT3DT(i,k,4) = DT3DT(i,k,4) + (GT0(i,k)-dtdt(i,k)) * FRAIN
!             DQ3DT(i,k,2) = DQ3DT(i,k,2) + (gq0(i,k,1)-dqdt(i,k,1))
!    &                                                           * FRAIN
              DU3DT(i,k,3) = DU3DT(i,k,3) + (GU0(i,k)-dudt(i,k)) * tem
              DV3DT(i,k,3) = DV3DT(i,k,3) + (GV0(i,k)-dvdt(i,k)) * tem
            enddo
          ENDDO
        ENDIF
        IF ( LDIAG3D .OR. LGGFS3D ) THEN
          DO K=1,LEVS
            do i=1,im
              DQ3DT(i,k,2) = DQ3DT(i,k,2) + (gq0(i,k,1)-dqdt(i,k,1))
     &                                                           * FRAIN
              upd_mf(i,k)  = upd_mf(i,k)  + ud_mf(i,k) * FRAIN
              dwn_mf(i,k)  = dwn_mf(i,k)  + dd_mf(i,k) * FRAIN
              det_mf(i,k)  = det_mf(i,k)  + dt_mf(i,k) * FRAIN
            enddo
          ENDDO
        ENDIF
      endif
!
      do i=1,im
        RAINC(i) = FRAIN * RAIN1(i)
      enddo
      if(lssav) then
        do i=1,im
          BENGSH(i) = BENGSH(i) + RAINC(i)
        enddo
      endif
!
!************************************************************************
      if (cnvgwd) then         !        call CONVECTIVE GRAVITY WAVE DRAG
!
!-----------------------------------------------------------------------
!        Calculate Maximum Convective Heating Rate            qmax [K/s]
!        cuhr = Temperature change due to deep convection
!-----------------------------------------------------------------------

      do i=1,IM
        qmax(i)   = 0.
        cumabs(i) = 0.
      enddo
      do k=1,LEVS
        do i=1,IM
!         cuhr(i,k) = (GT0(i,k)-DTDT(i,k)) / DTF
          cuhr(i,k) = (GT0(i,k)-DTDT(i,k)) / DTP    ! Moorthi

          cumchr(i,k) = 0.
          GWDCU(i,k)  = 0.
          GWDCV(i,k)  = 0.
          DIAGN1(i,k) = 0.
          DIAGN2(i,k) = 0.
!
          if (k >= kbot(i) .and. k <= ktop(i)) then
            qmax(i)     = max(qmax(i),cuhr(i,k))
            cumabs(i)   = cuhr(i,k) + cumabs(i)
          endif
        enddo
      enddo

      do i=1,IM
        do k=KBOT(i),KTOP(i)
          do k1=KBOT(i),k
            cumchr(i,k) = cuhr(i,k1) + cumchr(i,k)
          enddo
          cumchr(i,k) = cumchr(i,k) / cumabs(i)
        enddo
      enddo
!
      if (lprnt) then
        if (KBOT(ipr).le.KTOP(ipr)) then
          write(*,*) 'KBOT <= KTOP     for (lat,lon) = ',
     &               xlon(ipr)*57.29578,xlat(ipr)*57.29578
          write(*,*) 'kcnv KBOT KTOP QMAX DLENGTH  ',
     +    kcnv(ipr),kbot(ipr),ktop(ipr),(86400.*qmax(ipr)),dlength(ipr)
          write(*,9000) kdt
          do k=KTOP(ipr),KBOT(ipr),-1
            write(*,9010) k,(86400.*cuhr(ipr,k)),(100.*cumchr(ipr,k))
          enddo
        endif

 9000 format(/,3x,'K',5x,'CUHR(K)',4x,'CUMCHR(K)',5x,'at KDT = ',i4,/)
 9010 format(2x,i2,2x,f8.2,5x,f6.0)
!
!       print *,' Before GWDC in GBPHYS fhour ',fhour
        if (fhour >= fhourpr) then
          print *,' Before GWDC in GBPHYS start print'
          write(*,*) 'FHOUR IX IM LEVS = ',fhour,ix,im,levs
          print *,'dtp  dtf  RCS = ',dtp,dtf,RCS(ipr)

          write(*,9100)
          k=LEVS+1
          write(*,9110) k,(10.*prsi(ipr,k))
          do k=LEVS,1,-1
            write(*,9120) k,(10.*prsl(ipr,k)),(10.*del(ipr,k))
            write(*,9110) k,(10.*prsi(ipr,k))
          enddo

 9100 format(//,14x,'PRESSURE LEVELS',//,
     +' ILEV',7x,'PRSI',8x,'PRSL',8x,'DELP',/)
 9110 format(i4,2x,f10.3)
 9120 format(i4,12x,2(2x,f10.3))
         write(*,9130)
          do k=LEVS,1,-1
            write(*,9140) k,UGRS(ipr,k),GU0(ipr,k),
     +                      VGRS(ipr,k),GV0(ipr,k),
     +                      TGRS(ipr,k),GT0(ipr,k),DTDT(ipr,k),
     +                      dudt(ipr,k),dvdt(ipr,k)
          enddo

 9130 format(//,10x,'Before GWDC in GBPHYS',//,' ILEV',6x,
     +'UGRS',9x,'GU0',8x,'VGRS',9x,'GV0',8x,
     +'TGRS',9x,'GT0',8x,'GT0B',8x,'DUDT',8x,'DVDT',/)
 9140 format(i4,9(2x,f10.3))

          print *,' Before GWDC in GBPHYS end print'

        endif
      endif
      CALL GWDC(IM, IX, IM, LEVS, LAT, UGRS, VGRS, TGRS, QGRS,
     &          RCS, PRSL, PRSI, DEL, QMAX, CUMCHR, KTOP, KBOT, kcnv,
     &          GWDCU, GWDCV, grav, CP, RD, fv, dlength,
     &          lprnt, ipr, fhour,
     &          DUSFCG,DVSFCG,DIAGN1,DIAGN2)
!
      if (lprnt) then
        if (fhour.ge.fhourpr) then
          print *,' After GWDC in GBPHYS start print'
          write(*,9131)
          do k=LEVS,1,-1
            write(*,9141) k,UGRS(ipr,k),GU0(ipr,k),
     +                      VGRS(ipr,k),GV0(ipr,k),
     +                      TGRS(ipr,k),GT0(ipr,k),DTDT(ipr,k),
     +                      GWDCU(ipr,k),GWDCV(ipr,k)
          enddo

 9131 format(//,10x,'After GWDC in GBPHYS',//,' ILEV',6x,
     +'UGRS',9x,'GU0',8x,'VGRS',9x,'GV0',8x,
     +'TGRS',9x,'GT0',8x,'GT0B',7x,'GWDCU',7x,'GWDCV',/)
 9141 format(i4,9(2x,f10.3))
          print *,' After GWDC in GBPHYS end print'
        endif
      endif
!
!-----------------------------------------------------------------------
!        Write out cloud top stress and wind tendencies
!-----------------------------------------------------------------------

      if(lssav) then
        do i=1,im
          DUGWD(i) = DUGWD(i) + DUSFCG(i)*DTF
          DVGWD(i) = DVGWD(i) + DVSFCG(i)*DTF
        enddo
        IF (LDIAG3D) THEN
          DO K=1,LEVS
            do i=1,im
              tem = rcs(i) * dtf
              DU3DT(i,k,4) = DU3DT(i,k,4) + GWDCU(i,K)*tem
              DV3DT(i,k,4) = DV3DT(i,k,4) + GWDCV(i,K)*tem
!             DU3DT(i,k,2) = DU3DT(i,k,2) + DIAGN1(i,K)*tem
!             DV3DT(i,k,2) = DV3DT(i,k,2) + DIAGN2(i,K)*tem
            enddo
          ENDDO
        ENDIF
      endif

!-----------------------------------------------------------------------
!     Update the wind components with  GWDC tendencies
!-----------------------------------------------------------------------

      DO  K=1,LEVS
        do i=1,im
          GU0(i,K)   = GU0(i,K)   + GWDCU(i,K)   * DTP
          GV0(i,K)   = GV0(i,K)   + GWDCV(i,K)   * DTP
        enddo
      enddo
!
      if (lprnt) then
        if (fhour.ge.fhourpr) then
          print *,' After Tendency GWDC in GBPHYS start print'
          write(*,9132)
          do k=LEVS,1,-1
            write(*,9142) k,UGRS(ipr,k),GU0(ipr,k),VGRS(ipr,k),
     &      GV0(ipr,k),TGRS(ipr,k),GT0(ipr,k),DTDT(ipr,k),
     +      GWDCU(ipr,k),GWDCV(ipr,k)
          enddo
 9132 format(//,10x,'After Tendency GWDC in GBPHYS',//,' ILEV',6x,
     +'UGRS',9x,'GU0',8x,'VGRS',9x,'GV0',8x,
     +'TGRS',9x,'GT0',8x,'GT0B',7x,'GWDCU',7x,'GWDCV',/)
 9142 format(i4,9(2x,f10.3))
          print *,' After Tendency GWDC in GBPHYS end print'
        endif
      endif
!
      endif              !       END CONVECTIVE GRAVITY WAVE DRAG
!************************************************************************
!
!
      IF (LDIAG3D) THEN
        do k=1,levs
          do i=1,im
            dtdt(i,k)   = GT0(i,k)
!           dqdt(i,k,1) = gq0(i,k,1)
          enddo
        enddo
      ENDIF
      IF ( LDIAG3D .OR. LGGFS3D ) THEN
        do k=1,levs
          do i=1,im
            dqdt(i,k,1) = gq0(i,k,1)
          enddo
        enddo
      ENDIF
!
      if (.not. sashal) then
        ud_mf = 0.0
        dt_mf = 0.0
!       if (lprnt) print *,' levshcm=',levshcm,' gt0sh=',gt0(ipr,:)
        if (.not. mstrat) then
          CALL SHALCVt3(IM,IX,LEVSHCM,dtp,DEL,PRSI,PRSL,PRSLK,
     &                                             kcnv,GQ0,GT0)  ! for pry
        else
          CALL SHALCV(IM,IX,LEVSHCM,dtp,DEL,PRSI,PRSL,PRSLK,kcnv,GQ0,GT0
     &,               levshc,phil,kinver,ctei_r,ctei_rm)
!    &,               DPSHC,phil,kinver)
!    &,               DPSHC,kinver)
!       if (lprnt) print *,' levshcm=',levshcm,' gt0sha=',gt0(ipr,:)
        endif
!
      else
          CALL SHALCNV(IM,IX,LEVS,JCAP,dtp,DEL,PRSL,PGR,PHIL,
     &                CLW,GQ0,GT0,gu0,gv0,rcs,
     &                RAIN1,KBOT,KTOP,kcnv,SLMSK,
     &                VVEL,ncld,HPBL,HFLX,EVAP,ud_mf,dt_mf)
!
        do i=1,im
          RAINCS(i) = FRAIN * RAIN1(i)
        enddo
        if(lssav) then
          do i=1,im
            BENGSH(i) = BENGSH(i) + RAINCS(i)
          enddo
        endif
        do i=1,im
          RAINC(i) = RAINC(i) + RAINCS(i)
        enddo
        if(lssav) then
          do i=1,im
            CLDWRK(i) = CLDWRK(i) + CLD1D(i) * DTF
          enddo
        endif
      endif
!
!
      if(lssav) then
        IF (LDIAG3D) THEN
          DO K=1,LEVS
            do i=1,im
              DT3DT(i,k,5) = DT3DT(i,k,5) + (GT0(i,k)-dtdt(i,k)) * FRAIN
!             DQ3DT(i,k,3) = DQ3DT(i,k,3) + (gq0(i,k,1)-dqdt(i,k,1))
!    &                                                           * FRAIN
            enddo
          ENDDO
          do k=1,levs
            do i=1,im
              dtdt(i,k)   = GT0(i,k)
!             dqdt(i,k,1) = gq0(i,k,1)
            enddo
          enddo
        ENDIF
        IF ( LDIAG3D .OR. LGGFS3D ) THEN
          DO K=1,LEVS
            do i=1,im
              DQ3DT(i,k,3) = DQ3DT(i,k,3) + (gq0(i,k,1)-dqdt(i,k,1))
     &                                                          * FRAIN
              upd_mf(i,k)  = upd_mf(i,k)  + ud_mf(i,k) * FRAIN
              det_mf(i,k)  = det_mf(i,k)  + dt_mf(i,k) * FRAIN
            enddo
          ENDDO
!
          do k=1,levs
            do i=1,im
              dqdt(i,k,1) = gq0(i,k,1)
            enddo
          enddo
        ENDIF
      endif
!
!
      DO K=1,LEVS
        do i=1,im
          if (CLW(I,K,2) .le. -999.0) CLW(I,K,2) = 0.0
        enddo
      ENDDO
      if (ntcw .gt. 0) then
        if (num_p3d .eq. 3) then    ! Call Brad Ferrier's Microphysics
!
!***          EXTRACT CLOUD WATER & ICE FROM FC_ice
!
          DO K=1,LEVS
            do i=1,im
!             QI = CLW(I,K)*FC_ice(I,K)
!             QW = CLW(I,K) - QI
!
              QI = CLW(I,K,1)
              QW = CLW(I,K,2)
!
!***          ALGORITHM TO COMBINE DIFFERENT HYDROMETEOR SPECIES
!
!             GQ0(i,K,ntcw) = MAX(EPSQ, QI+QW+QR_col(i,K))
              GQ0(i,K,ntcw) = QI + QW + QR_col(i,K)
              IF (QI .LE. EPSQ) THEN
                phy_f3d(I,K,1) = 0.
              ELSE
                phy_f3d(I,K,1) = QI/GQ0(i,K,ntcw)
              ENDIF
              IF (QR_col(I,K) .LE. EPSQ) THEN
                phy_f3d(I,K,2) = 0.
              ELSE
                phy_f3d(I,K,2) = QR_col(I,K) / (QW+QR_col(I,K))
              ENDIF
            enddo
          ENDDO
        else
          DO K=1,LEVS
            do i=1,im
!             GQ0(i,K,ntcw) = CLW(i,K) + GQ0(i,K,ntcw)
              GQ0(i,K,ntcw) = CLW(i,K,1) + clw(i,K,2)
!             GQ0(i,K,ntcw) = CLW(i,K,1)               ! for pry
            enddo
          ENDDO
        endif
      else
        DO K=1,LEVS
          do i=1,im
              clw(i,K,1) = CLW(i,K,1) + clw(i,K,2)
          enddo
        ENDDO

      endif

!
      CALL CNVC90(CLSTP, IM,   IX,   RAINC, KBOT, KTOP, LEVS, PRSI,
     &            aCV,   aCVB, aCVT, CV,    CVB,  CVT)
!
!
      IF (NCLD .EQ. 0) THEN
        CALL LRGSCL(IX,IM,LEVS,dtp,GT0,GQ0,PRSL,DEL,PRSLK,RAIN1,CLW)
      ELSEIF (NCLD .EQ. 1) THEN
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     if (me .eq. 0) then
!     sumq = 0.0
!     DO K=1,LEVS
!       do i=1,im
!         sumq = sumq - (gq0(i,k,1)+gq0(i,k,ntcw)) * del(i,k)
!       enddo
!     enddo
!     endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!       To call moist convective adjustment
!
!       CALL MSTCNV(IM,IX,LEVS,DTP,GT0,GQ0,PRSL,DEL,PRSLK,RAIN1
!    &,                                                 lprnt,ipr)
!       do i=1,im
!         RAINC(i) = RAINC(i) + FRAIN * RAIN1(i)
!       enddo
!       if(lssav) then
!         do i=1,im
!           BENGSH(i) = BENGSH(i) + RAIN1(i) * FRAIN
!         enddo
!         IF (LDIAG3D) THEN
!           DO K=1,LEVS
!             do i=1,im
!               DT3DT(i,k,4) = DT3DT(i,k,4) + (GT0(i,k)-dtdt(i,k))
!    &                                      * FRAIN
!               DQ3DT(i,k,2) = DQ3DT(i,k,2) + (gq0(i,k,1)-dqdt(i,k,1))
!    &                                      * FRAIN
!             enddo
!           ENDDO
!         ENDIF
!       endif
!
!       IF (LDIAG3D) THEN
!         do k=1,levs
!           do i=1,im
!             dtdt(i,k)   = GT0(i,k)
!             dqdt(i,k,1) = gq0(i,k,1)
!           enddo
!         enddo
!       ENDIF
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     if (me .eq. 0) then
!     DO K=1,LEVS
!       do i=1,im
!         sumq = sumq + (gq0(i,k,1)+gq0(i,k,ntcw)) * del(i,k)
!       enddo
!     enddo
!     sumr = 0.0
!     do i=1,im
!       sumr = sumr + rain1(i)
!     enddo
!     sumq = sumq * 1000.0 / grav
!     sumr = sumr *1000
!     print *,' after MCN: sumq=',sumq,' sumr=',sumr, ' kdt=',kdt
!     endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!       moist convective adjustment over
!
!
      if (num_p3d .eq. 3) then    ! Call Brad Ferrier's Microphysics
!
        do i=1,im
          xncw(i) = ncw(2) * work1(i) + ncw(1) * work2(i)
        enddo
        if (kdt .eq. 1 .and. abs(xlon(1)) .lt. 0.0001)
     &          print *,' xncw=',xncw(1),' rhc=',rhc(1,1)
     &, ' work1=',work1(1),' work2=',work2(1),' flgmin=',flgmin_l(1)
     &, ' lon=',xlon(1) * 57.29578,' lat=',lat,' me=',me
!    &, ' lon=',xlon(1) * 57.29578,' lat=',xlat(1) * 57.29578
!    &,' kinver=',kinver(1)
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     if (me .eq. 0) then
!     sumq = 0.0
!     DO K=1,LEVS
!       do i=1,im
!         sumq = sumq - (gq0(i,k,1)+gq0(i,k,ntcw)) * del(i,k)
!       enddo
!     enddo
!     endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!          if (lprnt) print *,' ipr=',ipr,' gt0_gsmb=',gt0(ipr,:)
!    &,' xlon=',xlon(ipr),' xlat=',xlat(ipr)
        call GSMDRIVE(IM, IX, LEVS, DTP, PRSL, DEL
     &,               GT0, GQ0(1,1,1), GQ0(1,1,ntcw), slmsk
     &,               phy_f3d(1,1,1),  phy_f3d(1,1,2)
     &,               phy_f3d(1,1,3), RAIN1, SR, grav
     &,               hvap, hsub, cp, rhc, xncw, flgmin_l
     &,               me, lprnt, ipr)
!    &,               hvap, hsub, cp, rhc, xncw, me, lprnt, ipr)
!
!          if (lprnt) print *,' ipr=',ipr,' gt0_gsma=',gt0(ipr,:)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     if (me .eq. 0) then
!     DO K=1,LEVS
!       do i=1,im
!         sumq = sumq + (gq0(i,k,1)+gq0(i,k,ntcw)) * del(i,k)
!       enddo
!     enddo
!     sumr = 0.0
!     do i=1,im
!       sumr = sumr + rain1(i)
!     enddo
!     sumq = sumq * 1000.0 / grav
!     sumr = sumr *1000
!     print *,' after GSM: sumq=',sumq,' sumr=',sumr, ' kdt=',kdt
!     endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
      elseif (num_p3d .eq. 4) then ! Call Zhao/Carr/Sundqvist Microphysics
!
!   The following commented by Moorthi on 11/01/2002
!       do k=1,levs/2
!         do i=1,im
!!          if (k .le. kinver(i) .and. slmsk(i) .eq. 0.0) then
!           if (k .le. kinver(i)) then
!             rhc(i,k) = 0.85
!           endif
!         enddo
!       enddo
!       if (me .eq. 0 .and. kdt .eq. 1 .and. ilon .eq. 1)
!    &      print *,' rhc=',rhc
!       if (me .eq. 0 .and. kdt .eq. 1)
!    &          print *,' rhc=',rhc(1,1)
!    &, ' lon=',xlon(1) * 57.29578,' lat=',xlat(1) * 57.29578
!    &,' kinver=',kinver(1)
!       if (kdt .eq. 1 .and. abs(xlon(1)) .lt. 0.0001)
!    &          print *,' rhc=',rhc(1,1)
!    &, ' work1=',work1(1),' work2=',work2(1)
!    &, ' lon=',xlon(1) * 57.29578,' lat=',lat,' me=',me
!
!          if (lprnt) print *,' ipr=',ipr,' gt0_gsc=',gt0(ipr,:)
!    &,' xlon=',xlon(ipr),' xlat=',xlat(ipr)
!          if (lprnt) print *,' ipr=',ipr,' gq0_gsc=',gq0(ipr,:,1)
!    &,' xlon=',xlon(ipr),' xlat=',xlat(ipr)
!          if (lprnt) print *,' ipr=',ipr,' gc0_gsc=',gq0(ipr,:,ntcw)
!    &,' xlon=',xlon(ipr),' xlat=',xlat(ipr)
!          if (lprnt) print *,' ipr=',ipr,' phy_f3d1=',phy_f3d(ipr,:,1)
!          if (lprnt) print *,' ipr=',ipr,' phy_f3d2=',phy_f3d(ipr,:,2)
!          if (lprnt) print *,' ipr=',ipr,' phy_f3d3=',phy_f3d(ipr,:,3)
!          if (lprnt) print *,' ipr=',ipr,' phy_f2d=',
!    &phy_f2d(ipr,1:num_p2d),' num_p2d=',num_p2d
!    &,    phy_f2d(ipr,2)
        CALL GSCOND(IM, IX, LEVS, DTP, PRSL, PGR,
     &              GQ0(1,1,1), GQ0(1,1,ntcw), GT0,
     &              phy_f3d(1,1,1), phy_f3d(1,1,2), phy_f2d(1,1),
     &              phy_f3d(1,1,3), phy_f3d(1,1,4), phy_f2d(1,2),
!    &              rhc,lprnt)
     &              rhc,lprnt, ipr)
        CALL PRECPD(IM, IX, LEVS, DTP, DEL, PRSL, PGR,
     &              GQ0(1,1,1), GQ0(1,1,ntcw), GT0, RAIN1,
!hchuang code change [+1L] : for rain production rate
     &               rainp,
     &               rhc, lprnt, ipr)
!    &              GQ0(1,1,1), GQ0(1,1,ntcw), GT0, RAIN1, rhc, lprnt)
!     if (lprnt) print *,' rain1=',rain1(ipr)
      endif
!
!     ELSEIF (NCLD .EQ. 2) THEN
!     CALL CLOUD3(1, 1, LEVS,DTP,PSEXP,
!    1            GT0(1,1),GQ0(1,1,1),GQ0(1,1,ntcw),NCLD,SL,DEL,SLK,
!    2            RAIN1,LAT,VVEL,KDT,FHOUR)
!     ELSEIF (NCLD .EQ. 4) THEN
!     CALL CLOUD5(1, 1, LEVS,DTP,PSEXP,
!    1            GT0,GQ0(1,1),GQ0(1,ntcw),NCLD,SL,DEL,SLK,
!    2            RAIN1,LAT,VVEL,KDT,FHOUR)
!     ELSEIF (NCLD .EQ. 5) THEN
!     CALL CLOUD6(1, 1, LEVS,DTP,PSEXP,
!    1            GT0,GQ0(1,1),GQ0(1,ntcw),NCLD,SL,DEL,SLK,
!    2            RAIN1,LAT,VVEL,KDT,FHOUR)
      ENDIF
!
!     if (lprnt) print *,' rain1=',rain1(ipr),' rainc=',rainc(ipr)
      do i=1,im
        RAINL(i) = FRAIN    * RAIN1(i)
        RAIN(i)  = RAINC(i) + RAINL(i)
      enddo

!!-> Coupling insertion
      if(lssav_cc_dummy) then
        PRECR_cc_dummy(1:IM)=PRECR_cc_dummy(1:IM)+RAIN(1:IM)
      end if
!!<- Coupling insertion

      if (cal_pre) then
!       HCHUANG: add dominant precipitation type algorithm

        call calpreciptype(kdt,nrcm,im,ix,LEVS,LEVS+1,xkt2,
     &    xlat,xlon,gt0,gq0,prsl,prsi,RAIN,
     &    phii,num_p3d,tsea,sr,phy_f3d(1,1,3),    ! input
     &    DOMR,DOMZR,DOMIP,DOMS)                  ! output

!
!        if (lprnt) print*,'debug calpreciptype: DOMR,DOMZR,DOMIP,DOMS '
!     &,DOMR(ipr),DOMZR(ipr),DOMIP(ipr),DOMS(ipr)
!        do i=1,im
!         if (abs(xlon(i)*57.29578-114.0) .lt. 0.2  .and.
!     &    abs(xlat(i)*57.29578-40.0) .lt. 0.2)
!     &    print*,'debug calpreciptype: DOMR,DOMZR,DOMIP,DOMS ',
!     &    DOMR(i),DOMZR(i),DOMIP(i),DOMS(i)
!       end do
!       HCHUANG: use new precipitation type to decide snow flag for LSM snow accumulation
        do i=1,im
          if(DOMS(i) >0.0 .or. DOMIP(i)>0.0)then
            SRFLAG(i) = 1.
          else
            SRFLAG(i) = 0.
          end if
        enddo
      endif


      if(lssav) then
        do i=1,im
          GESHEM(i) = GESHEM(i) + RAIN(i)
        enddo
        IF (LDIAG3D) THEN
          DO K=1,LEVS
            do i=1,im
              DT3DT(i,k,6) = DT3DT(i,k,6) + (GT0(i,k)-dtdt(i,k)) * FRAIN
!             DQ3DT(i,k,4) = DQ3DT(i,k,4) + (gq0(i,k,1)-dqdt(i,k,1))
!    &                                                           * FRAIN
            enddo
          ENDDO
        ENDIF
        IF ( LDIAG3D .OR. LGGFS3D ) THEN
          DO K=1,LEVS
            do i=1,im
              DQ3DT(i,k,4) = DQ3DT(i,k,4) + (gq0(i,k,1)-dqdt(i,k,1))
     &                                                           * FRAIN
            enddo
          ENDDO
        ENDIF
!hchuang code change [+8L] 10/08/2008 rnp is 1.E+3 m/dtp, with mutiply by frain dtf/dtp
!              rnp becomes rain (km) amount when output it will become
!              rnp/(total time)  that is km/s
!
        IF (LGGFS3D) THEN
          DO K=1,LEVS
            do i=1,im
              rnp(i,k) = rainp(i,k) * frain ! 3D large scale liqid rain amount
                                            ! of one DTF time period in mm
            enddo
          ENDDO
        ENDIF
!
      endif
!
!  ESTIMATE T850 FOR RAIN-SNOW DECISION
!
      do i=1,im
        T850(i) = GT0(I,1)
      enddo
      DO K = 1, LEVS - 1
        do i=1,im
          IF(PRSL(I,K) .GT. P850 .AND. PRSL(I,K+1) .LE. P850) THEN
            T850(i) = GT0(i,K) - (PRSL(i,k)-P850)
     &              / (PRSL(I,K)-PRSL(I,K+1)) * (GT0(I,K)-GT0(I,K+1))
          ENDIF
        enddo
      ENDDO

!lu [-4L/+3L]: snow-rain detection is performed in land/sice module
!
      if (cal_pre) then ! HCHUANG: new precip type algorithm defines srflag
        do i = 1, im
          tprcp(i) = rain(i)            ! clu: rain -> tprcp
          if (t850(i) <= 273.16) then
!  --- ...  wei: when call osu lsm, neutral impact, for code consistency
            if (lsm == 0 .and. slmsk(i) /= 0.0) then
              sheleg(i)  = sheleg(i) + 1.e3*rain(i)
              tprcp(i)  = 0.
            endif
          endif
        enddo
      else
        do i = 1, im
          tprcp(i) = rain(i)            ! clu: rain -> tprcp
          srflag(i) = 0.                ! clu: default srflag as 'rain' (i.e. 0)
          if (t850(i) <= 273.16) then
            srflag(i) = 1.              ! clu: set srflag to 'snow' (i.e. 1)
!  --- ...  wei: when call osu lsm, neutral impact, for code consistency
            if (lsm == 0 .and. slmsk(i) /= 0.0) then
              sheleg(i)  = sheleg(i) + 1.e3*rain(i)
              tprcp(i)  = 0.
            endif
          endif
        enddo
      endif

!     if (lprnt) print *,' TPRCP=',tprcp(ipr),' rain=',rain(ipr)

!-->cpl insertion
!      SNW_cc_dummy=0.
      do i=1,im
!       if(T850(i).LE.273.16.AND.SLMSK(i).NE.0.) THEN
        if(T850(i).LE.273.16) THEN
           LPREC_cc_dummy(i)=0.0
           SNW_cc_dummy(i)=RAIN(i)
        else
           LPREC_cc_dummy(i)=RAIN(i)
           SNW_cc_dummy(i)=0.0
        endif
      enddo
!<-- cpl insertion
                                                                                                        
CWei [+2] when call OSU LSM
!
!  UPDATE SOIL MOISTURE AND CANOPY WATER AFTER PRECIPITATION computaion
!
      if (lsm == 0) then
        CALL PROGT2(IM,LSOIL,RHSCNPY,RHSMC,AI,BI,CCI,SMSOIL,
     &              SLMSK,CANOPY,TPRCP,RUNOF,SNOWMT,
     &              ZSOIL,SOILTYP,SIGMAF,dtf,me)
                                                                                                        
CWei [+5]: let soil liquid water equal to soil total water
        DO K = 1, LSOIL
          do i=1,im
           IF(SLMSK(i).EQ.1.) THEN
            SLSOIL(i,K) = SMSOIL(i,k)
           ENDIF
          enddo
        ENDDO
      endif

!
!  TOTAL RUNOFF IS COMPOSED OF DRAINAGE INTO WATER TABLE AND
!  RUNOFF AT THE SURFACE AND IS ACCUMULATED IN UNIT OF METERS
!
      if(lssav) then
        do i=1,im
          RUNOFF(i) = RUNOFF(i) + (DRAIN(i)+RUNOF(i)) * DTF / 1000.0
Cwei added 10/24/2006
          SRUNOFF(i) = SRUNOFF(i) + RUNOF(i) * DTF / 1000.0
!hchaung 11/19/2007 [+1L] ;  top soil moisture unit is in mm
          gsoil(i)   =  gsoil(i)  + smsoil(i,1)  * DTF
        enddo
      endif
       
!c-- XW: FOR SEA-ICE Nov04
!
!  RETURN UPDATED ICE THICKNESS & CONCENTRATION TO GLOBAL ARRAYS
!
      do i=1,im
        IF(SLMSK(i).EQ.2.) THEN
          HICE(i)  = ZICE(i)
          FICE(i)  = CICE(i)
          TISFC(i) = TICE(i)
        else
          HICE(i)  = 0.0
          FICE(i)  = 0.0
          TISFC(i) = TSEA(i)
        endif
      enddo
!c-- XW: END SEA-ICE
       
Clu [-10L]: comment out smc/stc update
!
!  RETURN UPDATED SMSOIL AND STSOIL TO GLOBAL ARRAYS
!
      DO K = 1, LSOIL
        do i=1,im
          SMC(i,k) = SMSOIL(i,K)
          STC(i,k) = STSOIL(i,K)
          SLC(i,k) = SLSOIL(i,K)
        enddo
      ENDDO
!
! calc. integral of moistue in pwat
!
      do i=1,im
        PWAT(i) = 0.
      enddo
      DO K=1,LEVS
        do i=1,im
          work1(i) = 0.0
        enddo
        if (ncld .gt. 0) then
          do ic=ntcw, ntcw+ncld-1
            do i=1,im
!             work1(i) = work1(i) + max(gq0(i,k,ic), qmin)
              work1(i) = work1(i) + gq0(i,k,ic)
            enddo
          enddo
        endif
        do i=1,im
! inside this routine, let t as dry temperature only            ! hmhj
!         work2(i) = 1.0 + fv * max(gq0(i,k,1),qmin)            ! hmhj
!         GT0(i,K) = GT0(i,K) * work2(i)                        ! hmhj
          PWAT(i)  = PWAT(i)  + DEL(i,K)*(GQ0(i,K,1)+work1(i))
        enddo
      ENDDO
  490 CONTINUE
      do i=1,im
        PWAT(i)  = PWAT(i)*(1.E3/grav)
      enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     if (me .eq. 0) then
!     do i=1,im
!       tem1 = dqsfc(i) - evapo(i)
!       tem2 = geshem(i) - raino(i)
!     print *,' pwatdif=',pwat(i)-pwato(i),' Edif=',tem1
!    &,' Pdif=',tem2,' E-P=',(tem1/hvap-tem2*1000)/frain
!    &,' pwato=',pwato(i),' pwat=',pwat(i)
!     enddo
!     endif
!     if (lprnt) then
!       do i=1,im
!          print *,' i=',i,' gt0=',gt0(i,:),' kdt=',kdt
!    &,' xlon=',xlon(i)*57.296,' xlat(i)=',xlat(i)*57.296
!          print *,' ipr=',ipr,' gt0=',gt0(ipr,:),' kdt=',kdt,' ipr='
!       print *,' gt0E=',gt0(ipr,:)
!    &,' xlon=',xlon(ipr),' xlat=',xlat(ipr)
!       print *,' gu0=',gu0(ipr,:)
!       print *,' gv0=',gv0(ipr,:)
!       print *,' gq0=',gq0(ipr,:,3)
!       print *,' gq0=',gq0(ipr,14,3)
!    &,' xlon=',xlon(ipr),' xlat=',xlat(ipr)
!    &,ipr
!       enddo
!     endif
!     do i=1,im
!        print *,' i=',i,' me=',me,' lat=',lat,' gt0=',gt0(i,:)
!     enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!     CALL dscal(LEVS*IX,rcl,GU0,1) ! Commented by Moorthi -- Moved to gbphys_call.f
!     CALL dscal(LEVS*IX,rcl,GV0,1)
!
csela  write(66,103)gq0(1,2),gq0(levs,2)
103    format(1h ,' at end  ozphys gq0(1,2) gq0(levs,2)',2(2x,e12.3))  
!
      deallocate (clw)

!     if(lprnt) call mpi_quit(7)
!     if (kdt .gt. 1)  call mpi_quit(7)
!
      RETURN
      END


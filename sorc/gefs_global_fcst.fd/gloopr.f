       subroutine gloopr
     x    (trie_ls,trio_ls,
     x     ls_node,ls_nodes,max_ls_nodes,
     x     lats_nodes_r,global_lats_r,
     x     lonsperlar,
     x     epse,epso,epsedn,epsodn,
     x     snnp1ev,snnp1od, plnev_r,plnod_r,
     x     pddev_r,pddod_r,
!    x     snnp1ev,snnp1od,ndexev,ndexod,
!    x     plnev_r,plnod_r,pddev_r,pddod_r,plnew_r,plnow_r,
     x     phour,
     &     xlon,xlat,coszdg,COSZEN,
     &     SLMSK,SHELEG,SNCOVR,SNOALB,ZORL,TSEA,HPRIME,
Clu [+1L]: extract snow-free albedo (SFALB)
     +     SFALB,
     &     ALVSF,ALNSF ,ALVWF ,ALNWF,FACSF ,FACWF,CV,CVT ,
     &     CVB  ,SWH,HLW,SFCNSW,SFCDLW,
     &     FICE ,TISFC, SFCDSW,                          ! FOR SEA-ICE - XW Nov04
     &     TSFLW,FLUXR ,       phy_f3d,slag,sdec,cdec,NBLCK,KDT,
     &     global_times_r)
cc
!gwvport#include "f_hpm.h"
!
      USE MACHINE              ,     ONLY : kind_phys
      USE FUNCPHYS             ,     ONLY : fpkap
      USE PHYSCONS, FV => con_fvirt, rerth => con_rerth 	! hmhj

      use module_radiation_driver,   only : radinit, grrad
      use module_radiation_astronomy,only : astronomy
!
!! ---  for optional spectral band heating outputs
!!    use module_radsw_parameters,   only : NBDSW
!!    use module_radlw_parameters,   only : NBDLW
!
      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use coordinate_def					! hmhj
      use tracer_const						! hmhj
      use d3d_def , only : cldcov
!
      implicit none
      include 'mpif.h'
!
      real (kind=kind_phys), parameter :: QMIN =1.0e-10
      real (kind=kind_evod), parameter :: Typical_pgr = 95.0
      real (kind=kind_evod), parameter :: cons0 = 0.0,  cons2 = 2.0
!
!  --- ...  inputs:
      integer, intent(in) :: ls_node(LS_DIM,3), ls_nodes(LS_DIM,NODES), &
     &                       max_ls_nodes(NODES), lats_nodes_r(NODES),  &
     &                       global_lats_r(LATR), lonsperlar(LATR)

      integer, intent(in) :: NBLCK

      real(kind=kind_evod), dimension(LEN_TRIE_LS), intent(in) ::       &
     &                       epse, epsedn, snnp1ev

      real(kind=kind_evod), dimension(LEN_TRIO_LS), intent(in) ::       &
     &                       epso, epsodn, snnp1od

      real(kind=kind_evod), intent(in) :: plnev_r(LEN_TRIE_LS, LATR2)
      real(kind=kind_evod), intent(in) :: plnod_r(LEN_TRIO_LS, LATR2)

      real (kind=kind_phys), dimension(LONR,LATS_NODE_R), intent(in) :: &
     &                       xlon, xlat, slmsk, sheleg, zorl, tsea,     &
     &                       alvsf, alnsf, alvwf, alnwf, facsf, facwf,  &
     &                       cv, cvt, cvb, FICE, tisfc, sncovr, snoalb

      real (kind=kind_phys), intent(in) ::                              &
     &                    hprime(NMTVR,LONR,LATS_NODE_R), phour,        &
     &                    phy_f3d(NGPTC,LEVS,NBLCK,LATS_NODE_R,NUM_P3D)
!
!  --- ...  input and output:
      real(kind=kind_evod), intent(inout) ::                            &
     &                    trie_ls(LEN_TRIE_LS,2,11*LEVS+3*LEVH+6),      &
     &                    trio_ls(LEN_TRIO_LS,2,11*LEVS+3*LEVH+6)
      integer              ipt_ls                                       ! hmhj
      real(kind=kind_evod) reall                                        ! hmhj
      real(kind=kind_evod) rlcs2(jcap1)                                 ! hmhj


      real (kind=kind_phys), intent(inout) ::                           &
     &                    fluxr (NFXR,LONR,LATS_NODE_R)

!  --- ...  inputs but not used anymore:
      real(kind=kind_evod), intent(in) :: pddev_r(LEN_TRIE_LS,LATR2),   &
     &                                    pddod_r(LEN_TRIO_LS,LATR2)    &
!    &                                    plnew_r(LEN_TRIE_LS,LATR2),   &
!    &                                    plnow_r(LEN_TRIO_LS,LATR2)
!    &                                    syn_ls_r(4*LS_DIM,LOTS,LATR2)

!     integer, intent(in) :: ndexev(LEN_TRIE_LS), ndexod(LEN_TRIO_LS)
      integer, intent(in) :: KDT
!  --- ...  outputs:
      real(kind=kind_evod), intent(out) ::                              &
     &                    global_times_r(LATG,NODES)
      real(kind=kind_evod) ::                                           &
     &                    for_gr_r_1(LONRX*LOTS,LATS_DIM_R),            &
     &                    dyn_gr_r_1(lonrx*lotd,lats_dim_r),            ! hmhj
     &                    for_gr_r_2(LONRX*LOTS,LATS_DIM_R),
     &                    dyn_gr_r_2(lonrx*lotd,lats_dim_r)             ! hmhj

      real (kind=kind_phys), intent(out) ::                             &
     &                    swh(NGPTC,LEVS,NBLCK,LATS_NODE_R),            &
     &                    hlw(NGPTC,LEVS,NBLCK,LATS_NODE_R)

      real (kind=kind_phys),dimension(LONR,LATS_NODE_R), intent(out) :: &
     &                    coszdg, coszen, sfcnsw, sfcdlw, tsflw,        &
     &                    sfcdsw, SFALB

      real (kind=kind_phys), intent(out) :: slag, sdec, cdec

!! --- ...  optional spectral band heating rates
!!    real (kind=kind_phys), optional, intent(out) ::                   &
!!   &                 htrswb(NGPTC,LEVS,NBDSW,NBLCK,LATS_NODE_R),      &
!!   &                 htrlwb(NGPTC,LEVS,NBDLW,NBLCK,LATS_NODE_R)

!  --- ...  locals:
!     real(kind=kind_phys) :: prsl(NGPTC,LEVS), prdel(NGPTC,LEVS),      &
      real(kind=kind_phys) :: prsl(NGPTC,LEVS),  prslk(NGPTC,LEVS),     &
     &                        prsi(NGPTC,LEVP1), prsik(NGPTC,LEVP1)

      real (kind=kind_phys) :: si_loc(LEVR+1)
!     real (kind=kind_phys) :: si_loc(LEVR+1), prslk(NGPTC,LEVR)

      real (kind=kind_phys) :: gu(NGPTC,LEVS), gv1(NGPTC,LEVS),         &
     &                        gt(NGPTC,LEVR), gd (NGPTC,LEVS),          &
     &                        gr(NGPTC,LEVR), gr1(NGPTC,LEVR,NTRAC-1),  &
     &                        gphi(NGPTC), glam(NGPTC), gq(NGPTC),      &
     &                        sumq(NGPTC,LEVR), xcp(NGPTC,LEVR),        &! hmhj	
     &                        gtv(NGPTC,LEVR), gtvx(NGPTC,LEVR),        &! hmhj	
     &                        gtvy(NGPTC,LEVR)				 ! hmhj

      real (kind=kind_phys) :: f_ice(NGPTC,LEVS), f_rain(NGPTC,LEVS),   &
     &                        r_rime(NGPTC,LEVS)

      real (kind=kind_phys) :: cldcov_v(NGPTC,LEVS), hprime_v(NGPTC),   &
     &                        fluxr_v(NGPTC,NFXR), vvel(NGPTC,LEVS)
      real (kind=kind_phys) :: flgmin_l(ngptc), work1, work2

      real (kind=kind_phys) :: rinc(5), dtsw, dtlw, solcon

      real (kind=kind_phys), save :: facoz

      integer :: njeff, lon, lan, lat, iblk, lon_dim, lons_lat, istrt
      integer :: idat(8), jdat(8), DAYS(13), iday, imon, midmon, id
      integer :: lmax

      integer, save :: icwp, k1oz, k2oz, midm, midp

!  ---  number of days in a month
      data DAYS / 31,28,31,30,31,30,31,31,30,31,30,31,30 /

!  --- ...  control parameters: 
!           (some of the them may be moved into model namelist)

!  ---  ICTM=yyyy#, controls time sensitive external data (e.g. CO2, solcon, aerosols, etc)
!     integer, parameter :: ICTM =    0 ! use data at initial cond time, if not
!                                       ! available, use latest, no extrapolation.
!!    integer, parameter :: ICTM =    1 ! use data at the forecast time, if not
!                                       ! available, use latest and extrapolation.
!     integer, parameter :: ICTM =yyyy0 ! use yyyy data for the forecast time,
!                                       ! no further data extrapolation.
!     integer, parameter :: ICTM =yyyy1 ! use yyyy data for the fcst. if needed, do
!                                       ! extrapolation to match the fcst time.

!  ---  ISOL controls solar constant data source
!!    integer, parameter :: ISOL = 0   ! use prescribed solar constant
!     integer, parameter :: ISOL = 1   ! use varying solar const with 11-yr cycle

!  ---  ICO2 controls co2 data source for radiation
!     integer, parameter :: ICO2 = 0   ! prescribed global mean value (old opernl)
!!    integer, parameter :: ICO2 = 1   ! use obs co2 annual mean value only
!     integer, parameter :: ICO2 = 2   ! use obs co2 monthly data with 2-d variation

!  ---  IALB controls surface albedo for sw radiation
!!    integer, parameter :: IALB = 0   ! use climatology alb, based on sfc type
!     integer, parameter :: IALB = 1   ! use modis derived alb (to be developed)

!  ---  IEMS controls surface emissivity for lw radiation
!!    integer, parameter :: IEMS = 0   ! use fixed value of 1.0
!     integer, parameter :: IEMS = 1   ! use varying sfc emiss, based on sfc type
!  ---  IAER  controls aerosols scheme selections
!     Old definition
!     integer, parameter :: IAER  = 1  ! opac climatology, without volc forcing
!     integer, parameter :: IAER  =11  ! opac climatology, with volcanic forcing
!     integer, parameter :: IAER  = 2  ! gocart prognostic, without volc forcing
!     integer, parameter :: IAER  =12  ! gocart prognostic, with volcanic forcing
!     New definition in this code
!  IAER =   0 --> no aerosol effect at all (volc, sw, lw)
!       =   1 --> only tropospheric sw aerosols, no trop-lw and volc
!       =  10 --> only tropospheric lw aerosols, no trop-sw and volc
!       =  11 --> both trop-sw and trop-lw aerosols, no volc
!       = 100 --> only strato-volc aeros, no trop-sw and trop-lw
!       = 101 --> only sw aeros (trop + volc), no lw aeros
!       = 110 --> only lw aeros (trop + volc), no sw aeros
!       = 111 --> both sw and lw aeros (trop + volc) 
!

!  ---  IOVR controls cloud overlapping method in radiation:
!     integer, parameter :: IOVR_SW = 0  ! sw: random overlap clouds
!!    integer, parameter :: IOVR_SW = 1  ! sw: max-random overlap clouds

!     integer, parameter :: IOVR_LW = 0  ! lw: random overlap clouds
!!    integer, parameter :: IOVR_LW = 1  ! lw: max-random overlap clouds

!  ---  iflip indicates model vertical index direction:
!     integer, parameter :: IFLIP = 0    ! virtical profile index from top to bottom
      integer, parameter :: IFLIP = 1    ! virtical profile index from bottom to top
!
!    The following parameters are from gbphys
!
      real (kind=kind_phys), parameter :: dxmax=-16.118095651,          &
     &                dxmin=-9.800790154, dxinv=1.0/(dxmax-dxmin)

      integer :: kr, kt, kd, kq, ku, kv, ierr, dimg, kx, ky
      integer :: i, j, k, n
      integer :: kdtphi,kdtlam,ks                                ! hmhj

      logical :: lslag, change, lprnt
      data  lslag / .false. /,    lprnt / .false. /
      logical, save :: first, sas_shal
      data  first / .true. /

!  ---  timers:
      real*8 :: rtc, timer1, timer2
!
!===> *** ...  begin here
!
!!
cc
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
cc
cc
c$$$      integer                lots,lotd,lota
c$$$cc
c$$$      parameter            ( lots = 5*levs+1*levh+3 )
c$$$      parameter            ( lotd = 6*levs+2*levh+0 )
c$$$      parameter            ( lota = 3*levs+1*levh+1 )
cc
cc
      integer              kap,kar,kat,kau,kav,kdrlam
      integer              ksd,ksplam,kspphi,ksq,ksr,kst
      integer              ksu,ksv,ksz,node
cc
!     real(kind=kind_evod) spdlat(levs,lats_dim_r)
!Moor real(kind=kind_phys) slk(levs)
!     real(kind=kind_evod) spdmax_node (levs)
!     real(kind=kind_evod) spdmax_nodes(levs,nodes)
cc
cc
ccxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
cc
cc
cc................................................................
cc  syn(1, 0*levs+0*levh+1, lan)  ze
cc  syn(1, 1*levs+0*levh+1, lan)  di
cc  syn(1, 2*levs+0*levh+1, lan)  te
cc  syn(1, 3*levs+0*levh+1, lan)  rq
cc  syn(1, 3*levs+1*levh+1, lan)  q
cc  syn(1, 3*levs+1*levh+2, lan)  dpdlam
cc  syn(1, 3*levs+1*levh+3, lan)  dpdphi
cc  syn(1, 3*levs+1*levh+4, lan)  uln
cc  syn(1, 4*levs+1*levh+4, lan)  vln
cc................................................................
cc  dyn(1, 0*levs+0*levh+1, lan)  d(t)/d(phi)
cc  dyn(1, 1*levs+0*levh+1, lan)  d(rq)/d(phi)
cc  dyn(1, 1*levs+1*levh+1, lan)  d(t)/d(lam)
cc  dyn(1, 2*levs+1*levh+1, lan)  d(rq)/d(lam)
cc  dyn(1, 2*levs+2*levh+1, lan)  d(u)/d(lam)
cc  dyn(1, 3*levs+2*levh+1, lan)  d(v)/d(lam)
cc  dyn(1, 4*levs+2*levh+1, lan)  d(u)/d(phi)
cc  dyn(1, 5*levs+2*levh+1, lan)  d(v)/d(phi)
cc................................................................
cc  anl(1, 0*levs+0*levh+1, lan)  w     dudt
cc  anl(1, 1*levs+0*levh+1, lan)  x     dvdt
cc  anl(1, 2*levs+0*levh+1, lan)  y     dtdt
cc  anl(1, 3*levs+0*levh+1, lan)  rt    drdt
cc  anl(1, 3*levs+1*levh+1, lan)  z     dqdt
cc................................................................
cc
cc
c$$$      parameter(ksz     =0*levs+0*levh+1,
c$$$     x          ksd     =1*levs+0*levh+1,
c$$$     x          kst     =2*levs+0*levh+1,
c$$$     x          ksr     =3*levs+0*levh+1,
c$$$     x          ksq     =3*levs+1*levh+1,
c$$$     x          ksplam  =3*levs+1*levh+2,
c$$$     x          kspphi  =3*levs+1*levh+3,
c$$$     x          ksu     =3*levs+1*levh+4,
c$$$     x          ksv     =4*levs+1*levh+4)
cc
c$$$      parameter(kdtphi  =0*levs+0*levh+1,
c$$$     x          kdrphi  =1*levs+0*levh+1,
c$$$     x          kdtlam  =1*levs+1*levh+1,
c$$$     x          kdrlam  =2*levs+1*levh+1,
c$$$     x          kdulam  =2*levs+2*levh+1,
c$$$     x          kdvlam  =3*levs+2*levh+1,
c$$$     x          kduphi  =4*levs+2*levh+1,
c$$$     x          kdvphi  =5*levs+2*levh+1)
cc
c$$$      parameter(kau     =0*levs+0*levh+1,
c$$$     x          kav     =1*levs+0*levh+1,
c$$$     x          kat     =2*levs+0*levh+1,
c$$$     x          kar     =3*levs+0*levh+1,
c$$$     x          kap     =3*levs+1*levh+1)
cc
cc
c$$$      integer   P_gz,P_zem,P_dim,P_tem,P_rm,P_qm
c$$$      integer   P_ze,P_di,P_te,P_rq,P_q,P_dlam,P_dphi,P_uln,P_vln
c$$$      integer   P_w,P_x,P_y,P_rt,P_zq
c$$$cc
c$$$cc                                               old common /comfspec/
c$$$      parameter(P_gz  = 0*levs+0*levh+1,  !      gze/o(lnte/od,2),
c$$$     x          P_zem = 0*levs+0*levh+2,  !     zeme/o(lnte/od,2,levs),
c$$$     x          P_dim = 1*levs+0*levh+2,  !     dime/o(lnte/od,2,levs),
c$$$     x          P_tem = 2*levs+0*levh+2,  !     teme/o(lnte/od,2,levs),
c$$$     x          P_rm  = 3*levs+0*levh+2,  !      rme/o(lnte/od,2,levh),
c$$$     x          P_qm  = 3*levs+1*levh+2,  !      qme/o(lnte/od,2),
c$$$     x          P_ze  = 3*levs+1*levh+3,  !      zee/o(lnte/od,2,levs),
c$$$     x          P_di  = 4*levs+1*levh+3,  !      die/o(lnte/od,2,levs),
c$$$     x          P_te  = 5*levs+1*levh+3,  !      tee/o(lnte/od,2,levs),
c$$$     x          P_rq  = 6*levs+1*levh+3,  !      rqe/o(lnte/od,2,levh),
c$$$     x          P_q   = 6*levs+2*levh+3,  !       qe/o(lnte/od,2),
c$$$     x          P_dlam= 6*levs+2*levh+4,  !  dpdlame/o(lnte/od,2),
c$$$     x          P_dphi= 6*levs+2*levh+5,  !  dpdphie/o(lnte/od,2),
c$$$     x          P_uln = 6*levs+2*levh+6,  !     ulne/o(lnte/od,2,levs),
c$$$     x          P_vln = 7*levs+2*levh+6,  !     vlne/o(lnte/od,2,levs),
c$$$     x          P_w   = 8*levs+2*levh+6,  !       we/o(lnte/od,2,levs),
c$$$     x          P_x   = 9*levs+2*levh+6,  !       xe/o(lnte/od,2,levs),
c$$$     x          P_y   =10*levs+2*levh+6,  !       ye/o(lnte/od,2,levs),
c$$$     x          P_rt  =11*levs+2*levh+6,  !      rte/o(lnte/od,2,levh),
c$$$     x          P_zq  =11*levs+3*levh+6)  !      zqe/o(lnte/od,2)
cc
cc
!     print *,' in gloopr vertcoord_id =',vertcoord_id

!
      ksz     =0*levs+0*levh+1
      ksd     =1*levs+0*levh+1
      kst     =2*levs+0*levh+1
      ksr     =3*levs+0*levh+1
      ksq     =3*levs+1*levh+1
      ksplam  =3*levs+1*levh+2
      kspphi  =3*levs+1*levh+3
      ksu     =3*levs+1*levh+4
      ksv     =4*levs+1*levh+4

      kdtphi  =0*levs+0*levh+1                          ! hmhj
      kdtlam  =1*levs+1*levh+1                          ! hmhj
cc
      lslag=.false.
cc
      idat = 0
      idat(1) = idate(4)
      idat(2) = idate(2)
      idat(3) = idate(3)
      idat(5) = idate(1)
      rinc = 0.
      rinc(2) = fhour
      call w3movdat(rinc, idat, jdat)
!
      if (ntoz .le. 0) then                ! Climatological Ozone!
!
!     if(me .eq. 0) WRITE (6,989) jdat(1),jdat(2),jdat(3),jdat(5)
! 989 FORMAT(' UPDATING OZONE FOR ', I4,I3,I3,I3)
!
        IDAY   = jdat(3)
        IMON   = jdat(2)
        MIDMON = DAYS(IMON)/2 + 1
        CHANGE = FIRST .OR.
     &          ( (IDAY .EQ. MIDMON) .AND. (jdat(5).EQ.0) )
!
        IF (CHANGE) THEN
            IF (IDAY .LT. MIDMON) THEN
               K1OZ = MOD(IMON+10,12) + 1
               MIDM = DAYS(K1OZ)/2 + 1
               K2OZ = IMON
               MIDP = DAYS(K1OZ) + MIDMON
            ELSE
               K1OZ = IMON
               MIDM = MIDMON
               K2OZ = MOD(IMON,12) + 1
               MIDP = DAYS(K2OZ)/2 + 1 + DAYS(K1OZ)
            ENDIF
        ENDIF
!
        IF (IDAY .LT. MIDMON) THEN
           ID = IDAY + DAYS(K1OZ)
        ELSE
           ID = IDAY
        ENDIF
        FACOZ = real (ID-MIDM) / real (MIDP-MIDM)
      endif
!
      if (first) then
        sas_shal = sashal .and. (.not. ras)
!
        if( hybrid.or.gen_coord_hybrid ) then                             ! hmhj

          if( gen_coord_hybrid ) then                                     ! hmhj
            si_loc(levr+1) = si(levp1)                                    ! hmhj
            do k=1,levr                                                   ! hmhj
              si_loc(k) = si(k)                                           ! hmhj
            enddo                                                         ! hmhj
          else                                                            ! hmhj
!  ---  get some sigma distribution for radiation-cloud initialization
!sela   si(k)=(ak5(k)+bk5(k)*Typical_pgr)/Typical_pgr   !ak(k) bk(k) go top to botto
            si_loc(levr+1)= ak5(1)/typical_pgr+bk5(1)
            do k=1,levr
              si_loc(levr+1-k)= ak5(levp1-levr+k)/typical_pgr
     &                        + bk5(levp1-levr+k)
            enddo
          endif
        else
          do k = 1, levr
            si_loc(k) = si(k)
          enddo
          si_loc(levr+1) = si(levp1)
        endif       ! end_if_hybrid

!  --- determin prognostic/diagnostic cloud scheme

        icwp   = 0
        if (NTCW > 0) icwp = 1
           
        first = .false.
           
      endif         ! end_if_first
!
!===> *** ...  radiation initialization
!
      dtsw  = 3600.0 * fhswr
      dtlw  = 3600.0 * fhlwr
                                                                                                            
      call radinit                                                      &
!  ---  input:
     &     ( si_loc, LEVR, IFLIP, NUM_P3D, ICTM,                        &
     &       ISOL, ICO2, ICWP, IALB, IEMS, IAER, idat, jdat, me )
!  ---  output: ( none )
                                                                                                            
!
!===> *** ...  astronomy for sw radiation calculation.
!
      call astronomy                                                    &
!  ---  inputs:
     &     ( lonsperlar, global_lats_r, sinlat_r, coslat_r, xlon,       &
!    &       fhswr, jdat, deltim,                                       &
     &       fhswr, jdat,                                               &
     &       LONR, LATS_NODE_R, LATR, IPT_LATS_NODE_R, lsswr, me,       &
!  ---  outputs:
     &       solcon, slag, sdec, cdec, coszen, coszdg                   &
     &      )
                                                                                                            
!     print *,' returned from astro'
!
!===> *** ...  spectrum to grid transformation for radiation calculation.
!     -----------------------------------
cc
      call f_hpmstart(61,"gr delnpe")
      call delnpe(trie_ls(1,1,P_q   ),
     x            trio_ls(1,1,P_dphi),
     x            trie_ls(1,1,P_dlam),
     x            epse,epso,ls_node)
      call f_hpmstop(61)
cc
      call f_hpmstart(62,"gr delnpo")
      call delnpo(trio_ls(1,1,P_q   ),
     x            trie_ls(1,1,P_dphi),
     x            trio_ls(1,1,P_dlam),
     x            epse,epso,ls_node)
      call f_hpmstop(62)
cc
cc
      call f_hpmstart(63,"gr dezouv dozeuv")
!
!$omp parallel do shared(trie_ls,trio_ls)
!$omp+shared(epsedn,epsodn,snnp1ev,snnp1od,ls_node)
!$omp+private(k)
      do k=1,levs
         call dezouv(trie_ls(1,1,P_di +k-1), trio_ls(1,1,P_ze +k-1),
     x               trie_ls(1,1,P_uln+k-1), trio_ls(1,1,P_vln+k-1),
     x               epsedn,epsodn,snnp1ev,snnp1od,ls_node)
cc
         call dozeuv(trio_ls(1,1,P_di +k-1), trie_ls(1,1,P_ze +k-1),
     x               trio_ls(1,1,P_uln+k-1), trie_ls(1,1,P_vln+k-1),
     x               epsedn,epsodn,snnp1ev,snnp1od,ls_node)
      enddo
      call f_hpmstop(63)
cc
!sela print*,'completed call to dztouv'
cc
!cmr  call mpi_barrier (mpi_comm_world,ierr)
cc
      CALL countperf(0,5,0.)
      CALL synctime()
      CALL countperf(1,5,0.)
!!
      dimg=0
      CALL countperf(0,1,0.)
cc
      call f_hpmstart(67,"gr sumfln")
cc
!sela print*,'begining  call to sumfln'
      call sumflna_r(trie_ls(1,1,P_ze),
     x            trio_ls(1,1,P_ze),
     x            lat1s_r,
     x            plnev_r,plnod_r,
     x            lots,ls_node,latr2,
     x            lslag,lats_dim_a,lots,for_gr_r_1,
     x            ls_nodes,max_ls_nodes,
     x            lats_nodes_r,global_lats_r,
     x            lats_node_r,ipt_lats_node_r,lon_dims_r,dimg,
     x            lonsperlar,lonrx,latr)
cc
!sela print*,'completed call to sumfln'
      call f_hpmstop(67)
cc
      CALL countperf(1,1,0.)
cc
! -----------------------------------
      if( vertcoord_id.eq.3. ) then
! -----------------------------------
      CALL countperf(0,1,0.)                                            ! hmhj
!
      call f_hpmstart(68,"gr sumder2")                                  ! hmhj
!
      call sumdera_r(trie_ls(1,1,P_te),                                 ! hmhj
     x             trio_ls(1,1,P_te),                                   ! hmhj
     x             lat1s_r,                                             ! hmhj
     x             pddev_r,pddod_r,                                     ! hmhj
     x             levs,ls_node,latr2,                                  ! hmhj
     x             lslag,lats_dim_r,lotd,                               ! hmhj
     x             dyn_gr_r_1,                                          ! hmhj
     x             ls_nodes,max_ls_nodes,                               ! hmhj
     x             lats_nodes_r,global_lats_r,                          ! hmhj
     x             lats_node_r,ipt_lats_node_r,lon_dims_r,dimg,         ! hmhj
     x             lonsperlar,lonrx,latr)                               ! hmhj
!
      call f_hpmstop(68)                                                ! hmhj
!
      CALL countperf(1,1,0.)                                            ! hmhj
! --------------------------------
      endif     ! vertcoord_id=3
! --------------------------------
!
 
!cmr  call mpi_barrier (mpi_comm_world,ierr)
 
      do lan=1,lats_node_r
       timer1=rtc()
cc
         lat = global_lats_r(ipt_lats_node_r-1+lan)
cc
         lon_dim = lon_dims_r(lan)
cc
         lons_lat = lonsperlar(lat)

! -------------------------------------------------------
         if( gen_coord_hybrid .and. vertcoord_id.eq.3. ) then
! -------------------------------------------------------
!
           lmax = min(jcap,lons_lat/2)                                  ! hmhj
!
          ipt_ls=min(lat,latr-lat+1)                                    ! hmhj

          do i=1,lmax+1                                                 ! hmhj
            if ( ipt_ls .ge. lat1s_r(i-1) ) then                        ! hmhj
               reall=i-1                                                ! hmhj
               rlcs2(i)=reall*rcs2_r(ipt_ls)/rerth                      ! hmhj
            else                                                        ! hmhj
               rlcs2(i)=cons0     !constant                             ! hmhj
            endif                                                       ! hmhj
          enddo                                                         ! hmhj
!
!$omp parallel do private(k,i)
          do k=1,levs                                                   ! hmhj
            do i=1,lmax+1                                               ! hmhj
!
!           d(t)/d(lam)                                                 ! hmhj
               dyn_gr_r_1(2*i-1+(kdtlam-2+k)*lon_dim,lan)=              ! hmhj
     x        -for_gr_r_1(2*i  +(kst   -2+k)*lon_dim,lan)*rlcs2(i)      ! hmhj
               dyn_gr_r_1(2*i  +(kdtlam-2+k)*lon_dim,lan)=              ! hmhj
     x         for_gr_r_1(2*i-1+(kst   -2+k)*lon_dim,lan)*rlcs2(i)      ! hmhj
            enddo                                                       ! hmhj
          enddo                                                         ! hmhj
! --------------------
        endif       ! gc and vertcoord_id=3
! ---------------------
!
cc
         CALL countperf(0,6,0.)
!sela    print*,' beginning call four2grid',lan
         CALL FOUR2GRID_thread(for_gr_r_1(1,lan),for_gr_r_2(1,lan),     &
     &                  lon_dim,lons_lat,lonrx,5*levs+levh+3,lan,me)

! -------------------------------------------------------
        if( gen_coord_hybrid.and.vertcoord_id.eq.3. ) then              ! hmhj
! -------------------------------------------------------
           CALL FOUR2GRID_thread(dyn_gr_r_1(1,lan),dyn_gr_r_2(1,lan),   ! hmhj
     &                    lon_dim,lons_lat,lonrx,levs,lan,me)           ! hmhj
           CALL FOUR2GRID_thread(dyn_gr_r_1((kdtlam-1)*lon_dim+1,lan),  ! hmhj
     &                           dyn_gr_r_2((kdtlam-1)*lon_dim+1,lan),  ! hmhj
     &                    lon_dim,lons_lat,lonrx,levs,lan,me)           ! hmhj
! -------------------------
        endif       ! gc and vertcoord_id=3
! -------------------------

!sela    print*,' completed call four2grid lan=',lan
         CALL countperf(1,6,0.)
!!
        if( .not. gen_coord_hybrid ) then                               ! hmhj

          do k = 1, LEVS
            kr = (KSR + k - 2) * lon_dim
            kt = (KST + k - 2) * lon_dim
            do j = 1, lons_lat
              if (for_gr_r_2(j+kr,lan) <= 0.0) then
                for_gr_r_2(j+kr,lan) = QMIN
              endif
              for_gr_r_2(j+kt,lan) = for_gr_r_2(j+kt,lan)               &
     &                             / (1.0 + FV*for_gr_r_2(j+kr,lan))
            enddo
          enddo
          kq = (KSQ - 1)*lon_dim
          do j = 1, lons_lat
            for_gr_r_2(j+kq,lan) = exp( for_gr_r_2(j+kq,lan) )
          enddo

        endif                                                           ! hmhj
c
        timer2=rtc()
        global_times_r(lat,me+1)=timer2-timer1
c$$$    print*,'timeloopr',me,timer1,timer2,global_times_r(lat,me+1)
 
!!
      enddo   !lan
!
      call f_hpmstart(69,"gr lat_loop2")
!
!===> *** ...  starting latitude loop
!
      do lan=1,lats_node_r
cc
         lat = global_lats_r(ipt_lats_node_r-1+lan)
cc
         lons_lat = lonsperlar(lat)

!!
!$omp parallel do schedule(dynamic,1) private(lon,j,k,lon_dim)
!$omp+private(istrt,njeff,iblk,ku,kv,kd,kq,kt,kr,kx,ky,ks,n)
!$omp+private(vvel,gu,gv1,gd,gt,gr,gr1,gq,gphi,glam)
!$omp+private(gtv,gtvx,gtvy,sumq,xcp)
!$omp+private(cldcov_v,hprime_v,fluxr_v,f_ice,f_rain,r_rime)
!$omp+private(prslk,prsl,prsik,prsi)

        DO lon=1,lons_lat,NGPTC
!!
          lon_dim = lon_dims_r(lan)
          NJEFF   = MIN(NGPTC,lons_lat-lon+1)
          ISTRT   = lon
          if (NGPTC.ne.1) then
            IBLK  = lon/NGPTC+1
          else
            IBLK  = lon
          endif
          do k = 1, LEVS
            ku = lon - 1 + (KSU + k - 2)*lon_dim
            kv = lon - 1 + (KSV + k - 2)*lon_dim
            kd = lon - 1 + (KSD + k - 2)*lon_dim
            do j = 1, njeff
              gu(j,k)  = for_gr_r_2(j+ku,lan)
              gv1(j,k) = for_gr_r_2(j+kv,lan)
              gd(j,k)  = for_gr_r_2(j+kd,lan)
            enddo
          enddo

          if( gen_coord_hybrid ) then                                    ! hmhj

            do k=1,levr                                                  ! hmhj
              kt = lon - 1 + (KST + k - 2)*lon_dim
              kr = lon - 1 + (KSR + k - 2)*lon_dim
              do j=1,njeff                                               ! hmhj
                gtv(j,k)   = for_gr_r_2(j+kt,lan)
                gr(j,k)    = max(qmin, for_gr_r_2(j+kr,lan))
              enddo                                                      ! hmhj
            enddo                                                        ! hmhj
! --------------------------------------
            if( vertcoord_id.eq.3. ) then
! --------------------------------------
              do k=1,levr                                                ! hmhj
                kx = lon - 1 + (kdtlam + k - 2)*lon_dim
                ky = lon - 1 + (kdtphi + k - 2)*lon_dim
                do j=1,njeff                                             ! hmhj
                  gtvx(j,k)  = dyn_gr_r_2(j+kx,lan)
                  gtvy(j,k)  = dyn_gr_r_2(j+ky,lan)
                enddo                                                    ! hmhj
              enddo							 ! hmhj
! -----------------------------
            endif
! -----------------------------
            if( thermodyn_id.eq.3 ) then
! get dry temperature from enthalpy					! hmhj
              sumq=0.0							! hmhj
              xcp=0.0							! hmhj
              do i=1,ntrac						! hmhj
                if( cpi(i).ne.0.0 ) then				! hmhj
                ks=ksr+(i-1)*levs					! hmhj
                do k=1,levr						! hmhj
                  kr = lon - 1 + (ks + k - 2)*lon_dim			! hmhj
                  do j=1,njeff						! hmhj
                    sumq(j,k)=sumq(j,k)+for_gr_r_2(j+kr,lan)		! hmhj
                    xcp(j,k)=xcp(j,k)+cpi(i)*for_gr_r_2(j+kr,lan)	! hmhj
                  enddo							! hmhj
                enddo							! hmhj
                endif							! hmhj
              enddo							! hmhj
              do k=1,levr						! hmhj
                do j=1,njeff						! hmhj
                  xcp(j,k)=(1.-sumq(j,k))*cpi(0)+xcp(j,k)		! hmhj
                  gt(j,k)=gtv(j,k)/xcp(j,k)				! hmhj
                enddo							! hmhj
              enddo							! hmhj
            else if( thermodyn_id.le.1 ) then				! hmhj
! get dry temperature from virtual temperature				! hmhj
             do k=1,levr                                                ! hmhj
              do j=1,njeff                                              ! hmhj
                gt(j,k)    = gtv(j,k) / (1.+fv*gr(j,k))                 ! hmhj
              enddo                                                     ! hmhj
             enddo							! hmhj
            else
! get dry temperature from dry temperature             			! hmhj
             do k=1,levr                                                ! hmhj
              do j=1,njeff                                              ! hmhj
                gt(j,k)    = gtv(j,k)                                   ! hmhj
              enddo                                                     ! hmhj
             enddo 
            endif

          else                                                          ! hmhj
!
            do k = 1, LEVR
              kt = lon - 1 + (KST + k - 2)*lon_dim
              kr = lon - 1 + (KSR + k - 2)*lon_dim
              do j = 1, njeff
                gt(j,k)  = for_gr_r_2(j+kt,lan)
                gr(j,k)  = for_gr_r_2(j+kr,lan)
              enddo
            enddo

          endif
!
!       Remaining tracers
!
          do n = 1, NTRAC-1
            do k = 1, LEVR
              kr = lon - 1 + (KSR + n*LEVS + k - 2)*lon_dim
              do j = 1, njeff
                gr1(j,k,n) = for_gr_r_2(j+kr,lan)
              enddo
            enddo
          enddo
          kq = lon - 1 + (KSQ - 1)*lon_dim
          kt = lon - 1 + (KSPPHI - 1)*lon_dim
          kr = lon - 1 + (KSPLAM - 1)*lon_dim
          do j = 1, njeff
            gq  (j) = for_gr_r_2(j+kq,lan)
            gphi(j) = for_gr_r_2(j+kt,lan)
            glam(j) = for_gr_r_2(j+kr,lan)
          enddo
!!
!  ---  vertical structure variables:   del,si,sl,prslk,prdel
!
          if( gen_coord_hybrid ) then                                    ! hmhj
!Moor       call  hyb2press_gc(njeff,ngptc,gq,gtv,prsi,prsl,prdel)       ! hmhj
            call  hyb2press_gc(njeff,ngptc,gq,gtv,prsi,prsl,prsik,prslk) ! hmhj
            call omegtes_gc(njeff,ngptc,rcs2_r(min(lat,latr-lat+1)),     ! hmhj
     &                     gq,gphi,glam,gtv,gtvx,gtvy,gd,gu,gv1,vvel)    ! hmhj
          elseif (hybrid) then
 !Moor      call  hyb2press(njeff,ngptc,gq, prsi, prsl,prdel)
            call  hyb2press(njeff,ngptc,gq, prsi, prsl,prsik, prslk)
            call omegtes(njeff,ngptc,rcs2_r(min(lat,latr-lat+1)),
     &                   gq,gphi,glam,gd,gu,gv1,vvel)
          else
 !Moor      call  sig2press(njeff,ngptc,gq,sl,del,si, prsi, prsl,prdel)
            call  sig2press(njeff,ngptc,gq,sl,si,slk,sik,
     &                                        prsi,prsl,prsik,prslk)
            CALL countperf(0,12,0.)
            call omegast3(njeff,ngptc,levs,
     &                    gphi,glam,gu,gv1,gd,del,
     &                    rcs2_r(min(lat,latr-lat+1)),vvel,gq,sl)
          endif
!.....
          if (levr .lt. levs) then
            do j=1,njeff
              prsi(j,levr+1) = prsi(j,levp1)
              prsl(j,levr)   = (prsi(j,levp1)+prsi(j,levr)) * 0.5
              prsik(j,levr+1) = prslk(j,levp1)
              prslk(j,levr)   = fpkap(prsl(j,levr)*1000.0)
            enddo
          endif
          if (ldiag3d .or. lggfs3d) then
            do k=1,levr
              do j=1,njeff
!Moor           prslk(j,k)    = fpkap(prsl(j,k)*1000.0)
                cldcov_v(j,k) = cldcov(k,istrt+j-1,lan)
              enddo
            enddo
          endif
!
          do j=1,njeff
            hprime_v(j) = hprime(1,istrt+j-1,lan)
          enddo
!
          do k=1,nfxr
            do j=1,njeff
              fluxr_v(j,k) = fluxr(k,istrt+j-1,lan)
            enddo
          enddo
          if (NUM_P3D == 3) then
            do k = 1, LEVR
              do j = 1, njeff
                f_ice (j,k) = phy_f3d(j,k,iblk,lan,1)
                f_rain(j,k) = phy_f3d(j,k,iblk,lan,2)
                r_rime(j,k) = phy_f3d(j,k,iblk,lan,3)
              enddo
            enddo
          endif
          work1 = (log(coslat_r(lat) / (lons_lat*latg)) - dxmin) * dxinv
          work1 = max(0.0, min(1.0,work1))
          work2 = flgmin(1)*work1 + flgmin(2)*(1.0-work1)
          do j=1,njeff
            flgmin_l(j) = work2
          enddo
 
!  *** ...  calling radiation driver
 
!
!     lprnt = me .eq. 0 .and. kdt .ge. 120
!     if (lprnt) then
!     if (kdt .gt. 85) then
!     print *,' calling grrad for me=',me,' lan=',lan,' lat=',lat
!    &,' num_p3d=',num_p3d,' snoalb=',snoalb(lon,lan),' lon=',lon
!    &,' tsea=',tsea(lon,lan),' sncovr=',sncovr(lon,lan),
!    &' sheleg=',sheleg(lon,lan)
!
          call grrad                                                    &
!  ---  inputs:
     &     ( prsi,prsl,prslk,gt,gr,gr1,vvel,slmsk(lon,lan),             &
     &       xlon(lon,lan),xlat(lon,lan),tsea(lon,lan),                 &
     &       sheleg(lon,lan),sncovr(lon,lan),snoalb(lon,lan),           &
     &       zorl(lon,lan),hprime_v,                                    &
     &       alvsf(lon,lan),alnsf(lon,lan),alvwf(lon,lan),              &
     &       alnwf(lon,lan),facsf(lon,lan),facwf(lon,lan),              &
                                          ! fice FOR SEA-ICE XW Nov04
     &       fice(lon,lan),tisfc(lon,lan),                              &
     &       solcon,coszen(lon,lan),coszdg(lon,lan),k1oz,k2oz,facoz,    &
     &       cv(lon,lan),cvt(lon,lan),cvb(lon,lan),                     &
     &       IOVR_SW,IOVR_LW,f_ice,f_rain,r_rime,flgmin_l,              &
     &       NUM_P3D,NTCW-1,NCLD,NTOZ-1,NTRAC-1,NFXR,                   &
     &       dtlw,dtsw, lsswr,lslwr,lssav,ldiag3d,sas_shal,norad_precip,&
     &       crick_proof, ccnorm,lggfs3d,                               &
!    &       dtlw,dtsw, lsswr,lslwr,lssav,ldiag3d,lggfs3d,              &
     &       NGPTC,njeff,LEVR,IFLIP, me, lprnt,                         &
!  ---  outputs:
     &       swh(1,1,iblk,lan),sfcnsw(lon,lan),sfcdsw(lon,lan),         & ! sfcdsw FOR SEA-ICE XW Nov04
     &       sfalb(lon,lan),                                            & ! lu [+1L]: add sfalb
     &       hlw(1,1,iblk,lan),sfcdlw(lon,lan),tsflw(lon,lan),          &
!  ---  input/output:
     &       fluxr_v,cldcov_v                                           &
!! ---  optional outputs:
!!   &,      HTRSWB=htrswb(1,1,1,iblk,lan),                             &
!!   &,      HTRLWB=htrlwb(1,1,1,iblk,lan)                              &
     &     )
!
!     if (lprnt) print *,' returned from grrad for me=',me,' lan=',
!    &lan,' lat=',lat,' kdt=',kdt
!
!
          if (ldiag3d .or. lggfs3d) then
            do k=1,levr
              do j=1,njeff
                cldcov(k,istrt+j-1,lan) = cldcov_v(j,k)
              enddo
            enddo
          endif
          do k=1,nfxr
            do j=1,njeff
              fluxr(k,istrt+j-1,lan) = fluxr_v(j,k)
            enddo
          enddo
          if (levr .lt. levs) then
            do k=levr+1,levs
              do j=1,njeff
                hlw(j,k,iblk,lan) = hlw(j,levr,iblk,lan)
                swh(j,k,iblk,lan) = swh(j,levr,iblk,lan)
              enddo
            enddo
          endif
 
c$$$          write(2900+lat,*) ' ilon = ',istrt
c$$$          write(2900+lat,'("swh",T16,"hlw")')
c$$$      do k=1,levs
c$$$         write(2900+lat,
c$$$     .         '(e10.3,T16,e10.3,T31,e10.3)')
c$$$     . swh(1,k,iblk,lan),hlw(1,k,iblk,lan)
c$$$       enddo
 
!!
          CALL countperf(1,12,0.)
          ENDDO
!
      enddo
cc
      call f_hpmstop(69)
!!
      CALL countperf(0,5,0.)
      CALL synctime()
      CALL countperf(1,5,0.)
!sela print*,'completed gloopr_v kdt=',kdt
!!
      return
      end subroutine gloopr

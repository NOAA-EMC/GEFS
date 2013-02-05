      subroutine gloopb
     x    (trie_ls,trio_ls,
     x     ls_node,ls_nodes,max_ls_nodes,
     x     lats_nodes_r,global_lats_r,
     x     lonsperlar,
     x     epse,epso,epsedn,epsodn,
     x     snnp1ev,snnp1od,ndexev,ndexod,
     x     plnev_r,plnod_r,pddev_r,pddod_r,plnew_r,plnow_r,
     &     tstep,phour,sfc_fld, flx_fld, nsst_fld, SFALB,
     &     xlon,
     &     swh,hlw,hprime,slag,sdec,cdec,
     &     ozplin,jindx1,jindx2,ddy,pdryini,
     &     phy_f3d, phy_f2d,xlat,nblck,kdt,
     &     global_times_b)
!!
#include "f_hpm.h"
!!
      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use coordinate_def                                                ! hmhj
      use bfilt_def
      use module_ras , only : ras_init
!     use physcons, grav => con_g
      use physcons, grav => con_g , rerth => con_rerth                  ! hmhj
      use ozne_def
      use Sfc_Flx_ESMFMod
      use Nsstm_ESMFMod
      use mersenne_twister
      include 'mpif.h'
      implicit none
!
      TYPE(Sfc_Var_Data)        :: sfc_fld
      TYPE(Flx_Var_Data)        :: flx_fld
      TYPE(Nsst_Var_Data)       :: nsst_fld

      integer nlons_v(ngptc)
!$$$      integer n1rac, n2rac,nlons_v(ngptc)
!$$$      parameter (n1rac=ntrac-ntshft-1, n2rac=n1rac+1)
!
      integer id,njeff,istrt,lon,iblk,kdt
!!
      integer nblck
!!
      real(kind=kind_phys)    typdel(levs)
      real(kind=kind_phys)    prsl(ngptc,levs),prdel(ngptc,levs)
      real(kind=kind_phys)   prslk(ngptc,levs),prsshc
      real(kind=kind_phys)    prsi(ngptc,levs+1),phii(ngptc,levs+1)
      real(kind=kind_phys)   prsik(ngptc,levs+1),phil(ngptc,levs)
!!
!     real(kind=kind_evod) sinlat_v(ngptc),coslat_v(ngptc),rcs2_v(ngptc)
      real(kind=kind_evod) trie_ls(len_trie_ls,2,11*levs+3*levh+6)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,11*levs+3*levh+6)
      real(kind=kind_evod) trie_ls_rqt(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_ls_rqt(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_ls_sfc(len_trie_ls,2)                   ! hmhj
      real(kind=kind_evod) trio_ls_sfc(len_trio_ls,2)                   ! hmhj
!     real(kind=kind_evod) smc_v(ngptc,lsoil),stc_v(ngptc,lsoil)
Clu [+1L]: add slc_v
!    +,                    slc_v(ngptc,lsoil)
!     real(kind=kind_evod) hprime_v(ngptc,nmtvr)
 
!!
      real (kind=kind_rad) gu(ngptc,levs),gv1(ngptc,levs)
      real (kind=kind_rad) gphi(ngptc),glam(ngptc)
      real (kind=kind_rad) gq(ngptc),gt(ngptc,levs)
      real (kind=kind_rad) gr(ngptc,levs,ntrac)
      real (kind=kind_rad) gd(ngptc,levs)
      real (kind=kind_rad) adt(ngptc,levs),adr(ngptc,levs,ntrac)
      real (kind=kind_rad) adu(ngptc,levs),adv(ngptc,levs)
!!
      real (kind=kind_evod) slag,sdec,cdec,phour
      real (kind=kind_rad) xlon(lonr,lats_node_r)
      real (kind=kind_rad) xlat(lonr,lats_node_r)
      real (kind=kind_rad) coszdg(lonr,lats_node_r),
     &                     hprime(nmtvr,lonr,lats_node_r),
     &                     fluxr(nfxr,lonr,lats_node_r),
     &                     sfalb(lonr,lats_node_r)
      real (kind=kind_rad)  swh(ngptc,levs,nblck,lats_node_r)
      real (kind=kind_rad)  hlw(ngptc,levs,nblck,lats_node_r)
!!
      real  (kind=kind_phys)
     &     phy_f3d(ngptc,levs,nblck,lats_node_r,num_p3d),
     &     phy_f2d(lonr,lats_node_r,num_p2d)
!
!     real (kind=kind_phys) q2m(lonr,lats_node_r),t2m(lonr,lats_node_r),
!    &     hpbl(lonr,lats_node_r),u10m  (lonr,lats_node_r),
!    &     v10m(lonr,lats_node_r),smc(lsoil,lonr,lats_node_r),
!    &     psmean(lonr,lats_node_r),
!    &     dvgwd(lonr,lats_node_r),bengsh(lonr,lats_node_r),
!    &     psurf(lonr,lats_node_r),dugwd(lonr,lats_node_r),
!    &     cldwrk(lonr,lats_node_r),gflux(lonr,lats_node_r),
!    &     ulwsfc(lonr,lats_node_r),runoff(lonr,lats_node_r),
!    &     dusfc(lonr,lats_node_r),dvsfc(lonr,lats_node_r),
!    &     dtsfc(lonr,lats_node_r),tmpmax(lonr,lats_node_r),
!    &     tmpmin(lonr,lats_node_r),geshem(lonr,lats_node_r),
!    &     dqsfc(lonr,lats_node_r),dlwsfc(lonr,lats_node_r),
!    &     f10m(lonr,lats_node_r),vtype(lonr,lats_node_r),
!    &     canopy(lonr,lats_node_r),tg3(lonr,lats_node_r),
!    &     vfrac(lonr,lats_node_r),ffhh(lonr,lats_node_r),
!    &     ffmm(lonr,lats_node_r),stype(lonr,lats_node_r),
!    &     uustar(lonr,lats_node_r),
!    &     ep(lonr,lats_node_r)
Clu [+3L]: add slc,slope,snoalb,shdmin,shdmax
!    +,    slc(lsoil,lonr,lats_node_r)
!    +,    slope(lonr,lats_node_r),snoalb(lonr,lats_node_r)
!    +,    shdmin(lonr,lats_node_r),shdmax(lonr,lats_node_r)
Cwei added 10/24/2006
!    +,    chh(lonr,lats_node_r),cmm(lonr,lats_node_r),
!    +     epi(lonr,lats_node_r),dlwsfci(lonr,lats_node_r),
!    +     ulwsfci(lonr,lats_node_r),uswsfci(lonr,lats_node_r),
!    +     dswsfci(lonr,lats_node_r),dtsfci(lonr,lats_node_r),
!    +     dqsfci(lonr,lats_node_r),gfluxi(lonr,lats_node_r),
!    +     srunoff(lonr,lats_node_r),
!    +     t1(lonr,lats_node_r),q1(lonr,lats_node_r),
!    +     u1(lonr,lats_node_r),v1(lonr,lats_node_r),
!    +     zlvl(lonr,lats_node_r),evbsa(lonr,lats_node_r),
!    +     evcwa(lonr,lats_node_r),transa(lonr,lats_node_r),
!    +     sbsnoa(lonr,lats_node_r),snowca(lonr,lats_node_r),
!    +     soilm(lonr,lats_node_r),snohfa(lonr,lats_node_r)

      real (kind=kind_rad) vvel(ngptc,levs),rand
!
      real (kind=kind_phys) exp,dtphys,dtp,dtf,sumed(2)
      real (kind=kind_evod) tstep
      real (kind=kind_phys) pdryini,sigshc,rk
!!
      integer              ls_node(ls_dim,3)
cc
!     ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!     ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!     ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
cc
      integer              ls_nodes(ls_dim,nodes)
cc
      integer              max_ls_nodes(nodes)
      integer              lats_nodes_r(nodes)
cc
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
      integer dimg
cc
      real(kind=kind_evod)    epse(len_trie_ls)
      real(kind=kind_evod)    epso(len_trio_ls)
      real(kind=kind_evod)  epsedn(len_trie_ls)
      real(kind=kind_evod)  epsodn(len_trio_ls)
cc
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
cc
      integer               ndexev(len_trie_ls)
      integer               ndexod(len_trio_ls)
cc
      real(kind=kind_evod)   plnev_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnod_r(len_trio_ls,latr2)
      real(kind=kind_evod)   pddev_r(len_trie_ls,latr2)
      real(kind=kind_evod)   pddod_r(len_trio_ls,latr2)
      real(kind=kind_evod)   plnew_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnow_r(len_trio_ls,latr2)
cc
c$$$      integer                lots,lotd,lota
      integer lotn
c$$$cc
c$$$      parameter            ( lots = 5*levs+1*levh+3 )
c$$$      parameter            ( lotd = 6*levs+2*levh+0 )
c$$$      parameter            ( lota = 3*levs+1*levh+1 )
cc
      real(kind=kind_evod) for_gr_r_1(lonrx*lots,lats_dim_r)
      real(kind=kind_evod) dyn_gr_r_1(lonrx*lotd,lats_dim_r)            ! hmhj
      real(kind=kind_evod) bak_gr_r_1(lonrx*lota,lats_dim_r)
cc
      real(kind=kind_evod) for_gr_r_2(lonrx*lots,lats_dim_r)
      real(kind=kind_evod) dyn_gr_r_2(lonrx*lotd,lats_dim_r)            ! hmhj
      real(kind=kind_evod) bak_gr_r_2(lonrx*lota,lats_dim_r)
cc
      integer              i,ierr,iter,j,k,kap,kar,kat,kau,kav,ksq
      integer              kst,kdtphi,kdtlam                            ! hmhj
      integer              l,lan,lan0,lat,lmax,locl
      integer              lon_dim,lons_lat,n,node
      integer nsphys
!
      real(kind=kind_evod) pwatg(latr),pwatj(lats_node_r),
     &                     pwatp,ptotg(latr),sumwa,sumto,
     &                     ptotj(lats_node_r),pcorr,pdryg,
     &                     solhr,clstp
cc
      integer              ipt_ls                                       ! hmhj
      real(kind=kind_evod) reall                                        ! hmhj
      real(kind=kind_evod) rlcs2(jcap1)                                 ! hmhj
 

       real(kind=kind_evod) typical_pgr
c
!timers______________________________________________________---
 
      real*8 rtc ,timer1,timer2
      real(kind=kind_evod) global_times_b(latr,nodes)
 
!timers______________________________________________________---
cc
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
cc
cc
      integer              indlsev,jbasev,n0
      integer              indlsod,jbasod
cc
      include 'function2'
cc
      real(kind=kind_evod) cons0,cons2     !constant
cc
      logical lslag
      logical, parameter :: flipv = .true.
cc
c for nasa ozone production and distruction rates:(input throu fixio)
      real ozplin(latsozp,levozp,pl_coeff,timeoz)
      integer jindx1(lats_node_r),jindx2(lats_node_r)!for ozone interpolaton
      real ddy(lats_node_r)                          !for ozone interpolaton
      real ozplout(levozp,lats_node_r,pl_coeff)
!!
      real(kind=kind_phys), allocatable :: acv(:,:),acvb(:,:),acvt(:,:)
      save acv,acvb,acvt
!!
      real (kind=kind_phys) rannum(lonr*latr*nrcm)
!     real (kind=kind_phys) rannum(lonr,latr,nrcm)
     &,                     xkt2(lonr,lats_node_r,nrcm)
!     integer, allocatable ::  nrnd(:)
!     integer krsize, iseed, nrc
      integer iseed, nrc, seed0
      integer nf0,nf1,ind,nt,indod,indev
      real(kind=kind_evod) fd2
      real(kind=kind_evod) wrk(1)
      logical first,ladj
      parameter (ladj=.true.)
      data first/.true./
!     save    krsize, first, nrnd,seed0
      save    first, seed0
!
      real(kind=kind_evod), parameter :: cons_0=0.0,   cons_24=24.0
     &,                                  cons_99=99.0, cons_1p0d9=1.0E9
!
cc
cc--------------------------------------------------------------------
cc
!     print *,' in gloopb vertcoord_id =',vertcoord_id

      ksq     = 0*levs + 0*levh + 1
      kst     = 4*levs + 0*levh + 4                          ! hmhj
      kdtphi  = 0*levs + 0*levh + 1                          ! hmhj
      kdtlam  = 1*levs+1*levh+1                              ! hmhj
c$$$      kau     =0*levs+0*levh+1
c$$$      kav     =1*levs+0*levh+1
c$$$      kat     =2*levs+0*levh+1
c$$$      kar     =3*levs+0*levh+1
c$$$      kap     =3*levs+1*levh+1
cc
cc--------------------------------------------------------------------
cc
      if (first) then
      allocate (bfilte(lnt2),bfilto(lnt2))
!
!     initializations for the gloopb filter
!     *************************************
        nf0=(jcap+1)*2/3  ! highest wavenumber gloopb filter keeps fully
        nf1=(jcap+1)      ! lowest wavenumber gloopb filter removes fully
        fd2=1./(nf1-nf0)**2
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
!sela    if (l.eq.0) then
!sela       n0=2
!sela    else
!sela       n0=l
!sela    endif
!
!sela     indev=indlsev(n0,l)
          indev=indlsev(l,l)
!sela     do n=n0,jcap1,2
!mjr      do n=l,jcap1,2
          do n=l,jcap,2
            bfilte(indev)=max(1.-fd2*max(n-nf0,0)**2,cons_0)     !constant
             indev=indev+1
          enddo
          if (mod(L,2).eq.mod(jcap+1,2)) bfilte(indev)=1.
      enddo
!!
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasod=ls_node(locl,3)
         indod=indlsod(l+1,l)
!mjr     do n=l+1,jcap1,2
         do n=l+1,jcap,2
           bfilto(indod)=max(1.-fd2*max(n-nf0,0)**2,cons_0)     !constant
            indod=indod+1
         enddo
          if (mod(L,2).ne.mod(jcap+1,2)) bfilto(indod)=1.
      enddo
!!
!       call random_seed(size=krsize)
!       if (me.eq.0) print *,' krsize=',krsize
!       allocate (nrnd(krsize))
        allocate (acv(lonr,lats_node_r))
        allocate (acvb(lonr,lats_node_r))
        allocate (acvt(lonr,lats_node_r))
!
        seed0 = idate(1) + idate(2) + idate(3) + idate(4)

!       nrnd  = idate(1) + idate(3) * 24
!       call random_seed(generator=2)
!       call random_seed(put=nrnd)

        call random_setseed(seed0)
        call random_number(wrk)
        seed0 = seed0 + nint(wrk(1)*1000.0)
!
        if (me .eq. 0) then
          print *,' seed0=',seed0,' idate=',idate,' wrk=',wrk
          if (num_p3d .eq. 3) print *,' USING Ferrier-MICROPHYSICS'
          if (num_p3d .eq. 4) print *,' USING ZHAO-MICROPHYSICS'
        endif
        if (fhour .eq. 0.0) then
          do j=1,lats_node_r
            do i=1,lonr
              phy_f2d(i,j,num_p2d) = 0.0
            enddo
          enddo
        endif
       
        if (ras) call ras_init(levs, me)
       
        first = .false.
      endif
cc
      dtphys=3600.
      nsphys=max(int(2*tstep/dtphys+0.9999),1)
      dtp=2*tstep/nsphys
      dtf=0.5*dtp
      if(lsfwd) dtf=dtp
c
      solhr=mod(phour+idate(1),cons_24)
c...  set switch for saving convective clouds
      if(lscca.and.lsswr) then
        clstp=1100+min(fhswr,fhour,cons_99)  !initialize,accumulate,convert
      elseif(lscca) then
        clstp=0100+min(fhswr,fhour,cons_99)  !accumulate,convert
      elseif(lsswr) then
        clstp=1100                       !initialize,accumulate
      else
        clstp=0100                       !accumulate
      endif
!
!
      iseed = mod(100.0*sqrt(fhour*3600),cons_1p0d9) + 1 + seed0

!     nrnd  = iseed
!     call random_seed(generator=2)
!     call random_seed(put=nrnd)

      call random_setseed(iseed)
      call random_number(rannum)
      do nrc=1,nrcm
        do j=1,lats_node_r
           lat=global_lats_r(ipt_lats_node_r-1+j)
           do i=1,lonr
!             xkt2(i,j,nrc) = rannum(i,lat,nrc)
              xkt2(i,j,nrc) = rannum(i+(lat-1)*lonr+(nrc-1)*latr)
           enddo
        enddo
      enddo
!
      if(.not.random_xkt2)then
        if(kdt.lt.3)print*,'random_xkt2=',random_xkt2
        xkt2 = 0.6
      endif
!     print *,' xkt2=',xkt2(1:5,1:5,1),' kdt=',kdt
!     if (kdt .eq. 1) print *,' xkt2=',xkt2(1:5,1:5,1)
!
! doing ozone i/o and latitudinal interpolation to local gauss lats
c      ifozphys=.true.
 
      if (ntoz .gt. 0) then
       call ozinterpol(me,lats_node_r,lats_node_r,idate,fhour,
     &                 jindx1,jindx2,ozplin,ozplout,ddy)
      endif

!!
c ----------------------------------------------------
c
      lslag=.false.
cc
cc
cc................................................................
cc
cc
      call f_hpmstart(41,"gb delnpe")
      call delnpe(trie_ls(1,1,p_zq   ),
     x            trio_ls(1,1,p_dphi),
     x            trie_ls(1,1,p_dlam),
     x            epse,epso,ls_node)
      call f_hpmstop(41)
cc
      call f_hpmstart(42,"gb delnpo")
      call delnpo(trio_ls(1,1,p_zq   ),
     x            trie_ls(1,1,p_dphi),
     x            trio_ls(1,1,p_dlam),
     x            epse,epso,ls_node)
      call f_hpmstop(42)
cc
cc
      call f_hpmstart(43,"gb dezouv dozeuv")
!$OMP parallel do shared(trie_ls,trio_ls)
!$OMP+shared(epsedn,epsodn,snnp1ev,snnp1od,ls_node)
!$OMP+private(k)
      do k=1,levs
         call dezouv(trie_ls(1,1,p_x  +k-1), trio_ls(1,1,p_w  +k-1),
     x               trie_ls(1,1,p_uln+k-1), trio_ls(1,1,p_vln+k-1),
     x               epsedn,epsodn,snnp1ev,snnp1od,ls_node)
cc
         call dozeuv(trio_ls(1,1,p_x  +k-1), trie_ls(1,1,p_w  +k-1),
     x               trio_ls(1,1,p_uln+k-1), trie_ls(1,1,p_vln+k-1),
     x               epsedn,epsodn,snnp1ev,snnp1od,ls_node)
      enddo
      call f_hpmstop(43)
cc
!     call mpi_barrier (mpi_comm_world,ierr)
cc
      call countperf(0,4,0.)
      call synctime()
      call countperf(1,4,0.)
!!
      dimg=0
      call countperf(0,1,0.)
cc
!     call f_hpmstart(48,"gb syn_ls2lats")
cc
!     call f_hpmstop(48)
cc
      call f_hpmstart(49,"gb sumfln")
cc
      call sumflna_r(trie_ls(1,1,p_q),
     x            trio_ls(1,1,p_q),
     x            lat1s_r,
     x            plnev_r,plnod_r,
     x            lots,ls_node,latr2,
     x            lslag,lats_dim_r,lots,for_gr_r_1,
     x            ls_nodes,max_ls_nodes,
     x            lats_nodes_r,global_lats_r,
     x            lats_node_r,ipt_lats_node_r,lon_dims_r,dimg,
     x            lonsperlar,lonrx,latr)
cc
      call f_hpmstop(49)
cc
      call countperf(1,1,0.)
cc
cc
      pwatg=0.
      ptotg=0.

!--------------------
      if( vertcoord_id.eq.3. ) then
!--------------------
      call countperf(0,11,0.)
!
      CALL countperf(0,1,0.)                                            ! hmhj
!
      call f_hpmstart(50,"gb sumder2")                                  ! hmhj
!
      call sumder2_r(trie_ls(1,1,P_te),                                 ! hmhj
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
      call f_hpmstop(50)                                                ! hmhj
!
      CALL countperf(1,1,0.)                                            ! hmhj
! -----------------
      endif
! -----------------

c
      do lan=1,lats_node_r
       timer1=rtc()
cc
         lat = global_lats_r(ipt_lats_node_r-1+lan)
cc
         lon_dim = lon_dims_r(lan)
cc
         lons_lat = lonsperlar(lat)

!-----------------------------------------
         if( vertcoord_id.eq.3. ) then
!-----------------------------------------
!!       calculate t rq u v zonal derivs. by multiplication with i*l
!!       note rlcs2=rcs2*L/rerth
!
         lmax = min(jcap,lons_lat/2)                                    ! hmhj
!
         ipt_ls=min(lat,latr-lat+1)                                     ! hmhj

         do i=1,lmax+1                                                  ! hmhj
            if ( ipt_ls .ge. lat1s_r(i-1) ) then                        ! hmhj
               reall=i-1                                                ! hmhj
               rlcs2(i)=reall*rcs2_r(ipt_ls)/rerth                      ! hmhj
            else                                                        ! hmhj
               rlcs2(i)=cons0     !constant                             ! hmhj
            endif                                                       ! hmhj
         enddo                                                          ! hmhj
!
!$omp parallel do private(k,i)
         do k=1,levs                                                    ! hmhj
            do i=1,lmax+1                                               ! hmhj
!
!           d(t)/d(lam)                                                 ! hmhj
               dyn_gr_r_1(2*i-1+(kdtlam-2+k)*lon_dim,lan)=              ! hmhj
     x        -for_gr_r_1(2*i  +(kst   -2+k)*lon_dim,lan)*rlcs2(i)      ! hmhj
               dyn_gr_r_1(2*i  +(kdtlam-2+k)*lon_dim,lan)=              ! hmhj
     x         for_gr_r_1(2*i-1+(kst   -2+k)*lon_dim,lan)*rlcs2(i)      ! hmhj
            enddo                                                       ! hmhj
         end do                                                         ! hmhj
! -----------------------
         endif
! -----------------------

cc
         call countperf(0,6,0.)
         call four2grid_thread(for_gr_r_1(1,lan),for_gr_r_2(1,lan),
     &                  lon_dim,lons_lat,lonrx,5*levs+levh+3,lan,me)

! ----------------------------------
         if( vertcoord_id.eq.3. ) then
! ----------------------------------
         CALL FOUR2GRID_thread(dyn_gr_r_1((kdtphi-1)*lon_dim+1,lan),    ! hmhj
     &                         dyn_gr_r_2((kdtphi-1)*lon_dim+1,lan),    ! hmhj
     &                  lon_dim,lons_lat,lonrx,levs,lan,me)             ! hmhj
         CALL FOUR2GRID_thread(dyn_gr_r_1((kdtlam-1)*lon_dim+1,lan),    ! hmhj
     &                         dyn_gr_r_2((kdtlam-1)*lon_dim+1,lan),    ! hmhj
     &                  lon_dim,lons_lat,lonrx,levs,lan,me)             ! hmhj
! ----------------------------
         endif
! ---------------------------

         call countperf(1,6,0.)
!!mjr
          timer2=rtc()
          global_times_b(lat,me+1)=timer2-timer1
c$$$       print*,'timeloopb',me,timer1,timer2,global_times_b(lat,me+1)
!!
      enddo   !lan
cc
      call f_hpmstart(51,"gb lat_loop2")
cjfe
      do lan=1,lats_node_r
cc
         lat = global_lats_r(ipt_lats_node_r-1+lan)
cc
         lon_dim = lon_dims_r(lan)
cc
         lons_lat = lonsperlar(lat)
         pwatp=0.
!$omp parallel do  schedule(dynamic,1) private(lon)
!$omp+private(njeff,istrt,iblk)
         do lon=1,lons_lat,ngptc
!!
          njeff=min(ngptc,lons_lat-lon+1)
          istrt=lon
          iblk  = (lon-1)/ngptc + 1
!         if (ngptc.ne.1) then
!           iblk=lon/ngptc+1
!         else
!           iblk=lon
!         endif
!!
!     print *,' calling gbphys_call for me=',me,' kdt=',kdt
!    &, tsea(1,1),njeff,istrt,iblk,lat,lon_dim,lons_lat,lan
!

          call gbphys_call(solhr,clstp,nsphys,
     &     for_gr_r_2,dyn_gr_r_2,bak_gr_r_2,                            ! hmhj
     &     dtp,dtf,xkt2, sfc_fld, flx_fld, nsst_fld, sfalb,
     &     acv,acvb,acvt,
     &     swh,hlw,hprime,slag,sdec,cdec,ozplout,
     &     phy_f3d, phy_f2d,nblck,kdt,
     &     njeff,istrt,iblk,lat,lon_dim,lons_lat,lan,
     &     xlon, xlat)
 
!sela      do i=1,njeff
!sela        pwatp     = pwatp + pwat(istrt+i-1,lan)*(grav/1.e3)
!sela      enddo
          enddo   !lon
 
!
         ptotj(lan)=0.
         do j=1,lons_lat
            ptotj(lan)=ptotj(lan)+for_gr_r_2(j+(ksq   -1)*lon_dim,lan)
            pwatp     = pwatp + flx_fld%pwat(j,lan)
         enddo
        pwatj(lan)=pwatp*grav/(2.*lonsperlar(lat)*1.e3)
        ptotj(lan)=ptotj(lan)/(2.*lonsperlar(lat))
!!
c$$$        if (kdt.eq.1) then
c$$$        do j=1,lons_lat
c$$$        do i=1,levs
c$$$        write(8700+lat,*)
c$$$     &    bak_gr_r_2(j+(kat-1)*lon_dim+lon_dim*(i-1),lan),i,j
c$$$        write(8800+lat,*)
c$$$     &    bak_gr_r_2(j+(kar-1)*lon_dim+lon_dim*(i-1),lan),i,j
c$$$        write(8900+lat,*)
c$$$     &    bak_gr_r_2(j+(kau-1)*lon_dim+lon_dim*(i-1),lan),i,j
c$$$        write(8100+lat,*)
c$$$     &    bak_gr_r_2(j+(kav-1)*lon_dim+lon_dim*(i-1),lan),i,j
c$$$        write(8200+lat,*)
c$$$     &   bak_gr_r_2(j+(kar+levs-1)*lon_dim+lon_dim*(i-1),lan),i,j
c$$$        write(8300+lat,*)
c$$$     &  bak_gr_r_2(j+(kar+2*levs-1)*lon_dim+lon_dim*(i-1),lan),i,j
c$$$        enddo
c$$$        enddo
c$$$        endif
!!
      enddo   !lan
cc
      call f_hpmstop(51)
cc
      lotn=3*levs+1*levh
c
      do lan=1,lats_node_r
cc
         lat = global_lats_r(ipt_lats_node_r-1+lan)
cc
         lon_dim = lon_dims_r(lan)
cc
         lons_lat = lonsperlar(lat)
         call countperf(0,6,0.)
         call grid2four_thread(bak_gr_r_2(1,lan),bak_gr_r_1(1,lan),
     &                  lon_dim,lons_lat,lonrx,lotn)
         call countperf(1,6,0.)
cc
      enddo
      call countperf(1,11,0.)
!!
      call countperf(0,4,0.)
      call synctime()
      call countperf(1,4,0.)
!!
      call excha(lats_nodes_r,global_lats_r,ptotj,pwatj,ptotg,pwatg)
      sumwa=0.
      sumto=0.
      do lat=1,latr
         sumto=sumto+wgt_r(min(lat,latr-lat+1))*ptotg(lat)
         sumwa=sumwa+wgt_r(min(lat,latr-lat+1))*pwatg(lat)
      enddo
cjfe
cjfe  write(70+me,*) sumto,sumwa,kdt
      pdryg=sumto-sumwa
!!
      if (pdryini .le. 0.) pdryini = pdryg

      if( gen_coord_hybrid ) then                               ! hmhj
        pcorr=(pdryini-pdryg)      *sqrt(2.)                    ! hmhj
      else                                                      ! hmhj
        pcorr=(pdryini-pdryg)/sumto*sqrt(2.)
      endif                                                     ! hmhj
!!
!     call f_hpmstart(53,"gb lats2ls")
cc
      dimg=0
      call countperf(0,1,0.)
cc
!     call f_hpmstop(53)
!!
!     call f_hpmstart(54,"gb fl2eov")
cc
!     call f_hpmstop(54)
cc
      call f_hpmstart(52,"gb four2fln")
cc
      call four2fln(lslag,lats_dim_r,lota,lotn,bak_gr_r_1,
     x              ls_nodes,max_ls_nodes,
     x              lats_nodes_r,global_lats_r,lon_dims_r,
     x              lats_node_r,ipt_lats_node_r,dimg,
     x              lat1s_r,lonrx,latr,latr2,
     x              trie_ls(1,1,p_ze), trio_ls(1,1,p_ze),
     x              plnew_r, plnow_r,
     x              ls_node)
cc
      call f_hpmstop(52)
cc
cc
      call f_hpmstart(55,"gb uveodz uvoedz")
!
!$OMP parallel do shared(trie_ls,trio_ls)
!$OMP+shared(epse,epso,ls_node)
!$OMP+private(k)
      do k=1,levs
         call uveodz(trie_ls(1,1,p_ze +k-1), trio_ls(1,1,p_di +k-1),
     x               trie_ls(1,1,p_uln+k-1), trio_ls(1,1,p_vln+k-1),
     x               epse,epso,ls_node)
cc
         call uvoedz(trio_ls(1,1,p_ze +k-1), trie_ls(1,1,p_di +k-1),
     x               trio_ls(1,1,p_uln+k-1), trie_ls(1,1,p_vln+k-1),
     x               epse,epso,ls_node)
      enddo
      call f_hpmstop(55)
cc
cc
cc.............................................................
 
cc
      do k=1,levs
         do i=1,len_trie_ls
 
         trie_ls(i,1,p_w+k-1)=trie_ls(i,1,p_w+k-1)+bfilte(i)*
     &        (trie_ls(i,1,p_vln+k-1)-trie_ls(i,1,p_w+k-1))
!!
         trie_ls_rqt(i,1,k)=bfilte(i)*
     &        (trie_ls(i,1,p_rq+k-1)-trie_ls(i,1,p_rt+k-1))
!!
         trie_ls(i,1,p_rt+k-1)=trie_ls(i,1,p_rt+k-1)+bfilte(i)*
     &        (trie_ls(i,1,p_rq+k-1)-trie_ls(i,1,p_rt+k-1))
!!
         trie_ls(i,2,p_w+k-1)=trie_ls(i,2,p_w+k-1)+bfilte(i)*
     &        (trie_ls(i,2,p_vln+k-1)-trie_ls(i,2,p_w+k-1))
!!
         trie_ls_rqt(i,2,k)=bfilte(i)*
     &        (trie_ls(i,2,p_rq+k-1)-trie_ls(i,2,p_rt+k-1))
!!
         trie_ls(i,2,p_rt+k-1)=trie_ls(i,2,p_rt+k-1)+bfilte(i)*
     &        (trie_ls(i,2,p_rq+k-1)-trie_ls(i,2,p_rt+k-1))
 
         enddo
      enddo
!!
      do k=1,levs
         do i=1,len_trio_ls
 
         trio_ls(i,1,p_w+k-1)=trio_ls(i,1,p_w+k-1)+bfilto(i)*
     &        (trio_ls(i,1,p_vln+k-1)-trio_ls(i,1,p_w+k-1))
 
         trio_ls_rqt(i,1,k)=bfilto(i)*
     &        (trio_ls(i,1,p_rq+k-1)-trio_ls(i,1,p_rt+k-1))
 
         trio_ls(i,1,p_rt+k-1)=trio_ls(i,1,p_rt+k-1)+bfilto(i)*
     &        (trio_ls(i,1,p_rq+k-1)-trio_ls(i,1,p_rt+k-1))
 
         trio_ls(i,2,p_w+k-1)=trio_ls(i,2,p_w+k-1)+bfilto(i)*
     &        (trio_ls(i,2,p_vln+k-1)-trio_ls(i,2,p_w+k-1))
 
         trio_ls_rqt(i,2,k)=bfilto(i)*
     &        (trio_ls(i,2,p_rq+k-1)-trio_ls(i,2,p_rt+k-1))
 
         trio_ls(i,2,p_rt+k-1)=trio_ls(i,2,p_rt+k-1)+bfilto(i)*
     &        (trio_ls(i,2,p_rq+k-1)-trio_ls(i,2,p_rt+k-1))
 
         enddo
      enddo
!!
      do nt=2,ntrac
      do k=levs*(nt-2)+1,levs*(nt-1)
       do i=1,len_trie_ls
       trie_ls(i,1,p_rt+levs+k-1)=trie_ls(i,1,p_rt+levs+k-1)+bfilte(i)*
     &        (trie_ls(i,1,p_rq+levs+k-1)-trie_ls(i,1,p_rt+levs+k-1))
 
       trie_ls(i,2,p_rt+levs+k-1)=trie_ls(i,2,p_rt+levs+k-1)+bfilte(i)*
     &        (trie_ls(i,2,p_rq+levs+k-1)-trie_ls(i,2,p_rt+levs+k-1))
 
       enddo
       do i=1,len_trio_ls
 
       trio_ls(i,1,p_rt+levs+k-1)=trio_ls(i,1,p_rt+levs+k-1)+bfilto(i)*
     &        (trio_ls(i,1,p_rq+levs+k-1)-trio_ls(i,1,p_rt+levs+k-1))
 
       trio_ls(i,2,p_rt+levs+k-1)=trio_ls(i,2,p_rt+levs+k-1)+bfilto(i)*
     &        (trio_ls(i,2,p_rq+levs+k-1)-trio_ls(i,2,p_rt+levs+k-1))
 
       enddo
      enddo
      enddo
!!
!----------------------------------------------------------------------
        if(hybrid)then
 
!     get some sigma distribution and compute   typdel from it.
 
      typical_pgr=85.
!sela  si(k)=(ak5(k)+bk5(k)*typical_pgr)/typical_pgr   !ak(k) bk(k) go top to botto
      do k=1,levp1
       si(levs+2-k)= ak5(k)/typical_pgr+bk5(k)
      enddo
 
        endif

      DO k=1,LEVS
        typDEL(k)= SI(k)-SI(k+1)
      ENDDO
 
!----------------------------------------------------------------------
 
      if (ladj) then
        trie_ls(:,:,p_zq)=0.
        trio_ls(:,:,p_zq)=0.
        if (me.eq.me_l_0) then
          trie_ls(1,1,p_zq)=pcorr
        endif
!!
      if( gen_coord_hybrid ) then                                       ! hmhj

       trie_ls_sfc=0.0                                                  ! hmhj
       trio_ls_sfc=0.0                                                  ! hmhj
       do k=1,levs                                                      ! hmhj
        do i=1,len_trie_ls                                              ! hmhj
         trie_ls_sfc(i,1)=trie_ls_sfc(i,1)+typdel(k)*trie_ls_rqt(i,1,k) ! hmhj
         trie_ls_sfc(i,2)=trie_ls_sfc(i,2)+typdel(k)*trie_ls_rqt(i,2,k) ! hmhj
        enddo                                                           ! hmhj
        do i=1,len_trio_ls                                              ! hmhj
         trio_ls_sfc(i,1)=trio_ls_sfc(i,1)+typdel(k)*trio_ls_rqt(i,1,k) ! hmhj
         trio_ls_sfc(i,2)=trio_ls_sfc(i,2)+typdel(k)*trio_ls_rqt(i,2,k) ! hmhj
        enddo                                                           ! hmhj
       enddo                                                            ! hmhj

       do i=1,len_trie_ls                                               ! hmhj
        trie_ls(i,1,p_zq)=trie_ls(i,1,p_zq)+                            ! hmhj
     &                    trie_ls(i,1,p_q )*trie_ls_sfc(i,1)            ! hmhj
        trie_ls(i,2,p_zq)=trie_ls(i,2,p_zq)+                            ! hmhj
     &                    trie_ls(i,2,p_q )*trie_ls_sfc(i,2)            ! hmhj
       enddo
       do i=1,len_trio_ls                                               ! hmhj
        trio_ls(i,1,p_zq)=trio_ls(i,1,p_zq)+                            ! hmhj
     &                    trio_ls(i,1,p_q )*trio_ls_sfc(i,1)            ! hmhj
        trio_ls(i,2,p_zq)=trio_ls(i,2,p_zq)+                            ! hmhj
     &                   trio_ls(i,2,p_q )*trio_ls_sfc(i,2)             ! hmhj
       enddo

      else                                                              ! hmhj
 
      do k=1,levs
       do i=1,len_trie_ls
 
        trie_ls(i,1,p_zq)=trie_ls(i,1,p_zq)+typdel(k)*trie_ls_rqt(i,1,k)
        trie_ls(i,2,p_zq)=trie_ls(i,2,p_zq)+typdel(k)*trie_ls_rqt(i,2,k)
 
       enddo
       do i=1,len_trio_ls
 
        trio_ls(i,1,p_zq)=trio_ls(i,1,p_zq)+typdel(k)*trio_ls_rqt(i,1,k)
        trio_ls(i,2,p_zq)=trio_ls(i,2,p_zq)+typdel(k)*trio_ls_rqt(i,2,k)
 
       enddo
      enddo

      endif                                                             ! hmhj
!!
      do k=1,levs
       do i=1,len_trie_ls
 
         trie_ls(i,1,p_di+k-1)=bfilte(i)*
     &        (trie_ls(i,1,p_uln+k-1)-trie_ls(i,1,p_x+k-1))
 
         trie_ls(i,1,p_te+k-1)=bfilte(i)*
     &        (trie_ls(i,1,p_te+k-1)-trie_ls(i,1,p_y+k-1))
 
         trie_ls(i,2,p_di+k-1)=bfilte(i)*
     &        (trie_ls(i,2,p_uln+k-1)-trie_ls(i,2,p_x+k-1))
 
         trie_ls(i,2,p_te+k-1)=bfilte(i)*
     &        (trie_ls(i,2,p_te+k-1)-trie_ls(i,2,p_y+k-1))
 
       enddo
       do i=1,len_trio_ls
 
         trio_ls(i,1,p_di+k-1)=bfilto(i)*
     &        (trio_ls(i,1,p_uln+k-1)-trio_ls(i,1,p_x+k-1))
 
         trio_ls(i,1,p_te+k-1)=bfilto(i)*
     &        (trio_ls(i,1,p_te+k-1)-trio_ls(i,1,p_y+k-1))
 
         trio_ls(i,2,p_di+k-1)=bfilto(i)*
     &        (trio_ls(i,2,p_uln+k-1)-trio_ls(i,2,p_x+k-1))
 
         trio_ls(i,2,p_te+k-1)=bfilto(i)*
     &        (trio_ls(i,2,p_te+k-1)-trio_ls(i,2,p_y+k-1))
 
       enddo
      enddo
 
!---------------------------------------------------------
      if( gen_coord_hybrid ) then                                       ! hmhj

!$OMP parallel do private(locl)
      do locl=1,ls_max_node                                             ! hmhj

      call impadje_hyb_gc(trie_ls(1,1,p_x),trie_ls(1,1,p_y),            ! hmhj
     &                    trie_ls(1,1,p_q),trie_ls(1,1,p_di),           ! hmhj
     &                    trie_ls(1,1,p_te),trie_ls(1,1,p_zq),          ! hmhj
     &                      tstep,                                      ! hmhj
     &                    trie_ls(1,1,p_uln),trie_ls(1,1,p_vln),        ! hmhj
     &                    snnp1ev,ndexev,ls_node,locl)                  ! hmhj
!!
      call impadjo_hyb_gc(trio_ls(1,1,p_x),trio_ls(1,1,p_y),            ! hmhj
     &                    trio_ls(1,1,p_q),trio_ls(1,1,p_di),           ! hmhj
     &                    trio_ls(1,1,p_te),trio_ls(1,1,p_zq),          ! hmhj
     &                      tstep,                                      ! hmhj
     &                    trio_ls(1,1,p_uln),trio_ls(1,1,p_vln),        ! hmhj
     &                    snnp1od,ndexod,ls_node,locl)                  ! hmhj
      enddo                                                             ! hmhj
      else if(hybrid)then                                               ! hmhj
 
!$OMP parallel do private(locl)
      do locl=1,ls_max_node
 
      call impadje_hyb(trie_ls(1,1,p_x),trie_ls(1,1,p_y),
     &             trie_ls(1,1,p_q),trie_ls(1,1,p_di),
     &             trie_ls(1,1,p_te),trie_ls(1,1,p_zq),
     &                      tstep,
     &             trie_ls(1,1,p_uln),trie_ls(1,1,p_vln),
     &             snnp1ev,ndexev,ls_node,locl)
!!
      call impadjo_hyb(trio_ls(1,1,p_x),trio_ls(1,1,p_y),
     &             trio_ls(1,1,p_q),trio_ls(1,1,p_di),
     &             trio_ls(1,1,p_te),trio_ls(1,1,p_zq),
     &                      tstep,
     &             trio_ls(1,1,p_uln),trio_ls(1,1,p_vln),
     &             snnp1od,ndexod,ls_node,locl)
      enddo
 
 
      else  ! fin massadj in hybrid
 
      call countperf(0,9,0.)
!$OMP parallel do private(locl)
      do locl=1,ls_max_node
 
      call impadje(trie_ls(1,1,p_x),trie_ls(1,1,p_y),
     &             trie_ls(1,1,p_q),trie_ls(1,1,p_di),
     &             trie_ls(1,1,p_te),trie_ls(1,1,p_zq),
     &             am,bm,sv,tstep,
     &             trie_ls(1,1,p_uln),trie_ls(1,1,p_vln),
     &             snnp1ev,ndexev,ls_node,locl)
!!
      call impadjo(trio_ls(1,1,p_x),trio_ls(1,1,p_y),
     &             trio_ls(1,1,p_q),trio_ls(1,1,p_di),
     &             trio_ls(1,1,p_te),trio_ls(1,1,p_zq),
     &             am,bm,sv,tstep,
     &             trio_ls(1,1,p_uln),trio_ls(1,1,p_vln),
     &             snnp1od,ndexod,ls_node,locl)
      enddo
 
      call countperf(1,9,0.)
 
      endif  ! fin massadj in sigma
!---------------------------------------------------------
 
      else  ! fin massadj,    following is with no masadj
 
      if (me.eq.me_l_0) then
        trie_ls(1,1,p_q)=trie_ls(1,1,p_q)+pcorr
      endif
! testing mass correction on sep 25
!!
      do k=1,levs
       do i=1,len_trie_ls
        trie_ls(i,1,p_q)=trie_ls(i,1,p_q)+del(k)*trie_ls_rqt(i,1,k)
        trie_ls(i,2,p_q)=trie_ls(i,2,p_q)+del(k)*trie_ls_rqt(i,2,k)
       enddo
       do i=1,len_trio_ls
        trio_ls(i,1,p_q)=trio_ls(i,1,p_q)+del(k)*trio_ls_rqt(i,1,k)
        trio_ls(i,2,p_q)=trio_ls(i,2,p_q)+del(k)*trio_ls_rqt(i,2,k)
       enddo
      enddo
! testing mass correction on sep 25
!!
      do k=1,levs
       do i=1,len_trie_ls
 
         trie_ls(i,1,p_x+k-1)=trie_ls(i,1,p_x+k-1)+bfilte(i)*
     &        (trie_ls(i,1,p_uln+k-1)-trie_ls(i,1,p_x+k-1))
 
         trie_ls(i,2,p_x+k-1)=trie_ls(i,2,p_x+k-1)+bfilte(i)*
     &        (trie_ls(i,2,p_uln+k-1)-trie_ls(i,2,p_x+k-1))
 
         trie_ls(i,1,p_y+k-1)=trie_ls(i,1,p_y+k-1)+bfilte(i)*
     &        (trie_ls(i,1,p_te+k-1)-trie_ls(i,1,p_y+k-1))
 
         trie_ls(i,2,p_y+k-1)=trie_ls(i,2,p_y+k-1)+bfilte(i)*
     &        (trie_ls(i,2,p_te+k-1)-trie_ls(i,2,p_y+k-1))
 
       enddo
       do i=1,len_trio_ls
 
 
         trio_ls(i,1,p_x+k-1)=trio_ls(i,1,p_x+k-1)+bfilto(i)*
     &        (trio_ls(i,1,p_uln+k-1)-trio_ls(i,1,p_x+k-1))
 
         trio_ls(i,2,p_x+k-1)=trio_ls(i,2,p_x+k-1)+bfilto(i)*
     &        (trio_ls(i,2,p_uln+k-1)-trio_ls(i,2,p_x+k-1))
 
         trio_ls(i,1,p_y+k-1)=trio_ls(i,1,p_y+k-1)+bfilto(i)*
     &        (trio_ls(i,1,p_te+k-1)-trio_ls(i,1,p_y+k-1))
 
         trio_ls(i,2,p_y+k-1)=trio_ls(i,2,p_y+k-1)+bfilto(i)*
     &        (trio_ls(i,2,p_te+k-1)-trio_ls(i,2,p_y+k-1))
 
       enddo
      enddo
 
      endif   ! fin no massadj
!!
      return
      end

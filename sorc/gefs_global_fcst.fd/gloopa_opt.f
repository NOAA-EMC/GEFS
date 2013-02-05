      subroutine gloopa
     x    (deltim,trie_ls,trio_ls,
     x     ls_node,ls_nodes,max_ls_nodes,
     x     lats_nodes_a,global_lats_a,
     x     lonsperlat,
     x     epse,epso,epsedn,epsodn,
     x     snnp1ev,snnp1od,ndexev,ndexod,
     x     plnev_a,plnod_a,pddev_a,pddod_a,plnew_a,plnow_a,
     x     global_times_a,kdt)
!

c
!include "f_hpm.h" !  jjt include file for hpm
!
      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use mpi_def
      use physcons, rerth => con_rerth
      implicit none
!
      real(kind=kind_evod) trie_ls(len_trie_ls,2,11*levs+3*levh+6)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,11*levs+3*levh+6)
!
      integer              ls_node(ls_dim,3)
!
!     ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!     ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!     ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
      integer              ls_nodes(ls_dim,nodes)
!
      integer              max_ls_nodes(nodes)
      integer              lats_nodes_a(nodes)
!
!timers______________________________________________________---
 
      real*8 rtc ,timer1,timer2
      real(kind=kind_evod) global_times_a(latg,nodes)
 
!timers______________________________________________________---
 
      integer              global_lats_a(latg)
      integer                 lonsperlat(latg)
      integer dimg
!
      real(kind=kind_evod)    epse(len_trie_ls)
      real(kind=kind_evod)    epso(len_trio_ls)
      real(kind=kind_evod)  epsedn(len_trie_ls)
      real(kind=kind_evod)  epsodn(len_trio_ls)
!
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
!
      integer               ndexev(len_trie_ls)
      integer               ndexod(len_trio_ls)
!
      real(kind=kind_evod)   plnev_a(len_trie_ls,latg2)
      real(kind=kind_evod)   plnod_a(len_trio_ls,latg2)
      real(kind=kind_evod)   pddev_a(len_trie_ls,latg2)
      real(kind=kind_evod)   pddod_a(len_trio_ls,latg2)
      real(kind=kind_evod)   plnew_a(len_trie_ls,latg2)
      real(kind=kind_evod)   plnow_a(len_trio_ls,latg2)
!
c$$$      integer                lots,lotd,lota
c$$$!
c$$$      parameter            ( lots = 5*levs+1*levh+3 )
c$$$      parameter            ( lotd = 6*levs+2*levh+0 )
c$$$      parameter            ( lota = 3*levs+1*levh+1 )
!
      real(kind=kind_evod) for_gr_a_1(lonfx*lots,lats_dim_a)
      real(kind=kind_evod) dyn_gr_a_1(lonfx*lotd,lats_dim_a)
      real(kind=kind_evod) bak_gr_a_1(lonfx*lota,lats_dim_a)
!
      real(kind=kind_evod) for_gr_a_2(lonfx*lots,lats_dim_a)
      real(kind=kind_evod) dyn_gr_a_2(lonfx*lotd,lats_dim_a)
      real(kind=kind_evod) bak_gr_a_2(lonfx*lota,lats_dim_a)
!
!     saved vertical advection of tracers from time step n-1
      real(kind=kind_evod),allocatable, save:: szdrdt(:,:)
      logical,save:: zfirst=.true.
!
      integer              i,ierr,iter,j,k,kap,kar,kat,kau,kav,kdrlam
      integer              kdrphi,kdtlam,kdtphi,kdulam,kduphi,kdvlam
      integer              kdvphi,ksd,ksplam,kspphi,ksq,ksr,kst
      integer              ksu,ksv,ksz,l,lan,lan0,lat,lmax,locl
      integer              lon_dim,lons_lat,n,node,nvcn,ii
!
      integer              init
      integer              ibmsign
      integer              ipt_ls
!
      real(kind=kind_evod) rone,deltim
      real(kind=kind_evod) scale_ibm
!
      real(kind=kind_evod) xvcn
!
      integer              iter_max
!
      real(kind=kind_evod) ,allocatable :: spdlat(:,:)
!
      real(kind=kind_evod) spdmax_node (levs)
      real(kind=kind_mpi) spdmax_nodem (levs)
      real(kind=kind_evod) spdmax_nodes(levs,nodes)
      real(kind=kind_mpi) spdmax_nodesm(levs,nodes)
!
      real(kind=kind_evod) reall
      real(kind=kind_evod) rlcs2(jcap1)
!
!
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
! ................................................................
!   syn(1, 0*levs+0*levh+1, lan)  ze
!   syn(1, 1*levs+0*levh+1, lan)  di
!   syn(1, 2*levs+0*levh+1, lan)  te
!   syn(1, 3*levs+0*levh+1, lan)  rq
!   syn(1, 3*levs+1*levh+1, lan)  q
!   syn(1, 3*levs+1*levh+2, lan)  dpdlam
!   syn(1, 3*levs+1*levh+3, lan)  dpdphi
!   syn(1, 3*levs+1*levh+4, lan)  uln
!   syn(1, 4*levs+1*levh+4, lan)  vln
! ................................................................
!   dyn(1, 0*levs+0*levh+1, lan)  d(t)/d(phi)
!   dyn(1, 1*levs+0*levh+1, lan)  d(rq)/d(phi)
!   dyn(1, 1*levs+1*levh+1, lan)  d(t)/d(lam)
!   dyn(1, 2*levs+1*levh+1, lan)  d(rq)/d(lam)
!   dyn(1, 2*levs+2*levh+1, lan)  d(u)/d(lam)
!   dyn(1, 3*levs+2*levh+1, lan)  d(v)/d(lam)
!   dyn(1, 4*levs+2*levh+1, lan)  d(u)/d(phi)
!   dyn(1, 5*levs+2*levh+1, lan)  d(v)/d(phi)
! ................................................................
!   anl(1, 0*levs+0*levh+1, lan)  w     dudt
!   anl(1, 1*levs+0*levh+1, lan)  x     dvdt
!   anl(1, 2*levs+0*levh+1, lan)  y     dtdt
!   anl(1, 3*levs+0*levh+1, lan)  rt    drdt
!   anl(1, 3*levs+1*levh+1, lan)  z     dqdt
! ................................................................
!
!
c$$$      parameter(ksz     =0*levs+0*levh+1,
c$$$     x          ksd     =1*levs+0*levh+1,
c$$$     x          kst     =2*levs+0*levh+1,
c$$$     x          ksr     =3*levs+0*levh+1,
c$$$     x          ksq     =3*levs+1*levh+1,
c$$$     x          ksplam  =3*levs+1*levh+2,
c$$$     x          kspphi  =3*levs+1*levh+3,
c$$$     x          ksu     =3*levs+1*levh+4,
c$$$     x          ksv     =4*levs+1*levh+4)
c$$$!
c$$$      parameter(kdtphi  =0*levs+0*levh+1,
c$$$     x          kdrphi  =1*levs+0*levh+1,
c$$$     x          kdtlam  =1*levs+1*levh+1,
c$$$     x          kdrlam  =2*levs+1*levh+1,
c$$$     x          kdulam  =2*levs+2*levh+1,
c$$$     x          kdvlam  =3*levs+2*levh+1,
c$$$     x          kduphi  =4*levs+2*levh+1,
c$$$     x          kdvphi  =5*levs+2*levh+1)
c$$$!
c$$$      parameter(kau     =0*levs+0*levh+1,
c$$$     x          kav     =1*levs+0*levh+1,
c$$$     x          kat     =2*levs+0*levh+1,
c$$$     x          kar     =3*levs+0*levh+1,
c$$$     x          kap     =3*levs+1*levh+1)
!
!
c$$$      integer   P_gz,P_zem,P_dim,P_tem,P_rm,P_qm
c$$$      integer   P_ze,P_di,P_te,P_rq,P_q,P_dlam,P_dphi,P_uln,P_vln
c$$$      integer   P_w,P_x,P_y,P_rt,P_zq
c$$$!
c$$$!                                                old common /comfspec/
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
c$$$!
!
      integer              indlsev,jbasev
      integer              indlsod,jbasod
      integer kdt
!
      include 'function2'
!
      integer njeff,istrt,lon,iblk
!
      integer     ngptcd
!
      parameter ( ngptcd = 12 )
!
      real(kind=kind_evod) cons0,cons2     !constant
!
      logical lslag
!
      if(zfirst) then
        allocate (szdrdt(lonfx*levh,lats_dim_a))
        szdrdt=0.
      endif
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
!
      kdtphi  =0*levs+0*levh+1
      kdrphi  =1*levs+0*levh+1
      kdtlam  =1*levs+1*levh+1
      kdrlam  =2*levs+1*levh+1
      kdulam  =2*levs+2*levh+1
      kdvlam  =3*levs+2*levh+1
      kduphi  =4*levs+2*levh+1
      kdvphi  =5*levs+2*levh+1
!
      kau     =0*levs+0*levh+1
      kav     =1*levs+0*levh+1
      kat     =2*levs+0*levh+1
      kar     =3*levs+0*levh+1
      kap     =3*levs+1*levh+1

      cons0 = 0.d0     !constant
      cons2 = 2.d0     !constant
      lslag=.false.
!
      iter_max=0
      do lan=1,lats_node_a
         lat = global_lats_a(ipt_lats_node_a-1+lan)
         lons_lat = lonsperlat(lat)
         iter_max = max ( iter_max , (lons_lat+ngptcd-1)/ngptcd )
      enddo
!
      allocate ( spdlat(levs,iter_max ) )
!
! ................................................................
!
!
      call f_hpmstart(1,"ga delnpe")
      call delnpe(trie_ls(1,1,P_q   ),
     x            trio_ls(1,1,P_dphi),
     x            trie_ls(1,1,P_dlam),
     x            epse,epso,ls_node)
      call f_hpmstop(1)
!
      call f_hpmstart(2,"ga delnpo")
      call delnpo(trio_ls(1,1,P_q   ),
     x            trie_ls(1,1,P_dphi),
     x            trio_ls(1,1,P_dlam),
     x            epse,epso,ls_node)
      call f_hpmstop(2)
!
!
      call f_hpmstart(3,"ga dezouv dozeuv")
!
!$omp parallel do shared(trie_ls,trio_ls)
!$omp+shared(epsedn,epsodn,snnp1ev,snnp1od,ls_node)
!$omp+private(k)
      do k=1,levs
         call dezouv(trie_ls(1,1,P_di +k-1), trio_ls(1,1,P_ze +k-1),
     x               trie_ls(1,1,P_uln+k-1), trio_ls(1,1,P_vln+k-1),
     x               epsedn,epsodn,snnp1ev,snnp1od,ls_node)
!
         call dozeuv(trio_ls(1,1,P_di +k-1), trie_ls(1,1,P_ze +k-1),
     x               trio_ls(1,1,P_uln+k-1), trie_ls(1,1,P_vln+k-1),
     x               epsedn,epsodn,snnp1ev,snnp1od,ls_node)
      enddo
!
      call f_hpmstop(3)
!
!
!
!sela call mpi_barrier (mpi_comm_world,ierr)
!
      CALL countperf(0,3,0.)
      CALL synctime()
      CALL countperf(1,3,0.)
!!
      dimg=0
      CALL countperf(0,1,0.)
!
      call f_hpmstart(8,"ga sumflna")
!
      call sumflna(trie_ls(1,1,P_ze),
     x            trio_ls(1,1,P_ze),
     x            lat1s_a,
     x            plnev_a,plnod_a,
     x            lots,ls_node,latg2,
     x            lslag,lats_dim_a,lots,
     x            for_gr_a_1,
     x            ls_nodes,max_ls_nodes,
     x            lats_nodes_a,global_lats_a,
     x            lats_node_a,ipt_lats_node_a,lon_dims_a,dimg,
     x            lonsperlat,lonfx,latg)
!
      call f_hpmstop(8)
!
      CALL countperf(1,1,0.)
!
!jfe  do j=1,lats_node_a
!jfe     lat = global_lats_a(ipt_lats_node_a-1+j)
!jfe     ii=0
!jfe  do k=1,lots
!jfe  do i=1,lon_dims_a(j)
!jfe      ii=ii+1
!jfe      if (
!jfe &     i.le.lonsperlat(lat))
!jfe &    write(60+me,*) for_gr_a_1(ii,j),i,lat,k
!jfe  enddo
!jfe  enddo
!jfe  enddo
!
!sela call mpi_barrier (mpi_comm_world,ierr)
!
      CALL countperf(0,1,0.)
!
      call f_hpmstart(9,"ga sumdera")
!
      call sumdera(trie_ls(1,1,P_te),
     x             trio_ls(1,1,P_te),
     x             lat1s_a,
     x             pddev_a,pddod_a,
     x             levs+levh,ls_node,latg2,
     x             lslag,lats_dim_a,lotd,
     x             dyn_gr_a_1,
     x             ls_nodes,max_ls_nodes,
     x             lats_nodes_a,global_lats_a,
     x             lats_node_a,ipt_lats_node_a,lon_dims_a,dimg,
     x             lonsperlat,lonfx,latg)
!
      call f_hpmstop(9)
!
      CALL countperf(1,1,0.)
!
      do k=1,levs
         spdmax_node(k) = cons0     !constant
      enddo
!
!sela call mpi_barrier (mpi_comm_world,ierr)
!
      call f_hpmstart(10,"ga lat_loop")
!11111111111111111111111111111111111111111111111111111111111111111111
      do lan=1,lats_node_a   !sela begin lan loop 1
 
       timer1=rtc()
!
         lat = global_lats_a(ipt_lats_node_a-1+lan)
!
c
         lon_dim = lon_dims_a(lan)
         lons_lat = lonsperlat(lat)
!!
!!       calculate t rq u v zonal derivs. by multiplication with i*l
!!       note rlcs2=rcs2*L/rerth
!
         lmax = min(jcap,lons_lat/2)
!
         ipt_ls=min(lat,latg-lat+1)
 
         do i=1,lmax+1
            if ( ipt_ls .ge. lat1s_a(i-1) ) then
               reall=i-1
               rlcs2(i)=reall*rcs2_a(ipt_ls)/rerth
            else
               rlcs2(i)=cons0     !constant
            endif
         enddo
!
!$omp parallel do private(k,i)
         do k=1,levs
            do i=1,lmax+1
!
!           d(t)/d(lam)
               dyn_gr_a_1(2*i-1+(kdtlam-2+k)*lon_dim,lan)=
     x        -for_gr_a_1(2*i  +(kst   -2+k)*lon_dim,lan)*rlcs2(i)
               dyn_gr_a_1(2*i  +(kdtlam-2+k)*lon_dim,lan)=
     x         for_gr_a_1(2*i-1+(kst   -2+k)*lon_dim,lan)*rlcs2(i)
!
!           d(u)/d(lam)
               dyn_gr_a_1(2*i-1+(kdulam-2+k)*lon_dim,lan)=
     x        -for_gr_a_1(2*i  +(ksu   -2+k)*lon_dim,lan)*rlcs2(i)
               dyn_gr_a_1(2*i  +(kdulam-2+k)*lon_dim,lan)=
     x         for_gr_a_1(2*i-1+(ksu   -2+k)*lon_dim,lan)*rlcs2(i)
!
!           d(v)/d(lam)
               dyn_gr_a_1(2*i-1+(kdvlam-2+k)*lon_dim,lan)=
     x        -for_gr_a_1(2*i  +(ksv   -2+k)*lon_dim,lan)*rlcs2(i)
               dyn_gr_a_1(2*i  +(kdvlam-2+k)*lon_dim,lan)=
     x         for_gr_a_1(2*i-1+(ksv   -2+k)*lon_dim,lan)*rlcs2(i)
!
            enddo
         end do
!
!$omp parallel do private(k,i)
         do k=1,levh
            do i=1,lmax+1
!
!           d(rq)/d(lam)
               dyn_gr_a_1(2*i-1+(kdrlam-2+k)*lon_dim,lan)=
     x        -for_gr_a_1(2*i  +(ksr   -2+k)*lon_dim,lan)*rlcs2(i)
               dyn_gr_a_1(2*i  +(kdrlam-2+k)*lon_dim,lan)=
     x         for_gr_a_1(2*i-1+(ksr   -2+k)*lon_dim,lan)*rlcs2(i)
!
            enddo
         enddo
!
         CALL countperf(0,6,0.)
         CALL FOUR2GRID_thread(for_gr_a_1(1,lan),for_gr_a_2(1,lan),
     &                  lon_dim,lons_lat,lonfx,5*levs+levh+3,lan,me)
!!
         CALL FOUR2GRID_thread(dyn_gr_a_1(1,lan),dyn_gr_a_2(1,lan),
     &                  lon_dim,lons_lat,lonfx,4*levs+2*levh,lan,me)
         CALL countperf(1,6,0.)
!
          timer2=rtc()
          global_times_a(lat,me+1)=timer2-timer1
c$$$       print*,'timeloopa',me,timer1,timer2,global_times_a(lat,me+1)
 
      enddo !sela fin lan loop 1
!11111111111111111111111111111111111111111111111111111111111111111111
!
!22222222222222222222222222222222222222222222222222222222222
!22222222222222222222222222222222222222222222222222222222222
      do lan=1,lats_node_a   !sela begin lan loop 2
 
       timer1=rtc()
!
         lat = global_lats_a(ipt_lats_node_a-1+lan)
!
         lon_dim = lon_dims_a(lan)
         lons_lat = lonsperlat(lat)
!!
!!  calculate grid meridional derivatives of u and v.
!!
!!  cos*d(u)/d(theta)= d(v)/d(lam)-a*zeta*cos**2
!!  cos*d(v)/d(theta)=-d(u)/d(lam)+a*divr*cos**2
!!
!$omp parallel do private(k,j)
      do k=1,levs
         do j=1,lons_lat
!
            dyn_gr_a_2(j+(kduphi-2+k)*lon_dim,lan)=
     x      dyn_gr_a_2(j+(kdvlam-2+k)*lon_dim,lan)-
     x      for_gr_a_2(j+(ksz   -2+k)*lon_dim,lan)
!
            dyn_gr_a_2(j+(kdvphi-2+k)*lon_dim,lan)=
     x     -dyn_gr_a_2(j+(kdulam-2+k)*lon_dim,lan)+
     x      for_gr_a_2(j+(ksd   -2+k)*lon_dim,lan)
!
           if( .not. gen_coord_hybrid ) then			! hmhj
            for_gr_a_2(j+(kst-2+k)*lon_dim,lan)=
     x      for_gr_a_2(j+(kst-2+k)*lon_dim,lan)-tov(k)
           endif						! hmhj
!
         enddo
      enddo
!
      if(hybrid.or.gen_coord_hybrid) then !-----  hybrid ----------- ! hmhj
! beginlon omp loop 3333333333333333333333333333333333333333333333333
!$omp parallel do schedule(dynamic,1) private(lon)
!$omp+private(istrt,njeff,iblk)
!$omp+private(nvcn,xvcn)
      do lon=1,lons_lat,ngptcd
!!
         njeff=min(ngptcd,lons_lat-lon+1)
         istrt=lon
         if (ngptcd.ne.1) then
           iblk=lon/ngptcd+1
         else
           iblk=lon
         endif
!
         CALL countperf(0,10,0.)
         if( gen_coord_hybrid ) then					! hmhj
           if( thermodyn_id.eq.3 ) then					! hmhj
             call gfidi_hyb_gc_h(lon_dim, njeff, lat,			! hmhj
     x               for_gr_a_2(istrt+(ksd   -1)*lon_dim,lan),		! hmhj
     x               for_gr_a_2(istrt+(kst   -1)*lon_dim,lan),		! hmhj
     x               for_gr_a_2(istrt+(ksz   -1)*lon_dim,lan),		! hmhj
     x               for_gr_a_2(istrt+(ksu   -1)*lon_dim,lan),		! hmhj
     x               for_gr_a_2(istrt+(ksv   -1)*lon_dim,lan),		! hmhj
     x               for_gr_a_2(istrt+(ksr   -1)*lon_dim,lan),		! hmhj
     x               for_gr_a_2(istrt+(kspphi-1)*lon_dim,lan),		! hmhj
     x               for_gr_a_2(istrt+(ksplam-1)*lon_dim,lan),		! hmhj
     x               for_gr_a_2(istrt+(ksq   -1)*lon_dim,lan),		! hmhj
     x               rcs2_a(min(lat,latg-lat+1)),			! hmhj
     x               spdlat(1,iblk),					! hmhj
     x               deltim,nvcn,xvcn,					! hmhj
     x               dyn_gr_a_2(istrt+(kdtphi-1)*lon_dim,lan),		! hmhj
     x               dyn_gr_a_2(istrt+(kdtlam-1)*lon_dim,lan),		! hmhj
     x               dyn_gr_a_2(istrt+(kdrphi-1)*lon_dim,lan),		! hmhj
     x               dyn_gr_a_2(istrt+(kdrlam-1)*lon_dim,lan),		! hmhj
     x               dyn_gr_a_2(istrt+(kdulam-1)*lon_dim,lan),		! hmhj
     x               dyn_gr_a_2(istrt+(kdvlam-1)*lon_dim,lan),		! hmhj
     x               dyn_gr_a_2(istrt+(kduphi-1)*lon_dim,lan),		! hmhj
     x               dyn_gr_a_2(istrt+(kdvphi-1)*lon_dim,lan),		! hmhj
     x               bak_gr_a_2(istrt+(kap   -1)*lon_dim,lan),		! hmhj
     x               bak_gr_a_2(istrt+(kat   -1)*lon_dim,lan),		! hmhj
     x               bak_gr_a_2(istrt+(kar   -1)*lon_dim,lan),		! hmhj
     x               bak_gr_a_2(istrt+(kau   -1)*lon_dim,lan),		! hmhj
     x               bak_gr_a_2(istrt+(kav   -1)*lon_dim,lan),		! hmhj
     x               szdrdt(istrt,lan),zfirst)                          ! fyang 
           else								! hmhj
             call gfidi_hyb_gc(lon_dim, njeff, lat,
     x               for_gr_a_2(istrt+(ksd   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(kst   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksz   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksu   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksv   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksr   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(kspphi-1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksplam-1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksq   -1)*lon_dim,lan),
     x               rcs2_a(min(lat,latg-lat+1)),
     x               spdlat(1,iblk),
     x               deltim,nvcn,xvcn,
     x               dyn_gr_a_2(istrt+(kdtphi-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdtlam-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdrphi-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdrlam-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdulam-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdvlam-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kduphi-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdvphi-1)*lon_dim,lan),
     x               bak_gr_a_2(istrt+(kap   -1)*lon_dim,lan),
     x               bak_gr_a_2(istrt+(kat   -1)*lon_dim,lan),
     x               bak_gr_a_2(istrt+(kar   -1)*lon_dim,lan),
     x               bak_gr_a_2(istrt+(kau   -1)*lon_dim,lan),
     x               bak_gr_a_2(istrt+(kav   -1)*lon_dim,lan),
     x               szdrdt(istrt,lan),zfirst)                
           endif							! hmhj
         else								! hmhj
           call gfidi_hyb(lon_dim, njeff, lat,
     x               for_gr_a_2(istrt+(ksd   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(kst   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksz   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksu   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksv   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksr   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(kspphi-1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksplam-1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksq   -1)*lon_dim,lan),
     x               rcs2_a(min(lat,latg-lat+1)),
     x               spdlat(1,iblk),
     x               deltim,nvcn,xvcn,
     x               dyn_gr_a_2(istrt+(kdtphi-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdtlam-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdrphi-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdrlam-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdulam-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdvlam-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kduphi-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdvphi-1)*lon_dim,lan),
     x               bak_gr_a_2(istrt+(kap   -1)*lon_dim,lan),
     x               bak_gr_a_2(istrt+(kat   -1)*lon_dim,lan),
     x               bak_gr_a_2(istrt+(kar   -1)*lon_dim,lan),
     x               bak_gr_a_2(istrt+(kau   -1)*lon_dim,lan),
     x               bak_gr_a_2(istrt+(kav   -1)*lon_dim,lan),
     x               szdrdt(istrt,lan),zfirst)              
         endif								! hmhj
         CALL countperf(1,10,0.)
!
         enddo   !lon
       else  !-------------  hybrid  ---------------------------
! beginlon omp loop 3333333333333333333333333333333333333333333333333
!$omp parallel do schedule(dynamic,1) private(lon)
!$omp+private(istrt,njeff,iblk)
!$omp+private(nvcn,xvcn)
      do lon=1,lons_lat,ngptcd
!!
         njeff=min(ngptcd,lons_lat-lon+1)
         istrt=lon
         if (ngptcd.ne.1) then
           iblk=lon/ngptcd+1
         else
           iblk=lon
         endif
!
         CALL countperf(0,10,0.)
         call gfidi_sig(lon_dim, njeff, lat,
     x               for_gr_a_2(istrt+(ksd   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(kst   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksz   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksu   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksv   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksr   -1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(kspphi-1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksplam-1)*lon_dim,lan),
     x               for_gr_a_2(istrt+(ksq   -1)*lon_dim,lan),
     x               rcs2_a(min(lat,latg-lat+1)),
     x               del,rdel2,ci,tov,spdlat(1,iblk),
     x               deltim,sl,nvcn,xvcn,
     x               dyn_gr_a_2(istrt+(kdtphi-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdtlam-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdrphi-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdrlam-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdulam-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdvlam-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kduphi-1)*lon_dim,lan),
     x               dyn_gr_a_2(istrt+(kdvphi-1)*lon_dim,lan),
     x               bak_gr_a_2(istrt+(kap   -1)*lon_dim,lan),
     x               bak_gr_a_2(istrt+(kat   -1)*lon_dim,lan),
     x               bak_gr_a_2(istrt+(kar   -1)*lon_dim,lan),
     x               bak_gr_a_2(istrt+(kau   -1)*lon_dim,lan),
     x               bak_gr_a_2(istrt+(kav   -1)*lon_dim,lan))
         CALL countperf(1,10,0.)
!
         enddo   !lon
       endif ! -----------------------  hybrid  ------------------
! beginlon omp loop 3333333333333333333333333333333333333333333333333
!
         iblk=1
         do lon=1,lons_lat,ngptcd
            do k=1,levs
               spdmax_node(k)=max(spdmax_node(k),spdlat(k,iblk))
            enddo
         iblk=iblk+1
         enddo
!
         CALL countperf(0,6,0.)
         CALL GRID2FOUR_thread(bak_gr_a_2(1,lan),bak_gr_a_1(1,lan),
     &                  lon_dim,lons_lat,lonfx,lota)
         CALL countperf(1,6,0.)
!
          timer2=rtc()
          global_times_a(lat,me+1)=
     x    global_times_a(lat,me+1)+timer2-timer1
c$$$       print*,'timeloopa',me,timer1,timer2,global_times_a(lat,me+1)
 
      enddo !sela fin lan loop 2
!22222222222222222222222222222222222222222222222222222222222
!22222222222222222222222222222222222222222222222222222222222
!
      call f_hpmstop(10)
!!
      CALL countperf(0,3,0.)
      CALL synctime()
      CALL countperf(1,3,0.)
!
      CALL countperf(0,1,0.)
!
      call f_hpmstart(12,"ga four2fln")
!
      call four2fln(lslag,lats_dim_a,lota,lota,bak_gr_a_1,
     x              ls_nodes,max_ls_nodes,
     x              lats_nodes_a,global_lats_a,lon_dims_a,
     x              lats_node_a,ipt_lats_node_a,dimg,
     x              lat1s_a,lonfx,latg,latg2,
     x              trie_ls(1,1,P_w), trio_ls(1,1,P_w),
     x              plnew_a, plnow_a,
     x              ls_node)
!
      call f_hpmstop(12)
!
      CALL countperf(1,1,0.)
!
      call f_hpmstart(15,"ga uveodz uvoedz")
!
!$omp parallel do shared(trie_ls,trio_ls)
!$omp+shared(epse,epso,ls_node)
!$omp+private(k)
      do k=1,levs
         call uveodz(trie_ls(1,1,P_w  +k-1), trio_ls(1,1,P_x  +k-1),
     x               trie_ls(1,1,P_uln+k-1), trio_ls(1,1,P_vln+k-1),
     x               epse,epso,ls_node)
!
         call uvoedz(trio_ls(1,1,P_w  +k-1), trie_ls(1,1,P_x  +k-1),
     x               trio_ls(1,1,P_uln+k-1), trie_ls(1,1,P_vln+k-1),
     x               epse,epso,ls_node)
      enddo
!
      call f_hpmstop(15)
!
!   move div tendency into x and add topog. contrib.
!   integrate vorticity amd moisture in time
!   remember uln is old x
!   remember vln is old w
!
      do k=1,levs
         do i=1,len_trie_ls
            trie_ls(i,1,P_x  +k-1)=
     x      trie_ls(i,1,P_uln+k-1)+             trie_ls(i,1,P_gz)
 
            trie_ls(i,2,P_x  +k-1)=
     x      trie_ls(i,2,P_uln+k-1)+             trie_ls(i,2,P_gz)
 
            trie_ls(i,1,P_w  +k-1)=
     x      trie_ls(i,1,P_zem+k-1)+cons2*deltim*trie_ls(i,1,P_vln+k-1)     !cons
 
            trie_ls(i,2,P_w  +k-1)=
     x      trie_ls(i,2,P_zem+k-1)+cons2*deltim*trie_ls(i,2,P_vln+k-1)     !cons
 
         enddo
         do i=1,len_trio_ls
            trio_ls(i,1,P_x  +k-1)=
     x      trio_ls(i,1,P_uln+k-1)+             trio_ls(i,1,P_gz)
 
            trio_ls(i,2,P_x  +k-1)=
     x      trio_ls(i,2,P_uln+k-1)+             trio_ls(i,2,P_gz)
 
            trio_ls(i,1,P_w  +k-1)=
     x      trio_ls(i,1,P_zem+k-1)+cons2*deltim*trio_ls(i,1,P_vln+k-1)     !cons
 
            trio_ls(i,2,P_w  +k-1)=
     x      trio_ls(i,2,P_zem+k-1)+cons2*deltim*trio_ls(i,2,P_vln+k-1)     !cons
 
         enddo
      enddo
!
      do k=1,levh
         do i=1,len_trie_ls
            trie_ls(i,1,P_rt+k-1)=
     x      trie_ls(i,1,P_rm+k-1)+cons2*deltim* trie_ls(i,1,P_rt+k-1)     !const
            trie_ls(i,2,P_rt+k-1)=
     x      trie_ls(i,2,P_rm+k-1)+cons2*deltim* trie_ls(i,2,P_rt+k-1)     !const
         enddo
         do i=1,len_trio_ls
            trio_ls(i,1,P_rt+k-1)=
     x      trio_ls(i,1,P_rm+k-1)+cons2*deltim* trio_ls(i,1,P_rt+k-1)     !const
            trio_ls(i,2,P_rt+k-1)=
     x      trio_ls(i,2,P_rm+k-1)+cons2*deltim* trio_ls(i,2,P_rt+k-1)     !const
         enddo
      enddo
c$$$      print*,' fin gloopa trie and trio '
!
!
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         if ( l .eq. 0 ) then
            n = 0
!
            do k=1,levs
               trie_ls(indlsev(n,l),1,P_w+k-1)=cons0     !constant
               trie_ls(indlsev(n,l),2,P_w+k-1)=cons0     !constant
            enddo
!
         endif
      end do
!
! hmhj ----  do explicit scheme --- sicdif should be turned off --- 
      if( explicit ) then						! hmhj
      do k=1,levs							! hmhj
         do i=1,len_trie_ls						! hmhj
            trie_ls(i,1,P_x  +k-1)=					! hmhj
     x      trie_ls(i,1,P_dim+k-1)+cons2*deltim*trie_ls(i,1,P_x+k-1) 	! hmhj
            trie_ls(i,2,P_x  +k-1)=					! hmhj
     x      trie_ls(i,2,P_dim+k-1)+cons2*deltim*trie_ls(i,2,P_x+k-1) 	! hmhj
            trie_ls(i,1,P_y  +k-1)=					! hmhj
     x      trie_ls(i,1,P_tem+k-1)+cons2*deltim*trie_ls(i,1,P_y+k-1)	! hmhj
            trie_ls(i,2,P_y  +k-1)=					! hmhj
     x      trie_ls(i,2,P_tem+k-1)+cons2*deltim*trie_ls(i,2,P_y+k-1)	! hmhj
         enddo								! hmhj
         do i=1,len_trio_ls						! hmhj
            trio_ls(i,1,P_x  +k-1)=					! hmhj
     x      trio_ls(i,1,P_dim+k-1)+cons2*deltim*trio_ls(i,1,P_x+k-1)	! hmhj
            trio_ls(i,2,P_x  +k-1)=					! hmhj
     x      trio_ls(i,2,P_dim+k-1)+cons2*deltim*trio_ls(i,2,P_x+k-1)	! hmhj
            trio_ls(i,1,P_y  +k-1)=					! hmhj
     x      trio_ls(i,1,P_tem+k-1)+cons2*deltim*trio_ls(i,1,P_y+k-1)	! hmhj
            trio_ls(i,2,P_y  +k-1)=					! hmhj
     x      trio_ls(i,2,P_tem+k-1)+cons2*deltim*trio_ls(i,2,P_y+k-1)	! hmhj
         enddo								! hmhj
      enddo								! hmhj
!     
         do i=1,len_trie_ls						! hmhj
            trie_ls(i,1,P_zq)=						! hmhj
     x      trie_ls(i,1,P_qm)+cons2*deltim*trie_ls(i,1,P_zq)  		! hmhj
            trie_ls(i,2,P_zq)=						! hmhj
     x      trie_ls(i,2,P_qm)+cons2*deltim*trie_ls(i,2,P_zq) 		! hmhj
         enddo								! hmhj
         do i=1,len_trio_ls						! hmhj
            trio_ls(i,1,P_zq)=						! hmhj
     x      trio_ls(i,1,P_qm)+cons2*deltim*trio_ls(i,1,P_zq)		! hmhj
            trio_ls(i,2,P_zq)=						! hmhj
     x      trio_ls(i,2,P_qm)+cons2*deltim*trio_ls(i,2,P_zq) 		! hmhj
         enddo								! hmhj
      endif	! explicit						! hmhj
! hmhj -------   end of explicit -------
!
!sela call mpi_barrier (mpi_comm_world,ierr)
!
      spdmax_nodem=spdmax_node
      call mpi_gather(spdmax_nodem,levs,MPI_R_MPI,
     x                spdmax_nodesm,levs,MPI_R_MPI,
     x                0,MC_COMP,ierr)
      spdmax_nodes=spdmax_nodesm
!
!sela call mpi_barrier (mpi_comm_world,ierr)
!
      if ( me .eq. 0 ) then
!
         do k=1,levs
            spdmax(k) = cons0     !constant
            do node=1,nodes
               spdmax(k)=max(spdmax(k),spdmax_nodes(k,node))
            enddo
            spdmax(k)=sqrt(spdmax(k))
         enddo
!
!        print*,'in gloopa for spdmx at kdt=',kdt
!        print 100,(spdmax(k),k=1,levs)
100      format(' spdmx(01:10)=',10f5.0,:/' spdmx(11:20)=',10f5.0,
     x        :/' spdmx(21:30)=',10f5.0,:/' spdmx(31:40)=',10f5.0,
     x        :/' spdmx(41:50)=',10f5.0,:/' spdmx(51:60)=',10f5.0,
     x        :/' spdmx(61:70)=',10f5.0,:/' spdmx(71:80)=',10f5.0,
     x        :/' spdmx(81:90)=',10f5.0,:/' spdmx(91:00)=',10f5.0)
!
      endif
!
      call mpi_bcast(spdmax,levs,mpi_real8,
     x               0,MC_COMP,ierr)
!
      deallocate ( spdlat )
!
      if(zfirst) then
        zfirst=.false.
      endif
!
      return
      end

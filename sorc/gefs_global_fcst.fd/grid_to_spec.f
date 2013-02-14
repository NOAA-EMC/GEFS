      subroutine grid_to_spect
     &    (zsg,psg,uug,vvg,teg,rqg,
     x     trie_zs,trio_zs,trie_ps,trio_ps,
     x     trie_di,trio_di,trie_ze,trio_ze,
     x     trie_te,trio_te,trie_rq,trio_rq,
     x     ls_node,ls_nodes,max_ls_nodes,
     x     lats_nodes_r,global_lats_r,lonsperlar,
     x     epse,epso,snnp1ev,snnp1od,plnew_r,plnow_r)
!!
!! hmhj - this routine do spectral to grid transform in model partial reduced grid
!!
      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use coordinate_def 
      use tracer_const
      use physcons, fv => con_fvirt, rerth => con_rerth,
     &              grav => con_g
      implicit none
!!
      real(kind=kind_rad) zsg(lonr,lats_node_r)
      real(kind=kind_rad) psg(lonr,lats_node_r)
      real(kind=kind_rad) uug(lonr,lats_node_r,levs)
      real(kind=kind_rad) vvg(lonr,lats_node_r,levs)
      real(kind=kind_rad) teg(lonr,lats_node_r,levs)
      real(kind=kind_rad) rqg(lonr,lats_node_r,levh)
!
      real(kind=kind_evod) trie_zs(len_trie_ls,2)
      real(kind=kind_evod) trio_zs(len_trio_ls,2)
      real(kind=kind_evod) trie_ps(len_trie_ls,2)
      real(kind=kind_evod) trio_ps(len_trio_ls,2)
      real(kind=kind_evod) trie_di(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_di(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_ze(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_ze(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_te(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_te(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_rq(len_trie_ls,2,levh)
      real(kind=kind_evod) trio_rq(len_trio_ls,2,levh)
!
!!!!  integer, parameter :: lota = 3*levs+1*levh+1 
!
      real(kind=kind_evod) trie_ls(len_trie_ls,2,lota+1)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,lota+1)
!!
      real(kind=kind_evod) for_gr_r_1(lonrx*(lota+1),lats_dim_r)
      real(kind=kind_evod) for_gr_r_2(lonrx*(lota+1),lats_dim_r)
!
      integer              ls_node(ls_dim,3)
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
      integer              lats_nodes_r(nodes)
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
      integer dimg
!
      real(kind=kind_evod)  epse(len_trie_ls)
      real(kind=kind_evod)  epso(len_trio_ls)
!
      real(kind=kind_evod)  snnp1ev(len_trie_ls)
      real(kind=kind_evod)  snnp1od(len_trio_ls)
!
      real(kind=kind_evod)   plnew_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnow_r(len_trio_ls,latr2)
!
      real(kind=kind_evod)   tfac(lonr,levs), sumq(lonr,levs), rcs2
!
      integer              i,j,k,kaz,kap,kar,kat,kau,kav, nn, nnl
      integer              l,lan,lat,lotx
      integer              lon_dim,lons_lat
!
      integer              locl,n
      integer              indev
      integer              indod
      integer              indev1,indev2
      integer              indod1,indod2
      INTEGER              INDLSEV,JBASEV
      INTEGER              INDLSOD,JBASOD
!
      logical 	lslag
!

      real(kind=kind_evod), parameter :: one=1.0, pa2cb=0.001
!
!timers______________________________________________________---
      real*8 rtc ,timer1,timer2
!timers______________________________________________________---
!
!
      real(kind=kind_evod), parameter :: cons_0=0.0,   cons_24=24.0
     &,                                  cons_99=99.0, cons_1p0d9=1.0E9
     &,                                  qmin=1.0e-10
!
      real(kind=kind_evod) ga2, tem
!
      INCLUDE 'function2'
!
!--------------------------------------------------------------------
!
      lslag   = .false.
      lotx    = lota + 1

      kau     = 0*levs + 0*levh + 1
      kav     = 1*levs + 0*levh + 1
      kat     = 2*levs + 0*levh + 1
      kar     = 3*levs + 0*levh + 1
      kap     = 3*levs + 1*levh + 1
      kaz     = 3*levs + 1*levh + 2
!
!--------------------------------------------------------------------
      do lan=1,lats_node_r
        lon_dim = lon_dims_r(lan)
        lat = global_lats_r(ipt_lats_node_r-1+lan)
        lons_lat = lonsperlar(lat)
        rcs2     = rcs2_r(min(lat,latg-lat+1))
!
        if (thermodyn_id == 3) then
          do k=1,levs
            do i=1,lons_lat
              tfac(i,k) = 0.0
              sumq(i,k) = 0.0
            enddo
          enddo
          do nn=1,ntrac
            nnl = (nn-1)*levs
            if (cpi(nn) .ne. 0.0) then
              do k=1,levs
                do i=1,lons_lat
                  sumq(i,k) = sumq(i,k) + rqg(i,lan,nnl+k)
                  tfac(i,k) = tfac(i,k) + cpi(nn)*rqg(i,lan,nnl+k)
                enddo
              enddo
            endif
          enddo
          do k=1,levs
            do i=1,lons_lat
              tfac(i,k) = (one-sumq(i,k))*cpi(0) + tfac(i,k)
            enddo
          enddo
        else
          do k=1,levs
            do i=1,lons_lat
              tfac(i,k) = one + fv*max(rqg(i,lan,k),qmin) 
            enddo
          enddo
        endif
        do k=1,levs
!       print *,' k=',k,' uug=',uug(1,lan,k),' vvg=',vvg(1,lan,k)
!    &,' rqg=',rqg(1,lan,k),rqg(1,lan,k+levs),rqg(1,lan,k+levs+levs)
          do i=1,lons_lat
            for_gr_r_2(i+(kau+k-2)*lon_dim,lan) = uug(i,lan,k)
     &                                          * coslat_r(lat) * rcs2
            for_gr_r_2(i+(kav+k-2)*lon_dim,lan) = vvg(i,lan,k)
     &                                          * coslat_r(lat) * rcs2
            for_gr_r_2(i+(kat+k-2)*lon_dim,lan) = teg(i,lan,k)
     &                                          * tfac(i,k)
          enddo
        enddo
        do k=1,levh
          do i=1,lons_lat
            for_gr_r_2(i+(kar+k-2)*lon_dim,lan)=rqg(i,lan,k)
          enddo
        enddo
        if (gen_coord_hybrid) then   ! Ps is the prognostic variable
          do i=1,lons_lat
            for_gr_r_2(i+(kaz-1)*lon_dim,lan) = zsg(i,lan)
            for_gr_r_2(i+(kap-1)*lon_dim,lan) = psg(i,lan)*pa2cb
          enddo
        else                         ! ln(Ps) is the prognostic variable
          do i=1,lons_lat
            for_gr_r_2(i+(kaz-1)*lon_dim,lan) = zsg(i,lan)
            for_gr_r_2(i+(kap-1)*lon_dim,lan) = log(psg(i,lan)*pa2cb)
          enddo
        endif
      enddo
!
      do lan=1,lats_node_r
!
         lon_dim = lon_dims_r(lan)
!
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)

         call grid2four_thread(for_gr_r_2(1,lan),for_gr_r_1(1,lan),
     &                  lon_dim,lons_lat,lonrx,lotx)
!
      enddo
!
      dimg=0
      call four2fln(lslag,lats_dim_r,lotx,lotx,for_gr_r_1,
     x              ls_nodes,max_ls_nodes,
     x              lats_nodes_r,global_lats_r,lon_dims_r,
     x              lats_node_r,ipt_lats_node_r,dimg,
     x              lat1s_r,lonrx,latr,latr2,
     x              trie_ls(1,1,1), trio_ls(1,1,1),
     x              plnew_r, plnow_r,
     x              ls_node)
!
!
!$OMP parallel do shared(trie_ls,trio_ls)
!$OMP+shared(trie_di,trio_di,trie_ze,trio_ze,trie_te,trio_te)
!$OMP+shared(kau,kav,kat,epse,epso,snnp1ev,snnp1od,ls_node)
!$OMP+private(k)
      do k=1,levs
         call uveodz(trie_ls(1,1,kau+k-1), trio_ls(1,1,kav+k-1),
     x               trie_di(1,1,k),       trio_ze(1,1,k),
     x               epse,epso,ls_node)
!
         call uvoedz(trio_ls(1,1,kau+k-1), trie_ls(1,1,kav+k-1),
     x               trio_di(1,1,k),       trie_ze(1,1,k),
     x               epse,epso,ls_node)
        trie_te(:,:,k)=trie_ls(:,:,kat+k-1)
        trio_te(:,:,k)=trio_ls(:,:,kat+k-1)
      enddo
      do k=1,levh
        trie_rq(:,:,k)=trie_ls(:,:,kar+k-1)
        trio_rq(:,:,k)=trio_ls(:,:,kar+k-1)
      enddo
      trie_zs(:,:)=trie_ls(:,:,kaz)
      trio_zs(:,:)=trio_ls(:,:,kaz)
      trie_ps(:,:)=trie_ls(:,:,kap)
      trio_ps(:,:)=trio_ls(:,:,kap)
!
      GA2 = grav / (RERTH*RERTH)
      DO LOCL=1,LS_MAX_NODE
             L=ls_node(locl,1)
        jbasev=ls_node(locl,2)
        indev1 = indlsev(L,L)
        if (mod(L,2).eq.mod(jcap+1,2)) then
          indev2 = indlsev(jcap+1,L)
        else
          indev2 = indlsev(jcap  ,L)
        endif
        do indev = indev1 , indev2
          tem = GA2 * SNNP1EV(INDEV)
!     print *,' indev=',indev,' tem=',tem,' trie=',trie_zs(indev,2)
          trie_zs(INDEV,1) = trie_zs(INDEV,1) * tem
          trie_zs(INDEV,2) = trie_zs(INDEV,2) * tem
        END DO
      END DO
      DO LOCL=1,LS_MAX_NODE
             L=ls_node(locl,1)
        jbasod=ls_node(locl,3)
        indod1 = indlsod(L+1,L)
        if (mod(L,2).eq.mod(jcap+1,2)) then
          indod2 = indlsod(jcap  ,L)
        else
          indod2 = indlsod(jcap+1,L)
        endif
        do indod = indod1 , indod2
          tem = GA2 * SNNP1OD(INDOD)
!     print *,' indod=',indod,' tem=',tem,' trio=',trio_zs(indev,2)
          trio_zs(INDOD,1) = trio_zs(INDOD,1) * tem
          trio_zs(INDOD,2) = trio_zs(INDOD,2) * tem
        END DO
      END DO
!
!     print *,' exit grid_to_spect '
!!
      return
      end

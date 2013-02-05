      subroutine spect_tv_enthalpy
     x    (direction,
     x     trie_te,trio_te,trie_rq,trio_rq,
     x     ls_node,ls_nodes,max_ls_nodes,
     x     lats_nodes_r,global_lats_r,lonsperlar,
     x     plnev_r,plnod_r,plnew_r,plnow_r)
!!
!! hmhj - this routine is to convert between virtual temperature and enthalpy
!!        direction=1	convert from virtual temperature to enthalpy
!!	  direction=-1	convert from enthalpy to virtual temperature
!!
      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use coordinate_def 
      use tracer_const
      use physcons, fv => con_fvirt, rk => con_rocp, rerth => con_rerth,
     &              grav => con_g
      implicit none
!!
      real(kind=kind_evod) trie_te(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_te(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_rq(len_trie_ls,2,levh)
      real(kind=kind_evod) trio_rq(len_trio_ls,2,levh)
!
!     integer, parameter  ::   lotx = levs+levh 

      real(kind=kind_evod) trie_ls(len_trie_ls,2,levs+levh)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,levs+levh)
      real(kind=kind_evod) for_gr_r_1(lonrx*(levs+levh),lats_dim_r)
      real(kind=kind_evod) for_gr_r_2(lonrx*(levs+levh),lats_dim_r)
!
cc
      real(kind=kind_rad) teg(lonr,levs)
      real(kind=kind_rad) rqg(lonr,levh)
cc
!
      integer              ls_node(ls_dim,3)
!
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
      integer              lats_nodes_r(nodes)
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
      integer dimg
cc
      real(kind=kind_evod)  epsedn(len_trie_ls)
      real(kind=kind_evod)  epsodn(len_trio_ls)
cc
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
cc
      real(kind=kind_evod)   plnev_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnod_r(len_trio_ls,latr2)
!
      real(kind=kind_evod)   plnew_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnow_r(len_trio_ls,latr2)
!
!
      real(kind=kind_evod)   to_enthalpy(lonr,levs)
      real(kind=kind_evod)   to_virttemp(lonr,levs)
      real(kind=kind_evod)   sumq(lonr,levs)
cc
      integer              i,j,k,kar,kat,kk,nn,nnl
      integer              l,lan,lat,lotx,direction
      integer              lon_dim,lons_lat
!
      logical   lslag
!
cc
cc--------------------------------------------------------------------
cc
      lslag   = .false.

      kat     =1
      kar     =kat+levs
cc
cc--------------------------------------------------------------------
cc
      do k=1,levs
        trie_ls(:,:,k) = trie_te(:,:,k)
        trio_ls(:,:,k) = trio_te(:,:,k)
      enddo
      do k=1,levh
        trie_ls(:,:,levs+k) = trie_rq(:,:,k)
        trio_ls(:,:,levs+k) = trio_rq(:,:,k)
      enddo
!
      dimg=0
cc
      lotx    =levs+levh
      call sumflna(trie_ls,trio_ls,
     x            lat1s_r,
     x            plnev_r,plnod_r,
     x            lotx,ls_node,latr2,
     x            lslag,lats_dim_r,lotx,for_gr_r_1,
     x            ls_nodes,max_ls_nodes,
     x            lats_nodes_r,global_lats_r,
     x            lats_node_r,ipt_lats_node_r,lon_dims_r,dimg,
     x            lonsperlar,lonrx,latr)
c
      do lan=1,lats_node_r
cc
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lon_dim = lon_dims_r(lan)
         lons_lat = lonsperlar(lat)
cc
         call four2grid_thread(for_gr_r_1(1,lan),for_gr_r_2(1,lan),
     &                  lon_dim,lons_lat,lonrx,lotx,lan,me)
!
      enddo   !lan
cc
! -------------------------------------------------------------------
! --------------- convert between virttemp and enthalpy -------------
      do lan=1,lats_node_r

        lon_dim  = lon_dims_r(lan)
        lat      = global_lats_r(ipt_lats_node_r-1+lan)
        lons_lat = lonsperlar(lat)
!
        do k=1,levh
          do i=1,lons_lat
            rqg(i,k) = for_gr_r_2(i+(kar+k-2)*lon_dim,lan)
          enddo
        enddo
!
        do k=1,levs
          do i=1,lons_lat
            to_enthalpy(i,k) = 0.0
            sumq(i,k) = 0.0
          enddo
        enddo
        do nn=1,ntrac
          nnl = (nn-1)*levs
          if (cpi(nn) .ne. 0.0) then
            do k=1,levs
              do i=1,lons_lat
                sumq(i,k) = sumq(i,k) + rqg(i,nnl+k)
                to_enthalpy(i,k) = to_enthalpy(i,k)         
     &                            + cpi(nn)*rqg(i,nnl+k)
              enddo
            enddo
          endif
        enddo
        do k=1,levs
          do i=1,lons_lat
            to_enthalpy(i,k) = (1.0-sumq(i,k))*cpi(0) + to_enthalpy(i,k)
          enddo
        enddo
!
        do k=1,levs
          do i=1,lons_lat
            to_virttemp(i,k) = 1.0 + fv*rqg(i,k)
          enddo
        enddo

        do k=1,levs
          do i=1,lons_lat
            teg(i,k) = for_gr_r_2(i+(kat+k-2)*lon_dim,lan)
          enddo
        enddo
        if( direction.eq.1 ) then 	! virttemp to enthalpy
          do k=1,levs
            do i=1,lons_lat
              teg(i,k) = teg(i,k) / to_virttemp(i,k)
              teg(i,k) = teg(i,k) * to_enthalpy(i,k)
            enddo
          enddo
        endif
        if( direction.eq.-1 ) then 	! enthalpy to virttemp 
          do k=1,levs
            do i=1,lons_lat
              teg(i,k) = teg(i,k) / to_enthalpy(i,k)
              teg(i,k) = teg(i,k) * to_virttemp(i,k)
            enddo
          enddo
        endif
        do k=1,levs
          do i=1,lons_lat
            for_gr_r_2(i+(kat+k-2)*lon_dim,lan) = teg(i,k)
          enddo
        enddo
!
      enddo


cc ------------------------ transform back to coefficient ----
      do lan=1,lats_node_r
!
         lon_dim = lon_dims_r(lan)
!
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)

         call grid2four_thread(for_gr_r_2(1,lan),for_gr_r_1(1,lan),
     &                  lon_dim,lons_lat,lonrx,levs)
!
      enddo
!
      dimg=0
      call four2fln(lslag,lats_dim_r,lotx,levs,for_gr_r_1,
     x              ls_nodes,max_ls_nodes,
     x              lats_nodes_r,global_lats_r,lon_dims_r,
     x              lats_node_r,ipt_lats_node_r,dimg,
     x              lat1s_r,lonrx,latr,latr2,
     x              trie_ls(1,1,1), trio_ls(1,1,1),
     x              plnew_r, plnow_r,
     x              ls_node)
!
!
      do k=1,levs
        trie_te(:,:,k)=trie_ls(:,:,k)
        trio_te(:,:,k)=trio_ls(:,:,k)
      enddo
!!
      return
      end

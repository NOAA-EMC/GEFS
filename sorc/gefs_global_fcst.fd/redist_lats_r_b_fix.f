       subroutine redist_lats_r_b(kdt,global_times_r,global_times_b,
     x                  lats_nodes_r,global_lats_r,
     x                  lats_nodes_r_old,global_lats_r_old,
     x                  lonsperlar,ifshuff,iprint)
cc

      use resol_def
      use layout1
      use mpi_def
      implicit none
cc
!!
      integer              i,j,k,l,lat,lev
cc
      integer               lats_nodes_r(nodes)
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
cc
      integer              iprint,locl
cc
cc
cc
      integer kdt
c timings
      real(kind=kind_evod) global_times_r(latr,nodes)
      real(kind=kind_evod) global_time_r(latr)
c
      integer               lats_nodes_r_old(nodes)
      integer              global_lats_r_old(latr)
      logical ifshuff
c
      real(kind=kind_evod), allocatable :: timesum_r_b(:)
cmy
      real(kind=kind_evod) global_times_b(latr,nodes)
      real(kind=kind_evod) global_time_b(latr)
      real shuffle_overhead
c loadbal
      integer node,gl_lats_index
      integer global_time_sort_index(latr)
cmy save timesum_a for each kdt
c
      save timesum_r_b
      allocate (timesum_r_b(latr))
c
      if (iprint .eq. 1) print*,' redist_lats_r_b '
c
         if (kdt .eq. 1) return
 
         if (kdt .eq. 2) timesum_r_b = 0.0
            global_time_r = -99.99
cmy gather lat timings for load balancing
         if (iprint .eq. 1) then
            print*, 'before gather global lats_r  kdt = ',kdt
 
        do j=1,latr
           write(300+kdt,800) j,global_lats_r(j)
        enddo
           close(300+kdt)
          endif
800   format(i4,2x,i5)
 
c
       if (iprint .eq. 1) print*, ' global_times_r = ',global_times_r
cmy
         if (maxval(global_times_r) .eq. 0.0) then
          if (iprint .eq. 1) print*, 'skipping adding r times - zero '
            goto 200
         endif
c
         call gather_times_r(lats_nodes_r,global_lats_r,
     .                     global_times_r,global_time_r)
         if (iprint .eq. 1)
     .       print*, ' write after gather global lats_r kdt = ',kdt
 
         do j=1,latr
            timesum_r_b(j) = timesum_r_b(j) + global_time_r(j)
         enddo
c
 200  continue
 
         global_time_b = -99.99
         if (iprint .eq. 1) print*, ' global_times_b = ',global_times_b
         call gather_times_r(lats_nodes_r,global_lats_r,
     .                     global_times_b,global_time_b)
         if (iprint .eq. 1)
     .     print*, ' write after gather global lats_b kdt = ',kdt
 
         do j=1,latr
            timesum_r_b(j) = timesum_r_b(j) + global_time_b(j)
         enddo
c
        if (iprint .eq. 1) then
        do j=1,latr
           write(500+kdt,777) j,global_time_r(j),
     .                        global_time_b(j),timesum_r_b(j)
        enddo
           close(500+kdt)
        endif
777   format(i4,2x,e13.6,5x,e13.6,5x,e13.6)
 
c
cmy completed assembly of total times per lat
cmy return if no redistribution required
 
c$$$      if (mod(kdt,10) .ne. 0) return
c
c
cmy sort lats and reshuffle
cmy sort the lat times in descending order
c
      call sortrx(latr,-timesum_r_b,global_time_sort_index)
      if (iprint .eq. 1)
     .  print*,' dotstep kdt,after sortrx timesum_r_b index = ',
     .        kdt,global_time_sort_index
c
      gl_lats_index = 0
c
      global_lats_r_old = global_lats_r
      lats_nodes_r_old = lats_nodes_r
c
      do node=1,nodes
         call get_lats_node_r( node-1, global_lats_r,
     x                     lats_nodes_r(node),
     .                     gl_lats_index,global_time_sort_index,iprint)
       if (me+1 .eq. node .and. iprint .eq. 1)
     .  print*,' node, lats_nodes_r = ',node,lats_nodes_r(node)
      enddo
        if (iprint .eq. 1) then
        do j=1,latr
           write(100+kdt,800) j,global_lats_r(j)
        enddo
           close(100+kdt)
        endif
c
       if (iprint .eq. 1)
     . print*,' after shuff global lats_r = ',global_lats_r
cmy check whether to reshuffle considering overhead
c
       shuffle_overhead = 0.0
       call if_shuff(global_lats_r_old,lats_nodes_r_old,
     .         global_lats_r,lats_nodes_r,timesum_r_b,kdt,ifshuff,
     .         shuffle_overhead)
 
cmy hardwire to true
         ifshuff = .true.
       if (.not. ifshuff) then
          global_lats_r = global_lats_r_old
          lats_nodes_r  = lats_nodes_r_old
       endif
c
cc
cmy needed for redistributed number of lats for each mpi process
cmy compared to original distribution in getcon
c$$$      lats_dim_a=0
c$$$      lats_dim_r=0
c$$$      do node=1,nodes
c$$$         lats_dim_a = max(lats_dim_a,lats_nodes_a(node))
c$$$         lats_dim_r = max(lats_dim_r,lats_nodes_r(node))
c$$$      enddo
c$$$cc
c$$$      lats_dim_ext=0
c$$$      do node=1,nodes
c$$$             lats_dim_ext =
c$$$     .   max(lats_dim_ext, lats_nodes_ext(node), lats_nodes_r(node))
c$$$      enddo
c$$$cc
c$$$      lats_node_a = lats_nodes_a(me+1)
c$$$      lats_node_r = lats_nodes_r(me+1)
c$$$      lats_node_ext = lats_nodes_ext(me+1)
c$$$!!
c$$$      lats_node_r_max=0
c$$$      do i=1,nodes
c$$$        lats_node_r_max=max(lats_node_r_max,lats_nodes_r(i))
c$$$      enddo
c$$$c
c$$$      lats_node_a_max=0
c$$$      do i=1,nodes
c$$$        lats_node_a_max=max(lats_node_a_max,lats_nodes_a(i))
c$$$      enddo
c$$$cc
c$$$      ipt_lats_node_a=1
c$$$      ipt_lats_node_r=1
c$$$      ipt_lats_node_ext=1
c$$$      if ( me .gt. 0 ) then
c$$$         do node=1,me
c$$$            ipt_lats_node_a = ipt_lats_node_a + lats_nodes_a(node)
c$$$            ipt_lats_node_r = ipt_lats_node_r + lats_nodes_r(node)
c$$$            ipt_lats_node_ext = ipt_lats_node_ext + lats_nodes_ext(node)
c$$$         enddo
c$$$      endif
cc
cmy needs analysis for redistribution
cmy !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c$$$      do j=1,latg2
c$$$        sinlat_a(j) = cos(colrad_a(j))
c$$$      enddo
c$$$cc
c$$$cc
c$$$      do j=1,latr
c$$$        if (j.le.latr2) then
c$$$          sinlat_r(j) = cos(colrad_r(j))
c$$$        else
c$$$          sinlat_r(j) = -cos(colrad_r(j))
c$$$        endif
c$$$        coslat_r(j) = sqrt(1. E 0 -sinlat_r(j)*sinlat_r(j))
c$$$      enddo
cmy !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cc
cc
cc
cc
      do j=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+j)
         if ( lonsperlar(lat) .eq. lonf ) then
            lon_dims_r(j) = lonfx
         else
            lon_dims_r(j) = lonsperlar(lat) + 2
         endif
      enddo
cc
      timesum_r_b = 0.0
      return
      end
 

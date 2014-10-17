      subroutine redist_lats_a(kdt,global_times_a,
     x                  lats_nodes_a,global_lats_a,
     x                  lonsperlat,
     x                  lats_nodes_ext,global_lats_ext,iprint)
 
cc
      use resol_def
      use layout1
      use mpi_def
      implicit none
cc
!!
      integer              i,j,k,l,lat,lev
cc
      integer               lats_nodes_a(nodes)
      integer              global_lats_a(latg)
      integer                 lonsperlat(latg)
cc
      integer                lats_nodes_ext(nodes)
      integer        global_lats_ext(latg+2*jintmx+2*nypt*(nodes-1))
cc
      integer              iprint,locl
cc
cc
cc
      integer kdt
c timings
      real(kind=kind_evod) global_times_a(latg,nodes)
      real(kind=kind_evod) global_time_a(latg)
      real(kind=kind_evod), allocatable :: timesum_a(:)
c loadbal
      integer node,nodesio,gl_lats_index
      integer global_time_sort_index(latg)
cmy save timesum_a for each kdt
c
      save timesum_a
!
      allocate (timesum_a(latg))
c
c$$$      print 100, jcap, levs
c$$$100   format (1h0,'redist_lats ',i3,i3)
c
         if (kdt .eq. 1) return
 
cmy gather lat timings for load balancing
         global_time_a = -99.99
         if (iprint .eq. 1) then
           print*, 'before gather global lats_a kdt = ',kdt
 
        do j=1,latg
           write(800+kdt,800) j,global_lats_a(j)
        enddo
           close(800+kdt)
        endif
800   format(i4,2x,i5)
 
         if (iprint .eq. 1) print*, ' global_times_a = ',global_times_a
         call gather_times_a(lats_nodes_a,global_lats_a,
     .                     global_times_a,global_time_a)
         if (iprint .eq. 1)
     .      print*, ' write after gather global lats_a kdt = ',kdt
 
         if (kdt .eq. 2) timesum_a = 0.0
         do j=1,latg
            timesum_a(j) = timesum_a(j) + global_time_a(j)
         enddo
c
        if (iprint .eq. 1) then
        do j=1,latg
           write(200+kdt,777) j,global_time_a(j),timesum_a(j)
        enddo
           close(200+kdt)
        endif
777   format(i4,2x,e13.6,5x,e13.6)
 
cmy completed assembly of total times per lat
cmy return if no redistribution required
 
      if (liope) then
         if (icolor.eq.2) then
           nodesio=1
         else
           nodesio=nodes
         endif
      else
         nodesio=nodes
      endif
c
cmy sort lats and reshuffle
cmy sort the lat times in descending order
c
      call sortrx(latg,-timesum_a,global_time_sort_index)
      if (iprint .eq. 1) print*,' dotstep kdt,after sortrx index = ',
     .        kdt,global_time_sort_index
c
      gl_lats_index = 0
c
      do node=1,nodesio
         call get_lats_node_a( node-1, global_lats_a,
     x                     lats_nodes_a(node),
     .                     gl_lats_index,global_time_sort_index,iprint)
       if (me+1 .eq. node .and. iprint .eq. 1)
     .  print*,' node, lats_nodes_a = ',node,lats_nodes_a(node)
      enddo
c
        if (iprint .eq. 1) then
        do j=1,latg
           write(900+kdt,800) j,global_lats_a(j)
        enddo
           close(900+kdt)
        endif
c
       write(6,*) ' after redist_lats_a new global_lats_a = '
       write(6,830)     global_lats_a
 830   format(15(i3,1x))
 
       if (iprint .eq. 1)
     .  print*,' after shuff global lats_a = ',global_lats_a
 
      call setlats_a_ext(lats_nodes_a,lats_nodes_ext,global_lats_a,
     &               global_lats_ext,iprint,lonsperlat)
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
      do j=1,lats_node_a
         lat = global_lats_a(ipt_lats_node_a-1+j)
         if ( lonsperlat(lat) .eq. lonf ) then
            lon_dims_a(j) = lonfx
         else
            lon_dims_a(j) = lonsperlat(lat) + 2
         endif
      enddo
cc
cc
      do j=1,lats_node_ext
         lat = global_lats_ext(ipt_lats_node_ext-1+j)
         if ( lonsperlat(lat) .eq. lonf ) then
            lon_dims_ext(j) = lonfx
         else
            lon_dims_ext(j) = lonsperlat(lat) + 1+2*nxpt+1
         endif
      enddo
cmy reset timesum to zero
      timesum_a = 0.0
      return
      end

      subroutine get_lats_node_r(me_fake,global_lats_r,
     .          lats_nodes_r_fake,gl_lats_index,
     .          global_time_sort_index,iprint)
cc
      use resol_def
      use layout1
      use mpi_def
      implicit none
cc
      integer   gl_lats_index,gl_start
      integer   me_fake
      integer   global_lats_r(latr)
      integer   lats_nodes_r_fake
      integer   iprint
cc
      integer   ijk
      integer   jptlats
      integer   lat
      integer   node,nodesio
      integer global_time_sort_index(latr)
      integer nodes_tmp
cc
c
      if (liope) then
         if (icolor.eq.2) then
           nodesio=1
         else
           nodesio=nodes
         endif
      else
         nodesio=nodes
      endif
!!
cc
      lat = 1
      nodes_tmp = nodes
      if (liope .and. icolor .eq. 2) nodes_tmp = nodes -1
 
!sela print*,' in get_lats_r me nodes_tmp = ',me,nodes_tmp
      gl_start = gl_lats_index
cc.............................................
      do ijk=1,latr
cc
         do node=1,nodes_tmp
            if (node.eq.me_fake+1) then
               gl_lats_index=gl_lats_index+1
            global_lats_r(gl_lats_index) = global_time_sort_index(lat)
            endif
            lat = lat + 1
            if (lat .gt. latr) go to 200
         enddo
cc
         do node=nodes_tmp,1,-1
            if (node.eq.me_fake+1) then
               gl_lats_index=gl_lats_index+1
            global_lats_r(gl_lats_index) = global_time_sort_index(lat)
            endif
            lat = lat + 1
            if (lat .gt. latr) go to 200
         enddo
cc
      enddo
cc.............................................
cc
  200 continue
cc
cc.............................................
cc
      if (liope .and. icolor .eq. 2) gl_start = 0
         do node=1,nodes_tmp
            if (node.eq.me_fake+1) then
               lats_nodes_r_fake=gl_lats_index-gl_start
c$$$               print*,' setting lats_nodes_r_fake = ',
c$$$     .         lats_nodes_r_fake
            endif
         enddo
 
      if(iprint.eq.1) print 220
  220 format ('completed loop 200 in  get_lats_r  ')
c
      if(iprint.eq.1)
     x   print*,'completed  get_lats_node, lats_nodes_r_fake=',
     x   lats_nodes_r_fake
cc
      return
      end

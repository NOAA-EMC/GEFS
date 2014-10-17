      subroutine shuffle_grid(grid_1,grid_2,lon,max_index,
     x           index_1,index_2,indexes_max_1,indexes_max_2)
cc
      USE MACHINE , ONLY : KIND_RAD,KIND_PHYS
cc
      use resol_def
      use layout1
      use mpi_def
      implicit none
cc
      integer lon,max_index
      real(kind=kind_rad) grid_1(lon,max_index)
      real(kind=kind_rad) grid_2(lon,max_index)
cc
      integer index_1(max_index,nodes)
      integer index_2(max_index,nodes)
cc
      integer indexes_max_1(nodes)
      integer indexes_max_2(nodes)
cc
      integer icntr
      integer icnts
      integer ierr
      integer ii
      integer ind_1
      integer ind_2
      integer ireqr(max_index)
      integer ireqs(max_index)
      integer istatus
      integer node
cc
      icnts=0
cc
      do ind_1=1,indexes_max_1(me+1)
cc
         do ind_2=1,indexes_max_2(me+1)
            if ( index_1(ind_1,me+1) .eq. index_2(ind_2,me+1) ) then
               do ii=1,lon
                  grid_2(ii,ind_2) = grid_1(ii,ind_1)
               enddo
               go to 200
            endif
         enddo
cc
         do node=1,nodes
cc
            do ind_2=1,indexes_max_2(node)
               if ( index_1(ind_1,me+1) .eq. index_2(ind_2,node) ) then
                  icnts=icnts+1
                  call mpi_isend(grid_1(1,ind_1),
     x                           lon,
     x                           mpi_real8,
     x                           node-1,
     x                           100000*me+ind_1,
     x                           mc_comp,
     x                           ireqs(icnts),
     x                           ierr)
                  go to 200
               endif
            enddo
cc
         enddo
cc
  200 continue
      enddo
cc
      icntr=0
cc
      do ind_2=1,indexes_max_2(me+1)
cc
         do node=1,nodes
cc
            do ind_1=1,indexes_max_1(node)
               if ( index_1(ind_1,node) .eq. index_2(ind_2,me+1)
     x            .and. ( (node-1) .ne. me ) ) then
                  icntr=icntr+1
                  call mpi_irecv(grid_2(1,ind_2),
     x                           lon,
     x                           mpi_real8,
     x                           node-1,
     x                           100000*(node-1)+ind_1,
     x                           mc_comp,
     x                           ireqr(icntr),
     x                           ierr)
                  go to 400
               endif
            enddo
cc
         enddo
cc
  400 continue
      enddo
cc
      if ( icnts .gt. 0 ) then
           do ii=1,icnts
              call mpi_wait (ireqs(ii),istatus,ierr)
           enddo
      endif
cc
      if ( icntr .gt. 0 ) then
           do ii=1,icntr
              call mpi_wait (ireqr(ii),istatus,ierr)
           enddo
      endif
cc
      return
      end

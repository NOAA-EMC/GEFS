      MODULE TILES_cc
      
      USE ATM_cc

      implicit none

      integer process_rank_TILES,stat(MPI_STATUS_SIZE),
     >master_rank_TILES,ierr
      integer,dimension(:),allocatable:: ipt_lats_node_rs,lats_node_rs
      logical INIT /.true./, master

      SAVE

      END MODULE TILES_cc
c
c***********************************************************************
c
      subroutine INITIALIZE_TILING

      USE TILES_cc

      implicit none

      integer i
C

      IF (.not.INIT) RETURN

      COMP=process_rank_local.lt.TILES_nprocs

      master=component_master_rank_local.eq.process_rank_local

      IF (master .and. .not.COMP) THEN
        call GLOB_ABORT(1,'AM: INITIALIZE_TILING: COMP is .false. '//
     >  'for Coupling master process',1)
      END IF

      IF (COMP) THEN

        call MPI_COMM_SIZE(COMM_TILES,i,ierr)
        call GLOB_ABORT(ierr,'AM: MPI_COMM_SIZE(COMM_TILES,...) '//
     >  'failure in Coupling proc. in INITIALIZE_TILING',ierr)
        call GLOB_ABORT(i-TILES_nprocs,'AM: size of COMM_TILES.ne. '//
     >  'TILES_nprocs in INITIALIZE_TILING',i-TILES_nprocs)
        
        call MPI_COMM_RANK(COMM_TILES,process_rank_TILES,ierr)
        if (master) master_rank_TILES=process_rank_TILES
        call MPI_BCAST(master_rank_TILES,1,
     >  MPI_INTEGER,master_rank_TILES,COMM_TILES,ierr)

        allocate(ipt_lats_node_rs(0:TILES_nprocs-1))
        call MPI_GATHER(ipt_lats_node_r,1,MPI_INTEGER,
     >  ipt_lats_node_rs,1,MPI_INTEGER,
     >  master_rank_TILES,COMM_TILES,ierr)
        call GLOB_ABORT(ierr,
     >  'AM: MPI_GATHER(ipt_lats_node_r,...) failure',ierr)

        allocate(lats_node_rs(0:TILES_nprocs-1))
        call MPI_GATHER(lats_node_r,1,MPI_INTEGER,
     >  lats_node_rs,1,MPI_INTEGER,
     >  master_rank_TILES,COMM_TILES,ierr)
        call GLOB_ABORT(ierr,
     >  'AM: MPI_GATHER(lats_node_r,...) failure',ierr)

      END IF

      INIT=.false.

      call ATM_ANNOUNCE('INITIALIZE_TILING: exiting',2)

      end
c
c***********************************************************************
c
      subroutine ASSEMBLE_cc(x,xl)

c***********************************************************************

      USE TILES_cc

      implicit none
!!
      real(kind=kind_REAL) x(lonr,latr)
      real (kind=kind_REAL) xl(lonr,lats_node_r)
      real(kind=kind_REAL) tmp(lonr,latr)
      integer ipt_lats_node_rl,nodesr
      integer lats_nodes_rl
      integer maxfld,nproct
      integer proc,j,lat,msgtag,nproc,i
      integer ifldu/0/
      save ifldu
      integer illen
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      call INITIALIZE_TILING

      if (.not.COMP) RETURN
!!
      maxfld=50
      ifldu=ifldu+1
!!
      nproct=TILES_nprocs

      IF (.not.master) THEN
c
c         Sending the data
c         ----------------
        msgtag=1000+(process_rank_TILES+1)*nproct*maxfld+ifldu
        CALL MPI_SEND(xl,lats_node_r*lonr,MPI_kind_REAL,
     >                  master_rank_TILES,
     &                  msgtag,COMM_TILES,ierr)
      ELSE
!!
        x = 0.0               ! added by Moorthi on 2005111700
        do j=1,lats_node_r
          lat=global_lats_r(ipt_lats_node_r-1+j)
          do i=1,lonr
            x(i,lat)=xl(i,j)
          enddo
        enddo
        do proc=0,nproct-1
          if (proc.eq.master_rank_TILES) CYCLE
          msgtag=1000+(proc+1)*nproct*maxfld+ifldu
          illen=lats_node_rs(proc)
          CALL MPI_RECV(tmp,illen*lonr,MPI_kind_REAL,proc,
     &                msgtag,COMM_TILES,stat,ierr)
          ipt_lats_node_rl=ipt_lats_node_rs(proc)
          lats_nodes_rl=illen
          do j=1,lats_nodes_rl
            lat=global_lats_r(ipt_lats_node_rl-1+j)
            do i=1,lonr
              x(i,lat)=tmp(i,j)
            enddo
          enddo
        enddo
!!
      ENDIF
!!
      return
      END
c
c***********************************************************************
c
      subroutine DISASSEMBLE_cc(x,xl)

c***********************************************************************

      USE TILES_cc

      implicit none

      real(kind=kind_REAL) x(lonr,latr),xl(lonr,lats_node_r)

      real(kind=kind_REAL) tmp(lonr,latr)
      integer ipt_lats_node_rl,nodesr
      integer lats_nodes_rl
      integer maxfld,nproct
      integer proc,j,lat,msgtag,nproc,i
      integer ifldu/0/
      save ifldu
      integer illen
c

      call INITIALIZE_TILING

      if (.not.COMP) RETURN
!!
      xl=0.
      maxfld=50
      ifldu=ifldu+1
!!
      nproct=TILES_nprocs

      IF (.not.master) THEN
c
c         Receiving the data
c         ----------------
        msgtag=1111+(process_rank_TILES+1)*nproct*maxfld+ifldu
        CALL MPI_RECV(xl,lats_node_r*lonr,MPI_kind_REAL,
     >                  master_rank_TILES,
     &                  msgtag,COMM_TILES,stat,ierr)
      ELSE
!!
        do j=1,lats_node_r
          lat=global_lats_r(ipt_lats_node_r-1+j)
          do i=1,lonr
            xl(i,j)=x(i,lat)
          enddo
        enddo
        do proc=0,nproct-1
          if (proc.eq.master_rank_TILES) CYCLE
          ipt_lats_node_rl=ipt_lats_node_rs(proc)
          illen=lats_node_rs(proc)
          lats_nodes_rl=illen
          do j=1,lats_nodes_rl
            lat=global_lats_r(ipt_lats_node_rl-1+j)
            do i=1,lonr
              tmp(i,j)=x(i,lat)
            enddo
          enddo
          msgtag=1111+(proc+1)*nproct*maxfld+ifldu
          CALL MPI_SEND(tmp,illen*lonr,MPI_kind_REAL,proc,
     &                msgtag,COMM_TILES,ierr)
        enddo
!!
      ENDIF
!!
      return
      end
c
c***********************************************************************
c
      subroutine interpred_cc(iord,kmsk,f,fi)
      USE ATM_cc
      implicit none
      integer,intent(in):: iord
      integer,intent(in):: kmsk(lonr,latd)
      real(kind=kind_REAL), intent(in):: f(lonr,latd)
      real(kind=kind_REAL), intent(out):: fi(lonr,latd)
      integer j,lons
C

      call interpred(iord,kmsk,f,fi,global_lats_r,lonsperlar)

      return
      end
c
c**********************************************************************
c
      subroutine uninterpred_cc(iord,kmsk,fi,f)
      USE ATM_cc
      implicit none
      integer,intent(in):: iord
      integer,intent(in):: kmsk(lonr,latd)
      real(kind=kind_REAL), intent(in):: fi(lonr,latd)
      real(kind=kind_REAL), intent(out):: f(lonr,latd)

      call uninterpred(iord,kmsk,f,fi,global_lats_r,lonsperlar)

      return
      end
c
c**********************************************************************
c

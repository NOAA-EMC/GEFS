      module mod_state
!
! hchuang  Add flx files output to wrtflx_a
! hchuang  Add sub wrt3gd and wrt3gd_hyb for gocart only g3d files output
! hchuang  g3d files control by LGGFS3D
!
!    New module to supply domain information to the GFS output routines
!    called by wrtout.
!
      use machine
      use resol_def
      use gfsio_module
      use gfsio_def
      implicit none
!
!     integer, parameter :: ngrids_sfcc=32+LSOIL*3
!     integer, parameter :: ngrids_sfcc=29+LSOIL*3  ! No CV, CVB, CVT!
!    &,                     ngrids_flx=66+30        ! additional fields (most related to land surface)
!
      real(kind=kind_io4), allocatable :: buff_mult_pieceg(:,:,:),
     1                                    buff_mult_piecesg(:,:,:,:)
!
      real(kind=kind_io4), allocatable :: buff_mult_piece(:,:,:),
     1                                    buff_mult_pieces(:,:,:,:)
      real(kind=kind_io4), allocatable :: buff_mult_piecef(:,:,:),
     1                                    buff_mult_piecesf(:,:,:,:)
      real(kind=kind_io4), allocatable :: buff_mult_pieceo(:,:,:),
     1                                    buff_mult_pieceso(:,:,:,:)
      real(kind=kind_io4), allocatable :: buff_mult_piecea(:,:,:),
     1                                    buff_mult_piecesa(:,:,:,:)
      integer , allocatable :: ivar_global(:),ivar_global_a(:,:)
     &,                        ivarg_global(:),ivarg_global_a(:,:)
      integer , allocatable :: maskss(:,:,:)
!
      integer ngrid ,ngrida,ngridg
      save ngrid,ngrida,buff_mult_piece,buff_mult_pieces,ivar_global
     &,    ngridg,buff_mult_pieceg,buff_mult_piecesg,ivarg_global
     &,    buff_mult_pieceo
      save maskss
      end module mod_state
      subroutine wrtout(phour,fhour,zhour,idate,
     &                  TRIE_LS,TRIO_LS,
     x                  sl,si,
     x                  ls_node,ls_nodes,max_ls_nodes,
     &                  sfc_fld, flx_fld, nsst_fld,
     &                  fluxr,pdryini,
     &                  lats_nodes_r,global_lats_r,lonsperlar,nblck,
     &                  colat1,cfhour1,pl_coeff,
     &                  epsedn,epsodn,snnp1ev,snnp1od,plnev_r,plnod_r,
     &                  plnew_r,plnow_r,sigf,sfcf,flxf,wrt_g3d)
!!
      use resol_def
      use layout1
      use sig_io
      use namelist_def
      use mpi_def
      use sigio_module
      use sigio_r_module
      use nsstio_module
      use Sfc_Flx_ESMFMod
      use Nsstm_ESMFMod
      implicit none
!!
      TYPE(Sfc_Var_Data)        :: sfc_fld
      TYPE(Flx_Var_Data)        :: flx_fld
      TYPE(Nsst_Var_Data)       :: nsst_fld
      CHARACTER(16)             :: CFHOUR1    ! for ESMF Export State Creation
      integer ixgr, pl_coeff
      real(kind=kind_evod) phour,fhour,zhour
!     real(kind=kind_evod) phour,fhour,zhour, xgf
!!
      integer              idate(4),nblck,km,iostat,no3d,nog3d,ks
      logical lfnhr
      logical wrt_g3d
      real colat1
      real(kind=8) t1,t2,t3,t4,t5,ta,tb,tc,td,te,tf,rtc,tx,ty
      real timesum
!!
      real(kind=kind_evod) sl(levs), si(levp1)
!!
      integer              ls_node(ls_dim,3)
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
      integer              lats_nodes_r(nodes)
!!
      integer              ierr,j,k,l,lenrec,locl,n,node
      integer nosig,nosfc,noflx,nonsst,nfill
      character*16 cosfc, consst
      character*5 sigf,sfcf,flxf
      data timesum/0./
!!
      REAL(KIND=KIND_EVOD) TRIE_LS(LEN_TRIE_LS,2,11*LEVS+3*LEVH+6)
     &,                    TRIO_LS(LEN_TRIO_LS,2,11*LEVS+3*LEVH+6)
!!
      REAL(KIND=KIND_EVOD) TRIE_Q (LEN_TRIE_LS,2)       ! hmhj
     &,                    TRIO_Q (LEN_TRIO_LS,2)       ! hmhj
      REAL(KIND=KIND_EVOD) TRIE_TE(LEN_TRIE_LS,2,LEVS)  ! hmhj
     &,                    TRIO_TE(LEN_TRIO_LS,2,LEVS)  ! hmhj
      logical convert_temp_ps                           ! hmhj
      integer direction , kk                            ! hmhj
!!
!     INTEGER   P_GZ,P_ZEM,P_DIM,P_TEM,P_RM,P_QM
!    &,         P_ZE,P_DI,P_TE,P_RQ,P_Q,P_DLAM,P_DPHI,P_ULN,P_VLN
!    &,         P_W,P_X,P_Y,P_RT,P_ZQ
!     PARAMETER(P_GZ  = 0*LEVS+0*LEVH+1,  !      GZE/O(LNTE/OD,2),
!    X          P_ZEM = 0*LEVS+0*LEVH+2,  !     ZEME/O(LNTE/OD,2,LEVS),
!    X          P_DIM = 1*LEVS+0*LEVH+2,  !     DIME/O(LNTE/OD,2,LEVS),
!    X          P_TEM = 2*LEVS+0*LEVH+2,  !     TEME/O(LNTE/OD,2,LEVS),
!    X          P_RM  = 3*LEVS+0*LEVH+2,  !      RME/O(LNTE/OD,2,LEVH),
!    X          P_QM  = 3*LEVS+1*LEVH+2,  !      QME/O(LNTE/OD,2),
!    X          P_ZE  = 3*LEVS+1*LEVH+3,  !      ZEE/O(LNTE/OD,2,LEVS),
!    X          P_DI  = 4*LEVS+1*LEVH+3,  !      DIE/O(LNTE/OD,2,LEVS),
!    X          P_TE  = 5*LEVS+1*LEVH+3,  !      TEE/O(LNTE/OD,2,LEVS),
!    X          P_RQ  = 6*LEVS+1*LEVH+3,  !      RQE/O(LNTE/OD,2,LEVH),
!    X          P_Q   = 6*LEVS+2*LEVH+3,  !       QE/O(LNTE/OD,2),
!    X          P_DLAM= 6*LEVS+2*LEVH+4,  !  DPDLAME/O(LNTE/OD,2),
!    X          P_DPHI= 6*LEVS+2*LEVH+5,  !  DPDPHIE/O(LNTE/OD,2),
!    X          P_ULN = 6*LEVS+2*LEVH+6,  !     ULNE/O(LNTE/OD,2,LEVS),
!    X          P_VLN = 7*LEVS+2*LEVH+6,  !     VLNE/O(LNTE/OD,2,LEVS),
!    X          P_W   = 8*LEVS+2*LEVH+6,  !       WE/O(LNTE/OD,2,LEVS),
!    X          P_X   = 9*LEVS+2*LEVH+6,  !       XE/O(LNTE/OD,2,LEVS),
!    X          P_Y   =10*LEVS+2*LEVH+6,  !       YE/O(LNTE/OD,2,LEVS),
!    X          P_RT  =11*LEVS+2*LEVH+6,  !      RTE/O(LNTE/OD,2,LEVH),
!    X          P_ZQ  =11*LEVS+3*LEVH+6)  !      ZQE/O(LNTE/OD,2)
!!
      character CFHOUR*40,CFORM*40
      integer jdate(4),nzsig,ndigyr,ndig,kh,IOPROC
!!
      REAL (KIND=KIND_IO8) GESHEM(LONR,LATS_NODE_R)
      REAL (KIND=KIND_IO8) pdryini
      INTEGER              GLOBAL_LATS_R(LATR),   lonsperlar(LATR)
!
      real(kind=kind_evod)  epsedn(len_trie_ls)
      real(kind=kind_evod)  epsodn(len_trio_ls)
!!
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
!!
      real(kind=kind_evod) plnev_r(len_trie_ls,latr2)
      real(kind=kind_evod) plnod_r(len_trio_ls,latr2)
!!
      real(kind=kind_evod) plnew_r(len_trie_ls,latr2)
      real(kind=kind_evod) plnow_r(len_trio_ls,latr2)
!!
      REAL (KIND=kind_io8) fluxr(nfxr,LONR,LATS_NODE_R)

!!
!     real(kind=kind_rad) zsg(lonr,lats_node_r)
      real(kind=kind_rad) psg(lonr,lats_node_r)
      real(kind=kind_rad) uug(lonr,lats_node_r,levs)
      real(kind=kind_rad) vvg(lonr,lats_node_r,levs)
      real(kind=kind_rad) teg(lonr,lats_node_r,levs)
      real(kind=kind_rad) rqg(lonr,lats_node_r,levh)
      real(kind=kind_rad) dpg(lonr,lats_node_r,levs)
!!
      real secphy,secswr,seclwr
      real(kind=kind_mpi),allocatable :: trieo_ls_nodes_buf(:,:,:,:,:)
      real(kind=kind_mpi),allocatable :: trieo_ls_node(:,:,:)
      save trieo_ls_nodes_buf,trieo_ls_node
      real(kind=8) tba,tbb,tbc,tbd
      integer iret
!
!     print *,' in wrtout me=',me
      t3=rtc()
!!
! ---------------------------------------------------------------
!
!   convert to virtual temperature and lnps if need
!
      convert_temp_ps=.false.

      if( gen_coord_hybrid
     &    .and. out_virttemp .and. .not. gfsio_out) then

        convert_temp_ps=.true.
!
        if (.NOT.LIOPE.or.icolor.ne.2) then
!
! keep enthalpy and ps variables before write
!
          do k=1,levs
            kk = P_TE + k - 1
            trie_te(:,:,k) = trie_ls(:,:,kk)
            trio_te(:,:,k) = trio_ls(:,:,kk)
          enddo
          trie_q (:,:) = trie_ls(:,:,P_Q)
          trio_q (:,:) = trio_ls(:,:,P_Q)

          direction=-1          ! from (enthalpy,ps) to (virttemp,lnps)
          call spect_tv_enthalpy_ps
!    &       (direction,run_enthalpy,
     &       (direction,
     X        TRIE_LS(1,1,P_Q ), TRIO_LS(1,1,P_Q ),
     X        TRIE_LS(1,1,P_TE), TRIO_LS(1,1,P_TE),
     X        TRIE_LS(1,1,P_RQ), TRIO_LS(1,1,P_RQ),
     &        ls_node,ls_nodes,max_ls_nodes,
     &        lats_nodes_r,global_lats_r,lonsperlar,
     &        plnev_r,plnod_r,plnew_r,plnow_r)

        endif           ! (.NOT.LIOPE.or.icolor.ne.2)
!
        thermodyn_id = 1
        sfcpress_id  = 1

      endif

      t4=rtc()
      print *,' time spent on spect_tv_enthalpy is ',t4-t3

!
! --------------------------------------------------------------------
      t3=rtc()
      call mpi_barrier(mpi_comm_all,ierr)
      t4=rtc()
      tba=t4-t3
      if(nodes_comp .lt. 1 .or. nodes_comp .gt. nodes) then
        print *, '  NODES_COMP UNDEFINED, CANNOT DO I.O '
        call mpi_finalize()
         stop 333
      endif
!
      ioproc=nodes_comp-1
      if(liope) ioproc=nodes_comp
      if(allocated ( trieo_ls_node)) then
        continue
      else
        allocate ( trieo_ls_node  ( len_trie_ls_max+len_trio_ls_max,
     x                            2, 3*levs+1*levh+1 ) )
      endif
      t3=rtc()
      call shapeset (ls_nodes,max_ls_nodes,pdryini)
      call MPI_BARRIER(mpi_comm_all,ierr)
      t4=rtc()
      tbb=t4-t3
       
      if ( allocated (trieo_ls_nodes_buf) )then
        continue
      else
        allocate( trieo_ls_nodes_buf ( len_trie_ls_max+len_trio_ls_max,
     x                               2, 3*levs+1*levh+1, nodes,1 ) )
      endif
      t1=rtc()

!!
!sela if (.NOT.LIOPE.or.icolor.ne.2) then
!sela   write(*,*)'In wrtout  before rmsgteo fhour=',fhour
!sela   CALL RMS_spect(TRIE_LS(1,1,P_Q  ), TRIE_LS(1,1,P_DI ),
!selaX             TRIE_LS(1,1,P_TE ), TRIE_LS(1,1,P_ZE ),
!selaX             TRIE_LS(1,1,P_RQ ),
!selaX             TRIO_LS(1,1,P_Q  ), TRIO_LS(1,1,P_DI ),
!selaX             TRIO_LS(1,1,P_TE ), TRIO_LS(1,1,P_ZE ),
!selaX             TRIO_LS(1,1,P_RQ ),
!selaX             LS_NODES,MAX_LS_NODES)
!sela endif

      IF (me.eq.0) THEN
       write(*,*)'In wrtout fhour=',fhour
!---------------------------------------------------------------

!sela   write(*,*)'************'
!sela   CALL bar3(trie_ls(1,1,P_ze),trio_ls(1,1,P_ze),'ze ',levs)
!sela   CALL bar3(trie_ls(1,1,P_di),trio_ls(1,1,P_di),'di ',levs)
!sela   CALL bar3(trie_ls(1,1,P_te),trio_ls(1,1,P_te),'te ',levs)
!sela   CALL bar3(trie_ls(1,1,P_rq),trio_ls(1,1,P_rq),'rq ',levs)
!sela   CALL bar3(trie_ls(1,1,P_rq+levs),trio_ls(1,1,P_rq+levs),
!sela&            'oz1 ',levs)
!sela   CALL bar3(trie_ls(1,1,P_rq+2*levs),trio_ls(1,1,P_rq+2*levs),
!sela&            'oz2 ',levs)
        CALL bar3(trie_ls(1,1,P_q),trio_ls(1,1,P_q),'q  ',1)
!sela   CALL bar3(trie_ls(1,1,P_gz),trio_ls(1,1,P_gz),'gz ',1)
      ENDIF
!
!     CREATE CFHOUR
      JDATE=IDATE
      ndigyr=4
      IF(NDIGYR.EQ.2) THEN
        JDATE(4)=MOD(IDATE(4)-1,100)+1
      ENDIF

!sela set lfnhr to false for writing one step output etc.
      lfnhr=.true.    ! no output
      lfnhr=3600*abs(fhour-nint(fhour)).le.1.or.phour.eq.0
      IF(LFNHR) THEN
        KH=NINT(FHOUR)
        NDIG=MAX(LOG10(KH+0.5)+1.,2.)
        WRITE(CFORM,'("(I",I1,".",I1,")")') NDIG,NDIG
        WRITE(CFHOUR,CFORM) KH
      ELSE
        KS=NINT(FHOUR*3600)
        KH=KS/3600
        KM=(KS-KH*3600)/60
        KS=KS-KH*3600-KM*60
        NDIG=MAX(LOG10(KH+0.5)+1.,2.)
        WRITE(CFORM,
     &      '("(I",I1,".",I1,",A1,I2.2,A1,I2.2)")') NDIG,NDIG
        WRITE(CFHOUR,CFORM) KH,':',KM,':',KS
      ENDIF
      CFHOUR = CFHOUR(1:nfill(CFHOUR)) // ens_nam(1:nfill(ens_nam))
      print *,' in wrtout cfhour=',cfhour,' ens_nam=',ens_nam
!jfe
      nosig  = 61
      nosfc  = 62
      noflx  = 63
      nonsst = 65
!!
      t3=rtc()
      call MPI_BARRIER(mpi_comm_all,ierr)
      t4=rtc()
      tbd=t4-t3
      t3=rtc()
      SECPHY=(FHOUR-ZHOUR)*3600.
      SECSWR=MAX(SECPHY,FHSWR*3600.)
      SECLWR=MAX(SECPHY,FHLWR*3600.)
!
!*** BUILD STATE ON EACH NODE ********
! build state on each node.   COMP tasks only
! assemble spectral state first then sfc state,
! then (only if liope)  flux state.
!
      t3=rtc()
      if(mc_comp .ne. MPI_COMM_NULL) then
      if (.not. gfsio_out) then
        call spect_collect(
     X              TRIE_LS(1,1,P_ZQ), TRIE_LS(1,1,P_Q ),
     X              TRIE_LS(1,1,P_TE), TRIE_LS(1,1,P_DI),
     X              TRIE_LS(1,1,P_ZE), TRIE_LS(1,1,P_RQ),
     X              TRIE_LS(1,1,P_GZ),
     X              TRIO_LS(1,1,P_ZQ), TRIO_LS(1,1,P_Q ),
     X              TRIO_LS(1,1,P_TE), TRIO_LS(1,1,P_DI),
     X              TRIO_LS(1,1,P_ZE), TRIO_LS(1,1,P_RQ),
     X              TRIO_LS(1,1,P_GZ),
     x              trieo_ls_node)

        else
          call spect_to_grid
!    &      (trie_ls(1,1,P_gz),trio_ls(1,1,P_gz),trie_ls(1,1,P_q),
     &      (trie_ls(1,1,P_q),
     &       trio_ls(1,1,P_q), trie_ls(1,1,P_di),trio_ls(1,1,P_di),
     &       trie_ls(1,1,P_ze),trio_ls(1,1,P_ze),trie_ls(1,1,P_te),
     &       trio_ls(1,1,P_te),trie_ls(1,1,P_rq),trio_ls(1,1,P_rq),
     &       sfc_fld%oro,psg,uug,vvg,teg,rqg,dpg,
!    &       zsg,psg,uug,vvg,teg,rqg,dpg,
     &       ls_node,ls_nodes,max_ls_nodes,
     &       lats_nodes_r,global_lats_r,lonsperlar,
     &       epsedn,epsodn,snnp1ev,snnp1od,plnev_r,plnod_r)
        endif
        if(.not.adiab) then
          CALL sfc_collect(sfc_fld,global_lats_r,lonsperlar)
          if ( nsst_active ) then
            call nsst_collect(nsst_fld,global_lats_r,lonsperlar)
          endif
          IF(LIOPE) then
!
! Collect flux grids as was done with sfc grids above, but only if liope
! is true.  If liope is false, the fluxes are handled by the original wrtsfc
! predating the I/O task updates.
!
            call   wrtflx_a
     &             (IOPROC,noflx,ZHOUR,FHOUR,IDATE,colat1,SECSWR,SECLWR,
     &              sfc_fld, flx_fld, fluxr, global_lats_r,lonsperlar)
          endif             ! liope
!
        endif               ! not adiab
      endif                 ! comp node
      t4=rtc()
      td=t4-t3
!
!  done with state build
!  NOW STATE IS ASSEMBLED ON EACH NODE.  GET EVERYTHING OFF THE COMPUTE
!  NODES (currently done with a send to the I/O task_
!  send state to I/O task.  All tasks
!
      if (.not. gfsio_out) then
        t3=rtc()
        lenrec = (len_trie_ls_max+len_trio_ls_max)*2*(3*levs+1*levh+1)
        call mpi_gather( trieo_ls_node , lenrec, MPI_R_MPI,
     x                 trieo_ls_nodes_buf(1,1,1,1,1), lenrec, MPI_R_MPI,
     x                 ioproc, MPI_COMM_ALL, ierr)
!       CALL spect_send(IOPROC, trieo_ls_node, trieo_ls_nodes_buf)
        call mpi_barrier(mpi_comm_all,ierr)
      else
        call grid_collect
     &        (sfc_fld%oro,psg,uug,vvg,teg,rqg,dpg,
!    &        (zsg,psg,uug,vvg,teg,rqg,dpg,
     &        global_lats_r,lonsperlar)
        call atmgg_move(ioproc)
      endif
!
      if(.not.adiab)then
        IF(LIOPE) then                    ! move all grids (fluxes and sfc)
          call GRIDS_MOVE(ioproc )
        ELSE             ! move sfc grids only,  handle fluxes in original wrtsfc
          call SFC_ONLY_MOVE(ioproc)

          if ( nsst_active )  then
            call nsst_only_move(ioproc)
            write(*,*) ' nsst_only_move done'
          endif

          if(me .eq. ioproc) then
!           call BAOPENWT(NOFLX,'FLX.F'//CFHOUR,iostat)
            call BAOPENWT(NOFLX,flxf//CFHOUR,iostat)
          endif
          call  WRTSFC
     &          (IOPROC,noflx,ZHOUR,FHOUR,IDATE,colat1,SECSWR,SECLWR,
     &           sfc_fld, flx_fld, fluxr, global_lats_r,lonsperlar)
        ENDIF          !  LIOPE
!
        t4=rtc()
        te=t4-t3
!
        IF (LDIAG3D) THEN
          no3d=64
          if(icolor.eq.2.and.me.eq.IOPROC)
     &    call BAOPENWT(NO3D,'D3D.F'//CFHOUR,iostat)
          if (hybrid .or. gen_coord_hybrid) then
!     print *,' pl_coeff bef call wrt3d_hyb=',pl_coeff
            call WRT3D_hyb(IOPROC,no3d,ZHOUR,FHOUR,IDATE,colat1,
     .                     global_lats_r,lonsperlar,pl_coeff,
     &                     SECSWR,SECLWR,sfc_fld%slmsk,flx_fld%psurf)
          else
            call WRT3D(IOPROC,no3d,ZHOUR,FHOUR,IDATE,colat1,
     .                 global_lats_r,lonsperlar,pl_coeff,
     &                 SECSWR,SECLWR,sl,si,sfc_fld%slmsk,flx_fld%psurf)
          endif
        ENDIF
!
!hchuang code change [+14L] add G3D output calls
!        need to know where to put BACLOSE(nog3d, iostat)
        IF (LGGFS3D .and. wrt_g3d) THEN
          nog3d=69
          if(icolor.eq.2.and.me.eq.IOPROC)
     &    call BAOPENWT(NOG3D,'G3D.F'//CFHOUR,iostat)
          if (hybrid .or. gen_coord_hybrid) then
            call WRTG3D_hyb(IOPROC,nog3d,nblck,ZHOUR,FHOUR,IDATE,colat1,
     &                   global_lats_r,lonsperlar,pl_coeff,secswr)
          else
            call WRTG3D(IOPROC,nog3d,nblck,ZHOUR,FHOUR,IDATE,colat1,
     &                   global_lats_r,lonsperlar,pl_coeff,secswr,sl,si)
          endif
        ENDIF
      endif             !  not.adiab
!
! ioproc only
      CFHOUR1 = CFHOUR          !for the ESMF Export State Creation
      ta=rtc()
      if (me .eq. ioproc) then
!       CFORM = 'SIG.F'//CFHOUR
        CFORM = sigf//CFHOUR
        if (.not. gfsio_out) then           ! write using sigio
          call sigio_rwopen(nosig,CFORM,iret)
          ixgr = 0
          print *,' calling spect_write fhour=',fhour
          CALL spect_write(nosig,IOPROC,FHOUR,JDATE,
     X                SL,SI,pdryini,
     X                LS_NODES,MAX_LS_NODES,
     &                global_lats_r,lonsperlar,
     &                trieo_ls_nodes_buf,ixgr)
          call sigio_rclose(nosig,iret)
          print *,' return from spect_write fhour=',fhour,' iret=',iret
        else                               ! write using gfsio
          print *,' calling atmgg_wrt fhour=',fhour
     &,' cform=',cform,' idate=',idate
          call atmgg_wrt(IOPROC,CFORM,fhour,idate
     &,                  global_lats_r,lonsperlar,pdryini)
          print *,' returning fromatmgg_wrt=',fhour
        endif
!
        if(.not.adiab) then
!         if(liope) call BAOPENWT(NOFLX,'FLX.F'//CFHOUR,iostat)
          if(liope) call BAOPENWT(NOFLX,flxf//CFHOUR,iostat)
!
!  Now write the surface file
!
!         cosfc='SFC.F'//CFHOUR
          cosfc=sfcf//CFHOUR
          call sfc_wrt(ioproc,nosfc,cosfc,fhour,jdate
     &,                global_lats_r,lonsperlar)
!         CLOSE(NOSFC)
!
          if ( nsst_active ) then            !  Now write the ocean file
            consst='NSST.F'//CFHOUR
            call nsst_wrt(ioproc,nonsst,consst,fhour,jdate
     &,                  global_lats_r,lonsperlar)
!                                           !  Ocean WRITE DONE
          endif       ! if ( nsst_active )
        endif       ! not.adiab
      endif
!
      tc=rtc()
      if(me .eq. 0) t2=rtc()
!gwv  t2=rtc()
      t3=rtc()
      if(MC_COMP   .ne. MPI_COMM_NULL) then
        call mpi_barrier(mc_comp,info)
      endif
      t4=rtc()
      if(liope) then                     !  WRITE THE FLUXES
        if(me .eq. ioproc) then
          call  WRTFLX_w
     &      (IOPROC,noflx,ZHOUR,FHOUR,IDATE,colat1,SECSWR,SECLWR,
     &       sfc_fld%slmsk, global_lats_r,lonsperlar)
        endif
      endif
!                                        !  FLUX WRITE DONE
!
      if(me .eq. ioproc) then
        if (.not. adiab) then
          call baclose(noflx,iostat)
          print *,' iostat after baclose of noflx ',iostat,noflx
          close(nonsst)
        endif
      endif
!     if(me .eq. ioproc)  call wrtlog(fhour,idate)
      if(me .eq. ioproc)  call wrtlog(phour,fhour,idate)
      tb=rtc()
      tf=tb-ta
      t2=rtc()
 1011 format(' WRTOUT TIME ',f10.4)
      timesum=timesum+(t2-t1)
      print 1012,timesum,t2-t1,td,te,tf,t4-t3,tba,tbb,tbd
!     print 1012,timesum,t2-t1,td,te,tf,t4-t3,tba,tbb,tbc,tbd
 1012 format(
     1 ' WRTOUT TIME ALL TASKS  ',f10.4,f10.4,
     1 ' state, send, io  iobarr, (beginbarr),
     1 spectbarr,open, openbarr )  ' ,
     1  8f9.4)
!
! restore temperature for continuing computation
!
      if( convert_temp_ps ) then

        if( run_enthalpy ) then
          thermodyn_id = 3
          sfcpress_id  = 2
        else
          thermodyn_id = 1
          sfcpress_id  = 2
        endif

        if (.NOT.LIOPE.or.icolor.ne.2) then
! te
          do k=1,levs
            kk = P_TE + k - 1
            trie_ls(:,:,kk) = trie_te(:,:,k)
            trio_ls(:,:,kk) = trio_te(:,:,k)
          enddo
! ps
          trie_ls(:,:,P_Q) = trie_q (:,:)
          trio_ls(:,:,P_Q) = trio_q (:,:)
!
        endif           ! (.NOT.LIOPE.or.icolor.ne.2)

      endif
!
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE wrt_restart(TRIE_LS,TRIO_LS,
     &       sfc_fld, nsst_fld,
     &       SI,SL,fhour,idate,
     &       igen,pdryini,
     x       ls_node,ls_nodes,max_ls_nodes,
     &       global_lats_r,lonsperlar,SNNP1EV,SNNP1OD,
     &       phy_f3d, phy_f2d, ngptc, nblck, adiab, ens_nam,
     &       nsst_active,sigr1,sigr2,sfcr,cnsstr)
!!
      use resol_def
      use layout1
      use mpi_def
      use sigio_module
      use sigio_r_module
      use nsstio_module
      use Sfc_Flx_ESMFMod
      use Nsstm_ESMFMod
      implicit none
!!
      TYPE(Sfc_Var_Data)        :: sfc_fld
      TYPE(Nsst_Var_Data)       :: nsst_fld
      real(kind=kind_evod) fhour
!     real(kind=kind_evod) xgf
      logical              adiab, nsst_active
      character (len=*)  :: ens_nam
!!
      integer              idate(4), ixgr
      INTEGER              LS_NODE (LS_DIM*3)
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
!
      REAL(KIND=KIND_EVOD) SNNP1EV(LEN_TRIE_LS)
      REAL(KIND=KIND_EVOD) SNNP1OD(LEN_TRIO_LS)
!!
      integer              ngptc, nblck
      REAL (KIND=KIND_phys)
     &            phy_f3d(ngptc,levs,nblck,LATS_NODE_R,num_p3d)
     &,           phy_f2d(LONR,LATS_NODE_R,num_p2d)
!!
      real(kind=kind_evod) sl(levs)
      real(kind=kind_evod) si(levp1)
!!
      REAL(KIND=KIND_EVOD) TRIE_LS(LEN_TRIE_LS,2,11*LEVS+3*LEVH+6)
      REAL(KIND=KIND_EVOD) TRIO_LS(LEN_TRIO_LS,2,11*LEVS+3*LEVH+6)
!!
!     INTEGER   P_GZ,P_ZEM,P_DIM,P_TEM,P_RM,P_QM
!     INTEGER   P_ZE,P_DI,P_TE,P_RQ,P_Q,P_DLAM,P_DPHI,P_ULN,P_VLN
!     INTEGER   P_W,P_X,P_Y,P_RT,P_ZQ
!     PARAMETER(P_GZ  = 0*LEVS+0*LEVH+1,  !      GZE/O(LNTE/OD,2),
!    X          P_ZEM = 0*LEVS+0*LEVH+2,  !     ZEME/O(LNTE/OD,2,LEVS),
!    X          P_DIM = 1*LEVS+0*LEVH+2,  !     DIME/O(LNTE/OD,2,LEVS),
!    X          P_TEM = 2*LEVS+0*LEVH+2,  !     TEME/O(LNTE/OD,2,LEVS),
!    X          P_RM  = 3*LEVS+0*LEVH+2,  !      RME/O(LNTE/OD,2,LEVH),
!    X          P_QM  = 3*LEVS+1*LEVH+2,  !      QME/O(LNTE/OD,2),
!    X          P_ZE  = 3*LEVS+1*LEVH+3,  !      ZEE/O(LNTE/OD,2,LEVS),
!    X          P_DI  = 4*LEVS+1*LEVH+3,  !      DIE/O(LNTE/OD,2,LEVS),
!    X          P_TE  = 5*LEVS+1*LEVH+3,  !      TEE/O(LNTE/OD,2,LEVS),
!    X          P_RQ  = 6*LEVS+1*LEVH+3,  !      RQE/O(LNTE/OD,2,LEVH),
!    X          P_Q   = 6*LEVS+2*LEVH+3,  !       QE/O(LNTE/OD,2),
!    X          P_DLAM= 6*LEVS+2*LEVH+4,  !  DPDLAME/O(LNTE/OD,2),
!    X          P_DPHI= 6*LEVS+2*LEVH+5,  !  DPDPHIE/O(LNTE/OD,2),
!    X          P_ULN = 6*LEVS+2*LEVH+6,  !     ULNE/O(LNTE/OD,2,LEVS),
!    X          P_VLN = 7*LEVS+2*LEVH+6,  !     VLNE/O(LNTE/OD,2,LEVS),
!    X          P_W   = 8*LEVS+2*LEVH+6,  !       WE/O(LNTE/OD,2,LEVS),
!    X          P_X   = 9*LEVS+2*LEVH+6,  !       XE/O(LNTE/OD,2,LEVS),
!    X          P_Y   =10*LEVS+2*LEVH+6,  !       YE/O(LNTE/OD,2,LEVS),
!    X          P_RT  =11*LEVS+2*LEVH+6,  !      RTE/O(LNTE/OD,2,LEVH),
!    X          P_ZQ  =11*LEVS+3*LEVH+6)  !      ZQE/O(LNTE/OD,2)
!!
      integer igen
!!
      INTEGER              GLOBAL_LATS_R(LATR)
      INTEGER              lonsperlar(LATR)
      integer IOPROC, IPRINT
      integer needoro, iret, nfill
!
!!
      real runid,usrid
      integer n3,n4,nflop,nsstr
      character*20 cflop,sigr51, sigr52, nsstr54
      character*5 sigr1,sigr2,cnsstr
      character*4 sfcr
      real pdryini
      integer nn
!!
!     if(me.eq.0) print*,'writing restart files fhour in wrt_res=',fhour
      IPRINT = 0
!
!     sigr51  = 'SIGR1' // ens_nam(1:nfill(ens_nam))
!     sigr52  = 'SIGR2' // ens_nam(1:nfill(ens_nam))
!     cflop   = 'SFCR'  // ens_nam(1:nfill(ens_nam))
!     nsstr54 = 'NSSTR' // ens_nam(1:nfill(ens_nam))

      sigr51  = sigr1  // ens_nam(1:nfill(ens_nam))
      sigr52  = sigr2  // ens_nam(1:nfill(ens_nam))
      cflop   = sfcr   // ens_nam(1:nfill(ens_nam))
      nsstr54 = cnsstr // ens_nam(1:nfill(ens_nam))

      print *,' sigr51=',sigr51,' sigr52=',sigr52,' cflop=',cflop
     &,'ens_nam=',ens_nam(1:nfill(ens_nam)),' nsstr54=',nsstr54
!
      n3=51
      call sigio_rwopen(n3,sigr51,iret)
!     print *,' in rest bef rewind fhour=',fhour,' iret=',iret
      rewind(n3)
!     print *,' in rest write fhour=',fhour
      IF (icolor.eq.2) then
         IOPROC=nodes-1
      else
         IOPROC=nodes
      endif
!
      ixgr = 0
      if (.not. adiab) then
        if (num_p3d .eq. 4) then          ! Zhao microphysics
!         ixgr = 2
          ixgr = 4
        elseif (num_p3d .eq. 3) then      ! Ferrier microphysics
!         ixgr = 3
          ixgr = 5
        endif
      endif
!     xgf = ixgf
!
!     print *,' in rest write 2  fhour=',fhour
        CALL TWRITEEO(n3,ioproc,FHOUR,idate,
     X                TRIE_LS(1,1,P_ZQ), TRIE_LS(1,1,P_QM ),
     X                TRIE_LS(1,1,P_TEM), TRIE_LS(1,1,P_DIM),
     X                TRIE_LS(1,1,P_ZEM), TRIE_LS(1,1,P_RM),
     X                TRIE_LS(1,1,P_GZ),
     X                TRIO_LS(1,1,P_ZQ), TRIO_LS(1,1,P_QM ),
     X                TRIO_LS(1,1,P_TEM), TRIO_LS(1,1,P_DIM),
     X                TRIO_LS(1,1,P_ZEM), TRIO_LS(1,1,P_RM),
     X                TRIO_LS(1,1,P_GZ),
     X                SL,SI,pdryini,
     X                LS_NODES,MAX_LS_NODES,ixgr,
     &                phy_f3d,phy_f2d,global_lats_r,lonsperlar,nblck)
!    &                global_lats_r,lonsperlar,xgf)
!     print *,' in rest write 3  fhour=',fhour
!
!     IF (icolor.eq.2.and.me.eq.ioproc) CLOSE(n3)
      IF (icolor.eq.2.and.me.eq.ioproc) print *,' closed ',n3
!
      n4=52
      call sigio_rwopen(n4,sigr52,iret)
      rewind(n4)
!     print *,' in rest write 3  fhour=',fhour
      IF (icolor.eq.2) then
         IOPROC=nodes-1
      else
         IOPROC=nodes
      endif
!     xgf = 0.0
      ixgr = 0
!     print *,' in rest write 4  fhour=',fhour,' ixgr=',ixgr
        CALL TWRITEEO(n4,ioproc,FHOUR,idate,
     X                TRIE_LS(1,1,P_ZQ), TRIE_LS(1,1,P_Q ),
     X                TRIE_LS(1,1,P_TE), TRIE_LS(1,1,P_DI),
     X                TRIE_LS(1,1,P_ZE), TRIE_LS(1,1,P_RQ),
     X                TRIE_LS(1,1,P_GZ),
     X                TRIO_LS(1,1,P_ZQ), TRIO_LS(1,1,P_Q ),
     X                TRIO_LS(1,1,P_TE), TRIO_LS(1,1,P_DI),
     X                TRIO_LS(1,1,P_ZE), TRIO_LS(1,1,P_RQ),
     X                TRIO_LS(1,1,P_GZ),
     X                SL,SI,pdryini,
     X                LS_NODES,MAX_LS_NODES,ixgr,
     &                phy_f3d,phy_f2d,global_lats_r,lonsperlar,nblck)
!jfe
!     IF (icolor.eq.2.and.me.eq.ioproc) CLOSE(n4)
!     print *,' finished writing restart files'
!
      nflop=53
!     cflop='fort.53'
      IF (icolor.eq.2) then
         IOPROC=nodes-1
      else
         IOPROC=nodes
      endif
      if (.not. adiab) then
        CALL para_fixio_w(ioproc,sfc_fld, nflop,cflop,fhour,idate,
     &                    global_lats_r,lonsperlar)
        if (nsst_active) then
          nsstr = 54
          CALL para_nsstio_w(ioproc,nsst_fld,nsstr,nsstr54,fhour,idate,
     &                      global_lats_r,lonsperlar)
        endif
      endif ! .not. adiab
!
!
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE wrtlog(phour,fhour,idate)
      use resol_def
      use namelist_def
      implicit none

      integer idate(4),ndigyr,nolog
      integer ks,kh,km,ndig,nfill
      character CFHOUR*40,CFORM*40
      logical lfnhr
      real phour,fhour
!
!     CREATE CFHOUR

!sela set lfnhr to false for writing one step output etc.
      lfnhr=.true.    ! no output
!!mr  lfnhr=.false.   !    output
      lfnhr=3600*abs(fhour-nint(fhour)).le.1.or.phour.eq.0
      IF(LFNHR) THEN
        KH=NINT(FHOUR)
        NDIG=MAX(LOG10(KH+0.5)+1.,2.)
        WRITE(CFORM,'("(I",I1,".",I1,")")') NDIG,NDIG
        WRITE(CFHOUR,CFORM) KH
        WRITE(CFORM,'("(I",I1,".",I1,")")') NDIG,NDIG
        WRITE(CFHOUR,CFORM) KH
      ELSE
        KS=NINT(FHOUR*3600)
        KH=KS/3600
        KM=(KS-KH*3600)/60
        KS=KS-KH*3600-KM*60
        NDIG=MAX(LOG10(KH+0.5)+1.,2.)
        WRITE(CFORM,
     &      '("(I",I1,".",I1,",A1,I2.2,A1,I2.2)")') NDIG,NDIG
        WRITE(CFHOUR,CFORM) KH,':',KM,':',KS
      ENDIF
      CFHOUR = CFHOUR(1:nfill(CFHOUR)) // ens_nam(1:nfill(ens_nam))

      nolog=99
      OPEN(NOlog,FILE='LOG.F'//CFHOUR,FORM='FORMATTED')
      write(nolog,100)fhour,idate
100   format(' completed mrf fhour=',f10.3,2x,4(i4,2x))
      CLOSE(NOlog)

      RETURN
      END
      subroutine  shapeset (ls_nodes,max_ls_nodes,pdryini)
!
      use resol_def
      use layout1
      use sig_io
      use namelist_def
      use mpi_def
      implicit none
!
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
!!
      integer              ierr,j,k,l,lenrec,locl,n,node
!!
      integer              indjoff
      integer              indev
      integer              indod
!!
      real(kind=kind_evod) gencode,order,ppid,realform,rlatd
      real(kind=kind_evod) rlatp,rlatr,rlond,rlonp,rlonr,slid
      real(kind=kind_evod) subcen,tracers,trun,vcid,vmid,vtid
      real(kind=kind_evod) waves,xlayers,xgf
!!
      real(kind=kind_evod) dummy(201-levp1-levs)
      real(kind=kind_evod) ensemble(2),dummy2(18)
!!
      real(kind=kind_io4)   tmps(4+nodes+jcap1*nodes)
      real(kind=kind_io4)   tmpr(3+nodes+jcap1*(nodes-1))
      REAL (KIND=KIND_IO8) pdryini
!!
      INTEGER              GLOBAL_LATS_R(LATR)
      INTEGER                 LONSPERLAR(LATR)
!!
      integer  il,ilen,i,msgtag,ls_diml,nodesl,ioproc, itmpr
                                                                                                        
!  Now define shape of the coefficients array
!  as a function of node. This will define how
!  to assemble the few wavenumbers on each node
!  into a full coefficient array.
!
      IF (icolor.eq.2) then
         IOPROC=nodes-1
      else
         IOPROC=nodes
      endif
       IF (LIOPE) then
 199    format(' GWVX MAX_LS_NODES ',i20)
        if (me.eq.0.or. me .eq. ioproc) then
        tmps=0.
        tmps(1)=PDRYINI
        tmps(2:nodes_comp+1)=max_ls_nodes(1:nodes_comp)
        tmps(nodes_comp+2)=ls_dim
        tmps(nodes_comp+3)=len_trie_ls_max
        tmps(nodes_comp+4)=len_trio_ls_max
        il=nodes_comp+4
        do i=1,nodes_comp
        do j=1,ls_dim
           il=il+1
           tmps(il)=ls_nodes(j,i)
        enddo
        enddo
        ilen=4+nodes_comp+jcap1*nodes_comp
        msgtag=2345
        if(me .eq. 0) then
            CALL mpi_send(tmps,ilen,MPI_R_IO,ioproc,
     &                msgtag,MPI_COMM_ALL,info)
           endif
        endif
!
        if (me.eq.ioproc) then
         ilen=4+nodes_comp+jcap1*(nodes_comp)
         msgtag=2345
             CALL mpi_recv(tmpr,ilen,MPI_R_IO,0,
     &                msgtag,MPI_COMM_ALL,stat,info)

          itmpr=3+nodes+jcap1*(nodes-1)
          tmps(1:itmpr) = tmpr(1:itmpr)
          ls_nodes=0
          pdryini=tmps(1)
          max_ls_nodes(1:nodes_comp)=int(tmps(2:nodes_comp+1))
          ls_diml= int(tmps(nodes_comp+2))
          len_trie_ls_max=int(tmps(nodes_comp+3))
          len_trio_ls_max=int(tmps(nodes_comp+4))
           il=nodes_comp+3+1
                                                                                                        
          do i=1,nodes_comp
          do j=1,ls_diml
             il=il+1
             ls_nodes(j,i)=int(tmps(il))
          enddo
          enddo
        endif
      ENDIF
                                                                                                        
      return
      end
      SUBROUTINE sfc_collect (sfc_fld,global_lats_r,lonsperlar)
!!
      use resol_def
      use mod_state
      use layout1
      use mpi_def
      use Sfc_Flx_ESMFMod
      implicit none
!!
      TYPE(Sfc_Var_Data)        :: sfc_fld
!
      INTEGER              GLOBAL_LATS_R(latr)
      INTEGER              lonsperlar(latr)
!!
!!!   real(kind=kind_io4) buff4(lonr,latr,4),bufsave(lonr,lats_node_r)
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      real(kind=kind_io8) buffi(lonr,lats_node_r)
      integer kmsk(lonr,lats_node_r),kmskcv(lonr,lats_node_r)
      integer k,il
      integer ubound
      integer icount
      integer  ierr
      save    icount
!!
      CHARACTER*8 labfix(4)
      real(kind=kind_io4) yhour
      integer,save:: version
      data version/200004/
!     data  icount/0/
      integer maxlats_comp
!
      ngrid=1
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!!
      if (.not. allocated(buff_mult_piece)) then
         allocate(buff_mult_piece(lonr,ngrids_sfcc,lats_node_r))
      endif
      if (.not. allocated(buff_mult_piecef)) then
         allocate(buff_mult_piecef(lonr,0:ngrids_flx,lats_node_r))
      endif
      if (.not. allocated(buff_mult_piecea)) then
         allocate (buff_mult_piecea(lonr,
     1             1:ngrids_flx+ngrids_sfcc+ngrids_nsst+1,lats_node_r))
      endif

!     if(allocated(buff_mult_piece)) then
!        continue
!     else
!        allocate(buff_mult_piece(lonr,ngrids_sfcc,lats_node_r))
!        allocate(buff_mult_piecef(lonr,0:ngrids_flx,lats_node_r))
!        allocate
!    1 (buff_mult_piecea(lonr,1:ngrids_flx+ngrids_sfcc+1,lats_node_r))
!     endif
!
      kmsk= nint(sfc_fld%slmsk)
      CALL uninterprez(1,kmsk,buffo,sfc_fld%tsea,
     &                global_lats_r,lonsperlar)
!
! ngrid=2 here
                                                                                                        
!
      DO k=1,LSOIL
        buffi(:,:) = sfc_fld%SMC(k,:,:)
        CALL uninterprez(1,kmsk,buffo,buffi,global_lats_r,lonsperlar)
      ENDDO
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SHELEG,
     &                 global_lats_r,lonsperlar)
!
      DO k=1,LSOIL
        buffi(:,:) = sfc_fld%STC(k,:,:)
        CALL uninterprez(1,kmsk,buffo,buffi,global_lats_r,lonsperlar)
      ENDDO
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%TG3,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%ZORL,
     &                 global_lats_r,lonsperlar)
!!
!     where(CV.gt.0.)
!         kmskcv=1
!     elsewhere
!         kmskcv=0
!     endwhere
!
!*********************************************************************
!   Not in version 200501
!     CALL uninterprez(1,kmskcv,buffo,CV,global_lats_r,lonsperlar)
!     CALL uninterprez(1,kmskcv,buffo,CVB,global_lats_r,lonsperlar)
!     CALL uninterprez(1,kmskcv,buffo,CVT,global_lats_r,lonsperlar)
!*********************************************************************
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%ALVSF,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%ALVWF,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%ALNSF,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%ALNWF,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SLMSK,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%VFRAC,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%CANOPY,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%F10M,
     &                 global_lats_r,lonsperlar)
! T2M
      CALL uninterprez(1,kmsk,buffo,sfc_fld%T2M,
     &                 global_lats_r,lonsperlar)
! Q2M
      CALL uninterprez(1,kmsk,buffo,sfc_fld%Q2M,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%VTYPE,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%STYPE,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%FACSF,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%FACWF,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%UUSTAR,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%FFMM,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,sfc_fld%FFHH,
     &                 global_lats_r,lonsperlar)
!
!c-- XW: FOR SEA-ICE Nov04
      CALL uninterprez(1,kmsk,buffo,sfc_fld%HICE,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,sfc_fld%FICE,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,sfc_fld%TISFC,
     &                 global_lats_r,lonsperlar)
!c-- XW: END SEA-ICE Nov04
!
!lu: the addition of 8 Noah-related records starts here ........................
!tprcp
      CALL uninterprez(1,kmsk,buffo,sfc_fld%TPRCP,
     &                 global_lats_r,lonsperlar)
!srflag
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SRFLAG,
     &                 global_lats_r,lonsperlar)
!snwdph
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SNWDPH,
     &                 global_lats_r,lonsperlar)
!slc
      DO k=1,LSOIL
        buffi(:,:) = sfc_fld%SLC(k,:,:)
        CALL uninterprez(1,kmsk,buffo,buffi,global_lats_r,lonsperlar)
!       buffo(:,:)=buff_mult_piece(:,k+3+lsoil,:)
      ENDDO
!shdmin
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SHDMIN,
     &                 global_lats_r,lonsperlar)
!shdmax
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SHDMAX,
     &                 global_lats_r,lonsperlar)
!slope
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SLOPE,
     &                 global_lats_r,lonsperlar)
!snoalb
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SNOALB,
     &                 global_lats_r,lonsperlar)
!lu: the addition of 8 Noah records ends here .........................
!
! Oro
      CALL uninterprez(1,kmsk,buffo,sfc_fld%ORO,
     &                 global_lats_r,lonsperlar)
!
!     print *,' finished sfc_collect for  ngrid=',ngrid
  999 continue
      ngrid=1
      return
      end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!
       SUBROUTINE nsst_collect(nsst_fld,global_lats_r,lonsperlar)
cc
      use resol_def
      use mod_state
      use layout1
      use mpi_def
      use Nsstm_ESMFMod
      implicit none
!!
      TYPE(Nsst_Var_Data)       :: nsst_fld
      INTEGER              GLOBAL_LATS_R(latr)
      INTEGER              lonsperlar(latr)
!!
!     REAL (KIND=KIND_IO8) SLMSK(LONR,LATS_NODE_R),

      real(kind=kind_io8) buffo(lonr,lats_node_r),x(lonr,lats_node_r)
      integer kmsk(lonr,lats_node_r)
      integer k,il
      integer  ierr
!
!     ngrid=ngrids_sfcc+ngrids_flx+1+1
      ngrid=ngrids_sfcc+1
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!!
      if(allocated(buff_mult_piece)) then
         continue
      else
         allocate(buff_mult_piece(lonr,ngrids_sfcc,lats_node_r))
         allocate(buff_mult_piecef(lonr,0:ngrids_flx,lats_node_r))
         allocate(buff_mult_pieceo(lonr,ngrids_nsst,lats_node_r))
         allocate (buff_mult_piecea(lonr,
     1             1:ngrids_flx+ngrids_sfcc+ngrids_nsst+1,lats_node_r))
      endif
      kmsk= nint(nsst_fld%slmsk)
!
!*********************************************************************
!
      CALL uninterprez(1,kmsk,buffo,nsst_fld%slmsk,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%TREF,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%DT_COOL,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%Z_C,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%DT_WARM,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%Z_W,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%C_0,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%C_d,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%W_0,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%W_d,
     &                 global_lats_r,lonsperlar)
!
      CALL uninterprez(1,kmsk,buffo,nsst_fld%IFD,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%TIME_OLD,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%TIME_INS,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%I_Sw,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%I_Q,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%I_QRAIN,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%I_M,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%I_TAU,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%I_Sw_Zw,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%I_Q_Ts,
     &                 global_lats_r,lonsperlar)
      CALL uninterprez(1,kmsk,buffo,nsst_fld%I_M_Ts,
     &                 global_lats_r,lonsperlar)

!
!     print *,' finished nsst_collect for  ngrid=',ngrid
  999 continue
      ngrid=1
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!
       subroutine sfc_only_move(ioproc)
!
!***********************************************************************
!
      use resol_def
      use mod_state
      use layout1
      use mpi_def
      implicit none
!
      integer ipt_lats_node_rl,nodesr
      integer lats_nodes_rl
!     integer lats_nodes_r(nodes),ipt,maxfld,ioproc,nproct
      integer ioproc
      integer proc,j,lat,msgtag,nproc,i,msgtag1,buff,startlat,ierr
      integer illen,ubound
      integer icount
      save    icount
      data icount/0/
      integer maxlats_comp
!  allocate the data structures
!
      if(icount .eq. 0) then
         allocate(ivar_global(10))
         allocate(ivar_global_a(10,nodes))
         ivar_global(1)=ipt_lats_node_r
         ivar_global(2)= lats_node_r
         ivar_global(3)=lats_node_r_max
         call mpi_gather(ivar_global,10,MPI_INTEGER,
     1       ivar_global_a,10,MPI_INTEGER,ioproc,MPI_COMM_ALL,ierr)
         icount=icount+1
      endif
!!
      if(allocated(buff_mult_pieces)) then
          continue
      else
          maxlats_comp=lats_node_r_max
          if(.not. liope .or. me .ne. ioproc) then
            continue
          else
!           maxlats_comp=ivar_global_a(3,ioproc)
            maxlats_comp=ivar_global_a(3,1)
          endif
          print *,' INDEX FOR MAXLAT SET ',ioproc
!gwv watch this!!
!         print *,' allocating ', lonr,ngrids_sfcc,maxlats_comp,nodes
          allocate
     &    (buff_mult_pieces(lonr,ngrids_sfcc,maxlats_comp,nodes))
          allocate
     &    (buff_mult_piecesf(lonr,0:ngrids_flx,maxlats_comp,nodes))
          allocate(buff_mult_pieceo(lonr,ngrids_nsst,maxlats_comp))
          allocate
     1    (buff_mult_pieceso(lonr,ngrids_nsst,maxlats_comp,nodes))
          allocate
     &    (buff_mult_piecesa(lonr,1:ngrids_flx+ngrids_sfcc+ngrids_nsst+1
     &,    maxlats_comp,nodes))
      endif
                                                                                                        
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!   SENDLOOP of grids from comp processors to I/O task.  The
!   I/O task may or may not be a comp task also.  The
!   send logic on that task is different for these two cases
!
!  big send
!     if(me .gt. -1) return
!
       buff_mult_piece(:,1:ngrids_sfcc,:)=
     1 buff_mult_piecea(:,1:ngrids_sfcc,:)
!
      IF (ME .ne. ioproc) THEN    !   Sending the data
         msgtag=me
         illen=lats_node _r
         CALL mpi_send            !  send the local grid domain
     &(buff_mult_piece,illen*lonr*ngrids_sfcc,MPI_R_IO,ioproc,
     &                  msgtag,MPI_COMM_ALL,info)
      ELSE
        if( MC_COMP .ne. MPI_COMM_NULL) then
!
c iotask is also a compute task.  send is replaced with direct
c  array copy
!
                                                                                                        
           buff_mult_pieces(:,:,1:lats_node_r,ioproc+1)=
     1     buff_mult_piece(:,:,1:lats_node_r)
!                              END COMPUTE TASKS PORTION OF LOGIC
        endif
!
!  END COMPUTE TASKS PORTION OF LOGIC
!  receiving part of I/O task
!
!!
!!      for pes ioproc
        DO proc=1,nodes_comp
          if (proc.ne.ioproc+1) then
            msgtag=proc-1
            illen=ivar_global_a(2,proc)
!           print *,' pux target ',ubound(buff_mult_pieces)
            CALL mpi_recv(buff_mult_pieces(1,1,1,proc),
     1        illen*lonr*ngrids_sfcc
     1        ,MPI_R_IO,proc-1,
     &                msgtag,MPI_COMM_ALL,stat,info)
          endif
        enddo
        buff_mult_piecesa(:,1:ngrids_sfcc,:,:)=
     1 buff_mult_pieces(:,1:ngrids_sfcc,:,:)
      ENDIF
!!
      return
      end
       subroutine nsst_only_move(ioproc)
c
c***********************************************************************
c
      use resol_def
      use mod_state
      use layout1
      use mpi_def
      implicit none
!
      integer ipt_lats_node_rl,nodesr
      integer lats_nodes_rl
c     integer lats_nodes_r(nodes),ipt,maxfld,ioproc,nproct
      integer ioproc,nstart,nend
      integer proc,j,lat,msgtag,nproc,i,msgtag1,buff,startlat,ierr
      integer illen,ubound
      integer icount
      save    icount
      data icount/0/
      integer maxlats_comp
      integer nf
c  allocate the data structures
c
      if(icount .eq. 0) then
        if (.not. allocated(ivar_global)) then
         allocate(ivar_global(10))
         allocate(ivar_global_a(10,nodes))
         ivar_global(1)=ipt_lats_node_r
         ivar_global(2)= lats_node_r
         ivar_global(3)=lats_node_r_max
         call mpi_gather(ivar_global,10,MPI_INTEGER,
     1       ivar_global_a,10,MPI_INTEGER,ioproc,MPI_COMM_ALL,ierr)
         icount=icount+1
        endif
      endif
!!
      if(allocated(buff_mult_pieces)) then
          continue
      else
          maxlats_comp=lats_node_r_max
          if(.not. liope .or. me .ne. ioproc) then
            continue
          else
!           maxlats_comp=ivar_global_a(3,ioproc)
            maxlats_comp=ivar_global_a(3,1)
          endif
          print *,' INDEX FOR MAXLAT SET ',ioproc
!gwv watch this!!
!         print *,' allocating ', lonr,ngrids_sfcc,maxlats_comp,nodes
!    &,' ngrids_nsst=',ngrids_nsst,' lats_node_r=',lats_node_r
          allocate
     1    (buff_mult_pieces(lonr,ngrids_sfcc,maxlats_comp,nodes))
!         print *,' allocated', lonr,ngrids_sfcc,maxlats_comp,nodes
          allocate
     1    (buff_mult_piecesf(lonr,0:ngrids_flx,maxlats_comp,nodes))
          allocate(buff_mult_pieceo(lonr,ngrids_nsst,maxlats_comp))
          allocate
     1    (buff_mult_pieceso(lonr,ngrids_nsst,maxlats_comp,nodes))
          allocate
     1    (buff_mult_piecesa(lonr,1:ngrids_flx+1+ngrids_sfcc+ngrids_nsst
     1,    maxlats_comp,nodes))
      endif


c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c   SENDLOOP of grids from comp processors to I/O task.  The
c   I/O task may or may not be a comp task also.  The
c   send logic on that task is different for these two cases
c
c  big send
c     if(me .gt. -1) return
!
!     nstart = ngrids_flx+1+ngrids_sfcc+1
!     nend   = ngrids_flx+1+ngrids_sfcc+ngrids_nsst
      nstart = ngrids_sfcc+1
      nend   = ngrids_sfcc+ngrids_nsst

      buff_mult_pieceo(:,1:ngrids_nsst,1:lats_node_r)=
     1    buff_mult_piecea(:,nstart:nend,1:lats_node_r)
!
      IF (ME .ne. ioproc) THEN    !   Sending the data
         msgtag=me
         illen=lats_node _r
         CALL mpi_send            !  send the local grid domain
     &(buff_mult_pieceo,illen*lonr*ngrids_nsst,MPI_R_IO,ioproc,
     &                  msgtag,MPI_COMM_ALL,info)
      ELSE
        if( MC_COMP .ne. MPI_COMM_NULL) then
!
c iotask is also a compute task.  send is replaced with direct
c  array copy
!
           buff_mult_pieceso(:,:,1:lats_node_r,ioproc+1)=
     1     buff_mult_pieceo(:,:,1:lats_node_r)
!                              END COMPUTE TASKS PORTION OF LOGIC
        endif
!
c  END COMPUTE TASKS PORTION OF LOGIC
c  receiving part of I/O task
!
!!
!!      for pes ioproc
        DO proc=1,nodes_comp
          if (proc.ne.ioproc+1) then
            msgtag=proc-1
            illen=ivar_global_a(2,proc)
            print *,' pux target ',ubound(buff_mult_pieceso)
            CALL mpi_recv(buff_mult_pieceso(1,1,1,proc),
     1        illen*lonr*ngrids_nsst
     1        ,MPI_R_IO,proc-1,
     &                msgtag,MPI_COMM_ALL,stat,info)
          endif
        enddo
        buff_mult_piecesa(:,nstart:nend,:,:)=
     1    buff_mult_pieceso(:,1:ngrids_nsst,:,:)
      ENDIF
!!
      return
      end
      SUBROUTINE sfc_wrt(IOPROC,nw,cfile,xhour,idate
     &,                  global_lats_r,lonsperlar)
!!
      use sfcio_module
      use resol_def
      use mod_state
      use layout1
      use mpi_def
!     use mod_state , only : ngrids_sfcc
      implicit none
!!
      integer nw,IOPROC
      character*16 cfile
      real(kind=kind_io8) xhour
!!!   real(kind=kind_io4) buff4(lonr,latr,4)
      integer idate(4),k,il, ngridss
!     integer idate(4),k,il, ngrid, ngridss
!!
      CHARACTER*8 labfix(4)
      real(kind=kind_io4) yhour
      integer,save:: version
      data version/200501/
      INTEGER              GLOBAL_LATS_R(latr), lonsperlar(latr)
!
      type(sfcio_head) head
      type(sfcio_data) data
      integer iret
      logical first
      save head, first
      data first /.true./
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!    Build surface fields in to buff_mult
!
      ngrid=1
      do ngridss=1,ngrids_sfcc
!     print *,' inside sfc_wrt calling unsp ngridss=',ngridss
        call unsplit2z(ioproc,buff_mult(1,1,ngridss),global_lats_r)
      enddo
!    Building surface field is done
!
      if (me.eq.ioproc) then
!
        if (first) then
          head%clabsfc = CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//
     &                   CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)
          head%latb    = latr
          head%lonb    = lonr
          head%ivs     = ivssfc
!         head%irealf  = 1
          head%lsoil   = lsoil
          call sfcio_alhead(head,iret)
          head%lpl     = lonsperlar(1:latr/2)
          if (lsoil .eq. 4) then
            head%zsoil   = (/-0.1,-0.4,-1.0,-2.0/)
          elseif (lsoil .eq. 2) then
            head%zsoil   = (/-0.1,-2.0/)
          endif
          first = .false.
        endif
        head%fhour   = xhour
        head%idate   = idate
!
        PRINT 99,nw,xhour,IDATE
99      FORMAT(1H ,'in fixio nw=',i7,2x,'HOUR=',f8.2,3x,'IDATE=',
     &  4(1X,I4))
!
        ngrid = 1

        data%tsea=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%smc=>buff_mult(:,:,ngrid:ngrid+lsoil-1)
        ngrid=ngrid+lsoil
        data%sheleg=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%stc=>buff_mult(:,:,ngrid:ngrid+lsoil-1)
        ngrid=ngrid+lsoil
        data%tg3=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%zorl=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%alvsf=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%alvwf=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%alnsf=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%alnwf=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%slmsk=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%vfrac=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%canopy=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%f10m=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%t2m=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%q2m=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%vtype=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%stype=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%facsf=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%facwf=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%uustar=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%ffmm=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%ffhh=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
!c-- XW: FOR SEA-ICE Nov04
        data%hice=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%fice=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%tisfc=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
!c-- XW: END SEA-ICE Nov04
!
!lu: the addition of 8 Noah-related records starts here ...............
        data%tprcp=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%srflag=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%snwdph=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%slc=>buff_mult(:,:,ngrid:ngrid+lsoil-1)
        ngrid=ngrid+lsoil
        data%shdmin=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%shdmax=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%slope=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%snoalb=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
!lu: the addition of 8 Noah records ends here .........................
!        
   
        data%orog=>buff_mult(:,:,ngrid)      ! Orography
!
!       ngrid=ngrid+1
!
! Not needed for version 200501
!       data%cv=>buff_mult(:,:,ngrid)
!       data%cvb=>buff_mult(:,:,ngrid)
!       data%cvt=>buff_mult(:,:,ngrid)
!
        call sfcio_swohdc(nw,cfile,head,data,iret)
!
      endif
      return
      end

      SUBROUTINE nsst_wrt(IOPROC,nw,cfile,xhour,idate
     &,                  global_lats_r,lonsperlar)
cc
      use nsstio_module
      use namelist_def
      use resol_def
      use mod_state
      use layout1
      use mpi_def
      implicit none
!!
      integer nw,IOPROC
      character*16 cfile
      real(kind=kind_io8) xhour
      integer idate(4),k,il, ngridss
!!
      integer,save:: version
      data version/200710/
      INTEGER              GLOBAL_LATS_R(latr), lonsperlar(latr)
!
      type(nsstio_head) head
      type(nsstio_data) data
      integer iret
      logical first
      save head, first
      data first /.true./
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!    Build ocean fields in to buff_mult
!
!     ngrid=ngrids_sfcc+ngrids_flx+1+1
      ngrid=ngrids_sfcc+1
      do ngridss=ngrid,ngrid+ngrids_nsst-1
!       print *,' inside nsst_wrt calling unsp ngridss=',ngridss
        call unsplit2z(ioproc,buff_mult(1,1,ngridss),global_lats_r)
      enddo
!    Building ocean field is done
!
      if (me.eq.ioproc) then
!
        if (first) then
          head%clabnsst= CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//
     &                   CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)
          head%latb    = latr
          head%lonb    = lonr
          head%ivo     = ivsnsst
          head%lsea    = lsea
          call nsstio_alhead(head,iret)
          head%lpl     = lonsperlar(1:latr/2)
!         if (lsea == 0) then
!           head%zsea   = (/-0.1,-2.0/)
!         else
!         endif

          first = .false.
        endif
        head%fhour   = xhour
        head%idate   = idate
!
        PRINT 99,nw,xhour,IDATE
99      FORMAT(1H ,'in fixio nw=',i7,2x,'HOUR=',f8.2,3x,'IDATE=',
     &  4(1X,I4))
!
!       ngrid = 1
        ngrid = ngrids_sfcc+1

        data%slmsk    => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%tref     => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%dt_cool  => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%z_c      => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%dt_warm  => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%z_w      => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%C_0      => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%C_d      => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%W_0      => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%W_d      => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%ifd      => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%time_old => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%time_ins => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%I_Sw     => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%I_Q      => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%I_Qrain  => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%I_M      => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%I_Tau    => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%I_Sw_Zw  => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%I_Q_Ts   => buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%I_M_Ts   => buff_mult(:,:,ngrid)
!
        call nsstio_swohdc(nw,cfile,head,data,iret)
!
      endif
      return
      end

      SUBROUTINE wrtflx_a(IOPROC,noflx,ZHOUR,FHOUR,IDATE,colat1,
     &                  SECSWR,SECLWR, sfc_fld, flx_fld, fluxr,
     &                  global_lats_r,lonsperlar)
!!
      use resol_def
      use mod_state
      use layout1
      use sig_io
      use namelist_def
      use Sfc_Flx_ESMFMod
      implicit none
!!
      TYPE(Sfc_Var_Data)        :: sfc_fld
      TYPE(Flx_Var_Data)        :: flx_fld
      INTEGER              GLOBAL_LATS_R(LATR)
      INTEGER              lonsperlar(LATR)
      integer   IOPROC
!!
!     integer   IPRS,ITEMP,IZNLW,IMERW,ISPHUM,IPWAT,
!    $          IPCPR,ISNOWD,ICLDF,ICCLDF,
!    $          ISLMSK,IZORL,IALBDO,ISOILM,ICEMSK,
!    $          ILHFLX,ISHFLX,IZWS,IMWS,IGHFLX,
!    $          IUSWFC,IDSWFC,IULWFC,IDLWFC,
!    $          INSWFC,INLWFC,
!    $          IDSWVB,IDSWVD,IDSWNB,IDSWND,
!    $          ITMX,ITMN,IRNOF,IEP,
!    &          ICLDWK,IZGW,IMGW,IHPBL,
!    $          IDSWF,IDLWF,IUSWF,IULWF,ICPCPR,
!    $          ISFC,ITOA,IELEV,
!    $          ISGLEV,IDBLS,I2DBLS,ICOLMN,
!    $          IBLBL,IBLTL,IBLLYR,
!    $          ILCBL,ILCTL,ILCLYR,
!    $          IMCBL,IMCTL,IMCLYR,
!    $          IHCBL,IHCTL,IHCLYR,
!    $          ICVBL,ICVTL,ICVLYR,
!    $          INST,IWIN,IAVG,IACC,
!    $          IFHOUR,IFDAY,
!    $          LEN,NFLD,
!    $          IUVBF,IUVBFC,
!yth add ISUNTM for sunshine time sep/08
!    &          ISUNTM,
!    $   j,i,k,itop,ibot,k4,l,noflx
!    &,         ISIK                                    ! FOR SEA-ICE - XW Nov04
!Clu [+1L]: declare additional parameter index
!    +,         ISLC,ISNOD,ICNP
      integer LEN,NFLD
      integer j,i,k,itop,ibot,k4,l,noflx
!*RADFLX
!*    PARAMETER(NFLD=18)
      PARAMETER(NFLD=25)
       integer ilpds,iyr,imo,ida,ihr,ifhr,ithr,lg,ierr
       real (kind=kind_io8) RTIMER(NFLD),rtime,rtimsw,rtimlw
       real (kind=kind_io8) colat1
       real (kind=kind_io8) cl1,secswr,zhour,fhour,seclwr
C
!     PARAMETER(IPRS=1,ITEMP=11,IZNLW=33,IMERW=34,ISPHUM=51,IPWAT=54,
!    $          IPCPR=59,ISNOWD=65,ICLDF=71,ICCLDF=72,
!    $          ISLMSK=81,IZORL=83,IALBDO=84,ISOILM=144,ICEMSK=91,
!    $          ISIK=92,                                ! FOR SEA-ICE - XW Nov04
!    $          ILHFLX=121,ISHFLX=122,IZWS=124,IMWS=125,IGHFLX=155,
!    $          IUSWFC=160,IDSWFC=161,IULWFC=162,IDLWFC=163,
!    $          INSWFC=164,INLWFC=165,
!    $          IDSWVB=166,IDSWVD=167,IDSWNB=168,IDSWND=169,
!    $          ITMX=15,ITMN=16,IRNOF=90,IEP=145,
!    &          ICLDWK=146,IZGW=147,IMGW=148,IHPBL=221,
!    $          IDSWF=204,IDLWF=205,IUSWF=211,IULWF=212,ICPCPR=214,
!    $          IUVBF=200,IUVBFC=201)
!     PARAMETER(ISFC=1,ITOA=8,IELEV=105,
!    $          ISGLEV=107,IDBLS=111,I2DBLS=112,ICOLMN=200,
!    $          IBLBL=209,IBLTL=210,IBLLYR=211,
!    $          ILCBL=212,ILCTL=213,ILCLYR=214,
!    $          IMCBL=222,IMCTL=223,IMCLYR=224,
!    $          IHCBL=232,IHCTL=233,IHCLYR=234,
!    $          ICVBL=242,ICVTL=243,ICVLYR=244)

                                                                                     
!Clu [+1L]: define parameter index, using Table 130
!     PARAMETER(ISLC=160,ISNOD=66)
!Clu [+1L]: define parameter index, using Table 2
!     PARAMETER(ICNP=223)

!     PARAMETER(INST=10,IWIN=2,IAVG=3,IACC=4)
!     PARAMETER(IFHOUR=1,IFDAY=2)
!
!     PARAMETER(LEN=lonr*latr)
      real(kind=kind_io4) wrkga(lonr*latr),wrkgb(lonr*latr)
      real(kind=kind_io8) slmskful(lonr*latr)
      real(kind=kind_io8) slmskloc(LONR,LATS_NODE_R)
!hchuang code change add goro lan-sea output
      real(kind=kind_io8) goroloc(LONR,LATS_NODE_R)
!
!     LOGICAL(1) LBM(LEN)
!     CHARACTER G(200+LEN*(16+1)/8)
!     INTEGER   IPUR(NFLD),ITLR(NFLD)
!     DATA      IPUR/IULWF , IUSWF , IUSWF , IDSWF ,  ICLDF,   IPRS,
!    $                 IPRS, ITEMP ,  ICLDF,   IPRS,   IPRS, ITEMP ,
!    $                ICLDF,   IPRS,   IPRS, ITEMP ,  IUVBF, IUVBFC /
!     DATA      ITLR/ITOA  , ITOA  , ISFC  , ISFC  , IHCLYR, IHCTL ,
!    $               IHCBL , IHCTL , IMCLYR, IMCTL , IMCBL , IMCTL ,
!    $               ILCLYR, ILCTL , ILCBL , ILCTL , ISFC  , ISFC /
!     INTEGER     IDATE(4), IDS(255),IENS(5)
      INTEGER     IDATE(4)
      real (kind=kind_io8) SI(LEVP1)
!
!sela..................................................................
!*RADFLX*
!*    real (kind=kind_io8)   rflux(lonr,LATS_NODE_R,27)
      real (kind=kind_io8)   rflux(lonr,LATS_NODE_R,nfxr)
      real (kind=kind_io8)   glolal(lonr,LATS_NODE_R)
      real (kind=kind_io8)   buffo(lonr,LATS_NODE_R)
      real (kind=kind_io4)   buff1(lonr,latr)
      real (kind=kind_io4)   buff1l(lonr*latr)
!sela..................................................................
      real (kind=kind_io8)  FLUXR(nfxr,LONR,LATS_NODE_R)
!sela..................................................................
      integer kmsk(lonr,lats_node_r),kmsk0(lonr,lats_node_r)
      integer kmskcv(lonr,LATS_NODE_R)
!
!     integer nfluxes, il
!     parameter (nfluxes=153)
!     real(kind=kind_io4) rbufm
!     integer ibufm
!     common/wrtout_stuff/ibufm(50,nfluxes),rbufm(50,nfluxes)
!
      ngrid=ngrids_sfcc+1+ngrids_nsst
!
!     initialize ibufm
      ibufm(1,1)=-9
!     IDS=0
!     G=' '
!jfe
!!
      kmsk=nint(sfc_fld%slmsk)
      kmsk0=0
      CALL uninterprez(1,kmsk,glolal,sfc_fld%slmsk,
     &                global_lats_r,lonsperlar)
      slmskloc=glolal
      slmskful=buff1l
c
      do k=1,nfxr
       do j=1,LATS_NODE_R
        do i=1,lonr
         rflux(i,j,k)=fluxr(k,i,j)
        enddo
       enddo
      enddo
!!
!     CALL IDSDEF(1,IDS)
! UV-B scaling factor, if set up already, comment the next 2 lines out
!     ids(IUVBF)  = 2
!     ids(IUVBFC) = 2
! Ice conentration and thickness scaling factor
!     ids(icemsk) = 3      ! ICE CONCENTRATION ()
!     ids(isik)   = 2      ! ICE THICKNESS (M)
!
!     ILPDS=28
!     IF(ICEN2.EQ.2) ILPDS=45
!     IENS(1)=1
!     IENS(2)=IENST
!     IENS(3)=IENSI
!     IENS(4)=1
!     IENS(5)=255
!     IYR=IDATE(4)
!     IMO=IDATE(2)
!     IDA=IDATE(3)
!     IHR=IDATE(1)
!     IFHR=NINT(ZHOUR)
!     ITHR=NINT(FHOUR)
!
      IF(FHOUR.GT.ZHOUR) THEN
        RTIME=1./(3600.*(FHOUR-ZHOUR))
      ELSE
        RTIME=0.
      ENDIF
      IF(SECSWR.GT.0.) THEN
        RTIMSW=1./SECSWR
      ELSE
        RTIMSW=1.
      ENDIF
      IF(SECLWR.GT.0.) THEN
        RTIMLW=1./SECLWR
      ELSE
        RTIMLW=1.
      ENDIF
      RTIMER=RTIMSW
      RTIMER(1)=RTIMLW
!*RADFLX*
      RTIMER(20)=RTIMLW       ! CSULF_TOA
      RTIMER(22)=RTIMLW       ! CSDLF_SFC
      RTIMER(25)=RTIMLW       ! CSULF_SFC
!*RADFLX*
      CL1=colat1
!!
!..........................................................
      glolal=flx_fld%DUSFC*RTIME
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '01)Zonal compt of momentum flux (N/m**2) land and sea surface '
 
!..........................................................
      glolal=flx_fld%DVSFC*RTIME
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '02)Merid compt of momentum flux (N/m**2) land and sea surface '
!..........................................................
      glolal=flx_fld%DTSFC*RTIME
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '03)Sensible heat flux (W/m**2) land and sea surface           '
!..........................................................
      glolal=flx_fld%DQSFC*RTIME
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '04)Latent heat flux (W/m**2) land and sea surface             '
!..........................................................
      CALL uninterprez(2,kmsk0,buffo,sfc_fld%tsea,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribsn ierr=',ierr,'  ',
!    x '05)Temperature (K) land and sea surface                       '
!..........................................................
      glolal(:,:) = sfc_fld%SMC(1,:,:)
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribsn ierr=',ierr,'  ',
!    x '06)Volumetric soil moist content (frac) layer 10cm and 0cm    '
!..........................................................
      glolal(:,:) = sfc_fld%SMC(2,:,:)
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!lu  x '07)Volumetric soil moist content (frac) layer 200cm and 10cm  '
!    + '07)Volumetric soil moist content (frac) layer 40cm and 10cm  '
!..........................................................
      glolal(:,:) = sfc_fld%STC(1,:,:)
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '08)Temp (K) layer betw two depth below land sfc 10cm and 0cm  '
!..........................................................
      glolal(:,:) = sfc_fld%STC(2,:,:)
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!lu  x '09)Temp (K) layer betw two depth below land sfc 200cm and 10cm'
!    + '09)Temp (K) layer betw two depth below land sfc 40cm and 10cm'
!..........................................................
      CALL uninterprez(2,kmsk,buffo,sfc_fld%sheleg,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '10)Water equiv of accum snow depth (kg/m**2) land sea surface '
c..........................................................
      glolal = flx_fld%DLWSFC*RTIME
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '11)Downward long wave radiation flux (W/m**2) land sea surface'
!..........................................................
      glolal = flx_fld%ULWSFC*RTIME
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '12)Upward long wave radiation flux (W/m**2) land sea surface  '
!..........................................................
!.......  FIX FLUXES FOR APPROX DIURNAL CYCLE
      DO 113 K=1,4
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j) = rflux(i,j,k)*RTIMER(k)
        enddo
       enddo
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0.and.k.eq.1)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '13)Upward long wave radiation flux (W/m**2) top of atmosphere '
!     if(ierr.ne.0.and.k.eq.2)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '14)Upward solar radiation flux (W/m**2) top of atmosphere     '
!     if(ierr.ne.0.and.k.eq.3)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '15)Upward solar radiation flux (W/m**2) land and sea surface  '
!     if(ierr.ne.0.and.k.eq.4)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '16)Downward solar radiation flux (W/m**2) land and sea surface'
  113 CONTINUE
!..........................................................
!
!     For UV-B fluxes
!
      do j=1,LATS_NODE_R
        do i=1,lonr
          glolal(i,j) = rflux(i,j,21)*rtimsw
        enddo
      enddo
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '17)UV-B Downward solar flux (W/m**2) land sea surface'
      do j=1,LATS_NODE_R
        do i=1,lonr
          glolal(i,j) = rflux(i,j,22)*rtimsw
        enddo
      enddo
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '18)clear sky UV-B Downward solar flux (W/m**2) land sea surface'
!
!     End UV-B fluxes
!
!..........................................................
!..........................................................
      DO 813 K=5,7
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j) = rflux(i,j,k)*100.*rtimsw
        enddo
       enddo
      where(glolal.ge.0.5)
        kmskcv = 1
      elsewhere
        kmskcv = 0
      endwhere
!!
      CALL uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '19)Total cloud cover (percent) high cloud layer               '
!     if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '23)Total cloud cover (percent) middle cloud layer             '
!     if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '27)Total cloud cover (percent) low cloud layer                '
!
        K4=4+(K-5)*4
        L=K4+1
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         if(rflux(i,j,k).gt.0.)then
          glolal(i,j) = rflux(i,j,k+3)*1000./rflux(i,j,k)
         else
          glolal(i,j) = 0.
         endif
        enddo
       enddo
      CALL uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '20)Pressure (Pa) high cloud top level                         '
!     if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '24)Pressure (Pa) middle cloud top level                       '
!     if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '28)Pressure (Pa) low cloud top level                          '
        L=K4+2
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         if(rflux(i,j,k).gt.0.)then
          glolal(i,j) = rflux(i,j,k+6)*1000./rflux(i,j,k)
         else
          glolal(i,j) = 0.
         endif
        enddo
       enddo
      CALL uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '21)Pressure (Pa) high cloud bottom level                      '
!     if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '25)Pressure (Pa) middle cloud bottom level                    '
!     if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '29)Pressure (Pa) low cloud bottom level                       '
        L=K4+3
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         if(rflux(i,j,k).gt.0.)then
          glolal(i,j) = rflux(i,j,k+9)/rflux(i,j,k)
         else
          glolal(i,j) = 0.
         endif
        enddo
       enddo
      CALL uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '22)Temperature (K) high cloud top level                       '
!     if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '26)Temperature (K) middle cloud top level                     '
!     if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '30)Temperature (K) low cloud top level                        '
        L=K4+4
!
  813 CONTINUE
!!
!...................................................................
!                        GESHEM unit in m, final unit = mm/s = kg/m2/s
      glolal = flx_fld%GESHEM*1.E3*RTIME
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '31)Precipitation rate (kg/m**2/s) land and sea surface        '
c...................................................................
      glolal = flx_fld%BENGSH*1.E3*RTIME
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '32)Convective precipitation rate (kg/m**2/s) land sea surface '
!...................................................................
      glolal = flx_fld%GFLUX*RTIME
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '33)Ground heat flux (W/m**2) land and sea surface             '
!...................................................................
!     buffo=MOD(slmskloc,2._kind_io8)
!gwv   add something here
!     do j=1,lats_node_r
!       do i=1,lonr
!         buff_mult_piecea(i,ngrid,j)=buffo(i,j)
!       end do
!     end do
!     ngrid=ngrid+1
!...................................................................
!     Add land/sea mask here
      buffo=MOD(slmskloc,2._kind_io8)
      do j=1,lats_node_r
        do i=1,lonr
          buff_mult_piecea(i,ngrid,j)=buffo(i,j)
        end do
      end do
        ngrid=ngrid+1
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '34)Land-sea mask (1=land; 0=sea) (integer) land sea surface   '
!gwv   add something here
!
!c-- XW: FOR SEA-ICE Nov04
      CALL uninterprez(2,kmsk0,buffo,sfc_fld%fice,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '35)Ice concentration (ice>0; no ice=0) (1/0) land sea surface '
!c-- XW: END SEA-ICE
!...................................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%u10m,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '36)u wind (m/s) height above ground                           '
!...................................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%v10m,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '37)v wind (m/s) height above ground                           '
!...................................................................
      CALL uninterprez(2,kmsk0,buffo,sfc_fld%t2m,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '38)Temperature (K) height above ground                        '
!...................................................................
      CALL uninterprez(2,kmsk0,buffo,sfc_fld%q2m,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '39)Specific humidity (kg/kg) height above ground              '
!...................................................................
      glolal = flx_fld%PSURF*1.E3
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '40)Pressure (Pa) land and sea surface                         '
!...................................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%tmpmax,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '41)Maximum temperature (K) height above ground                '
!...................................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%tmpmin,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '42)Minimum temperature (K) height above ground                '
!...................................................................
!jwang add spfhmax/spfhmin
      CALL uninterprez(2,kmsk0,buffo,flx_fld%spfhmax,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '41a)Maximum specific humidity (kg/kg) height above ground      '
!...................................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%spfhmin,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '42a)Minimum specific humidity (kg/kg) height above ground      '
!...................................................................
!                   Output unit of runoff is kg/m2 (accumulative value)
      glolal = flx_fld%RUNOFF * 1.E3
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '43)Runoff (kg/m**2) land and sea surface                      '
!...................................................................
      glolal = flx_fld%EP * RTIME
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '44)Potential evaporation rate (w/m**/) land and sea surface   '
!...................................................................
      glolal = flx_fld%CLDWRK * RTIME
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '45)Cloud work function (J/Kg) total atmospheric column        '
!...................................................................
      glolal = flx_fld%DUGWD*RTIME
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '46)Zonal gravity wave stress (N/m**2) land and sea surface    '
!...................................................................
      glolal = flx_fld%DVGWD*RTIME
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '47)Meridional gravity wave stress (N/m**2) land sea surface   '
!...................................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%hpbl,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '48)Boundary layer height '
!...................................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%pwat,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '49)Precipitable water (kg/m**2) total atmospheric column      '
!...................................................................
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         if (rflux(i,j,4).GT.0.) then
          glolal(i,j) = rflux(i,j,3)/rflux(i,j,4) * 100.
          if (glolal(i,j).GT.100.) glolal(i,j) = 100.
         else
          glolal(i,j) = 0.
         endif
        enddo
       enddo
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '50)Albedo (percent) land and sea surface                      '
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j) = rflux(i,j,26)*100.*rtimsw
        enddo
       enddo
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '51)Total cloud cover (percent) total atmospheric column       '
!
! CONVECTIVE CLOUDS
! LABELED INSTANTANEOUS BUT ACTUALLY AVERAGED OVER FHSWR HOURS
!
      glolal = sfc_fld%CV*1.E2
      where(glolal.ge.0.5)
        kmskcv = 1
      elsewhere
        kmskcv = 0
      endwhere
      CALL uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '52)Total cloud cover (percent) convective cloud layer         '
!.................................................
       do j=1,LATS_NODE_R
        do i=1,lonr
        glolal(i,j) = 0.
        IF(sfc_fld%CV(i,j).GT.0.) THEN
!        ITOP=NINT(CVT(i,j))
!        IF(ITOP.GE.1.AND.ITOP.LE.LEVS)
!    &   glolal(i,j)=SI(ITOP+1)*PSURF(i,j)*1.E3
!...      cvt already a pressure (cb)...convert to Pa
         glolal(i,j) = sfc_fld%CVT(i,j)*1.E3
        END IF
       ENDDO
      ENDDO
      CALL uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '53)Pressure (Pa) convective cloud top level                   '
!.................................................
       do j=1,LATS_NODE_R
        do i=1,lonr
        glolal(i,j) = 0.
        IF(sfc_fld%CV(i,j).GT.0.) THEN
!        Ibot=NINT(CVB(i,j))
!        IF(Ibot.GE.1.AND.Ibot.LE.LEVS)
!    &   glolal(i,j)=SI(IBOT)*PSURF(i,j)*1.E3
!...      cvb already a pressure (cb)...convert to Pa
         glolal(i,j) = sfc_fld%CVB(i,j)*1.E3
        END IF
       ENDDO
      ENDDO
      CALL uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '54)Pressure (Pa) convective cloud bottom level                '
!.................................................
!...   SAVE B.L. CLOUD AMOUNT
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j) = rflux(i,j,27)*100.*rtimsw
        enddo
       enddo
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '55)Total cloud cover (percent) boundary layer cloud layer     '
!c-- XW: FOR SEA-ICE Nov04
      CALL uninterprez(2,kmsk0,buffo,sfc_fld%hice,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '56)Sea ice thickness (m) category 1'
!c-- XW: END SEA-ICE
!.................................................
!lu: add smc(3:4), stc(3:4), slc(1:4), snwdph, canopy
!lu: addition of 10 records starts here -------------------------------
      if(lsoil.gt.2)then
        glolal(:,:) = sfc_fld%SMC(3,:,:)
        CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!       if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x   '57)Volumetric soil moist content (frac) layer 100cm and 40cm '
!..........................................................
        glolal(:,:) = sfc_fld%SMC(4,:,:)
        CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!       if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x   '58)Volumetric soil moist content (frac) layer 200cm and 100cm '
!..........................................................
        glolal(:,:) = sfc_fld%STC(3,:,:)
        CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!       if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x   '59)Temp (K) layer betw two depth below land sfc 100cm and 40cm'
!..........................................................
        glolal(:,:) = sfc_fld%STC(4,:,:)
        CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!       if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x   '60)Temp (K) layer betw two depth below land sfc 200cm and 100cm'
      endif
!..........................................................
      glolal(:,:) = sfc_fld%SLC(1,:,:)
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '61)Liquid soil moist content (frac) layer 10cm and 0cm  '
!..........................................................
      glolal(:,:) = sfc_fld%SLC(2,:,:)
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '62)Liquid soil moist content (frac) layer 40cm and 10cm '
!..........................................................
      if(lsoil.gt.2)then
        glolal(:,:) = sfc_fld%SLC(3,:,:)
        CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!       if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x   '63)Liquid soil moist content (frac) layer 100cm and 40cm'
!..........................................................
        glolal(:,:) = sfc_fld%SLC(4,:,:)
        CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!       if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x   '64)Liquid soil moist content (frac) layer 200cm and 100cm'
      endif
!..........................................................
      glolal = sfc_fld%SNWDPH / 1.E3       !! convert from mm to m
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '65)Snow depth (m) land surface                  '
c..........................................................
!     LBM=slmskful.EQ.1._kind_io8
      CALL uninterprez(2,kmsk,buffo,sfc_fld%canopy,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '66)Canopy water content (kg/m^2) land surface      '
!lu: addition of 10 records ends here -------------------------------
!
!wei: addition of 30 records starts here -------------------------------
      glolal = sfc_fld%ZORL / 1.E2       !! convert from cm to m
      CALL uninterprez(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '67)Surface roughness (m)       '
!..........................................................
      glolal = sfc_fld%vfrac*100.
      CALL uninterprez(1,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '68)Vegetation fraction (fractional) land surface      '
!..........................................................
      CALL uninterprez(1,kmsk,glolal,sfc_fld%vtype,
     &                 global_lats_r,lonsperlar)
!     buffo=MOD(glolal,2._kind_io8)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '69)Vegetation type land surface      '
!..........................................................
      CALL uninterprez(1,kmsk,glolal,sfc_fld%stype,
     &                 global_lats_r,lonsperlar)
!     buffo=MOD(glolal,2._kind_io8)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '70)Soil type land surface      '
!..........................................................
      CALL uninterprez(1,kmsk,glolal,sfc_fld%slope,
     &                 global_lats_r,lonsperlar)
!     buffo=MOD(glolal,2._kind_io8)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '71)Slope type land surface      '
!..........................................................
      CALL uninterprez(2,kmsk0,buffo,sfc_fld%uustar,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '72)Frictional velocity (m/s)     '
!..........................................................
      CALL uninterprez(1,kmsk,buffo,sfc_fld%oro,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '73)Surface height (m)       '
!..........................................................
      CALL uninterprez(1,kmsk,buffo,sfc_fld%srflag,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '74)Freezing precip flag land surface      '
!..........................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%chh,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '75)Exchange coefficient CH(m/s)       '
!..........................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%cmm,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '76)Exchange coefficient CM(m/s)       '
!..........................................................
      CALL uninterprez(2,kmsk,buffo,flx_fld%EPI,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '77)Potential evaporation rate (w/m**2) land and sea surface   '
!..........................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%DLWSFCI,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '78)Downward long wave radiation flux (W/m**2) '
!..........................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%ULWSFCI,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '79)Upward long wave radiation flux (W/m**2)  '
!..........................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%USWSFCI,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '80)Upward short wave radiation flux (W/m**2)  '
!..........................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%DSWSFCI,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '81)Downward short wave radiation flux (W/m**2)   '
!..........................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%DTSFCI,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '82)Sensible heat flux (W/m**2) land and sea surface       '
!..........................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%DQSFCI,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '83)Latent heat flux (W/m**2) land and sea surface         '
!..........................................................
      CALL uninterprez(2,kmsk,buffo,flx_fld%GFLUXI,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '84)Ground heat flux (W/m**2) land and sea surface         '
!..........................................................
      glolal = flx_fld%SRUNOFF * 1.E3
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '85)Surface runoff (kg/m^2) land surface      '
!..........................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%t1,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '86)Lowest model level Temp (K)      '
!..........................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%q1,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '87)Lowest model specific humidity (kg/kg)    '
!..........................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%u1,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '88)Lowest model u wind (m/s)      '
!..........................................................
      CALL uninterprez(2,kmsk0,buffo,flx_fld%v1,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '89)Lowest model v wind (m/s)       '
!..........................................................
      CALL uninterprez(2,kmsk,buffo,flx_fld%zlvl,
     &                 global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '90)Lowest model level height (m) land surface      '
!..........................................................
      glolal = flx_fld%EVBSA*RTIME
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '91)Direct evaporation from bare soil(W/m^2) land surface      '
!..........................................................
      glolal = flx_fld%EVCWA*RTIME
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '92)Canopy water evaporation(W/m^2) land surface      '
!..........................................................
      glolal = flx_fld%TRANSA*RTIME
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '93)Transpiration (W/m^2) land surface      '
!..........................................................
      glolal = flx_fld%SBSNOA*RTIME
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '94)Snow Sublimation (W/m^2) land surface      '
!..........................................................
      glolal = flx_fld%SNOWCA*RTIME*100.
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '95)Snow Cover (fraction) land surface      '
!..........................................................
      glolal = flx_fld%soilm*1.E3       !! convert from m to (mm)kg/m^2
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '96)Total column soil moisture (Kg/m^2) land surface      '



Cwei: addition of 30 records ends here -------------------------------

!*RADFLX*
!Clu: addition of 7 records starts here -------------------------------
!dswrf_toa, csulf_toa, csusf_toa, csdlf_sfc,csusf_sfc, csdsf_sfc, csulf_sfc
!
      DO 115 K=19, 25
       if(K .eq. 19)  then
          L = 18
        else
          L = K + 8
       endif
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j)=rflux(i,j,L)*RTIMER(K)
        enddo
       enddo
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0.and.k.eq.19)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '97)Downward solar radiation flux (W/m**2) TOA '
!     if(ierr.ne.0.and.k.eq.20)print*,'wrtsfc gribit ierr=',ierr,'  ',

!    x '98)CS upward long wave radiation flux (W/m**2) TOA '
!     if(ierr.ne.0.and.k.eq.21)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '99)CS upward solar radiation flux (W/m**2) TOA     '
!     if(ierr.ne.0.and.k.eq.22)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '100)CS downward long radiation flux (W/m**2) SFC  '
!     if(ierr.ne.0.and.k.eq.23)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '101)CS upward solar radiation flux (W/m**2)  SFC '
!     if(ierr.ne.0.and.k.eq.24)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '102)CS downward solar radiation flux (W/m**2) SFC'
!     if(ierr.ne.0.and.k.eq.25)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '103)CS upward long wave radiation flux (W/m**2) SFC '

  115 CONTINUE
!..........................................................
      glolal = flx_fld%snohfa*RTIME     
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '104)Snow phase-change heat flux [W/m^2] land surface      '
!..........................................................
      glolal = flx_fld%smcwlt2
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '105)Wilting point [fraction] land surface      '
!..........................................................
      glolal = flx_fld%smcref2
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '106)Field capacity [fraction] land surface      '
!..........................................................

!lu: addition of 7 records ends here ---------------------------------
!..........................................................
!
!    accumulated sunshine time
!
      glolal = flx_fld%suntim
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '107)Accumulated sunshine duration (sec)'
!
!    end sunshine time
!
!...................................................................
! Output additional variable (averaged quantity) for GOCART
! If LGGFS3D = .TRUE.
!
      IF ( LGGFS3D ) THEN
!
!hchuang code change [+16L] 11/12/2007 :
!..........................................................
      glolal=flx_fld%gsoil*rtime !! gsoil already in mm (kg/m^2)
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '108)Average VOL soil moist content(frac) layer 10cm -> 0cm'
!..........................................................
      glolal=flx_fld%gtmp2m*rtime
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '109)Average temperature at 2 meter (K)                    '
!..........................................................
      glolal=flx_fld%gustar*rtime
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '110)Average Frictional Velocity (m/s)                     '
!..........................................................
      glolal=flx_fld%gpblh*rtime
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '111)Average Boundary layer height                        '
!..........................................................
      glolal=flx_fld%gu10m*rtime
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '112)Average u wind (m/s) height 10m above ground         '
!..........................................................
      glolal=flx_fld%gv10m*rtime
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '113)Average v wind (m/s) height 10m above ground         '
!..........................................................
!hchuang confirmed by Helin, correct bug, zorl unit in cm not mm
! BUG      glolal=gzorl*1.0E-3*rtime  !! convert from mm to m
      glolal=flx_fld%gzorl*1.0E-2*rtime  !! convert from cm to m
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '114)Average Surface roughness (m)
!
      END IF
!..........................................................
!
!hchuang when callng sub  uninterprez, array glolal is assign to
!        buff_mult_piecea at the ngrid location, then ngrid advanced
!        by 1.  Before assign the modified value (buffo) to
!        buff_mult_piecea again dial ngrid back by 1 for the correct
!        ngrid index otherwise, you risk the chance that ngrid might
!        > ngrids_flx+1  which cause the address error or arry over-run
!
!hchuang code change [+2]  when callng sub  uninterprez, array glolal is assign
to
        glolal=flx_fld%goro*rtime
        CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '115)Average Land Sea surface (fraction)                  '
!
!..........................................................

      if(me.eq.ioproc)
     &   PRINT *,'GRIB FLUX FILE WRITTEN ',FHOUR,IDATE,noflx
!!
      RETURN
      END
      SUBROUTINE wrtflx_w(IOPROC,noflx,ZHOUR,FHOUR,IDATE,colat1,SECSWR,
     &                  SECLWR,slmsk, global_lats_r,lonsperlar)
!
      use resol_def
      use mod_state
      use layout1
      use sig_io
      use namelist_def
      implicit none
!!
      INTEGER              GLOBAL_LATS_R(LATR)
      INTEGER              lonsperlar(LATR)
      integer   IOPROC
!!
      integer   IPRS,ITEMP,IZNLW,IMERW,ISPHUM,IPWAT,
     $          IPCPR,ISNOWD,ICLDF,ICCLDF,
     $          ISLMSK,IZORL,IALBDO,ISOILM,ISNOHF,ISMCWLT,
     $          ISMCREF,ICEMSK,
     $          ILHFLX,ISHFLX,IZWS,IMWS,IGHFLX,
     $          IUSWFC,IDSWFC,IULWFC,IDLWFC,
     $          INSWFC,INLWFC,
     $          IDSWVB,IDSWVD,IDSWNB,IDSWND,
     $          ITMX,ITMN,IRNOF,IEP,
!jwang add iqmx, iqmn
     $          IQMX,IQMN,
     &          ICLDWK,IZGW,IMGW,IHPBL,
     $          IDSWF,IDLWF,IUSWF,IULWF,ICPCPR,
     $          ISFC,ITOA,IELEV,
     $          ISGLEV,IDBLS,I2DBLS,ICOLMN,
     $          IBLBL,IBLTL,IBLLYR,
     $          ILCBL,ILCTL,ILCLYR,
     $          IMCBL,IMCTL,IMCLYR,
     $          IHCBL,IHCTL,IHCLYR,
     $          ICVBL,ICVTL,ICVLYR,
     $          INST,IWIN,IAVG,IACC,
     $          IFHOUR,IFDAY,
!    $          LEN,NFLD,
     $          NFLD,
     $          IUVBF,IUVBFC,
!yth add ISUNTM for sunshine time sep/08
     &          ISUNTM,
     $   j,i,k,itop,ibot,k4,l,noflx
     &,  isik, islc, isnod, icnp
     &,  iveg, ivtp, istp, islo,iust,ihgt,irst,ichh
     &,  icmm,isrf,ievbs,ievcw,itran,isbs,isnc,istc
!*RADFLX*
     +,  ICSUSW,ICSDSW,ICSULW,ICSDLW

!*RADFLX*
!     PARAMETER(NFLD=16)
!*    PARAMETER(NFLD=18)
      PARAMETER(NFLD=25)
       integer ilpds,iyr,imo,ida,ihr,ifhr,ithr,lg,ierr
       real (kind=kind_io8) RTIMER(NFLD),rtime,rtimsw,rtimlw
       real (kind=kind_io8) colat1
       real (kind=kind_io8) cl1,secswr,zhour,fhour,seclwr
!
      PARAMETER(IPRS=1,ITEMP=11,IZNLW=33,IMERW=34,ISPHUM=51,IPWAT=54,
     $          IPCPR=59,ISNOWD=65,ICLDF=71,ICCLDF=72,
     $          ISLMSK=81,IZORL=83,IALBDO=84,ISOILM=144,ICEMSK=91,
     $          ISIK=92,                                ! FOR SEA-ICE - XW Nov04
     $          ILHFLX=121,ISHFLX=122,IZWS=124,IMWS=125,IGHFLX=155,
     $          IUSWFC=160,IDSWFC=161,IULWFC=162,IDLWFC=163,
     $          INSWFC=164,INLWFC=165,
     $          IDSWVB=166,IDSWVD=167,IDSWNB=168,IDSWND=169,
     $          ITMX=15,ITMN=16,IRNOF=90,IEP=145,
!jwang add iqmx, iqmn
     $          IQMX=204,IQMN=205,
     &          ICLDWK=146,IZGW=147,IMGW=148,IHPBL=221,
     $          IDSWF=204,IDLWF=205,IUSWF=211,IULWF=212,ICPCPR=214,
!*RADFLX
!*   &          IUVBF=200,IUVBFC=201)
!    +          IUVBF=200,IUVBFC=201,
!yth  add ISUNTM for sunshine time  sep/08
     &          IUVBF=200,IUVBFC=201,ISUNTM=191,
     +          ICSUSW=160,ICSDSW=161,ICSULW=162,ICSDLW=163)
      PARAMETER(ISFC=1,ITOA=8,IELEV=105,
     $          ISGLEV=109,IDBLS=111,I2DBLS=112,ICOLMN=200,
!Cwei    $          ISGLEV=107,IDBLS=111,I2DBLS=112,ICOLMN=200,
     $          IBLBL=209,IBLTL=210,IBLLYR=211,
     $          ILCBL=212,ILCTL=213,ILCLYR=214,
     $          IMCBL=222,IMCTL=223,IMCLYR=224,
     $          IHCBL=232,IHCTL=233,IHCLYR=234,
     $          ICVBL=242,ICVTL=243,ICVLYR=244)

!Clu [+1L]: define parameter index, using Table 130
      PARAMETER(ISLC=160,ISNOD=66)
!Cwei
      PARAMETER(ISLO=222,ISBS=198,ISNC=238,ICMM=179)
      PARAMETER(ISNOHF=229,ISMCWLT=219,ISMCREF=220)
!Clu [+1L]: define parameter index, using Table 2
      PARAMETER(ICNP=223)
!Cwei
      PARAMETER(IVEG=87,IVTP=225,ISTP=224,IUST=253,IHGT=7,
     $          IRST=140,ICHH=208,ISRF=235,IEVBS=199,
     $          IEVCW=200,ITRAN=210,ISTC=86)

      PARAMETER(INST=10,IWIN=2,IAVG=3,IACC=4)
      PARAMETER(IFHOUR=1,IFDAY=2)
!     PARAMETER(LEN=lonr*latr)
      real(kind=kind_io4) wrkga(lonr*latr),wrkgb(lonr*latr)
      real(kind=kind_io8) slmskful(lonr*latr)
      real(kind=kind_io8) slmskloc(LONR,LATS_NODE_R)
c
      LOGICAL(1) LBM(lonr*latr)
      CHARACTER G(200+lonr*latr*(16+1)/8)
      INTEGER   IPUR(NFLD),ITLR(NFLD)
      DATA      IPUR/IULWF , IUSWF , IUSWF , IDSWF ,  ICLDF,   IPRS,
     $                 IPRS, ITEMP ,  ICLDF,   IPRS,   IPRS, ITEMP ,
!*RADFLX*
!*   $                ICLDF,   IPRS,   IPRS, ITEMP ,  IUVBF, IUVBFC /
     +                ICLDF,   IPRS,   IPRS, ITEMP ,  IUVBF, IUVBFC,
     +                IDSWF, ICSULW, ICSUSW, ICSDLW, ICSUSW, ICSDSW,
     +                ICSULW/
!    $                ICLDF,   IPRS,   IPRS, ITEMP /
      DATA      ITLR/ITOA  , ITOA  , ISFC  , ISFC  , IHCLYR, IHCTL ,
     $               IHCBL , IHCTL , IMCLYR, IMCTL , IMCBL , IMCTL ,
!*RADFLX*
!*   $               ILCLYR, ILCTL , ILCBL , ILCTL , ISFC  , ISFC /
     +               ILCLYR, ILCTL , ILCBL , ILCTL , ISFC  , ISFC ,
     +               ITOA  ,  ITOA ,  ITOA ,  ISFC , ISFC  , ISFC,
     +               ISFC /
!    $               ILCLYR, ILCTL , ILCBL , ILCTL /
      INTEGER     IDATE(4), IDS(255)
!     INTEGER     IDATE(4), IDS(255),IENS(5)
      real (kind=kind_io8) SI(LEVP1)
C
csela..................................................................
!     real (kind=kind_io8)   rflux(lonr,LATS_NODE_R,27)
!     real (kind=kind_io8)   glolal(lonr,LATS_NODE_R)
!     real (kind=kind_io8)   buffo(lonr,LATS_NODE_R)
!     real (kind=kind_io4)   buff1(lonr,latr)
      real (kind=kind_io4)   buff1l(lonr*latr)
csela..................................................................
!     real (kind=kind_io8)  FLUXR(nfxr,LONR,LATS_NODE_R)
      REAL (KIND=KIND_IO8) SLMSK (LONR,LATS_NODE_R)
csela..................................................................
      integer kmsk(lonr,lats_node_r),kmsk0(lonr,lats_node_r)
      integer kmskcv(lonr,LATS_NODE_R),il
!       ngrid=0
!       ngrid=0+ngrids_sfcc+1
        ngrid=0+ngrids_sfcc+1+ngrids_nsst
cjfe
      IDS=0
      G=' '
cjfe
!!
      kmsk=nint(slmsk)
      kmsk0=0
      call unsplit2z(ioproc,buff1l,global_lats_r)
      slmskful=buff1l
!
!     do k=1,nfxr
!      do j=1,LATS_NODE_R
!       do i=1,lonr
!        rflux(i,j,k)=fluxr(k,i,j)
!       enddo
!      enddo
!     enddo
!!
      CALL IDSDEF(1,IDS)
!jwang add spfhmax/spfhmin
      ids(IQMX)   = 5
      ids(IQMN)   = 5
! UV-B scaling factor, if set up already, comment the next 2 lines out
      ids(IUVBF)  = 2
      ids(IUVBFC) = 2
! Ice conentration and thickness scaling factor
      ids(icemsk) = 3      ! ICE CONCENTRATION ()
      ids(isik)   = 2      ! ICE THICKNESS (M)
!
!wei added 10/24/2006
      ids(IZORL)  = 4
      ids(IHGT)   = 3
      ids(IVEG)   = 2
      ids(IUST)   = 3
      ids(ICHH)   = 4
      ids(ICMM)   = 4
      ids(ISRF)   = 5
      ids(ITEMP)  = 3
      ids(ISPHUM) = 6
      ids(IZNLW)  = 2
      ids(IMERW)  = 2
      ids(ISNC)   = 3
      ids(ISTC)   = 4
      ids(ISOILM) = 4
      ids(ISNOD)  = 6
      ids(ISNOWD) = 5
      ids(ICNP)   = 5
      ids(IPCPR)  = 6
      ids(ICPCPR) = 6
      ids(IRNOF)  = 5
      ids(ISMCWLT)  = 4
      ids(ISMCREF)  = 4
!
      ILPDS = 28
      IF(ICEN2.EQ.2) ILPDS = 45
      IENS(1) = 1
      IENS(2) = IENST
      IENS(3) = IENSI
      IENS(4) = 1
      IENS(5) = 255
      IYR     = IDATE(4)
      IMO     = IDATE(2)
      IDA     = IDATE(3)
      IHR     = IDATE(1)
      IFHR    = NINT(ZHOUR)
      ITHR    = NINT(FHOUR)
      IF(FHOUR.GT.ZHOUR) THEN
        RTIME = 1./(3600.*(FHOUR-ZHOUR))
      ELSE
        RTIME = 0.
      ENDIF
      IF(SECSWR.GT.0.) THEN
        RTIMSW = 1./SECSWR
      ELSE
        RTIMSW=1.
      ENDIF
      IF(SECLWR.GT.0.) THEN
        RTIMLW=1./SECLWR
      ELSE
        RTIMLW=1.
      ENDIF
      RTIMER=RTIMSW
      RTIMER(1)=RTIMLW
!*RADFLX*
      RTIMER(20)=RTIMLW       ! CSULF_TOA
      RTIMER(22)=RTIMLW       ! CSDLF_SFC
      RTIMER(25)=RTIMLW       ! CSULF_SFC
!*RADFLX*
      CL1=colat1
!!
!..........................................................
!     glolal=DUSFC*RTIME
      call unsplit2z(ioproc,wrkga,global_lats_r)
!
      if(me.eq.ioproc) then
!     print *,' ngrid for u flx=',ngrid
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IZWS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IZWS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
!         print *, ' called wryte unit noflx' ,noflx,ngrid
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '01)Zonal compt of momentum flux (N/m**2) land and sea surface '
        endif
      endif
 
!..........................................................
!     glolal=DVSFC*RTIME
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IMWS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IMWS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '02)Merid compt of momentum flux (N/m**2) land and sea surface '
        endif
      endif
!..........................................................
!     glolal=DTSFC*RTIME
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISHFLX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '03)Sensible heat flux (W/m**2) land and sea surface           '
        endif
      endif
!..........................................................
!     glolal=DQSFC*RTIME
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ILHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ILHFLX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '04)Latent heat flux (W/m**2) land and sea surface             '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ITEMP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '05)Temperature (K) land and sea surface                       '
          stop
        endif
      endif
!..........................................................
!     glolal(:,:)=SMC(1,:,:)
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        LBM=slmskful.EQ.1._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISOILM,I2DBLS,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '06)Volumetric soil moist content (frac) layer 10cm and 0cm    '
        endif
      endif
!..........................................................
!     glolal(:,:)=SMC(2,:,:)
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        LBM=slmskful.EQ.1._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
!lu  &            1,ISOILM,I2DBLS,10,200,IYR,IMO,IDA,IHR,
     +            1,ISOILM,I2DBLS,10,40,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
!lu  x '07)Volumetric soil moist content (frac) layer 200cm and 10cm  '
     + '07)Volumetric soil moist content (frac) layer 40cm and 10cm  '
        endif
      endif
!..........................................................
!     glolal(:,:)=STC(1,:,:)
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        LBM=slmskful.EQ.1._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ITEMP,I2DBLS,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '08)Temp (K) layer betw two depth below land sfc 10cm and 0cm  '
        endif
      endif
!..........................................................
!     glolal(:,:)=STC(2,:,:)
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        LBM=slmskful.EQ.1._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
!lu  &            1,ITEMP,I2DBLS,10,200,IYR,IMO,IDA,IHR,
     +            1,ITEMP,I2DBLS,10,40,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
!lu  x '09)Temp (K) layer betw two depth below land sfc 200cm and 10cm'
     + '09)Temp (K) layer betw two depth below land sfc 40cm and 10cm'
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISNOWD,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISNOWD),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '10)Water equiv of accum snow depth (kg/m**2) land sea surface '
        endif
      endif
!..........................................................
!     glolal=DLWSFC*RTIME
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IDLWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IDLWF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '11)Downward long wave radiation flux (W/m**2) land sea surface'
        endif
      endif
!..........................................................
!     glolal=ULWSFC*RTIME
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IULWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IULWF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '12)Upward long wave radiation flux (W/m**2) land sea surface  '
        endif
      endif
!..........................................................
!.......  FIX FLUXES FOR APPROX DIURNAL CYCLE
      DO K=1,4
!       do j=1,LATS_NODE_R
!         do i=1,lonr
!            glolal(i,j)=rflux(i,j,k)*RTIMER(k)
!         enddo
!       enddo
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,IPUR(K),ITLR(K),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(K)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          if(k.eq.1)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '13)Upward long wave radiation flux (W/m**2) top of atmosphere '
          if(k.eq.2)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '14)Upward solar radiation flux (W/m**2) top of atmosphere     '
          if(k.eq.3)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '15)Upward solar radiation flux (W/m**2) land and sea surface  '
          if(k.eq.4)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '16)Downward solar radiation flux (W/m**2) land and sea surface'
        endif
      endif
      ENDDO
!..........................................................
!
!     For UV-B fluxes
!
!     do j=1,LATS_NODE_R
!       do i=1,lonr
!         glolal(i,j)=rflux(i,j,21)*rtimsw
!       enddo
!     enddo
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,129,ICEN,IGEN,
     &            0,IUVBF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IUVBF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '17)UV-B Downward solar flux (W/m**2) land sea surface'
        endif
      endif
 
!     do j=1,LATS_NODE_R
!       do i=1,lonr
!         glolal(i,j)=rflux(i,j,22)*rtimsw
!       enddo
!     enddo
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,129,ICEN,IGEN,
     &            0,IUVBFC,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IUVBFC),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '18)clear sky UV-B Downward solar flux (W/m**2) land sea surface'
        endif
      endif
!
!     End UV-B fluxes
!
!..........................................................
!..........................................................
      DO K=5,7
!
!      do j=1,LATS_NODE_R
!       do i=1,lonr
!        glolal(i,j)=rflux(i,j,k)*100.*rtimsw
!       enddo
!      enddo
!     where(glolal.ge.0.5)
!       kmskcv=1
!     elsewhere
!       kmskcv=0
!     endwhere
!!
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
!
        K4=4+(K-5)*4
        L=K4+1
        LBM=wrkga.Ge.0.5_kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,IPUR(L),ITLR(L),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(L)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          if(k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '19)Total cloud cover (percent) high cloud layer               '
          if(k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '23)Total cloud cover (percent) middle cloud layer             '
          if(k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '27)Total cloud cover (percent) low cloud layer                '
        endif
      endif
!        call baclose(noflx,ierr)
!
!      do j=1,LATS_NODE_R
!       do i=1,lonr
!        if(rflux(i,j,k).gt.0.)then
!         glolal(i,j)=rflux(i,j,k+3)*1000./rflux(i,j,k)
!        else
!         glolal(i,j)=0.
!        endif
!       enddo
!      enddo
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
!
        L=K4+2
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,IPUR(L),ITLR(L),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(L)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          if(k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '20)Pressure (Pa) high cloud top level                         '
          if(k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '24)Pressure (Pa) middle cloud top level                       '
          if(k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '28)Pressure (Pa) low cloud top level                          '
        endif
      endif
!
!      do j=1,LATS_NODE_R
!       do i=1,lonr
!        if(rflux(i,j,k).gt.0.)then
!         glolal(i,j)=rflux(i,j,k+6)*1000./rflux(i,j,k)
!        else
!         glolal(i,j)=0.
!        endif
!       enddo
!      enddo
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
!
        L=K4+3
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,IPUR(L),ITLR(L),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(L)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          if(k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '21)Pressure (Pa) high cloud bottom level                      '
          if(k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '25)Pressure (Pa) middle cloud bottom level                    '
          if(k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '29)Pressure (Pa) low cloud bottom level                       '
        endif
      endif
!
!      do j=1,LATS_NODE_R
!       do i=1,lonr
!        if(rflux(i,j,k).gt.0.)then
!         glolal(i,j)=rflux(i,j,k+9)/rflux(i,j,k)
!        else
!         glolal(i,j)=0.
!        endif
!       enddo
!      enddo
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        L=K4+4
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,IPUR(L),ITLR(L),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(L)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          if(k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '22)Temperature (K) high cloud top level                       '
          if(k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '26)Temperature (K) middle cloud top level                     '
          if(k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '30)Temperature (K) low cloud top level                        '
        endif
      endif
!
      ENDDO
!!
!...................................................................
!     glolal=GESHEM*1.E3*RTIME
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IPCPR,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPCPR),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '31)Precipitation rate (kg/m**2/s) land and sea surface        '
        endif
      endif
!...................................................................
!     glolal=BENGSH*1.E3*RTIME
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICPCPR,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICPCPR),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '32)Convective precipitation rate (kg/m**2/s) land sea surface '
        endif
      endif
!...................................................................
!     glolal=GFLUX*RTIME
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        LBM=slmskful.NE.0._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IGHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IGHFLX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '33)Ground heat flux (W/m**2) land and sea surface             '
        endif
      endif
!...................................................................
!     buffo=MOD(slmskloc,2._kind_io8)
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISLMSK,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISLMSK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '34)Land-sea mask (1=land; 0=sea) (integer) land sea surface   '
        endif
      endif
!...................................................................
!c-- XW: FOR SEA-ICE Nov04
!     buffo=MAX(slmskloc-1._kind_io8,0._kind_io8)
!     call unsplit2z(ioproc,wrkga,global_lats_r)
!     if(me.eq.ioproc) then
!       call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
!    &            0,ICEMSK,ISFC,0,0,IYR,IMO,IDA,IHR,
!    &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICEMSK),IENS,
!    &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
!       if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '35)Ice concentration (ice=1; no ice=0) (1/0) land sea surface '
!     endif
!     IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICEMSK,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICEMSK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '35)Ice concentration (ice>0; no ice=0) (1/0) land sea surface '
        endif
      endif

!...................................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IZNLW,IELEV,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IZNLW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '36)u wind (m/s) height above ground                           '
        endif
      endif
!...................................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IMERW,IELEV,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IMERW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '37)v wind (m/s) height above ground                           '
        endif
      endif
!...................................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ITEMP,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '38)Temperature (K) height above ground                        '
        endif
      endif
!...................................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISPHUM,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISPHUM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '39)Specific humidity (kg/kg) height above ground              '
        endif
      endif
!...................................................................
!     glolal=PSURF*1.E3
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IPRS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IPRS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '40)Pressure (Pa) land and sea surface                         '
        endif
      endif
!...................................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ITMX,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IWIN,0,0,ICEN2,IDS(ITMX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '41)Maximum temperature (K) height above ground                '
        endif
      endif
!...................................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ITMN,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IWIN,0,0,ICEN2,IDS(ITMN),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '42)Minimum temperature (K) height above ground                '
        endif
      endif
!...................................................................
!jwang add spfhmax,spfhmin
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,133,ICEN,IGEN,
     &            0,IQMX,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IWIN,0,0,ICEN2,IDS(IQMX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '41a)Maximum specific humidity (kg/kg) height above ground     '
        endif
      endif
!...................................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,133,ICEN,IGEN,
     &            0,IQMN,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IWIN,0,0,ICEN2,IDS(IQMN),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '42a)Minimum specific humidity (kg/kg) height above ground      '
        endif
      endif
!...................................................................
!  The output unit of runoff is kg/m2 (accumulative value)
!     glolal=RUNOFF * 1.E3
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        LBM=slmskful.NE.0._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IRNOF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IACC,0,0,ICEN2,IDS(IRNOF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '43)Runoff (kg/m**2) land and sea surface                      '
        endif
      endif
!...................................................................
!     glolal=EP * RTIME
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.NE.0._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IEP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IEP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '44)Potential evaporation rate (w/m**/) land and sea surface   '
        endif
      endif
!...................................................................
!     glolal=CLDWRK * RTIME
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICLDWK,ICOLMN,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICLDWK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '45)Cloud work function (J/Kg) total atmospheric column        '
        endif
      endif
!...................................................................
!     glolal=DUGWD*RTIME
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IZGW,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IZGW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '46)Zonal gravity wave stress (N/m**2) land and sea surface    '
        endif
      endif
!...................................................................
!     glolal=DVGWD*RTIME
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IMGW,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IMGW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '47)Meridional gravity wave stress (N/m**2) land sea surface   '
        endif
      endif
!...................................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IHPBL,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IHPBL),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '48)Boundary layer height '
        endif
      endif
!...................................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IPWAT,ICOLMN,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IPWAT),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '49)Precipitable water (kg/m**2) total atmospheric column      '
        endif
      endif
!...................................................................
!
!      do j=1,LATS_NODE_R
!       do i=1,lonr
!        if (rflux(i,j,4).GT.0.) then
!         glolal(i,j)=rflux(i,j,3)/rflux(i,j,4) * 100.
!         if (glolal(i,j).GT.100.) glolal(i,j)=100.
!        else
!         glolal(i,j)=0.
!        endif
!       enddo
!      enddo
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
!
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IALBDO,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IALBDO),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '50)Albedo (percent) land and sea surface                      '
        endif
      endif
!
!      do j=1,LATS_NODE_R
!       do i=1,lonr
!        glolal(i,j)=rflux(i,j,26)*100.*rtimsw
!       enddo
!      enddo
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
!
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,ICLDF,ICOLMN,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICLDF),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '51)Total cloud cover (percent) total atmospheric column       '
        endif
      endif
!
! CONVECTIVE CLOUDS
! LABELED INSTANTANEOUS BUT ACTUALLY AVERAGED OVER FHSWR HOURS
!
!     glolal=CV*1.E2
!     where(glolal.ge.0.5)
!       kmskcv=1
!     elsewhere
!       kmskcv=0
!     endwhere
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=wrkga.Ge.0.5_kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICLDF,ICVLYR,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICLDF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '52)Total cloud cover (percent) convective cloud layer         '
        endif
      endif
!.................................................
!      do j=1,LATS_NODE_R
!       do i=1,lonr
!       glolal(i,j) = 0.
!       IF(CV(i,j).GT.0.) THEN
!!       ITOP=NINT(CVT(i,j))
!!       IF(ITOP.GE.1.AND.ITOP.LE.LEVS)
!!   &   glolal(i,j)=SI(ITOP+1)*PSURF(i,j)*1.E3
!...      cvt already a pressure (cb)...convert to Pa
!        glolal(i,j)=CVT(i,j)*1.E3
!       END IF
!      ENDDO
!     ENDDO
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IPRS,ICVTL,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IPRS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '53)Pressure (Pa) convective cloud top level                   '
        endif
      endif
!.................................................
!      do j=1,LATS_NODE_R
!       do i=1,lonr
!       glolal(i,j) = 0.
!       IF(CV(i,j).GT.0.) THEN
!!       Ibot=NINT(CVB(i,j))
!!       IF(Ibot.GE.1.AND.Ibot.LE.LEVS)
!!   &   glolal(i,j)=SI(IBOT)*PSURF(i,j)*1.E3
c...      cvb already a pressure (cb)...convert to Pa
!        glolal(i,j)=CVB(i,j)*1.E3
!       END IF
!      ENDDO
!     ENDDO
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IPRS,ICVBL,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IPRS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '54)Pressure (Pa) convective cloud bottom level                '
        endif
      endif
!.................................................
!...   SAVE B.L. CLOUD AMOUNT
!
!      do j=1,LATS_NODE_R
!       do i=1,lonr
!        glolal(i,j)=rflux(i,j,27)*100.*rtimsw
!       enddo
!      enddo
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
!
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,ICLDF,IBLLYR,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICLDF),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
           print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '55)Total cloud cover (percent) boundary layer cloud layer     '
        endif
      endif
!
!c-- XW: FOR SEA-ICE Nov04
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.NE.1._kind_io8
!     LBM=slmskful.EQ.2._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISIK,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISIK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '56)Sea ice thickness (m) category 1'
        endif
      endif
!c-- XW: END SEA-ICE
!.................................................
!lu: add smc(3:4), stc(3:4), slc(1:4), snwdph, canopy
!lu: addition of 10 records starts here -------------------------------
      if(lsoil.gt.2)then
!     glolal(:,:)=SMC(3,:,:)
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISOILM,I2DBLS,40,100,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '57)Volumetric soil moist content (frac) layer 100cm and 40cm '
        endif
      endif
!..........................................................
!     glolal(:,:)=SMC(4,:,:)
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISOILM,I2DBLS,100,200,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '58)Volumetric soil moist content (frac) layer 200cm and 100cm '
        endif
      endif
!..........................................................
!     glolal(:,:)=STC(3,:,:)
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ITEMP,I2DBLS,40,100,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '59)Temp (K) layer betw two depth below land sfc 100cm and 40cm'
        endif
      endif
!..........................................................
!     glolal(:,:)=STC(4,:,:)
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ITEMP,I2DBLS,100,200,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '60)Temp (K) layer betw two depth below land sfc 200cm and 100cm'
        endif
      endif
      endif
!..........................................................
!     glolal(:,:)=SLC(1,:,:)
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLC,I2DBLS,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
         print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '61)Liquid soil moist content (frac) layer 10cm and 0cm  '
        endif
      endif
!..........................................................
!     glolal(:,:)=SLC(2,:,:)
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLC,I2DBLS,10,40,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '62)Liquid soil moist content (frac) layer 40cm and 10cm '
        endif
      endif
!..........................................................
      if(lsoil.gt.2)then
!     glolal(:,:)=SLC(3,:,:)
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLC,I2DBLS,40,100,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '63)Liquid soil moist content (frac) layer 100cm and 40cm'
        endif
      endif
!..........................................................
!     glolal(:,:)=SLC(4,:,:)
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLC,I2DBLS,100,200,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '64)Liquid soil moist content (frac) layer 200cm and 100cm'
        endif
      endif
      endif
!..........................................................
!     glolal=SNWDPH / 1.E3       !! convert from mm to m
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
!     LBM=slmskful.EQ.1._kind_io8
      LBM=slmskful.EQ.1._kind_io8 .or. slmskful.EQ.2._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISNOD,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISNOD),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
         print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '65)Snow depth (m) land surface                  '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ICNP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICNP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '66)Canopy water content (kg/m^2) land surface      '
        endif
      endif
!lu: addition of 10 records ends here -------------------------------
!
!wei: addition of 30 records starts here -------------------------------
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IZORL,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IZORL),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '67)Surface roughness (m)       '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IVEG,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IVEG),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '68)Vegetation fraction (fractional) land surface      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IVTP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IVTP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '69)Vegetation type land surface      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISTP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISTP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '70)Soil type land surface      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLO,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISLO),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '71)Slope type land surface      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IUST,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IUST),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '72)Frictional velocity (m/s)      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IHGT,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IHGT),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '73)Surface height (m)      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IRST,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IRST),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '74)Freezing precip flag land surface      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICHH,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICHH),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '75)Exchange coefficient CH(m/s)      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            0,ICMM,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICMM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '76)Exchange coefficient CM(m/s)      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IEP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IEP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '77)Potential evaporation rate (w/m**2) land and sea surface   '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IDLWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IDLWF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '78)Downward long wave radiation flux (W/m**2)'
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IULWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IULWF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '79)Upward long wave radiation flux (W/m**2)  '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IUSWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IUSWF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '80)Upward short wave radiation flux (W/m**2)  '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IDSWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IDSWF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '81)Downward short wave radiation flux (W/m**2)   '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISHFLX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '82)Sensible heat flux (W/m**2) land and sea surface           '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ILHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ILHFLX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '83)Latent heat flux (W/m**2) land and sea surface             '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        LBM=slmskful.NE.0._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IGHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IGHFLX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '84)Ground heat flux (W/m**2) land and sea surface             '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISRF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IACC,0,0,ICEN2,IDS(ISRF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '85)Surface runoff (kg/m^2) land surface      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ITEMP,isglev,1,1,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '86)Lowest model level Temp (K)       '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISPHUM,isglev,1,1,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISPHUM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '87)Lowest model specific humidity (kg/kg)       '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IZNLW,isglev,1,1,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IZNLW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '88)Lowest model u wind (m/s)      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IMERW,isglev,1,1,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IMERW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '89)Lowest model v wind (m/s)       '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IHGT,isglev,1,1,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IHGT),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '90)Lowest model level height (m) land surface      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IEVBS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IEVBS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '91)Direct evaporation from bare soil(W/m^2) land surface      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IEVCW,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IEVBS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '92)Canopy water evaporation(W/m^2) land surface      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ITRAN,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ITRAN),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '93)Transpiration (W/m^2) land surface      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISBS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISBS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '94)Snow Sublimation (W/m^2) land surface      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISNC,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISNC),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '95)Snow Cover (fraction) land surface      '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISTC,I2DBLS,0,200,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISTC),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '96)Total column soil moisture (Kg/m^2) land surface      '
        endif
      endif

!wei: addition of 30 records ends here -------------------------------

!!
!*RADFLX*
!Clu: Addition of 7 records starts here -------------------------------
!dswrf_toa, csulf_toa, csusf_toa, csdlf_sfc, csusf_sfc, csdsf_sfc, csulf_sfc

      DO K=19, 25
!       do j=1,LATS_NODE_R
!         do i=1,lonr
!            glolal(i,j)=rflux(i,j,k)*RTIMER(k)
!         enddo
!       enddo
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,IPUR(K),ITLR(K),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(K)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          if(k.eq.19)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '97)Downward solar radiation flux (W/m**2) TOA  '
          if(k.eq.20)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '98)CS upward long wave radiation flux (W/m**2) TOA     '
          if(k.eq.21)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '99)CS upward solar radiation flux (W/m**2) TOA  '
          if(k.eq.22)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '100)CS downward long wave radiation flux (W/m**2) SFC '
          if(k.eq.23)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '101)CS upward solar radiation flux (W/m**2)  SFC'
          if(k.eq.24)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '102)CS downward solar radiation flux (W/m**2) SFC'
          if(k.eq.25)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '103)CS upward long wave radiation flux (W/m**2) SFC'
        endif
      endif
      ENDDO
!..........................................................
!     glolal=SNOHFA*RTIME
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
       LBM=slmskful.EQ.1._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISNOHF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISNOHF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '104)Snow phase-change heat flux [W/m^2] land surface   '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
       LBM=slmskful.EQ.1._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISMCWLT,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISMCWLT),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '105)Wilting point [fraction] land surface   '
        endif
      endif
!..........................................................
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
       LBM=slmskful.EQ.1._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISMCREF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISMCREF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '106)Field capacity [fraction] land surface   '
        endif
      endif
!..........................................................
Clu: Addition of 7 records ends here ---------------------------------
!..........................................................
!
!     Sunshine duration time
!
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,133,ICEN,IGEN,
     &            0,ISUNTM,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IACC,0,0,ICEN2,IDS(ISUNTM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '107)Accumulated sunshine duration time (sec) '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!
!     end sunshine duration time
!
!hchuang code change add additional 2D output start here  -------------------------------
!...................................................................
! Output additional variable (averaged quantity) for GOCART
! If LGGFS3D = .TRUE.
!
      IF ( LGGFS3D ) THEN
!!      PRINT *, '***** DUMMY gribit ***** ', DUMMY(96,6)
!=======================================================================
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISOILM,I2DBLS,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '108)Average VOL soil moist content(frac) layer 10cm -> 0cm'
        endif
      endif
!=======================================================================
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=.TRUE.
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,itemp,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(itemp),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '109)Average temperature at 2 meter (K)                    '
        endif
      endif
!=======================================================================
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,iust,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(iust),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '110)Average Frictional Velocity (m/s)                     '
        endif
      endif
!=======================================================================
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ihpbl,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ihpbl),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '111)Average Boundary layer height                        '
        endif
      endif
!=======================================================================
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IZNLW,IELEV,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IZNLW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '112)Average u wind (m/s) height 10m above ground         '
        endif
      endif
!=======================================================================
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IMERW,IELEV,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IMERW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '113)Average v wind (m/s) height 10m above ground         '
        endif
      endif
!=======================================================================
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,izorl,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(izorl),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '114)Average Surface roughness (m)                        '
        endif
      endif
!=======================================================================
      call unsplit2z(ioproc,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISLMSK,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISLMSK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '115)Average Land-sea surface (fraction)                  '
        endif
      endif
!
      END IF   ! LGGFS3d Switch
!hchuang code change add dummy 2D output end here  ----------------------------
!!
      if(me.eq.ioproc)
     &   PRINT *,'GRIB FLUX FILE WRITTEN ',FHOUR,IDATE,noflx
!!
      RETURN
      END
       subroutine grids_move(ioproc)
!
!***********************************************************************
!
      use resol_def
      use mod_state
      use layout1
      use mpi_def
      implicit none
!
      integer ipt_lats_node_rl,nodesr
      integer lats_nodes_rl
!     integer lats_nodes_r(nodes),ipt,maxfld,ioproc,nproct
      integer ioproc
      integer proc,j,lat,msgtag,nproc,i,msgtag1,buff,startlat,ierr
      integer illen,ubound
      integer icount
      data icount/0/
      integer maxlats_comp
      save maxlats_comp, icount
      integer kllen
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      if(icount .eq. 0) then
        allocate(ivar_global(10))
        allocate(ivar_global_a(10,nodes))
        ivar_global(1)=ipt_lats_node_r
        ivar_global(2)= lats_node_r
        ivar_global(3)=lats_node_r_max
        call mpi_gather(ivar_global,10,MPI_INTEGER,
     1        ivar_global_a,10,MPI_INTEGER,ioproc,MPI_COMM_ALL,ierr)
        icount=icount+1
      endif
!!
      if(allocated(buff_mult_pieces)) then
         continue
      else
          maxlats_comp=lats_node_r_max
          if(.not. liope .or. me .ne. ioproc) then
             continue
          else
!            maxlats_comp=ivar_global_a(3,ioproc)
             maxlats_comp=ivar_global_a(3,1)
          endif
!                                                     gwv watch this!!
          allocate
     1       (buff_mult_pieces(lonr,ngrids_sfcc,maxlats_comp,nodes))
          allocate
     1       (buff_mult_piecesf(lonr,0:ngrids_flx,maxlats_comp,nodes))
          allocate (buff_mult_piecesa(lonr,
     &       1:ngrids_flx+1+ngrids_sfcc+ngrids_nsst,maxlats_comp,nodes))
      endif
 
!
!  big send
      IF (me.ne.ioproc) THEN
!
!        Sending the data
         msgtag = me
         illen  = lats_node_r
         kllen  = illen*lonr*(ngrids_flx+1+ngrids_sfcc+ngrids_nsst)
!                                    send the local grid domain
         CALL mpi_send(buff_mult_piecea,kllen,MPI_R_IO,ioproc,
     &                 msgtag,MPI_COMM_ALL,info)
      ELSE
        if( MC_COMP .ne. MPI_COMM_NULL) then
!         iotask is also a compute task.  send is replaced with direct
!         array copy
 
          buff_mult_piecesa(:,:,1:lats_node_r,ioproc+1)=
     1    buff_mult_piecea(:,:,1:lats_node_r)
!                                     END COMPUTE TASKS PORTION OF LOGIC
        endif
!                                     END COMPUTE TASKS PORTION OF LOGIC
!  receiving part of I/O task
 
!!
!!     for pes ioproc
        DO proc=1,nodes_comp
         if (proc.ne.ioproc+1) then
           msgtag = proc-1
           illen  = ivar_global_a(2,proc)
           kllen  = illen*lonr*(ngrids_flx+1+ngrids_sfcc+ngrids_nsst)
           CALL mpi_recv(buff_mult_piecesa(1,1,1,proc),kllen,
     &               MPI_R_IO,proc-1,msgtag,MPI_COMM_ALL,stat,info)
         endif
        enddo
      ENDIF
 
!!
      return
      end
 
      subroutine spect_collect(
     x           zqe_ls,qe_ls,tee_ls,die_ls,zee_ls,rqe_ls,gze_ls,
     x           zqo_ls,qo_ls,teo_ls,dio_ls,zeo_ls,rqo_ls,gzo_ls,
     x trieo_ls_node  )
!!
      use resol_def
      use layout1
      use mpi_def
      implicit none
!!
!!
      REAL(kind=8) t1,t2,t3,t4,t5,t6,ta,tb,rtc
!!
!!
      real(kind=kind_evod) zqe_ls(len_trie_ls,2)
      real(kind=kind_evod)  qe_ls(len_trie_ls,2)
      real(kind=kind_evod) tee_ls(len_trie_ls,2,levs)
      real(kind=kind_evod) die_ls(len_trie_ls,2,levs)
      real(kind=kind_evod) zee_ls(len_trie_ls,2,levs)
      real(kind=kind_evod) rqe_ls(len_trie_ls,2,levh)
      real(kind=kind_evod) gze_ls(len_trie_ls,2)
!!
      real(kind=kind_evod) zqo_ls(len_trio_ls,2)
      real(kind=kind_evod)  qo_ls(len_trio_ls,2)
      real(kind=kind_evod) teo_ls(len_trio_ls,2,levs)
      real(kind=kind_evod) dio_ls(len_trio_ls,2,levs)
      real(kind=kind_evod) zeo_ls(len_trio_ls,2,levs)
      real(kind=kind_evod) rqo_ls(len_trio_ls,2,levh)
      real(kind=kind_evod) gzo_ls(len_trio_ls,2)
!!
!!
      integer              ierr,j,k,l,lenrec,locl,n,node
!!
      integer  il,ilen,i,msgtag,ls_diml,nodesl
!     integer              kwq
!     integer              kwte
!     integer              kwdz
!     integer              kwrq
!!
!     parameter ( kwq  = 0*levs+0*levh+1 ,  !   qe/o_ls
!    x            kwte = 0*levs+0*levh+2 ,  !  tee/o_ls
!    x            kwdz = 1*levs+0*levh+2 ,  !  die/o_ls  zee/o_ls
!    x            kwrq = 3*levs+0*levh+2 )  !  rqe/o_ls
!!
!!
       real(kind=kind_mpi) trieo_ls_node(
     1 len_trie_ls_max+len_trio_ls_max, 2, 3*levs+1*levh+1)
!!
!!
!!
       integer len_trie_ls_nod, len_trio_ls_nod
!
      if (.NOT.LIOPE.or.icolor.ne.2) then
!        build state in trieo_ls_node
!!
      do j=1,len_trie_ls
!!
         trieo_ls_node(j,1,kwq) = qe_ls(j,1)
         trieo_ls_node(j,2,kwq) = qe_ls(j,2)
!!
      enddo
!!
!!
      do j=1,len_trio_ls
!!
         trieo_ls_node(j+len_trie_ls_max,1,kwq) = qo_ls(j,1)
         trieo_ls_node(j+len_trie_ls_max,2,kwq) = qo_ls(j,2)
!!
      enddo
      j= len_trie_ls_max+1
!!
!!
      do k=1,levs
!!
         do j=1,len_trie_ls
!!
           trieo_ls_node(j,1,kwte+  k-1) = tee_ls(j,1,k)
           trieo_ls_node(j,2,kwte+  k-1) = tee_ls(j,2,k)
!!
           trieo_ls_node(j,1,kwdz+2*k-2) = die_ls(j,1,k)
           trieo_ls_node(j,2,kwdz+2*k-2) = die_ls(j,2,k)
!!
           trieo_ls_node(j,1,kwdz+2*k-1) = zee_ls(j,1,k)
           trieo_ls_node(j,2,kwdz+2*k-1) = zee_ls(j,2,k)
!!
         enddo
!!
         do j=1,len_trio_ls
!!
           trieo_ls_node(j+len_trie_ls_max,1,kwte+  k-1) = teo_ls(j,1,k)
           trieo_ls_node(j+len_trie_ls_max,2,kwte+  k-1) = teo_ls(j,2,k)
!!
           trieo_ls_node(j+len_trie_ls_max,1,kwdz+2*k-2) = dio_ls(j,1,k)
           trieo_ls_node(j+len_trie_ls_max,2,kwdz+2*k-2) = dio_ls(j,2,k)
!!
           trieo_ls_node(j+len_trie_ls_max,1,kwdz+2*k-1) = zeo_ls(j,1,k)
           trieo_ls_node(j+len_trie_ls_max,2,kwdz+2*k-1) = zeo_ls(j,2,k)
!!
         enddo
      j= len_trie_ls_max+1
!!
      enddo
!!
      do k=1,levh
!!
         do j=1,len_trie_ls
!!
           trieo_ls_node(j,1,kwrq+  k-1) = rqe_ls(j,1,k)
           trieo_ls_node(j,2,kwrq+  k-1) = rqe_ls(j,2,k)
!!
!!
         enddo
!!
         do j=1,len_trio_ls
!1
           trieo_ls_node(j+len_trie_ls_max,1,kwrq+  k-1) = rqo_ls(j,1,k)
           trieo_ls_node(j+len_trie_ls_max,2,kwrq+  k-1) = rqo_ls(j,2,k)
!!
         enddo
!!
      enddo
!!
      endif !.NOT.LIOPE.or.icolor.ne.2
         t4=rtc()
      return
      end
      INTEGER FUNCTION nfill(C)
      implicit none
      integer j
      CHARACTER*(*) C
      NFILL=LEN(C)
      DO J=1,NFILL
        IF(C(J:J).EQ.' ') THEN
          NFILL=J-1
          RETURN
        ENDIF
      ENDDO
      RETURN
      END
 
 

      SUBROUTINE TREADEO_gfsio(FHOUR,IDATE,
     X                   GZE,QE,TEE,DIE,ZEE,RQE,
     X                   GZO,QO,TEO,DIO,ZEO,RQO,
     X                   LS_NODE,LS_NODES,MAX_LS_NODES,
     X                   SNNP1EV,SNNP1OD,pdryini,IPRINT,
     &                   global_lats_r,lats_nodes_r,lonsperlar,cfile,
     &                   epse,epso,plnew_r,plnow_r)
 
      use resol_def
      use layout1
      use coordinate_def					! hmhj
      use sig_io
      use namelist_def
      use vert_def
      use mpi_def
      use physcons, rerth => con_rerth, grav => con_g, rkap => con_rocp
     &,             cpd => con_cp
      use gfsio_module
      use gfsio_def
!
      IMPLICIT NONE
      character*(*) cfile
      REAL(KIND=KIND_EVOD) FHOUR
      INTEGER              IDATE(4),NTRACI, ntozi, ntcwi, ncldi, ixgr
     &,                    latbi, lonbi, levsi, jcapi,
     &                     latgi, lonfi, latri, lonri, nt0
!!
      real(kind=kind_evod)  epse(len_trie_ls)
      real(kind=kind_evod)  epso(len_trio_ls)
!!
      real(kind=kind_evod)   plnew_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnow_r(len_trio_ls,latr2)
!!
      REAL(KIND=KIND_EVOD) GZE(LEN_TRIE_LS,2)
     &,                     QE(LEN_TRIE_LS,2)
     &,                    TEE(LEN_TRIE_LS,2,LEVS)
     &,                    DIE(LEN_TRIE_LS,2,LEVS)
     &,                    ZEE(LEN_TRIE_LS,2,LEVS)
     &,                    RQE(LEN_TRIE_LS,2,LEVS,ntrac)
     &,                    GZO(LEN_TRIO_LS,2)
     &,                     QO(LEN_TRIO_LS,2)
     &,                    TEO(LEN_TRIO_LS,2,LEVS)
     &,                     DIO(LEN_TRIO_LS,2,LEVS)
     &,                    ZEO(LEN_TRIO_LS,2,LEVS)
     &,                    RQO(LEN_TRIO_LS,2,LEVS,ntrac)
 
!
      integer              ls_node(ls_dim,3)
!
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
      integer latb,lonb,iens(5),iorder,irealf,idpp,idrun,idusr,ncldt ! gwv add
!
      INTEGER              LS_NODES(LS_DIM,NODES)
      INTEGER              MAX_LS_NODES(NODES)
      integer              lats_nodes_r(nodes)
      REAL(KIND=KIND_EVOD) SNNP1EV(LEN_TRIE_LS)
      REAL(KIND=KIND_EVOD) SNNP1OD(LEN_TRIO_LS)
      INTEGER              IPRINT
      INTEGER              J,K,L,LOCL,N,lv,kk
      integer              i,lan,lat,iblk,lons_lat,il,lon,njeff,nn
      integer              indev
      integer              indod
      integer              indev1,indev2
      integer              indod1,indod2
      REAL(KIND=KIND_EVOD) GA2,GENCODE,GZBAR
!     REAL(KIND=KIND_EVOD) GA2,GENCODE,GZBAR,ORDER,REALFORM
      REAL(KIND=KIND_EVOD) TRUN,WAVES,XLAYERS
      REAL(KIND=KIND_EVOD) XI(LEVP1),XL(LEVS)
      REAL(KIND=KIND_EVOD) sikp1(levp1)
      REAL(KIND=KIND_IO4)   VTID,RUNID4,fhour4,pdryini4,XNCLD,xgf
      REAL(KIND=KIND_IO8)   PDRYINI
      real(kind=kind_io4), allocatable ::  vcoord4(:,:)
!     integer             idusr
!
!     type (gfsio_gfile) gfile
!
!     integer              idvc
!     integer              idsl, iret, num_dta
      integer              iret, num_dta
      real(kind=kind_evod) psurfff
      real(kind=kind_evod) pressk, tem
      real(kind=kind_evod), parameter :: rkapi=1.0/rkap,
     &                                   rkapp1=1.0+rkap
!
      integer kmsk(lonr,latr), global_lats_r(latr), lonsperlar(latr)
      real(kind=kind_io8) buffo(lonr,lats_node_r)
     &,                   buff2(lonr,lats_node_r)
      real(kind=kind_evod) teref(levp1),ck5p(levp1)			! hmhj
!    &,                    ttref(levp1)
!
      real (kind=kind_io4), allocatable ::  gfsio_data(:)
!!
      real(kind=kind_rad) zsg(lonr,lats_node_r)
      real(kind=kind_rad) psg(lonr,lats_node_r)
      real(kind=kind_rad) uug(lonr,lats_node_r,levs)
      real(kind=kind_rad) vvg(lonr,lats_node_r,levs)
      real(kind=kind_rad) teg(lonr,lats_node_r,levs)
      real(kind=kind_rad) rqg(lonr,lats_node_r,levh)
!
!       Input file is in grid-point space - use gfs_io package
!
      call gfsio_open(gfile_in,trim(cfile),'read',iret)
      call gfsio_getfilehead(gfile_in,iret=iret,
     &  version=ivsupa,fhour=fhour4,idate=idate,
     &  latb=latb,lonb=lonb,levs=levsi,jcap=jcapi,itrun=itrun,
     &  iorder=iorder,irealf=irealf,igen=igen,latf=latgi,lonf=lonfi,
     &  latr=latri,lonr=lonri,ntrac=ntraci,icen2=icen2,iens=iens,
     &  idpp=idpp,idsl=idsl,idvc=idvc,idvm=idvm,idvt=idvt,idrun=idrun,
     &  idusr=idusr,pdryini=pdryini4,ncldt=ncldt,nvcoord=nvcoord)
!
      if (me == 0) then
        print *,' idvt=',idvt,' nvcoord=',nvcoord,' levsi=',levsi
        if(lonf .ne. lonfi .or. latg .ne. latgi .or.
     &     jcap .ne. jcapi .or. levs .ne. levsi) then
          print *,' Input resolution and the model resolutions are'
     &,  ' different- run aborted'
          call mpi_quit(777)
        endif
      endif
!
      allocate (vcoord4(levsi+1,nvcoord))
      allocate (vcoord(levsi+1,nvcoord))
      call gfsio_getfilehead(gfile_in,iret=iret,vcoord=vcoord4)
!
!     if (me == 0) then
!     print *,' nvcoord=',nvcoord,' vcoord4=',vcoord4(:,1:nvcoord)
!    &,' iret=',iret
!     endif
!
!     usrid = idusr
!     runid - idrun
      vcoord(:,1:nvcoord) = vcoord4(:,1:nvcoord)
!     if (me .eq. 0) print *,' vcoord=',vcoord(:,1:nvcoord)
      deallocate (vcoord4)
!
      if (gen_coord_hybrid) then                                        ! hmhj

        sfcpress_id  = mod(idvm , 10)
        thermodyn_id = mod(idvm/10 , 10)
!   ak bk ck in file have the same order as model                       ! hmhj
        do k=1,levp1                                                    ! hmhj
          ak5(k) = vcoord(k,1)/1000.                                    ! hmhj
          bk5(k) = vcoord(k,2)                                          ! hmhj
          ck5(k) = vcoord(k,3)/1000.                                    ! hmhj
        enddo                                                           ! hmhj
        vertcoord_id=0                                                  ! hmhj
        do k=1,levp1                                                    ! hmhj
          if( ck5(k).ne.0.0 ) vertcoord_id=3                            ! hmhj
        enddo
! provide better estimated press                                        ! hmhj
        psurfff = 101.3                                                 ! hmhj
        if( thermodyn_id.eq.3 ) then                                    ! hmhj
          do k=1,levs                                                   ! hmhj
            thref(k) = 300.0*cpd                                        ! hmhj
            teref(k) = 255.0*cpd                                        ! hmhj
          enddo                                                         ! hmhj
        else                                                            ! hmhj
         do k=1,levp1                                                   ! hmhj
          thref(k) = 300.0                                              ! hmhj
          teref(k) = 255.0                                              ! hmhj
         enddo                                                          ! hmhj
        endif
        ck5p(levp1) = ck5(levp1)                                        ! hmhj
        do k=1,levs                                                     ! hmhj
          ck5p(k) = ck5(k)*(teref(k)/thref(k))**rkapi                   ! hmhj
        enddo
        if( me.eq.0 ) then                                              ! hmhj
          do k=1,levp1                                                  ! hmhj
            pressk=ak5(k)+bk5(k)*psurfff+ck5p(k)                        ! hmhj
            print 180,k,ak5(k),bk5(k),ck5(k),pressk                     ! hmhj
180         format('k=',i3,'  ak5=',f13.9,'  bk5=',e15.8,               ! hmhj
     &            '   ck5=',f13.9,'  closed pressk=',f10.6)             ! hmhj
          enddo                                                         ! hmhj
        endif                                                           ! hmhj
        do k=1,levp1                                                    ! hmhj
          si(k) = ak5(k)/psurfff + bk5(k) + ck5p(k)/psurfff             ! hmhj
        enddo                                                           ! hmhj
        do k=1,levs                                                     ! hmhj
          sl(k) = 0.5*(si(k)+si(k+1))                                   ! hmhj
        enddo                                                           ! hmhj

      else if (hybrid .and. idvc .eq. 2) then
!       idsl=slid  !=2,pk=0.5*(p(k+1/2)+p(k-1/2)) check alfa(1)  am_bm
!   ak bk order in "sigma" file is bottom to top !!!!!!!!!!!!!!!!!!
        psurfff = 101.3
        do k=1,levp1
          ak5(k) = vcoord(levp1+1-k,1)/1000.
          bk5(k) = vcoord(levp1+1-k,2)
          pressk = ak5(k) + bk5(k)*psurfff

          if(me.eq.0)print 190,k,ak5(k),bk5(k),pressk
190       format('k=',i3,'  ak5=',E14.6,'  bk5=',e15.8,
     &           '  pressk=',E14.6)

        enddo
        do k=1,levs
          dbk(k) = bk5(k+1)-bk5(k)
          bkl(k) = (bk5(k+1)+bk5(k))*0.5
          ck(k)  = ak5(k+1)*bk5(k)-ak5(k)*bk5(k+1)
          if(me.eq.0)print 200,k,dbk(k),ck(k)
200       format('k=',i3,'  dbk=',f9.6,'  ck=',e13.5)
        enddo
!
! hmhj give an estimated si and sl for dynamics
        do k=1,levs+1
          si(levs+2-k) = ak5(k)/psurfff + bk5(k) !ak(k) bk(k) go top to bottom
        enddo
        do k=1,levs
          sl(k) = 0.5*(si(k)+si(k+1))
        enddo
!
      elseif (idvc .le. 1) then
        si(:)    = vcoord(:,1)
        sik(:)   = si(:) ** rkap
        sikp1(:) = si(:) ** rkapp1
        do k=1,levs
          tem      = rkapp1 * (si(k) - si(k+1))
          slk(k)   = (sikp1(k)-sikp1(k+1))/tem
          sl(k)    = slk(k) ** rkapi
!         sl(k)    = ((sikp1(k)-sikp1(k+1))/tem)**rkapi
          if (me .eq. 0) print 250, k, si(k), sl(k)
250       format('k=',i2,'  si=',f9.6,'  sl=',e13.5)
        enddo
      else
        print *,' Non compatible Initial state IDVC=',idvc
     &,' iret=',iret
        call MPI_QUIT(333)
      endif
!
      FHOUR       = fhour4
      idate       = idate
      WAVES       = jcap
      XLAYERS     = levs
      itrun       = itrun
!     ORDER       = iorder
!     REALFORM    = irealf
      icen        = 7
      icen2       = icen2
      igen        = igen
      ienst       = iens(1)
      iensi       = iens(2)
!     runid       = idrun
!     usrid       = idusr
      if (pdryini .eq. 0.0) pdryini = pdryini4
      ntraci = ntrac
      if (idvt .gt. 0.0) then
        if (nt0 > 0) then
          ntcwi = idvt / 10
          ntozi = idvt - ntcwi * 10 + 1
          ntcwi = ntcwi + 1
        else
          ntcwi = ntcw
          ntozi = ntoz
        endif
        ncldi = ncldt
      elseif(ntraci .eq. 2) then
        ntozi = 2
        ntcwi = 0
        ncldi = 0
      elseif(ntraci .eq. 3) then
        ntozi = 2
        ntcwi = 3
        ncldi = 1
      else
        ntozi = 0
        ntcwi = 0
        ncldi = 0
      endif
!     ixgr = ixgr
!
      if (ntrac <= 3 .and. idvt <= 0) then
        idvt = (ntcw-1)*10 + ntoz - 1
      endif
!
!
      IF (me.eq.0) THEN
        write(*,*)'cfile,in treadeo fhour,idate=',cfile,fhour,idate
     &, ' ntozi=',ntozi,' ntcwi=',ntcwi,' ncldi=',ncldi
     &, ' ntraci=',ntraci,' tracers=',ntrac,' vtid=',idvt
     &,   ncldt,' idvc=',idvc,' jcap=',jcap
     &, ' ixgr=',ixgr,' pdryini=',pdryini
      ENDIF
!
        allocate (gfsio_data(lonb*latb))
!  Read orog
      call gfsio_readrecv(gfile_in,'hgt','sfc',1,gfsio_data,iret)
      call split2d(gfsio_data,buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,zsg,global_lats_r,lonsperlar)
!
!  Read ps
      call gfsio_readrecv(gfile_in,'pres','sfc',1,gfsio_data,iret)
      call split2d(gfsio_data,buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,psg,global_lats_r,lonsperlar)
!
      do k=1,levs
!  Read u
        call gfsio_readrecv(gfile_in,'ugrd','layer',k,gfsio_data,iret)
        call split2d(gfsio_data,buffo,global_lats_r)
        CALL interpred(1,kmsk,buffo,uug(1,1,k),global_lats_r,lonsperlar)
      enddo
!  Read v
      do k=1,levs
        call gfsio_readrecv(gfile_in,'vgrd','layer',k,gfsio_data,iret)
        call split2d(gfsio_data,buffo,global_lats_r)
        CALL interpred(1,kmsk,buffo,vvg(1,1,k),global_lats_r,lonsperlar)
      enddo
!  Read T   -- this is real temperature
      do k=1,levs
        call gfsio_readrecv(gfile_in,'tmp','layer',k,gfsio_data,iret)
        call split2d(gfsio_data,buffo,global_lats_r)
        CALL interpred(1,kmsk,buffo,teg(1,1,k),global_lats_r,lonsperlar)
      enddo
!
!  Read Tracers
!
      rqg(:,:,:) = 0.0
!  Read q
      do k=1,levs
        call gfsio_readrecv(gfile_in,'spfh','layer',k,gfsio_data,iret)
        call split2d(gfsio_data,buffo,global_lats_r)
        CALL interpred(1,kmsk,buffo,rqg(1,1,k),global_lats_r,lonsperlar)
      enddo
!
!  other tracers
!
!  Read O3
      if (ntozi .gt. 0) then
        do k=1,levs
          call gfsio_readrecv(gfile_in,'o3mr','layer',k,gfsio_data,
     &                                                  iret)
          call split2d(gfsio_data,buffo,global_lats_r)
          CALL interpred(1,kmsk,buffo,rqg(1,1,k+(ntoz-1)*levs),
     &                                global_lats_r,lonsperlar)
        enddo
      endif
!  Read clw
      if (ntcwi .gt. 0) then
        do k=1,levs
          call gfsio_readrecv(gfile_in,'clwmr','layer',k,gfsio_data,
     &                                                         iret)
          call split2d(gfsio_data,buffo,global_lats_r)
          CALL interpred(1,kmsk,buffo,rqg(1,1,k+(ntcw-1)*levs),
     &                                global_lats_r,lonsperlar)
        enddo
      endif
!
!   Convert from Gaussian grid to spectral space
!       Also convert from real T to either virtual T or enthalpy
!
      call grid_to_spect
     &     (zsg,psg,uug,vvg,teg,rqg,
     &      GZE,GZO,QE,QO,DIE,DIO,ZEE,ZEO,TEE,TEO,RQE,RQO,
     &      ls_node,ls_nodes,max_ls_nodes,
     &      lats_nodes_r,global_lats_r,lonsperlar,
     &      epse,epso,SNNP1EV,SNNP1OD,plnew_r,plnow_r)

!
      call gfsio_close(gfile_in,iret)
!
      iprint=0
 
!!!!
      RETURN
      END

      SUBROUTINE TREADEO(NFT,FHOUR,IDATE,
     X                   GZE,QE,TEE,DIE,ZEE,RQE,
     X                   GZO,QO,TEO,DIO,ZEO,RQO,
     X                   LS_NODE,LS_NODES,MAX_LS_NODES,
     &                   plnev_r,plnod_r,plnew_r,plnow_r,lats_nodes_r,
     X                   SNNP1EV,SNNP1OD,pdryini,IPRINT,
     &                   phy_f3d, phy_f2d, global_lats_r, nblck,
     &                   lonsperlar,cfile)
 
      use resol_def
      use layout1
      use coordinate_def					! hmhj
      use sig_io
      use namelist_def
      use vert_def
      use mpi_def
      use physcons, rerth => con_rerth, grav => con_g, rkap => con_rocp
     &            , cpd => con_cp					! hmhj
      use sigio_module
      use sigio_r_module
!
      IMPLICIT NONE
      character*(*) cfile
      INTEGER              NFT
      REAL(KIND=KIND_EVOD) FHOUR
      INTEGER              IDATE(4),NTRACI, ntozi, ntcwi, ncldi, ixgr
     &,                    nblck, nt0, direction
!
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
!    &,                    SL(LEVS)
!    &,                    SI(LEVP1)
     &,                    Z00
      real (kind=kind_phys)
     &     phy_f3d(NGPTC,LEVS,NBLCK,lats_node_r,num_p3d),
     &     phy_f2d(lonr,lats_node_r,num_p2d)
 
      REAL(KIND=KIND_EVOD) PLNEV_R(LEN_TRIE_LS,LATR2)
      REAL(KIND=KIND_EVOD) PLNOD_R(LEN_TRIE_LS,LATR2)
      REAL(KIND=KIND_EVOD) PLNEW_R(LEN_TRIE_LS,LATR2)
      REAL(KIND=KIND_EVOD) PLNOW_R(LEN_TRIE_LS,LATR2)
      integer            lats_nodes_r(nodes)
!
      integer              ls_node(ls_dim,3)
!
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
      INTEGER              LS_NODES(LS_DIM,NODES)
      INTEGER              MAX_LS_NODES(NODES)
      REAL(KIND=KIND_EVOD) SNNP1EV(LEN_TRIE_LS)
      REAL(KIND=KIND_EVOD) SNNP1OD(LEN_TRIO_LS)
      INTEGER              IPRINT
      INTEGER              J,K,L,LOCL,N,lv,kk
      integer              i,lan,lat,iblk,lons_lat,il,lon,njeff,nn
      integer              indev
      integer              indod
      integer              indev1,indev2
      integer              indod1,indod2
      REAL(KIND=KIND_EVOD) GA2,GENCODE,GZBAR,ORDER,REALFORM
      REAL(KIND=KIND_EVOD) TRUN,WAVES,XLAYERS
      REAL(KIND=KIND_EVOD) XI(LEVP1),XL(LEVS)
      REAL(KIND=KIND_EVOD), target ::  TRISCA(LNT2)
      REAL(KIND=KIND_EVOD) sikp1(levp1)
!     REAL(KIND=KIND_IO4)   BUF(LNT2)
!     REAL(KIND=KIND_IO4)   SUBCEN,PPID,SLID,VCID,VMID
      REAL(KIND=KIND_IO4)   VTID,RUNID4,USRID4,pdryini4,XNCLD,xgf
      REAL(KIND=KIND_IO8)   PDRYINI
      INTEGER              INDLSEV,JBASEV
      INTEGER              INDLSOD,JBASOD
c$$$      REAL(KIND=KIND_IO4) Z(lnt2)
!
      type(sigio_head) head
      type(sigio_dbti) dati
!
!     integer              idvc
      integer              iret, num_dta
      real(kind=kind_evod) psurfff
      real(kind=kind_evod) pressk, tem, rkapi, rkapp1
!
      integer kmsk(lonr,latr), global_lats_r(latr), lonsperlar(latr)
      real(kind=kind_ior), target ::  buff1(lonr*latr)
      real(kind=kind_io8) buffo(lonr,lats_node_r)
     &,                   buff2(lonr,lats_node_r)
      real(kind=kind_evod) teref(levp1),ck5p(levp1)			! hmhj
 
 
      INCLUDE 'function2'
!!
c$$$      common /z00_com/z
!!
!     print *,' enter treadeo.io_fd '					! hmhj

      call sigio_rropen(nft,cfile,iret)
      call sigio_alhead(head,iret)
      call sigio_rrhead(nft,head,iret)
!
      ivsinp = head%ivs
      if (me .eq. 0) then
        print *,' In treadeo iret=',iret,' cfile=',cfile
     &,' ivs=',head%ivs,' levs=',head%levs
      endif
      if (iret .ne. 0) then
        print *,' unable to read from unit ',nft,' Job Aborted'
     &,' iret=',iret,' me=',me
        call mpi_quit(7777)
      endif
!
      idvc = head%idvc  !idvc=3:sigma-theta and/or p, idvc=2:sigma-p, idvc=1:sigma files
      idsl = head%idsl
!     rewind(nft)
!     READ(NFT)

      RKAPI  = 1.0 / RKAP
      RKAPP1 = 1.0 + RKAP

      if (me .eq. 0) then
        print *,' gen_coord_hybrid=',gen_coord_hybrid,
     &' idvc=',head%idvc,' idvm=',head%idvm
      endif
      if (gen_coord_hybrid) then					! hmhj

        sfcpress_id = mod ( head%idvm , 10 )				! hmhj
        thermodyn_id = mod ( head%idvm / 10 , 10 )			! hmhj
        if (me .eq. 0) then
          print *,' sfcpress_id thermodyn_id ',sfcpress_id,thermodyn_id	! hmhj
        endif
!   ak bk ck in file have the same order as model			! hmhj
        do k=1,levp1							! hmhj
          ak5(k) = head%vcoord(k,1)/1000.				! hmhj
          bk5(k) = head%vcoord(k,2)					! hmhj
          ck5(k) = head%vcoord(k,3)/1000.				! hmhj
        enddo								! hmhj
        vertcoord_id=0							! hmhj
        do k=1,levp1							! hmhj
          if( ck5(k).ne.0.0 ) vertcoord_id=3				! hmhj
        enddo								! hmhj
! provide better estimated press 					! hmhj
        psurfff = 101.3							! hmhj
        if( thermodyn_id.eq.3 ) then					! hmhj
         do k=1,levp1							! hmhj
          thref(k)=300.*cpd						! hmhj
          teref(k)=255.*cpd						! hmhj
         enddo								! hmhj
        else								! hmhj
         do k=1,levp1							! hmhj
          thref(k)=300.							! hmhj
          teref(k)=255.							! hmhj
         enddo								! hmhj
        endif								! hmhj
        ck5p(levp1)=ck5(levp1)						! hmhj
        do k=1,levp1							! hmhj
          ck5p(k)=ck5(k)*(teref(k)/thref(k))**rkapi			! hmhj
        enddo								! hmhj
        if( me.eq.0 ) then						! hmhj
          do k=1,levp1							! hmhj
            pressk=ak5(k)+bk5(k)*psurfff+ck5p(k)			! hmhj
            print 180,k,ak5(k),bk5(k),ck5(k),pressk			! hmhj
180         format('k=',i3,'  ak5=',f13.9,'  bk5=',e15.8,		! hmhj
     &            '   ck5=',f13.9,'  closed pressk=',f10.6)		! hmhj
          enddo								! hmhj
        endif								! hmhj
        do k=1,levp1							! hmhj
          si(k)=ak5(k)/psurfff+bk5(k)+ck5p(k)/psurfff			! hmhj
        enddo								! hmhj
        do k=1,levs							! hmhj
          sl(k)=0.5*(si(k)+si(k+1))					! hmhj
        enddo								! hmhj

      else if (hybrid .and. idvc .eq. 2) then				! hmhj
!       idsl=slid  !=2,pk=0.5*(p(k+1/2)+p(k-1/2)) check alfa(1)  am_bm
!   ak bk order in "sigma" file is bottom to top !!!!!!!!!!!!!!!!!!
        psurfff = 101.3
        do k=1,levp1
          ak5(k) = head%vcoord(levp1+1-k,1)/1000.
          bk5(k) = head%vcoord(levp1+1-k,2)
          pressk = ak5(k)+bk5(k)*psurfff
          
          if(me.eq.0)print 190,k,ak5(k),bk5(k),pressk
190       format('k=',i3,'  ak5=',E15.8,'  bk5=',e15.8,
     &           '  pressk=',E14.6)
           
        enddo
        do k=1,levs
          dbk(k) = bk5(k+1)-bk5(k)
          bkl(k) = (bk5(k+1)+bk5(k))*0.5
          ck(k)  = ak5(k+1)*bk5(k)-ak5(k)*bk5(k+1)
          if(me.eq.0)print 200,k,dbk(k),ck(k)
200       format('k=',i3,'  dbk=',f8.6,'  ck=',e13.5)
        enddo
!
! hmhj give an estimated si and sl for dynamics
        do k=1,levs+1
          si(levs+2-k)=ak5(k)/psurfff+bk5(k) !ak(k) bk(k) go top to bottom
        enddo
        do k=1,levs
          sl(k)=0.5*(si(k)+si(k+1))
        enddo
!
!     elseif (head%idvc .eq. 1) then
      elseif (head%idvc .le. 1) then
        si(:)    = head%vcoord(:,1)
        sik(:)   = si(:) ** rkap
        sikp1(:) = si(:) ** rkapp1
        do k=1,levs
          tem      = rkapp1 * (si(k) - si(k+1))
          slk(k)   = (sikp1(k)-sikp1(k+1))/tem
          sl(k)    = slk(k) ** rkapi
!         sl(k)    = ((sikp1(k)-sikp1(k+1))/tem)**rkapi
          if (me .eq. 0) print 250, k, si(k), sl(k)
250       format('k=',i2,'  si=',f8.6,'  sl=',e13.5)
        enddo
      else
        print *,' Non compatible Initial state IDVC=',head%idvc
     &,' iret=',iret
        call MPI_QUIT(333)
      endif
!
csela print*,' read second record successfully '
      FHOUR       = head%fhour
      idate       = head%idate
      WAVES       = head%jcap
      XLAYERS     = head%levs
      itrun       = head%itrun
      ORDER       = head%iorder
      REALFORM    = head%irealf
      icen        = 7
      icen2       = head%icen2
      igen        = head%igen
      ienst       = head%iens(1)
      iensi       = head%iens(2)
!     runid       = head%idrun
!     usrid       = head%idusr
      if (fhour .gt. 0.0 .and. head%nxss .eq. 0 .and.
     &    head%pdryini > 0.0 ) then
        if (pdryini .eq. 0.0) pdryini = head%pdryini
      endif
      if (me == 0) print *,' IN TREAD PDRYINI=',pdryini,
     &                     ' head=',head%pdryini
!sela ntraci = nint(tracers-1)
      ntraci = head%ntrac
      if (head%idvt .gt. 0.0) then
        nt0   = mod(head%idvt,10)
        if (nt0 > 0) then
          ntcwi = head%idvt / 10
          ntozi = head%idvt - ntcwi * 10 + 1
          ntcwi = ntcwi + 1
        else
          ntcwi = ntcw
          ntozi = ntoz
        endif
        ncldi = head%ncldt
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
      ixgr = head%ixgr
!
      if (ntrac <= 3) then
        idvt = (ntcw-1)*10 + ntoz - 1
      else
        idvt = head%idvt
      endif
!
      IF (me.eq.0) THEN
        write(*,*)'nfile,in treadeo fhour,idate=',nft,fhour,idate
     &, ' ntozi=',ntozi,' ntcwi=',ntcwi,' ncldi=',ncldi
     &, ' ntraci=',ntraci,' tracers=',head%ntrac,' vtid=',head%idvt
     &,  head%ncldt,' idvc=',head%idvc,' jcap=',head%jcap
     &, ' ixgr=',ixgr,' pdryini=',pdryini
      ENDIF
!cjfe
      IF(IPRINT.EQ.1)
     X PRINT *,'TREAD UNIT,FHOUR,IDATE=',NFT,FHOUR,IDATE
 
!
      dati%i = 1                                           ! hs
      dati%f => TRISCA
      call sigio_rrdbti(nft,head,dati,iret)
      if (me == 0) print *,' Z_R=',trisca(1:10),' iret=',iret
      Z   = TRISCA
      Z_R = TRISCA
      CALL TRISEORI(TRISCA,GZE,GZO,1,LS_NODE)
      Z00=TRISCA(1)
      GA2=GRAV/(RERTH*RERTH)
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
            GZE(INDEV,1)=
     X      GZE(INDEV,1)*SNNP1EV(INDEV)*GA2
            GZE(INDEV,2)=
     X      GZE(INDEV,2)*SNNP1EV(INDEV)*GA2
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
            GZO(INDOD,1)=
     X      GZO(INDOD,1)*SNNP1OD(INDOD)*GA2
            GZO(INDOD,2)=
     X      GZO(INDOD,2)*SNNP1OD(INDOD)*GA2
         END DO
      END DO
 
      if (mod(head%idvm/10,10) == 3 .and. me == 0)then
        print *,' CPI=',head%cpi(1:ntraci+1)
        print *,' RI=',head%ri(1:ntraci+1)
      endif
      dati%i = 2                               ! Surface pressure
      dati%f => TRISCA                       
      call sigio_rrdbti(nft,head,dati,iret)
      if (me == 0) print *,' SFCPRES=',trisca(1:10)
      CALL TRISEORI(TRISCA,QE,QO,1,LS_NODE)
      DO K=1,LEVS
        dati%i = k + 2                        ! Virtual Temperature or CpT
        dati%f => TRISCA                       

        call sigio_rrdbti(nft,head,dati,iret)

        CALL TRISEORI(TRISCA,TEE(1,1,K),TEO(1,1,K),1,LS_NODE)
      enddo
!
      DO K=1,LEVS
         dati%i = levs + 2 + (k-1) * 2 + 1     ! Divergence
         dati%f => TRISCA
         call sigio_rrdbti(nft,head,dati,iret)
         CALL TRISEORI(TRISCA,DIE(1,1,K),DIO(1,1,K),1,LS_NODE)
!
         dati%i = levs + 2 + (k-1) * 2 + 2     ! Vorticity
         dati%f => TRISCA
         call sigio_rrdbti(nft,head,dati,iret)
         CALL TRISEORI(TRISCA,ZEE(1,1,K),ZEO(1,1,K),1,LS_NODE)
      END DO
csela print*,' levh=',levh
!
!
      RQE=0.
      RQO=0.
      DO K=1,ntraci
        kk = 0
        if (k .eq. 1) then
          kk = 1
        elseif (k .eq. ntozi) then
          kk = ntoz
        elseif (k .ge. ntcwi .and. k .lt. ntcwi+ncldi-1) then
          do n=1,ncldi
            if (k .eq. ntcwi+n-1) kk = ntcw+n-1
          enddo
        else
          kk = k
        endif
!
        DO lv=1,levs
          dati%i = levs * (2+k) + 2 + lv             ! Tracers starting with q
          dati%f => TRISCA
          call sigio_rrdbti(nft,head,dati,iret)
          CALL TRISEORI(TRISCA,RQE(1,1,lv,KK),RQO(1,1,lv,KK),1,LS_NODE)
        END DO
      END DO
!
      if (((ixgr .eq. 4 .and. num_p3d .eq. 4) .or.   ! Zhao Scheme!
     &     (ixgr .eq. 5 .and. num_p3d .eq. 3))       ! Ferrier Scheme!
     &     .and. fhour .gt. 0.1) then
        kmsk(:,:)  = 0
!       if (head%irealf .eq. 1) then
!         do nn=1,num_p3d
!           do k=1,levs
!             call split2d(data%xgr(1,1,(nn-1)*levs+k)
!    &,                                 buffo,global_lats_r)
!             CALL interpred(1,kmsk,buffo,buff2,global_lats_r,
!    &                lonsperlar)
!
!             do lan=1,LATS_NODE_R
!               lat = global_lats_r(ipt_lats_node_r-1+lan)
!               lons_lat = lonsperlar(lat)
!               iblk=0
!               il=1
!               DO lon=1,lons_lat,NGPTC
!                 NJEFF=MIN(NGPTC,lons_lat-lon+1)
!                 iblk=iblk+1
!                 do i=1,NJEFF
!                   phy_f3d(i,k,iblk,lan,nn) = buff2(il,lan)
!                   il=il+1
!                 enddo
!               enddo
!             enddo
!           enddo
!         enddo
!         do nn=1,num_p2d
!           call split2d(data%xgr(1,1,num_p3d*levs+nn)
!    &,                                 buffo,global_lats_r)
!           CALL interpred(1,kmsk,buffo,buff2,global_lats_r,
!    &              lonsperlar)
!           do j=1,lats_node_r
!             do i=1,lonr
!               phy_f2d(i,j,nn) = buff2(i,j)
!             enddo
!           enddo
!         enddo
!       elseif (head%irealf .eq. 2) then
          num_dta = (ntraci+3)*levs + 2
          do nn=1,num_p3d
            do k=1,levs
              dati%i = num_dta + (nn-1)*levs + k      ! physics 3D grid fields
              dati%f => buff1
              call sigio_rrdbti(nft,head,dati,iret)
              call split2d_r(buff1(1),buffo,global_lats_r)
              CALL interpred(1,kmsk,buffo,buff2,global_lats_r,
     &                lonsperlar)
!
              do lan=1,LATS_NODE_R
                lat = global_lats_r(ipt_lats_node_r-1+lan)
                lons_lat = lonsperlar(lat)
                iblk=0
                il=1
                DO lon=1,lons_lat,NGPTC
                  NJEFF=MIN(NGPTC,lons_lat-lon+1)
                  iblk=iblk+1
                  do i=1,NJEFF
                    phy_f3d(i,k,iblk,lan,nn) = buff2(il,lan)
                    il=il+1
                  enddo
                enddo
              enddo
            enddo
          enddo
          do nn=1,num_p2d
            dati%i = num_dta + num_p3d*levs + nn      ! physics 2D grid fields
            dati%f => buff1
            call sigio_rrdbti(nft,head,dati,iret)
            call split2d_r(buff1,buffo,global_lats_r)
            CALL interpred(1,kmsk,buffo,buff2,global_lats_r,
     &              lonsperlar)
            do j=1,lats_node_r
              do i=1,lonr
                phy_f2d(i,j,nn) = buff2(i,j)
              enddo
            enddo
          enddo
!       endif
      endif
      if (head%nxss .gt. 0) then
        dati%i = num_dta + num_p3d*levs + num_p2d + 1    ! pdryini
        dati%f => buff1
        call sigio_rrdbti(nft,head,dati,iret)
        pdryini = buff1(1)
      endif
!
      iprint=0
 
!sela IF(IPRINT.NE.1) RETURN
!     DO K=1,LEVS
!sela    XL(K)=XL(K)-SL(K)
!        sL(K)=XL(K)
!     END DO
!Moor PRINT 100,(XL(K),K=1,LEVS)
!     DO K=1,LEVP1
!sela    XI(K)=XI(K)-SI(K)
!        sI(K)=XI(K)
!     END DO
!Moor PRINT 100,(XI(K),K=1,LEVP1)
100   FORMAT(1H0, 12   (E9.3))
!Moor PRINT 101,NFT,FHOUR,IDATE,Z00
101   FORMAT (1H0, 'IF ABOVE TWO ROWS NOT ZERO,INCONSISTENCY IN SIG.DEF'
     1,'ON NFT=',I2,2X,F6.1,2X,4(I4),'Z00=',E12.4)
!!!!
!sela DO LOCL=1,LS_MAX_NODE
!sela         L=ls_node(locl,1)
!sela    jbasev=ls_node(locl,2)
!sela    do indev = indlsev(l,l) , indlsev(jcap+mod(l,2),l)
!sela       rqe(INDEV,1,3)=rqe(indev,1,2)
!sela       rqe(INDEV,2,3)=rqe(indev,2,2)
!sela    END DO
!sela END DO
!sela DO LOCL=1,LS_MAX_NODE
!sela         L=ls_node(locl,1)
!sela    jbasod=ls_node(locl,3)
!sela    do indod = indlsod(l+1,l) , indlsod(jcap+mod(l+1,2),l)
!sela       rqo(INDod,1,3)=rqo(indod,1,2)
!sela       rqo(INDod,2,3)=rqo(indod,2,2)
!sela    END DO
!sela END DO
!
!   Convert from virtual temperature to enthalpy if need
!
      if( thermodyn_id.le.1 .and. sfcpress_id.le.1
     &    .and. gen_coord_hybrid ) then

!
        if (.NOT.LIOPE.or.icolor.ne.2) then
!
          direction=1	! from (tv,lnps) to (enthalpy,ps)
          call spect_tv_enthalpy_ps
!    &       (direction,run_enthalpy,
     &       (direction,
     &        QE,QO,TEE,TEO,RQE,RQO,
     &        ls_node,ls_nodes,max_ls_nodes,
     &        lats_nodes_r,global_lats_r,lonsperlar,
     &        plnev_r,plnod_r,plnew_r,plnow_r)

        endif	! .NOT.LIOPE.or.icolor.ne.2
!
        if( run_enthalpy ) then
          do k=1,levp1
            thref(k)=300.*cpd	
            teref(k)=255.*cpd
          enddo		
          thermodyn_id = 3
          sfcpress_id  = 2
        else
          thermodyn_id = 1
          sfcpress_id  = 2
        endif

      endif

 
!     print *,' leave treadeo.io_fd '		! hmhj

      RETURN
      END

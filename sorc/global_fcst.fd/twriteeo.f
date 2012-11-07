      subroutine twriteeo(nsig,IOPROC,fhour,idate,
     x           zqe_ls,qe_ls,tee_ls,die_ls,zee_ls,rqe_ls,gze_ls,
     x           zqo_ls,qo_ls,teo_ls,dio_ls,zeo_ls,rqo_ls,gzo_ls,
     x           sl,si,pdryini,
     x           ls_nodes,max_ls_nodes,ixgr,
     &           phy_f3d,phy_f2d,global_lats_r,lonsperlar,nblck)
!
!
      use resol_def
      use layout1
      use coordinate_def					! hmhj
      use sig_io
      use namelist_def
      use mpi_def
      use sigio_module
      use sigio_r_module
      use tracer_const
!
      implicit none
!
      integer   nblck, nsig, ioproc
      real (kind=kind_phys)
     &     phy_f3d(NGPTC,LEVS,NBLCK,lats_node_r,num_p3d)
     &,    phy_f2d(lonr,lats_node_r,num_p2d)
!
      integer kmsk0(lonr,lats_node_r), global_lats_r(latr)
     &,       lonsperlar(latr)
      real(kind=kind_io8) buffo(lonr,lats_node_r)
     &,                   buff1(lonr,lats_node_r)
      real (kind=kind_ior), target :: buff2(lonr*latr)
!
      REAL(kind=8) t1,t2,t3,t4,t5,t6,ta,tb,rtc
!
      real(kind=kind_evod) fhour
!
      integer              idate(4)
!
      real(kind=kind_evod) zqe_ls(len_trie_ls,2)
     &,                     qe_ls(len_trie_ls,2)
     &,                    tee_ls(len_trie_ls,2,levs)
     &,                    die_ls(len_trie_ls,2,levs)
     &,                    zee_ls(len_trie_ls,2,levs)
     &,                    rqe_ls(len_trie_ls,2,levh)
     &,                    gze_ls(len_trie_ls,2)
!
     &,                    zqo_ls(len_trio_ls,2)
     &,                     qo_ls(len_trio_ls,2)
     &,                    teo_ls(len_trio_ls,2,levs)
     &,                    dio_ls(len_trio_ls,2,levs)
     &,                    zeo_ls(len_trio_ls,2,levs)
     &,                    rqo_ls(len_trio_ls,2,levh)
     &,                    gzo_ls(len_trio_ls,2)
!
      REAL (KIND=KIND_IO8) pdryini
!!
      real(kind=kind_evod) sl(levs), si(levp1)
!
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
!
      integer              ierr,j,k,l,lenrec,locl,n,node
!
      integer              indjoff
      integer              indev
      integer              indod
      integer              indev1,indev2
      integer              indod1,indod2
      integer              ixgr
!
      real(kind=kind_ior), target ::   buf(lnt2)
      real(kind=kind_ior)   tmps(4+nodes+jcap1*nodes)
      real(kind=kind_ior)   tmpr(3+nodes+jcap1*(nodes-1))
!
      real (kind=kind_evod), target :: data_xss(1)
      type(sigio_head) head
      type(sigio_dbti) dati
      integer iret, num_dta
      logical first
      save head, first
      data first /.true./
!
      integer  il,ilen,i,msgtag,ls_diml,nodesl
c$$$      integer              kwq
c$$$      integer              kwte
c$$$      integer              kwdz
c$$$      integer              kwrq
c$$$cc
c$$$      parameter ( kwq  = 0*levs+0*levh+1 ,  !   qe/o_ls
c$$$     x            kwte = 0*levs+0*levh+2 ,  !  tee/o_ls
c$$$     x            kwdz = 1*levs+0*levh+2 ,  !  die/o_ls  zee/o_ls
c$$$     x            kwrq = 3*levs+0*levh+2 )  !  rqe/o_ls
cc
cc
      integer              indlsev,jbasev
      integer              indlsod,jbasod
cc
      include 'function2'
cc
cc
      integer              joff, jj
cc
      joff(n,l)=(jcap1)*(jcap2)-(jcap1-l)*(jcap2-l)+2*(n-l)
cc
cc
      real(kind=kind_mpi_r),allocatable :: trieo_ls_node (:,:,:)
      real(kind=kind_mpi_r),allocatable :: trieo_ls_nodes(:,:,:,:)
cc
      integer      lan,lat,iblk,lons_lat,lon,NJEFF,nn,lv
cc
!     integer kmsk0(lonr,lats_node_r)
CC
c$$$      REAL(KIND=KIND_IO4) Z(lnt2)
!     real(kind=kind_io8) buffo(lonr,lats_node_r)
!!
c$$$      common /z00_com/z
!!
!     print *,' enter twriteeo_fd ' 				! hmhj

      print *,' GWVX ALLOC  trieo_ls_node  OLD TWRITEEO'
      allocate ( trieo_ls_node  ( len_trie_ls_max+len_trio_ls_max,
     x                            2, 3*levs+1*levh+1 ) )
      print *,' GWVX ALLOCATED  trieo_ls_node OLD TWRITEEO '
cc
      if (.NOT.LIOPE.or.icolor.ne.2) then
!
        do j=1,len_trie_ls
          trieo_ls_node(j,1,kwq) = qe_ls(j,1)
          trieo_ls_node(j,2,kwq) = qe_ls(j,2)
        enddo
!
        do j=1,len_trio_ls
          trieo_ls_node(j+len_trie_ls_max,1,kwq) = qo_ls(j,1)
          trieo_ls_node(j+len_trie_ls_max,2,kwq) = qo_ls(j,2)
        enddo
!
        do k=1,levs
          do j=1,len_trie_ls
            trieo_ls_node(j,1,kwte+  k-1) = tee_ls(j,1,k)
            trieo_ls_node(j,2,kwte+  k-1) = tee_ls(j,2,k)
            trieo_ls_node(j,1,kwdz+2*k-2) = die_ls(j,1,k)
            trieo_ls_node(j,2,kwdz+2*k-2) = die_ls(j,2,k)
            trieo_ls_node(j,1,kwdz+2*k-1) = zee_ls(j,1,k)
            trieo_ls_node(j,2,kwdz+2*k-1) = zee_ls(j,2,k)
          enddo
          do j=1,len_trio_ls
            jj = j+len_trie_ls_max
            trieo_ls_node(jj,1,kwte+  k-1) = teo_ls(j,1,k)
            trieo_ls_node(jj,2,kwte+  k-1) = teo_ls(j,2,k)
            trieo_ls_node(jj,1,kwdz+2*k-2) = dio_ls(j,1,k)
            trieo_ls_node(jj,2,kwdz+2*k-2) = dio_ls(j,2,k)
            trieo_ls_node(jj,1,kwdz+2*k-1) = zeo_ls(j,1,k)
            trieo_ls_node(jj,2,kwdz+2*k-1) = zeo_ls(j,2,k)
          enddo
        enddo
!
        do k=1,levh
          do j=1,len_trie_ls
            trieo_ls_node(j,1,kwrq+  k-1) = rqe_ls(j,1,k)
            trieo_ls_node(j,2,kwrq+  k-1) = rqe_ls(j,2,k)
          enddo
          do j=1,len_trio_ls
            jj = j+len_trie_ls_max
            trieo_ls_node(jj,1,kwrq+  k-1) = rqo_ls(j,1,k)
            trieo_ls_node(jj,2,kwrq+  k-1) = rqo_ls(j,2,k)
          enddo
        enddo
      endif !.NOT.LIOPE.or.icolor.ne.2
!
      IF (LIOPE) then
        if (me.eq.0) then
          tmps=0.
          tmps(1)=PDRYINI
          tmps(2:nodes+1)=max_ls_nodes(1:nodes)
          tmps(nodes+2)=ls_dim
          tmps(nodes+3)=len_trie_ls_max
          tmps(nodes+4)=len_trio_ls_max
          il=nodes+4
          do i=1,nodes
            do j=1,ls_dim
              il=il+1
              tmps(il)=ls_nodes(j,i)
            enddo
          enddo
          ilen=4+nodes+jcap1*nodes
          msgtag=2345
          CALL mpi_send(tmps,ilen,MPI_R_IO_R,ioproc,
     &                  msgtag,MPI_COMM_ALL,info)
        endif
        if (me.eq.ioproc) then
          ilen=4+nodes-1+jcap1*(nodes-1)
          msgtag=2345
          CALL mpi_recv(tmpr,ilen,MPI_R_IO_R,0,
     &                  msgtag,MPI_COMM_ALL,stat,info)
          ls_nodes=0
          max_ls_nodes(1:nodes-1)=int(tmpr(2:nodes-1+1))
          ls_diml= int(tmpr(nodes+1))
          len_trie_ls_max=int(tmpr(nodes+2))
          len_trio_ls_max=int(tmpr(nodes+3))
          il=nodes+3
          do i=1,nodes-1
            do j=1,ls_diml
              il=il+1
              ls_nodes(j,i)=int(tmpr(il))
            enddo
          enddo
        endif
      ELSE
        tmpr(1) = pdryini
      ENDIF                 ! end of if (liope) 
cc
cx      print *,' GWVX ALLOC  trieo_ls_nodes '
      if ( me .eq. ioproc ) then
cgwv      write(0,*)  'ALLOC PARMS TWRITE ',len_trie_ls_max+len_trio_ls_max,
cgwv     1 2,3*levs+1*levh+1, nodes,1
 
         allocate ( trieo_ls_nodes ( len_trie_ls_max+len_trio_ls_max,
     x                               2, 3*levs+1*levh+1, nodes ),
     1              stat=ierr )
      else
         allocate ( trieo_ls_nodes ( 2, 2, 2, 2 ),stat=ierr )
      endif
      if (ierr .ne. 0) then
        write (0,*) ' GWX trieo_ls_nodes allocate failed'
        call mpi_abort(mpi_comm_all,ierr,i)
       endif
!
      lenrec = (len_trie_ls_max+len_trio_ls_max) * 2 * (3*levs+1*levh+1)
!
      t1=rtc()
      call mpi_gather( trieo_ls_node , lenrec, MPI_R_MPI_R,
     x                 trieo_ls_nodes, lenrec, MPI_R_MPI_R,
     x                 ioproc, MPI_COMM_ALL, ierr)
!
      t2=rtc()
!sela print *,' WRT TWRITE GATHER 1 TIME ',t2-t1
!
      deallocate ( trieo_ls_node  )
!
      if ( me .ne. ioproc ) then
           deallocate ( trieo_ls_nodes )
      endif
!
      IF (me.eq.ioproc) THEN
!
!     print *,' in TWRITEEO fhour=',fhour
!     if (fhour .gt. 0.0) call mpi_quit(1111)
!
        head%ivs     = 200509
        head%levs    = levs
        head%ntrac   = ntrac
        if (gen_coord_hybrid) then				! hmhj
          head%nvcoord = 3					! hmhj
        else if (hybrid) then					! hmhj
          head%nvcoord = 2
        else
          head%nvcoord = 1
        endif
!
        if (first) then
!         allocate(dati%f(lonr*latr))
!         call sigio_alhead(head,iret)
          idvm = thermodyn_id*10 + sfcpress_id
          call sigio_alhead(head,iret,levs,head%nvcoord,ntrac,idvm)
          first = .false.
        endif
!
        rewind(nsig)
!       head%clabsig=char(0)//char(0)//char(0)//char(0)//
!    &               char(0)//char(0)//char(0)//char(0)
        head%clabsig='GFS SIG '
!       write(nsig)head%clabsig
!     print 3000,lab,n
 3000 format(/ 'twriteeo lab ',4a10,' n=',i3)
!
        head%fhour   = fhour
        head%idate   = idate
        head%jcap    = jcap
        head%levs    = levs
        head%latb    = latr
        head%lonb    = lonr
        head%itrun   = itrun
        head%iorder  = 2
        head%irealf  = 2      ! for real * 8 data
        head%igen    = igen
        head%latf    = latg
        head%latr    = latr
        head%lonf    = lonf
        head%lonr    = lonr
        head%ntrac   = ntrac
        head%icen2   = icen2
        head%iens(1) = ienst
        head%iens(2) = iensi
        head%idpp    = 0
        head%idsl    = 0    ! idsl=2 for middle of layer
        head%idvm    = 0
        head%idvt    = idvt
!       head%idvt    = (ntoz-1) + 10 * (ntcw-1)
        head%idrun   = 0
        head%idusr   = 0
        head%pdryini = tmpr(1)
        head%ncldt   = ncld
        head%ixgr    = ixgr
        if (ixgr .gt. 0) then
          head%nxgr  = num_p3d*levs + num_p2d
        else
          head%nxgr  = 0
        endif
        if (ixgr .eq. 4 .or. ixgr .eq. 5) then
          head%nxss  = 1
        else
          head%nxss  = 0
        endif
!!
!!
        if (gen_coord_hybrid) then				! hmhj

          head%idvc    = 3					! hmhj
          head%idvm    = thermodyn_id*10 + sfcpress_id 		! hmhj
          head%idsl    = 2    ! idsl=2 for middle of layer	! hmhj
          do k=1,levp1						! hmhj
            head%vcoord(k,1)=ak5(k)*1000.			! hmhj
            head%vcoord(k,2)=bk5(k)				! hmhj
            head%vcoord(k,3)=ck5(k)*1000.			! hmhj
          enddo							! hmhj
          if (thermodyn_id == 3) then
            head%cpi(1:ntrac+1) = cpi(0:ntrac)
            head%ri(1:ntrac+1)  = ri(0:ntrac)
          endif
!     print *,' thermodyn_id=',thermodyn_id,' cpi=',head%cpi(0:ntrac+1)
!    &,' ri=',head%ri(1:ntrac+1)

        else if (hybrid) then					! hmhj
          head%idvc    = 2    ! for hybrid vertical coord.
          do k=1,levp1
!sela       ak5(k)=ak5r4(levp1+1-k)/1000.  !this is from tread
            head%vcoord(k,1)=ak5(levp1+1-k)*1000.


!sela       bk5(k)=bk5r4(levp1+1-k)        !this is from tread
            head%vcoord(k,2)=bk5(levp1+1-k)


!           if(me.eq.0)print 190,k,head%vcoord(k,1),head%vcoord(k,2)
190         format('in twrite k=',i2,'  ak5r4=',f13.6,'  bk5r4=',e13.5)

          enddo
        else
          head%idvc    = 1    ! for sigma vertical coord. (default)
          head%vcoord(:,1) = si (:)
        endif
!
        call sigio_rwhead(nsig,head,iret)
!
        buf = Z_R
        dati%i=1
        dati%f => buf
        call sigio_rwdbti(nsig,head,dati,iret)
!!
        t3=rtc()
!
        if (liope) then
          nodesl=nodes-1
        else
          nodesl=nodes
        endif
!!
        do k=1,3*levs+1*levh+1
          do node=1,nodesl
!
            jbasev=0
            do locl=1,max_ls_nodes(node)
              l=ls_nodes(locl,node)
              indev1 = indlsev(L,L)
              if (mod(L,2).eq.mod(jcap+1,2)) then
                indev2 = indlsev(jcap-1,L)
              else
                indev2 = indlsev(jcap  ,L)
              endif
              indjoff=joff(l,l)
              do indev = indev1 , indev2
                buf(indjoff+1) = trieo_ls_nodes(indev,1,k,node)
                buf(indjoff+2) = trieo_ls_nodes(indev,2,k,node)
                indjoff=indjoff+4
              end do
              jbasev=jbasev+(jcap+3-l)/2
            end do
!
            jbasod=len_trie_ls_max
            do locl=1,max_ls_nodes(node)
              l=ls_nodes(locl,node)
!
              if ( l .ne. jcap ) then  ! fix for out of bound error
                indod1 = indlsod(L+1,L)
                if (mod(L,2).eq.mod(jcap+1,2)) then
                  indod2 = indlsod(jcap  ,L)
                else
                  indod2 = indlsod(jcap-1,L)
                endif
                indjoff=joff(l+1,l)
                do indod = indod1 , indod2
                  buf(indjoff+1) = trieo_ls_nodes(indod,1,k,node)
                  buf(indjoff+2) = trieo_ls_nodes(indod,2,k,node)
                  indjoff=indjoff+4
                end do
                jbasod=jbasod+(jcap+2-l)/2
              endif  ! fix for out of bound error
            end do
          end do
!
          dati%i = k+1
          dati%f => buf
          call sigio_rwdbti(nsig,head,dati,iret)
        end do
!
!
        t4=rtc  ()
!sela print *, ' DISK TIME FOR SIG TWRITEO WRT ',t4-t3
!
!       print *,' twriteeo fhour=',fhour,' idate=',idate,' nsig=',nsig
!       print 3001,fhour,idate,nsig
!3001 format(/ 'twriteeo fhour=',f6.2,2x,4i5,2x,'n=',i2)
!
        num_dta = 3*levs+1*levh+2
        deallocate ( trieo_ls_nodes )
!
      endif   !me.eq.ioproc
!!
      if (ixgr .gt. 0) then
!!
        kmsk0=0
        do nn=1,num_p3d
          do k=1,levs
            buff1 = 0.0
            do lan=1,LATS_NODE_R
              lat = global_lats_r(ipt_lats_node_r-1+lan)
              lons_lat = lonsperlar(lat)
              iblk=0
              il=1
              DO lon=1,lons_lat,NGPTC
                NJEFF=MIN(NGPTC,lons_lat-lon+1)
                iblk=iblk+1
                do i=1,NJEFF
                  buff1(il,lan) = phy_f3d(i,k,iblk,lan,nn)
                  il=il+1
                enddo
              enddo
            enddo
            call uninterpred(1,kmsk0,buffo,buff1,global_lats_r
     &,                                         lonsperlar)
            call unsplit2d_r(ioproc,buff2(1),buffo, global_lats_r)
!
            IF (me.eq.ioproc) THEN
              dati%i = num_dta + (nn-1)*levs + k
              dati%f => buff2
              call sigio_rwdbti(nsig,head,dati,iret)
            endif
          end do
        end do
!
!
        do nn=1,num_p2d
          buff1(:,:) = phy_f2d(:,:,nn)
          call uninterpred(1,kmsk0,buffo,buff1,global_lats_r,lonsperlar)
          call unsplit2d_r(ioproc,buff2(1),buffo,global_lats_r)
           IF (me.eq.ioproc) THEN
             dati%i = num_dta + num_p3d*levs + nn
             dati%f => buff2
             call sigio_rwdbti(nsig,head,dati,iret)
          endif
        enddo
!!
        if (me .eq. ioproc) then
          if (ixgr .eq. 4 .or. ixgr .eq. 5) then
            data_xss(1) = tmpr(1)
            dati%i = num_dta + num_p3d*levs + num_p2d + 1
            dati%f => data_xss
            call sigio_rwdbti(nsig,head,dati,iret)
          endif
        endif
      endif
!
!     print *,' leave twriteeo_fd ' 				! hmhj

      return
      end

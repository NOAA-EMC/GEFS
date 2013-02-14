      subroutine spect_write(nsig,IOPROC,fhour,idate,
     x           sl,si,pdryini,
     x           ls_nodes,max_ls_nodes,
     &           global_lats_r,lonsperlar,
     x           trieo_ls_nodes_buf,ixgr)
cc
      use resol_def
      use layout1
      use coordinate_def					! hmhj
      use sig_io
      use namelist_def
      use mpi_def
      use sigio_module
      use sigio_r_module
      use tracer_const
      implicit none
cc
      integer              nsig,IOPROC
      REAL(kind=8) t1,t2,t3,t4,t5,t6,ta,tb,rtc
cc
      real(kind=kind_evod) fhour
cc
!     logical hybrid
      integer              idate(4)
cc
      REAL (KIND=KIND_IO8) pdryini
!!
      real(kind=kind_evod) sl(levs)
      real(kind=kind_evod) si(levp1)
cc
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
cc
      integer              ierr,j,k,l,lenrec,locl,n,node,lv,nn,ivs_loc
cc
      integer              indjoff
      integer              indev
      integer              indod
!
      integer ixgr
!
      real(kind=kind_io4), target :: buf(lnt2)
      real(kind=kind_io4)   tmps(4+nodes+jcap1*nodes)
      real(kind=kind_io4)   tmpr(3+nodes+jcap1*(nodes-1))
!
      INTEGER              GLOBAL_LATS_R(LATR)
      INTEGER                 LONSPERLAR(LATR)
!
      type(sigio_head) head
      type(sigio_dati) dati
      integer iret, num_dta
      logical first
      save head, first
      data first /.true./
!
cc
      integer  il,ilen,i,msgtag,ls_diml,nodesl
c$$$      integer              kwq
c$$$      integer              kwte
c$$$      integer              kwdz
c$$$      integer              kwrq
cc
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
      integer              joff
cc
      joff(n,l)=(jcap1)*(jcap2)-(jcap1-l)*(jcap2-l)+2*(n-l)
cc
cc
      real(kind=kind_mpi),allocatable :: trieo_ls_node (:,:,:)
      real(kind=kind_mpi),allocatable :: trieo_ls_nodes(:,:,:,:)
      real(kind=kind_mpi)trieo_ls_nodes_buf
     1 (len_trie_ls_max+len_trio_ls_max, 2, 3*levs+1*levh+1,nodes,1)
 
cc
cc
!     integer kmsk0(lonr,lats_node_r)
CC
c$$$      REAL(KIND=KIND_IO4) Z(lnt2)
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      real(kind=kind_io4) buff2(lonr,latr)
      integer idvm
!!
c$$$      common /z00_com/z
!!
!     print *,' enter spect_write_fd ' 		! hmhj

      if(allocated ( trieo_ls_node)) then
        continue
      else
        allocate ( trieo_ls_node  ( len_trie_ls_max+len_trio_ls_max,
     x                            2, 3*levs+1*levh+1 ) )
      endif
      continue
cc
      if ( me .eq. ioproc ) then
         allocate ( trieo_ls_nodes ( len_trie_ls_max+len_trio_ls_max,
     x                               2, 3*levs+1*levh+1, nodes ) )
      else
         continue
      endif
cc
      lenrec = (len_trie_ls_max+len_trio_ls_max) * 2 * (3*levs+1*levh+1)
cc
      t1=rtc()
      if(me .eq. ioproc) then
        do i=1,nodes
          do j=1, 3*levs+1*levh+1
            do k=1,2
              do l=1, len_trie_ls_max+len_trio_ls_max
                trieo_ls_nodes(l,k,j,i)= trieo_ls_nodes_buf(l,k,j,i,1)
              end do
             end do
          end do
        end do
      endif
      t2=rtc()
cgwv      print *,' WRT TWRITE GATHER 1 TIME ',t2-t1
cc
cc
      deallocate ( trieo_ls_node  )
cgwv      write (0,*)' GWVX deallocated trieo_ls_NODE   in twritee_all'
cc
cc
cc
cc
      IF (me.eq.ioproc) THEN
!
        if (first) then
          first = .false.
!         if (ivsupa .gt. 0) head%ivs = ivsupa
!         ivs_loc = max(ivsinp, ivsupa)
!         head%ivs = max(head%ivs, ivs_loc)
          head%ivs = 198410
          if (levs > 99) head%ivs = 200509
          head%levs    = levs
          head%ntrac   = ntrac
          if (gen_coord_hybrid) then				! hmhj
            head%ivs = 200509
            head%nvcoord = 3					! hmhj
!           call sigio_alhead(head,iret,levs,3,ntrac)		! hmhj
          else if (hybrid) then					! hmhj
            head%nvcoord = 2
!           call sigio_alhead(head,iret,levs,2,ntrac)
          else
            head%nvcoord = 1
!           if (head%ivs .lt. 200509) head%nvcoord = 2
!           call sigio_alhead(head,iret,levs,head%nvcoord,ntrac)
          endif
          idvm = thermodyn_id*10 + sfcpress_id
!     print *,' before alhead idvm=',idvm,thermodyn_id,sfcpress_id
          call sigio_alhead(head,iret,levs,head%nvcoord,ntrac,idvm)
          head%nxgr = 0
          head%nxss = 0
        endif
        rewind(nsig)
        head%clabsig=char(0)//char(0)//char(0)//char(0)//
     &               char(0)//char(0)//char(0)//char(0)
!       write(nsig)lab
cc    print 3000,lab,n
 3000 format(/ 'twriteeo lab ',4a10,' n=',i3)
cc
        head%fhour   = fhour
        head%idate   = idate
        head%jcap    = jcap
        head%latb    = latr
        head%lonb    = lonr
        head%itrun   = 1
        head%iorder  = 2
        head%irealf  = 1      ! for real * 4 data
        head%igen    = igen
        head%latf    = latg
        head%latr    = latr
        head%lonf    = lonf
        head%lonr    = lonr
        head%icen2   = icen2
        head%iens(1) = ienst
        head%iens(2) = iensi
        head%idpp    = 0
        head%idsl    = 0    ! idsl=2 for middle of layer
        head%idvm    = 0
!       head%idvt    = (ntoz-1) + 10 * (ntcw-1)
        head%idvt    = idvt
        head%idrun   = 0
        head%idusr   = 0
        head%pdryini = pdryini
        head%ncldt   = ncld
        head%ixgr   = ixgr
        head%ldata(:) = lnt2

!!
        if (gen_coord_hybrid) then				! hmhj
          head%idvc    = 3	  				! hmhj
          head%idvm    = thermodyn_id*10+sfcpress_id  		! hmhj
          head%idsl    = 2   					! hmhj
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
                                                                                         
!           print 190,k,head%vcoord(k,1),head%vcoord(k,2)
190         format('in twrite k=',i2,'  ak5r4=',f13.6,'  bk5r4=',e13.5)
          enddo
        else
          head%idvc    = 1    ! for sigma vertical coord. (default)
          head%vcoord(:,1) = si(:)
        endif
!
!       head%ndata = 3*levs+levh+2
        call sigio_rwhead(nsig,head,iret)
!
        buf = Z
        dati%i=1
        dati%f => buf
        call sigio_rwdati(nsig,head,dati,iret)
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
             indjoff=joff(l,l)
             do indev = indlsev(l,l) , indlsev(jcap-mod(l,2),l)
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
             if ( l .ne. jcap ) then  ! fix for out of bound error
               indjoff=joff(l+1,l)
               do indod = indlsod(l+1,l) , indlsod(jcap-mod(l+1,2),l)
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
          call sigio_rwdati(nsig,head,dati,iret)
       end do
!
!
        t4=rtc  ()
!sela   print *, ' DISK TIME FOR SIG TWRITEO WRT ',t4-t3
!
        print *,' twriteeo fhour=',fhour,' idate=',idate,' nsig=',nsig
!       print 3001,fhour,idate,nsig
!3001   format(/ 'twriteeo fhour=',f6.2,2x,4i5,2x,'n=',i2)
!
        deallocate ( trieo_ls_nodes )
        write (0,*)' GWVX deallocated trieo_ls_nodes  in twritee_all IO'
!
      endif   !me.eq.ioproc
!!
!!
!     print *,' leave spect_write_fd ' 		! hmhj

      return
      end

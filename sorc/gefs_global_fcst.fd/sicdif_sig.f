      subroutine sicdife_sig(de,te,qe,xe,ye,ze,am,bm,tov,sv,dt,ue,ve,
     x                       ls_node,
     x                       snnp1ev,ndexev,locl,de_n)
      use resol_def
      use layout1
      use matrix_sig_def
      implicit none
      real(kind=kind_evod) de_n(len_trie_ls,2,levs)
 
      real(kind=kind_evod) de(len_trie_ls,2,levs),te(len_trie_ls,2,levs)
      real(kind=kind_evod) xe(len_trie_ls,2,levs),ye(len_trie_ls,2,levs)
      real(kind=kind_evod) ue(len_trie_ls,2,levs),ve(len_trie_ls,2,levs)
      real(kind=kind_evod) qe(len_trie_ls,2),    ze(len_trie_ls,2)
      real(kind=kind_evod)  am(levs,levs), bm(levs,levs)
      real(kind=kind_evod) tov(levs),    sv(levs)
      real(kind=kind_evod) dt
      integer              ls_node(ls_dim,3)
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      integer               ndexev(len_trie_ls)
      integer              i,indev,indev1,indev2,j,k,l,locl,n
      real(kind=kind_evod) qdtze(len_trie_ls,2), 
     .   elne(len_trie_ls,2,levs)
      real(kind=kind_evod) svdt, u1, u2
      integer              indlsev,jbasev
      integer              indlsod,jbasod
      include 'function2'
      real(kind=kind_evod) cons0,cons1,cons2     !constant

      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
      cons2 = 2.d0     !constant
           l = ls_node(locl,1)
      jbasev = ls_node(locl,2)
      indev1 = indlsev(L,L)
      if (mod(L,2).eq.mod(jcap+1,2)) then
         indev2 = indlsev(jcap+1,L)
      else
         indev2 = indlsev(jcap  ,L)
      endif
!!!
!!!mjr xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!!!
!!! subtract off linear dependence on divergence (formerly in gloopa)
!!!
!!!  do k=1,levs
!!!  do j=1,levs
!!!     do i=1,len_trie_ls
!!!          trie_ls(i,1,P_y+k-1)=
!!! x        trie_ls(i,1,P_y+k-1)-bm(k,j)*trie_ls(i,1,P_di+j-1)
!!!          trie_ls(i,2,P_y+k-1)=
!!! x        trie_ls(i,2,P_y+k-1)-bm(k,j)*trie_ls(i,2,P_di+j-1)
!!!     enddo
!!!  enddo
!!!  enddo
!!!
!!!mjr xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!!!
      do j=1,levs
!        do k=1,levs,2
         do k=1,levs
               do indev = indev1 , indev2
                  ye(indev,1,j) =
     x            ye(indev,1,j) - de_n(indev,1,k  )*bm(j,k  )
!    x                          - de_n(indev,1,k+1)*bm(j,k+1)
                  ye(indev,2,j) =
     x            ye(indev,2,j) - de_n(indev,2,k  )*bm(j,k  )
!    x                          - de_n(indev,2,k+1)*bm(j,k+1)
               enddo
         enddo
      enddo
!!!
!sela  if(indev2.gt.len_trie_ls)then
!sela  print*,'semi_imp length too big',indev2
!sela  call mpi_quit(12)
!sela  endif
!!!      call dgemm ('n', 't',
!!!  &               indev2-indev1+1, levs, levs, -1.d0,
!!!  &               de_n(indev1,1,1), len_trie_ls*2,
!!!  &               bm(1,1), levs, cons1,
!!!  &               ye(indev1,1,1), len_trie_ls*2)
!!!      call dgemm ('n', 't',
!!!  &               indev2-indev1+1, levs, levs, -1.d0,
!!!  &               de_n(indev1,2,1), len_trie_ls*2,
!!!  &               bm(1,1), levs, cons1,
!!!  &               ye(indev1,2,1), len_trie_ls*2)
!!!
!!!mjr xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!!!
         do indev = indev1 , indev2
            qdtze(indev,1)   =      qe(indev,1)
     x                       +   dt*ze(indev,1)
            qdtze(indev,2)   =      qe(indev,2)
     x                       +   dt*ze(indev,2)
         enddo
      do k=1,levs
            do indev = indev1 , indev2
               elne(indev,1,k) =             te(indev,1,k)
     x                                + dt*  ye(indev,1,k)
               elne(indev,2,k) =             te(indev,2,k)
     x                                + dt*  ye(indev,2,k)
            enddo
      enddo
!!!
!!!   do 17 j=1,levs
!!!         do indev = indev1 , indev2
!!!            ve(indev,1,j) = cons0     !constant
!!!            ve(indev,2,j) = cons0     !constant
!!!         enddo
!!!      do k=1,levs,2
!!!            do indev = indev1 , indev2
!!!               ve(indev,1,j) =
!!!  x            ve(indev,1,j) + elne(indev,1,k  )*am(j,k  )
!!!  x                          + elne(indev,1,k+1)*am(j,k+1)
!!!               ve(indev,2,j) =
!!!  x            ve(indev,2,j) + elne(indev,2,k  )*am(j,k  )
!!!  x                          + elne(indev,2,k+1)*am(j,k+1)
!!!            enddo
!!!      enddo
!!!
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               elne(indev1,1,1), len_trie_ls*2,
     &               am(1,1), levs, cons0,
     &               ve(indev1,1,1), len_trie_ls*2)
         call dgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               elne(indev1,2,1), len_trie_ls*2,
     &               am(1,1), levs, cons0,
     &               ve(indev1,2,1), len_trie_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               elne(indev1,1,1), len_trie_ls*2,
     &               am(1,1), levs, cons0,
     &               ve(indev1,1,1), len_trie_ls*2)
         call sgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               elne(indev1,2,1), len_trie_ls*2,
     &               am(1,1), levs, cons0,
     &               ve(indev1,2,1), len_trie_ls*2)
      endif !-----------------------------------------------------------
!!!
      do 17 j=1,levs
!!!
            do indev = indev1 , indev2
               ve(indev,1,j) =
     x         ve(indev,1,j) + tor_sig(j)*qdtze(indev,1)
               ve(indev,1,j) =
     x         ve(indev,1,j) *      snnp1ev(indev)
               ve(indev,1,j) =
     x         ve(indev,1,j) +           xe(indev,1,j)
               ue(indev,1,j) =           de(indev,1,j)
     x                       +           ve(indev,1,j)*dt
               ve(indev,2,j) =
     x         ve(indev,2,j) + tor_sig(j)*qdtze(indev,2)
               ve(indev,2,j) =
     x         ve(indev,2,j) *      snnp1ev(indev)
               ve(indev,2,j) =
     x         ve(indev,2,j) +           xe(indev,2,j)
               ue(indev,2,j) =           de(indev,2,j)
     x                       +           ve(indev,2,j)*dt
            enddo
   17 continue
!!!
!!!   do 10 j=1,levs
!!!         do indev = indev1 , indev2
!!!            ve(indev,1,j) = cons0     !constant
!!!            ve(indev,2,j) = cons0     !constant
!!!         enddo
!!!      do k=1,levs,2
!!!            do indev = indev1 , indev2
!!!               ve(indev,1,j) =
!!!  x            ve(indev,1,j) + D_m(j,k  ,ndexev(indev)+1)
!!!  x                                        *ue(indev,1,k  )
!!!  x                          + D_m(j,k+1,ndexev(indev)+1)
!!!  x                                        *ue(indev,1,k+1)
!!!               ve(indev,2,j) =
!!!  x            ve(indev,2,j) + D_m(j,k  ,ndexev(indev)+1)
!!!  x                                       * ue(indev,2,k  )
!!!  x                          + D_m(j,k+1,ndexev(indev)+1)
!!!  x                                       * ue(indev,2,k+1)
!!!            enddo
!!!      enddo
!!!10 continue
!!!
      if ( kind_evod .eq. 8 ) then !------------------------------------
         do indev = indev1 , indev2
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,1,1), len_trie_ls*2,
     &                  D_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  ve(indev,1,1), len_trie_ls*2)
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,2,1), len_trie_ls*2,
     &                  D_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  ve(indev,2,1), len_trie_ls*2)
         enddo
      else !------------------------------------------------------------
         do indev = indev1 , indev2
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,1,1), len_trie_ls*2,
     &                  D_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  ve(indev,1,1), len_trie_ls*2)
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,2,1), len_trie_ls*2,
     &                  D_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  ve(indev,2,1), len_trie_ls*2)
         enddo
      endif !-----------------------------------------------------------
!!!
!!!      do indev = indev1 , indev2
!!!         ue(indev,1,1) = cons0     !constant
!!!         ue(indev,2,1) = cons0     !constant
!!!      enddo
!!!   do k=1,levs
!!!         do indev = indev1 , indev2
!!!            ue(indev,1,1) =
!!!  x         ue(indev,1,1) + sv(k)*dt*ve(indev,1,k)
!!!            ue(indev,2,1) =
!!!  x         ue(indev,2,1) + sv(k)*dt*ve(indev,2,k)
!!!         enddo
!!!   enddo
!!!
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indev2-indev1+1, 1, levs, dt,
     &               ve(indev1,1,1), len_trie_ls*2,
     &               sv(1), 1, cons0,
     &               ue(indev1,1,1), len_trie_ls*2)
         call dgemm ('n', 't',
     &               indev2-indev1+1, 1, levs, dt,
     &               ve(indev1,2,1), len_trie_ls*2,
     &               sv(1), 1, cons0,
     &               ue(indev1,2,1), len_trie_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indev2-indev1+1, 1, levs, dt,
     &               ve(indev1,1,1), len_trie_ls*2,
     &               sv(1), 1, cons0,
     &               ue(indev1,1,1), len_trie_ls*2)
         call sgemm ('n', 't',
     &               indev2-indev1+1, 1, levs, dt,
     &               ve(indev1,2,1), len_trie_ls*2,
     &               sv(1), 1, cons0,
     &               ue(indev1,2,1), len_trie_ls*2)
      endif !-----------------------------------------------------------
!!!
         do indev = indev1 , indev2
            qdtze(indev,1)   =
     x      qdtze(indev,1)   +          ue(indev,1,1)
               ze(indev,1)   = cons2*qdtze(indev,1)  !constant
     x                       -          qe(indev,1)
            qdtze(indev,2)   =
     x      qdtze(indev,2)   +          ue(indev,2,1)
               ze(indev,2)   = cons2*qdtze(indev,2)  !constant
     x                       -          qe(indev,2)
         enddo
!!!
!!!   do j=1,levs
!!!         do indev = indev1 , indev2
!!!            ue(indev,1,j) = cons0   !constant
!!!            ue(indev,2,j) = cons0   !constant
!!!         enddo
!!!      do k=1,levs,2
!!!            do indev = indev1 , indev2
!!!               ue(indev,1,j) =
!!!  x            ue(indev,1,j) + ve(indev,1,k  )*bm(j,k  )
!!!  x                          + ve(indev,1,k+1)*bm(j,k+1)
!!!               ue(indev,2,j) =
!!!  x            ue(indev,2,j) + ve(indev,2,k  )*bm(j,k  )
!!!  x                          + ve(indev,2,k+1)*bm(j,k+1)
!!!            enddo
!!!      enddo
!!!
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               ve(indev1,1,1), len_trie_ls*2,
     &               bm(1,1), levs, cons0,
     &               ue(indev1,1,1), len_trie_ls*2)
         call dgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               ve(indev1,2,1), len_trie_ls*2,
     &               bm(1,1), levs, cons0,
     &               ue(indev1,2,1), len_trie_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               ve(indev1,1,1), len_trie_ls*2,
     &               bm(1,1), levs, cons0,
     &               ue(indev1,1,1), len_trie_ls*2)
         call sgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               ve(indev1,2,1), len_trie_ls*2,
     &               bm(1,1), levs, cons0,
     &               ue(indev1,2,1), len_trie_ls*2)
      endif !-----------------------------------------------------------
!!!
      do j=1,levs
!!!
            do indev = indev1 , indev2
               u1 = elne(indev,1,j) + dt * ue(indev,1,j)
               ye(indev,1,j) = cons2*u1-te(indev,1,j) !constant
               xe(indev,1,j) = cons2*   ve(indev,1,j) !constant
     x                                 -de(indev,1,j)
               u2 = elne(indev,2,j) + dt * ue(indev,2,j)
               ye(indev,2,j) = cons2*u2-te(indev,2,j) !constant
               xe(indev,2,j) = cons2*   ve(indev,2,j) !constant
     x                                 -de(indev,2,j)
            enddo
      enddo
      return
      end
      subroutine sicdifo_sig(do,to,qo,xo,yo,zo,am,bm,tov,sv,dt,uo,vo,
     x                       ls_node,
     x                       snnp1od,ndexod,locl,do_n)

      use resol_def
      use layout1
      use matrix_sig_def
      implicit none
      real(kind=kind_evod) do_n(len_trio_ls,2,levs)
 
      real(kind=kind_evod) do(len_trio_ls,2,levs),to(len_trio_ls,2,levs)
      real(kind=kind_evod) xo(len_trio_ls,2,levs),yo(len_trio_ls,2,levs)
      real(kind=kind_evod) uo(len_trio_ls,2,levs),vo(len_trio_ls,2,levs)
      real(kind=kind_evod) qo(len_trio_ls,2),    zo(len_trio_ls,2)
      real(kind=kind_evod)  am(levs,levs), bm(levs,levs)
      real(kind=kind_evod) tov(levs),    sv(levs)
      real(kind=kind_evod) dt
      integer              ls_node(ls_dim,3)
      real(kind=kind_evod) snnp1od(len_trio_ls)
      integer               ndexod(len_trio_ls)
      integer              i,indod,indod1,indod2,j,k,l,locl,n
      real(kind=kind_evod) qdtzo(len_trio_ls,2), 
     . elno(len_trio_ls,2,levs)
      real(kind=kind_evod) svdt, u1, u2
      integer              indlsev,jbasev
      integer              indlsod,jbasod
      include 'function2'
      real(kind=kind_evod) cons0,cons1,cons2     !constant
      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
      cons2 = 2.d0     !constant
           l = ls_node(locl,1)
      jbasod = ls_node(locl,3)
      indod1 = indlsod(L+1,L)
      if (mod(L,2).eq.mod(jcap+1,2)) then
         indod2 = indlsod(jcap  ,L)
      else
         indod2 = indlsod(jcap+1,L)
      endif
!!!
!!!mjr xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!!!
!!! subtract off linear dependence on divergence (formerly in gloopa)
!!!
!!!  do k=1,levs
!!!  do j=1,levs
!!!     do i=1,len_trio_ls
!!!          trio_ls(i,1,P_y+k-1)=
!!! x        trio_ls(i,1,P_y+k-1)-bm(k,j)*trio_ls(i,1,P_di+j-1)
!!!          trio_ls(i,2,P_y+k-1)=
!!! x        trio_ls(i,2,P_y+k-1)-bm(k,j)*trio_ls(i,2,P_di+j-1)
!!!     enddo
!!!  enddo
!!!  enddo
!!!
!!!mjr xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!!!
      do j=1,levs
!        do k=1,levs,2
         do k=1,levs
               do indod = indod1 , indod2
                  yo(indod,1,j) =
     x            yo(indod,1,j) - do_n(indod,1,k  )*bm(j,k  )
!    x                          - do_n(indod,1,k+1)*bm(j,k+1)
                  yo(indod,2,j) =
     x            yo(indod,2,j) - do_n(indod,2,k  )*bm(j,k  )
!    x                          - do_n(indod,2,k+1)*bm(j,k+1)
               enddo
         enddo
      enddo
!!!
!sela  if(indod2.gt.len_trio_ls)then
!sela  print*,'semi_imp od  length too big',indod2
!sela  call mpi_quit(14)
!sela  endif
!!!      call dgemm ('n', 't',
!!!  &               indod2-indod1+1, levs, levs, -1.d0,
!!!  &               do_n(indod1,1,1), len_trio_ls*2,
!!!  &               bm(1,1), levs, cons1,
!!!  &               yo(indod1,1,1), len_trio_ls*2)
!!!      call dgemm ('n', 't',
!!!  &               indod2-indod1+1, levs, levs, -1.d0,
!!!  &               do_n(indod1,2,1), len_trio_ls*2,
!!!  &               bm(1,1), levs, cons1,
!!!  &               yo(indod1,2,1), len_trio_ls*2)
!!!
!!!mjr xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!!!
         do indod = indod1, indod2
            qdtzo(indod,1)   =      qo(indod,1)
     x                       +   dt*zo(indod,1)
            qdtzo(indod,2)   =      qo(indod,2)
     x                       +   dt*zo(indod,2)
         enddo
      do k=1,levs
            do indod = indod1, indod2
               elno(indod,1,k) =             to(indod,1,k)
     x                                + dt*  yo(indod,1,k)
 
               elno(indod,2,k) =             to(indod,2,k)
     x                                + dt*  yo(indod,2,k)
            enddo
      enddo
!!!
!!!   do 17 j=1,levs
!!!         do indod = indod1, indod2
!!!            vo(indod,1,j) = cons0     !constant
!!!            vo(indod,2,j) = cons0     !constant
!!!         enddo
!!!      do k=1,levs,2
!!!            do indod = indod1, indod2
!!!               vo(indod,1,j) =
!!!  x            vo(indod,1,j) + elno(indod,1,k  )*am(j,k  )
!!!  x                          + elno(indod,1,k+1)*am(j,k+1)
!!!               vo(indod,2,j) =
!!!  x            vo(indod,2,j) + elno(indod,2,k  )*am(j,k  )
!!!  x                          + elno(indod,2,k+1)*am(j,k+1)
!!!            enddo
!!!      enddo
!!!
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               elno(indod1,1,1), len_trio_ls*2,
     &               am(1,1), levs, cons0,
     &               vo(indod1,1,1), len_trio_ls*2)
         call dgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               elno(indod1,2,1), len_trio_ls*2,
     &               am(1,1), levs, cons0,
     &               vo(indod1,2,1), len_trio_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               elno(indod1,1,1), len_trio_ls*2,
     &               am(1,1), levs, cons0,
     &               vo(indod1,1,1), len_trio_ls*2)
         call sgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               elno(indod1,2,1), len_trio_ls*2,
     &               am(1,1), levs, cons0,
     &               vo(indod1,2,1), len_trio_ls*2)
      endif !-----------------------------------------------------------
!!!
      do 17 j=1,levs
!!!
            do indod = indod1, indod2
               vo(indod,1,j) =
     x         vo(indod,1,j) + tor_sig(j)*qdtzo(indod,1)
 
               vo(indod,1,j) =
     x         vo(indod,1,j) *      snnp1od(indod)
 
               vo(indod,1,j) =
     x         vo(indod,1,j) +           xo(indod,1,j)
 
               uo(indod,1,j) =           do(indod,1,j)
     x                       +           vo(indod,1,j)*dt
               vo(indod,2,j) =
     x         vo(indod,2,j) + tor_sig(j)*qdtzo(indod,2)
               vo(indod,2,j) =
     x         vo(indod,2,j) *      snnp1od(indod)
               vo(indod,2,j) =
     x         vo(indod,2,j) +           xo(indod,2,j)
               uo(indod,2,j) =           do(indod,2,j)
     x                       +           vo(indod,2,j)*dt
            enddo
   17 continue
!!!
!!!   do 10 j=1,levs
!!!         do indod = indod1, indod2
!!!            vo(indod,1,j) = cons0     !constant
!!!            vo(indod,2,j) = cons0     !constant
!!!         enddo
!!!      do k=1,levs,2
!!!            do indod = indod1, indod2
!!!               vo(indod,1,j) =
!!!  x            vo(indod,1,j) + D_m(j,k  ,ndexod(indod)+1)
!!!  x                                        *uo(indod,1,k  )
!!!  x                          + D_m(j,k+1,ndexod(indod)+1)
!!!  x                                        *uo(indod,1,k+1)
!!!               vo(indod,2,j) =
!!!  x            vo(indod,2,j) + D_m(j,k  ,ndexod(indod)+1)
!!!  x                                        *uo(indod,2,k  )
!!!  x                          + D_m(j,k+1,ndexod(indod)+1)
!!!  x                                        *uo(indod,2,k+1)
!!!            enddo
!!!      enddo
!!!10 continue
!!!
      if ( kind_evod .eq. 8 ) then !------------------------------------
         do indod = indod1 , indod2
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,1,1), len_trio_ls*2,
     &                  D_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  vo(indod,1,1), len_trio_ls*2)
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,2,1), len_trio_ls*2,
     &                  D_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  vo(indod,2,1), len_trio_ls*2)
         enddo
      else !------------------------------------------------------------
         do indod = indod1 , indod2
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,1,1), len_trio_ls*2,
     &                  D_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  vo(indod,1,1), len_trio_ls*2)
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,2,1), len_trio_ls*2,
     &                  D_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  vo(indod,2,1), len_trio_ls*2)
         enddo
      endif !-----------------------------------------------------------
!!!
!!!      do indod = indod1, indod2
!!!         uo(indod,1,1) = cons0     !constant
!!!         uo(indod,2,1) = cons0     !constant
!!!      enddo
!!!   do k=1,levs
!!!         do indod = indod1, indod2
!!!            uo(indod,1,1) =
!!!  x         uo(indod,1,1) + sv(k)*dt*vo(indod,1,k)
!!!            uo(indod,2,1) =
!!!  x         uo(indod,2,1) + sv(k)*dt*vo(indod,2,k)
!!!         enddo
!!!   enddo
!!!
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indod2-indod1+1, 1, levs, dt,
     &               vo(indod1,1,1), len_trio_ls*2,
     &               sv(1), 1, cons0,
     &               uo(indod1,1,1), len_trio_ls*2)
         call dgemm ('n', 't',
     &               indod2-indod1+1, 1, levs, dt,
     &               vo(indod1,2,1), len_trio_ls*2,
     &               sv(1), 1, cons0,
     &               uo(indod1,2,1), len_trio_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indod2-indod1+1, 1, levs, dt,
     &               vo(indod1,1,1), len_trio_ls*2,
     &               sv(1), 1, cons0,
     &               uo(indod1,1,1), len_trio_ls*2)
         call sgemm ('n', 't',
     &               indod2-indod1+1, 1, levs, dt,
     &               vo(indod1,2,1), len_trio_ls*2,
     &               sv(1), 1, cons0,
     &               uo(indod1,2,1), len_trio_ls*2)
      endif !-----------------------------------------------------------
!!!
         do indod = indod1, indod2
            qdtzo(indod,1)   =
     x      qdtzo(indod,1)   +          uo(indod,1,1)
               zo(indod,1)   = cons2*qdtzo(indod,1)  !constant
     x                       -          qo(indod,1)
            qdtzo(indod,2)   =
     x      qdtzo(indod,2)   +          uo(indod,2,1)
               zo(indod,2)   = cons2*qdtzo(indod,2)  !constant
     x                       -          qo(indod,2)
         enddo
!!!
!!!   do j=1,levs
!!!         do indod = indod1, indod2
!!!            uo(indod,1,j) = cons0   ! constant
!!!            uo(indod,2,j) = cons0   ! constant
!!!         enddo
!!!      do k=1,levs,2
!!!            do indod = indod1, indod2
!!!               uo(indod,1,j) =
!!!  x            uo(indod,1,j) + vo(indod,1,k  )*bm(j,k  )
!!!  x                          + vo(indod,1,k+1)*bm(j,k+1)
!!!               uo(indod,2,j) =
!!!  x            uo(indod,2,j) + vo(indod,2,k  )*bm(j,k  )
!!!  x                          + vo(indod,2,k+1)*bm(j,k+1)
!!!            enddo
!!!      enddo
!!!
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               vo(indod1,1,1), len_trio_ls*2,
     &               bm(1,1), levs, cons0,
     &               uo(indod1,1,1), len_trio_ls*2)
         call dgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               vo(indod1,2,1), len_trio_ls*2,
     &               bm(1,1), levs, cons0,
     &               uo(indod1,2,1), len_trio_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               vo(indod1,1,1), len_trio_ls*2,
     &               bm(1,1), levs, cons0,
     &               uo(indod1,1,1), len_trio_ls*2)
         call sgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               vo(indod1,2,1), len_trio_ls*2,
     &               bm(1,1), levs, cons0,
     &               uo(indod1,2,1), len_trio_ls*2)
      endif !-----------------------------------------------------------
!!!
      do j=1,levs
!!!
            do indod = indod1, indod2
               u1 = elno(indod,1,j) + dt * uo(indod,1,j)
               yo(indod,1,j) = cons2*u1-to(indod,1,j) !constant
               xo(indod,1,j) = cons2*   vo(indod,1,j) !constant
     x                                 -do(indod,1,j)
               u2 = elno(indod,2,j) + dt * uo(indod,2,j)
               yo(indod,2,j) = cons2*u2-to(indod,2,j) !constant
               xo(indod,2,j) = cons2*   vo(indod,2,j) !constant
     x                                 -do(indod,2,j)
            enddo
      enddo
      return
      end
      subroutine matinv(a,m,n,d,p,r)
      use machine
      implicit none
      integer              i,j,k,l,m,n
      real(kind=kind_evod) a(m,n,n),d(m),p(m),r(m)
      real(kind=kind_evod) cons0,cons1     !constant
      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
      do 200 l=1,m
      d(l)=cons1     !constant
  200 continue
      do 100 k=1,n
      do 250 l=1,m
      p(l)=a(l,k,k)
  250 continue
      do 300 l=1,m
      r(l)=-cons1/p(l)     !constant
  300 continue
      do 350 l=1,m
      a(l,k,k)=cons0       !constant
  350 continue
      do  20 i=1,n
      do 400 l=1,m
      a(l,i,k)=a(l,i,k)*r(l)
  400 continue
   20 continue
      do 60 i=1,n
      if(i.eq.k) go to 60
      do  40 j=1,n
      do 450 l=1,m
      a(l,i,j)=a(l,i,k)*a(l,k,j)+a(l,i,j)
  450 continue
   40 continue
   60 continue
      do 600 l=1,m
      r(l)=-r(l)
  600 continue
      do  80 j=1,n
      do 650 l=1,m
      a(l,k,j)=a(l,k,j)*r(l)
  650 continue
   80 continue
      do 700 l=1,m
      d(l)=d(l)*p(l)
  700 continue
      do 750 l=1,m
      a(l,k,k)=r(l)
  750 continue
  100 continue
      return
      end

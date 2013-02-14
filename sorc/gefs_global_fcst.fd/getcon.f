      subroutine getcon(nges,nradr,nradf,nnmod,
!     subroutine getcon(n1,n2,nges,nradr,nradf,nnmod,
     x                  n3,n4,nflps,nsigi,nsigs,nsfci,
     x                  nznli,nsfcf,nznlf,nsfcs,nznls,
     x                  ndgi,ndgf,ngpken,
     x                  mods,niter,ini,nstep,nfiles,
     x                  ksout,ifges,ibrad,
     x                  ls_node,ls_nodes,max_ls_nodes,
     x                  lats_nodes_a,global_lats_a,
     x                  lonsperlat,
     x                  lats_nodes_r,global_lats_r,
     x                  lonsperlar,
     x                  lats_nodes_ext,global_lats_ext,
     x                  epse,epso,epsedn,epsodn,
     x                  snnp1ev,snnp1od,ndexev,ndexod,
     x                  plnev_a,plnod_a,pddev_a,pddod_a,
     x                  plnew_a,plnow_a,
     x                  plnev_r,plnod_r,pddev_r,pddod_r,
     x                  plnew_r,plnow_r,colat1)
cc
      use resol_def
      use layout1

      use gg_def
      use vert_def
!     use sig_io
      use date_def
      use namelist_def
      use mpi_def
      implicit none
cc
!!
      integer              ibrad,ifges,ini,j,k,ksout,l,lat,lev,mods
      integer              n,n3,n4,ndgf,ndgi,nfiles,nflps
!     integer              n,n1,n2,n3,n4,ndgf,ndgi,nfiles,nflps
      integer              nges,ngpken,niter,nnmod,nradf,nradr
      integer              nsfcf,nsfci,nsfcs,nsigi,nsigs,nstep
      integer              nznlf,nznli,nznls,i
cc
      integer              ls_node(ls_dim,3)
cc
!     ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!     ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!     ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
cc
      integer              ls_nodes(ls_dim,nodes)
      integer                 max_ls_nodes(nodes)
cc
      integer               lats_nodes_a(nodes)
      integer              global_lats_a(latg)
      integer                 lonsperlat(latg)
cc
      integer               lats_nodes_r(nodes)
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
cc
      integer                lats_nodes_ext(nodes)
      integer        global_lats_ext(latg+2*jintmx+2*nypt*(nodes-1))
cc
      real(kind=kind_evod)    epse(len_trie_ls)
      real(kind=kind_evod)    epso(len_trio_ls)
      real(kind=kind_evod)  epsedn(len_trie_ls)
      real(kind=kind_evod)  epsodn(len_trio_ls)
cc
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
cc
      integer               ndexev(len_trie_ls)
      integer               ndexod(len_trio_ls)
cc
      real(kind=kind_evod) plnev_a(len_trie_ls,latg2)
      real(kind=kind_evod) plnod_a(len_trio_ls,latg2)
      real(kind=kind_evod) pddev_a(len_trie_ls,latg2)
      real(kind=kind_evod) pddod_a(len_trio_ls,latg2)
      real(kind=kind_evod) plnew_a(len_trie_ls,latg2)
      real(kind=kind_evod) plnow_a(len_trio_ls,latg2)
cc
      real(kind=kind_evod) plnev_r(len_trie_ls,latr2)
      real(kind=kind_evod) plnod_r(len_trio_ls,latr2)
      real(kind=kind_evod) pddev_r(len_trie_ls,latr2)
      real(kind=kind_evod) pddod_r(len_trio_ls,latr2)
      real(kind=kind_evod) plnew_r(len_trie_ls,latr2)
      real(kind=kind_evod) plnow_r(len_trio_ls,latr2)
      real(kind=kind_dbl_prec) ,allocatable:: colrad_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::    wgt_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::  wgtcs_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::   rcs2_dp(:)
cc
      real(kind=kind_dbl_prec) ,allocatable::   epse_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::   epso_dp(:)
      real(kind=kind_dbl_prec) ,allocatable:: epsedn_dp(:)
      real(kind=kind_dbl_prec) ,allocatable:: epsodn_dp(:)
cc
      real(kind=kind_dbl_prec) ,allocatable::  plnev_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::  plnod_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::  pddev_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::  pddod_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::  plnew_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::  plnow_dp(:)
      integer              iprint,locl,node,nodesio
      integer              len_trie_ls_nod
      integer              len_trio_ls_nod
cc
      integer              indev
      integer              indod
cc
      integer              indlsev,jbasev
      integer              indlsod,jbasod
cc
      integer gl_lats_index
      integer global_time_sort_index_a(latg)
      integer global_time_sort_index_r(latr)
      integer nodes_tmp
cc
      include 'function2'
cc
      real(kind=kind_evod) global_time_a(latg)
      real(kind=kind_evod) global_time_r(latr)
cc
      logical shuffled
cc
      real(kind=kind_evod) colat1
cc
      real(kind=kind_evod) cons0,cons0p5,cons0p92      !constant
      real(kind=kind_evod) cons1                       !constant
cc
cc
      cons0    =   0.d0       !constant
      cons0p5  =   0.5d0      !constant
      cons0p92 =   0.92d0     !constant
      cons1    =   1.d0       !constant
cc
cc
!sela print 100, jcap, levs
100   format (1h0,'getcon ',i3,i3,'created april 2000')
cc
cc
      do lat = 1, latg2
         lonsperlat(latg+1-lat) = lonsperlat(lat)
      end do
cc
cc
      do lat = 1, latr2
         lonsperlar(latr+1-lat) = lonsperlar(lat)
      end do
cc
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
!sela print*,'me,liope,nodes,nodesio = ',me,liope,nodes,nodesio
      if (nodesio .eq. 1 .and. nodes .eq. 1
     .    .or. (nodes .eq. 2 .and. nodesio .eq. 1) ) then
         shuff_lats_a = .false.
         shuff_lats_r = .false.
 
!sela   print*,' NO SHUFFLING WITH 1 COMPUTE TASK - nodes = ',nodes
      endif
!sela print*,' shuff_lats_a,shuff_lats_r = ',shuff_lats_a,shuff_lats_r
      shuffled = shuff_lats_a .or. shuff_lats_r
 
!!
 
      iprint = 0
      if (shuff_lats_a) then
cc
cmy global_lats_a is latg
cmy call again for sh
 
      gl_lats_index = 0
      global_lats_a = -1
cmy intialize global_time_a to lonsperlat
      do lat = 1,latg
         global_time_a(lat) = lonsperlat(lat)
      enddo
c
cmy sort the lat times in descending order
c
      call sortrx(latg,-global_time_a,global_time_sort_index_a)
      if (iprint .eq. 1)
     .  print*,' after sortrx global_time_sort_index_a = ',
     .      global_time_sort_index_a
 
cmy input lat time index in descending order
cmy output global_lats_a and lats_nodes_a (gl_lats_index temp)
cmy
      gl_lats_index = 0
      nodes_tmp = nodes
      if (liope .and. icolor .eq. 2) nodes_tmp = nodes-1
cgwvbugfix   initialize lats_nodes_a(iotask) which is not set in the loop below 
      if (liope .and. icolor .eq. 2) lats_nodes_a(nodes)=0
      do node=1,nodes_tmp
         call get_lats_node_a( node-1, global_lats_a,
     x                 lats_nodes_a(node),
     .                 gl_lats_index,global_time_sort_index_a,iprint)
       if (me+1 .eq. node .and. iprint .eq. 1)
     .  print*,' node, lats_nodes_a = ',node,lats_nodes_a(node)
      enddo
 
!sela print*,' global_lats_a = ',global_lats_a
 
cmy the extended part will not work with shuffled lats
cmy it assumes sequential ordered lats
      call setlats_a_ext_shuff(lats_nodes_a,lats_nodes_ext,
     &           global_lats_a, global_lats_ext,iprint,lonsperlat)
 
        else
 
      call setlats_a(lats_nodes_a,lats_nodes_ext,global_lats_a,
     &               global_lats_ext,iprint,lonsperlat)
 
      endif ! shuff_lats_a
!sela  write(6,*) ' getcon after setlats_a global_lats_a = '
!sela  write(6,830)     global_lats_a
 830   format(15(i3,1x))
 
      iprint = 0
      do node=1,nodesio
cc
         call get_ls_node( node-1, ls_nodes(1,node),
     x                     max_ls_nodes(node), iprint )
cc
      enddo
cc
cc
      len_trie_ls_max=0
      len_trio_ls_max=0
      do node=1,nodesio
cc
         len_trie_ls_nod=0
         len_trio_ls_nod=0
         do locl=1,max_ls_nodes(node)
            l=ls_nodes(locl,node)
            len_trie_ls_nod=len_trie_ls_nod+(jcap+3-l)/2
            len_trio_ls_nod=len_trio_ls_nod+(jcap+2-l)/2
            if ( l .eq. 0 ) me_l_0 = node-1
         enddo
         len_trie_ls_max = max(len_trie_ls_max,len_trie_ls_nod)
         len_trio_ls_max = max(len_trio_ls_max,len_trio_ls_nod)
cc
      enddo
cc
      if (shuff_lats_r) then
 
cmy *************** new code for setlats_r
      gl_lats_index = 0
      global_lats_r = -1
cmy intialize global_time_r to lonsperlar
      do lat = 1,latr
         global_time_r(lat) = lonsperlar(lat)
      enddo
c
cmy sort the lat times in descending order
c
      call sortrx(latr,-global_time_r,global_time_sort_index_r)
      if (iprint .eq. 1) print*,' after sortrx for r index = ',
     .    global_time_sort_index_r
 
cmy input lat time index in descending order
cmy output global_lats_r and lats_nodes_r (gl_lats_index temp)
cmy
      gl_lats_index = 0
cmy
      nodes_tmp = nodes
      if (liope .and. icolor .eq. 2) nodes_tmp = nodes-1
cgwvbugfix   initialize lats_nodes_r(iotask) which is not set in the loop below 
      if (liope .and. icolor .eq. 2) lats_nodes_r(nodes)=0
      do node=1,nodes_tmp
         call get_lats_node_r( node-1, global_lats_r,
     x                 lats_nodes_r(node),
     .                 gl_lats_index,global_time_sort_index_r,iprint)
       if (me+1 .eq. node .and. iprint .eq. 1)
     .  print*,' node, lats_nodes_r = ',node,lats_nodes_r(node)
      enddo
       else
        call setlats_r(lats_nodes_r,global_lats_r,iprint,lonsperlar)
      endif ! shuff_lats_r
c
!sela  write(6,*) ' getcon after setlats_r global_lats_r = '
!sela  write(6,830)     global_lats_r
c
      iprint = 0
cc
      lats_dim_a=0
      lats_dim_r=0
      do node=1,nodes
         lats_dim_a = max(lats_dim_a,lats_nodes_a(node))
         lats_dim_r = max(lats_dim_r,lats_nodes_r(node))
      enddo
cc
cmy need to set lats_dim_ext used for declaring variables in main,digifilt,dotstep
c$$$      if (.not. shuffled) then
      lats_dim_ext=0
c$$$      nodes_tmp = nodes
c$$$      if (liope .and. icolor .eq. 2) nodes_tmp = nodes-1
c$$$      do node=1,nodes_tmp
      do node=1,nodes
             lats_dim_ext =
     .   max(lats_dim_ext, lats_nodes_ext(node), lats_nodes_r(node))
!selaxxx      print*,' node,lats_dim_ext = ',node,lats_dim_ext
      enddo
c$$$      endif
cc
      lats_node_a = lats_nodes_a(me+1)
      lats_node_r = lats_nodes_r(me+1)
 
c$$$      if (.not. shuffled) lats_node_ext = lats_nodes_ext(me+1)
         lats_node_ext = lats_nodes_ext(me+1)
c
      lats_node_r_max=0
      do i=1,nodes
        lats_node_r_max=max(lats_node_r_max,lats_nodes_r(i))
      enddo
c
      lats_node_a_max=0
      do i=1,nodes
        lats_node_a_max=max(lats_node_a_max,lats_nodes_a(i))
      enddo
 
cc
      ipt_lats_node_a=1
      ipt_lats_node_r=1
      ipt_lats_node_ext=1
 
      if ( .not. shuffled .and. me .gt. 0 ) then
         do node=1,me
          ipt_lats_node_ext = ipt_lats_node_ext + lats_nodes_ext(node)
         enddo
      endif
c
      if ( me .gt. 0 ) then
         do node=1,me
            ipt_lats_node_a = ipt_lats_node_a + lats_nodes_a(node)
            ipt_lats_node_r = ipt_lats_node_r + lats_nodes_r(node)
c$$$          ipt_lats_node_ext = ipt_lats_node_ext + lats_nodes_ext(node)
c$$$           print*,' node,ipt_lats_node_ext = ',node,ipt_lats_node_ext
         enddo
      endif
      if (liope .and. icolor .eq. 2) then
            ipt_lats_node_a = 1
            ipt_lats_node_r = 1
            ipt_lats_node_ext = 1
      endif
c
!sela print*,' ipt_lats_node_a = ',ipt_lats_node_a
!sela print*,' ipt_lats_node_r = ',ipt_lats_node_r
!sela print*,' ipt_lats_node_ext = ',ipt_lats_node_ext
cc
!sela filta= cons0p92                    !constant
!sela filtb =(cons1-filta) * cons0p5     !constant
cc
!     n1    = 11
!     n2    = 12
      n3    = 51
      n4    = 52
cc
      iprint = 0
c$$$      if ( me .eq. 0 ) iprint = 1
cc
      iprint = 0
c$$$      if ( me .eq. 0 ) iprint = 1
      if ( kind_evod .eq. 8 ) then !------------------------------------
           call glats(latg2,colrad_a,wgt_a,wgtcs_a,rcs2_a,iprint)
           call glats(latr2,colrad_r,wgt_r,wgtcs_r,rcs2_r,iprint)
!!
           colat1=colrad_r(1)
!!
           do i=latr2+1,latr
              colrad_r(i)=colrad_r(latr+1-i)
           enddo
cc
           call epslon(epse,epso,epsedn,epsodn,
     &                 ls_node)
cc
           call pln2eo_a(plnev_a,plnod_a,epse,epso,colrad_a,
     &                   ls_node,latg2)
cc
           call pln2eo_r(plnev_r,plnod_r,epse,epso,colrad_r,
     &                   ls_node,latr2)
cc
           call gozrineo_a(plnev_a,plnod_a,pddev_a,pddod_a,
     &                     plnew_a,plnow_a,epse,epso,
     &                     rcs2_a,wgt_a,ls_node,latg2)
cc
           call gozrineo_r(plnev_r,plnod_r,pddev_r,pddod_r,
     &                     plnew_r,plnow_r,epse,epso,
     &                     rcs2_r,wgt_r,ls_node,latr2)
      else !------------------------------------------------------------
           allocate  ( colrad_dp(max(latg2,latr2)) )
           allocate  (    wgt_dp(max(latg2,latr2)) )
           allocate  (  wgtcs_dp(max(latg2,latr2)) )
           allocate  (   rcs2_dp(max(latg2,latr2)) )
cc
           allocate  (   epse_dp(len_trie_ls) )
           allocate  (   epso_dp(len_trio_ls) )
           allocate  ( epsedn_dp(len_trie_ls) )
           allocate  ( epsodn_dp(len_trio_ls) )
cc
           allocate  (  plnev_dp(len_trie_ls) )
           allocate  (  plnod_dp(len_trio_ls) )
           allocate  (  pddev_dp(len_trie_ls) )
           allocate  (  pddod_dp(len_trio_ls) )
           allocate  (  plnew_dp(len_trie_ls) )
           allocate  (  plnow_dp(len_trio_ls) )
           call glats(latg2,colrad_dp,wgt_dp,wgtcs_dp,rcs2_dp,iprint)
cc
           do i=1,latg2
              colrad_a(i) = colrad_dp(i)
                 wgt_a(i) =    wgt_dp(i)
               wgtcs_a(i) =  wgtcs_dp(i)
                rcs2_a(i) =   rcs2_dp(i)
           enddo
cc
           call epslon(epse_dp,epso_dp,epsedn_dp,epsodn_dp,
     x                 ls_node)
cc
           do i=1,len_trie_ls
                epse(i) =   epse_dp(i)
              epsedn(i) = epsedn_dp(i)
           enddo
cc
           do i=1,len_trio_ls
                epso(i) =   epso_dp(i)
              epsodn(i) = epsodn_dp(i)
           enddo
cc
           do lat=1,latg2
cc
              call pln2eo_a(plnev_dp,plnod_dp,epse_dp,epso_dp,
     &                      colrad_dp(lat),ls_node,1)
cc
              call gozrineo_a(plnev_dp,plnod_dp,pddev_dp,pddod_dp,
     &                        plnew_dp,plnow_dp,epse_dp,epso_dp,
     &                        rcs2_dp(lat),wgt_dp(lat),ls_node,1)
cc
              do i=1,len_trie_ls
                 plnev_a(i,lat) = plnev_dp(i)
                 pddev_a(i,lat) = pddev_dp(i)
                 plnew_a(i,lat) = plnew_dp(i)
              enddo
cc
              do i=1,len_trio_ls
                 plnod_a(i,lat) = plnod_dp(i)
                 pddod_a(i,lat) = pddod_dp(i)
                 plnow_a(i,lat) = plnow_dp(i)
              enddo
cc
           enddo
cc
           call glats(latr2,colrad_dp,wgt_dp,wgtcs_dp,rcs2_dp,iprint)
!!
           colat1=colrad_dp(1)
!!
           do i=1,latr2
              colrad_r(i) = colrad_dp(i)
                 wgt_r(i) =    wgt_dp(i)
               wgtcs_r(i) =  wgtcs_dp(i)
                rcs2_r(i) =   rcs2_dp(i)
           enddo
cc
           do i=latr2+1,latr
              colrad_r(i) = colrad_dp(latr+1-i)
           enddo
cc
           do lat=1,latr2
cc
              call pln2eo_r(plnev_dp,plnod_dp,epse_dp,epso_dp,
     &                      colrad_dp(lat),ls_node,1)
cc
              call gozrineo_r(plnev_dp,plnod_dp,pddev_dp,pddod_dp,
     &                        plnew_dp,plnow_dp,epse_dp,epso_dp,
     &                        rcs2_dp(lat),wgt_dp(lat),ls_node,1)
cc
              do i=1,len_trie_ls
                 plnev_r(i,lat) = plnev_dp(i)
                 pddev_r(i,lat) = pddev_dp(i)
                 plnew_r(i,lat) = plnew_dp(i)
              enddo
cc
              do i=1,len_trio_ls
                 plnod_r(i,lat) = plnod_dp(i)
                 pddod_r(i,lat) = pddod_dp(i)
                 plnow_r(i,lat) = plnow_dp(i)
              enddo
cc
           enddo
           deallocate  ( colrad_dp )
           deallocate  (    wgt_dp )
           deallocate  (  wgtcs_dp )
           deallocate  (   rcs2_dp )
cc
           deallocate  (   epse_dp )
           deallocate  (   epso_dp )
           deallocate  ( epsedn_dp )
           deallocate  ( epsodn_dp )
cc
           deallocate  (  plnev_dp )
           deallocate  (  plnod_dp )
           deallocate  (  pddev_dp )
           deallocate  (  pddod_dp )
           deallocate  (  plnew_dp )
           deallocate  (  plnow_dp )
      endif !-----------------------------------------------------------
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         indev=indlsev(l,l)
         do n = l, jcap, 2
             ndexev(indev)=n
            snnp1ev(indev)=n*(n+1)
              indev=indev+1
         end do
      end do
cc
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasod=ls_node(locl,3)
         if ( l .le. jcap-1 ) then
            indod=indlsod(l+1,l)
            do n = l+1, jcap, 2
                ndexod(indod)=n
               snnp1od(indod)=n*(n+1)
                 indod=indod+1
            end do
         end if
      end do
cc
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         jbasod=ls_node(locl,3)
         if (mod(L,2).eq.mod(jcap+1,2)) then
cc          set the even (n-l) terms of the top row to zero
             ndexev(indlsev(jcap+1,l))=0
            snnp1ev(indlsev(jcap+1,l))=cons0     !constant
         else
cc          set the  odd (n-l) terms of the top row to zero
             ndexod(indlsod(jcap+1,l))=0
            snnp1od(indlsod(jcap+1,l))=cons0     !constant
         endif
      enddo
cc
cc
      do j=1,latg2
        sinlat_a(j) = cos(colrad_a(j))
      enddo
cc
cc
      do j=1,latr
        if (j.le.latr2) then
          sinlat_r(j) = cos(colrad_r(j))
        else
          sinlat_r(j) = -cos(colrad_r(j))
        endif
        coslat_r(j) = sqrt(1. E 0 -sinlat_r(j)*sinlat_r(j))
      enddo
cc
cc
      do L=0,jcap
         do lat = 1, latg2
            if ( L .le. min(jcap,lonsperlat(lat)/2) ) then
               lat1s_a(L) = lat
               go to 200
            endif
         end do
  200    continue
      end do
cc
cc
      do L=0,jcap
         do lat = 1, latr2
            if ( L .le. min(jcap,lonsperlar(lat)/2) ) then
               lat1s_r(L) = lat
               go to 220
            endif
         end do
  220    continue
      end do
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
      do j=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+j)
         if ( lonsperlar(lat) .eq. lonr ) then
            lon_dims_r(j) = lonrx
         else
            lon_dims_r(j) = lonsperlar(lat) + 2
         endif
      enddo
cc
cc
      if (.not. shuffled) then
      do j=1,lats_node_ext
         lat = global_lats_ext(ipt_lats_node_ext-1+j)
!selaxxx print*,'ipt_lats_node_ext = ',ipt_lats_node_ext
!selaxxx print*,' j,lat = ',j,lat
         if ( lonsperlat(lat) .eq. lonf ) then
            lon_dims_ext(j) = lonfx
         else
            lon_dims_ext(j) = lonsperlat(lat) + 1+2*nxpt+1
         endif
      enddo
      endif
cc
      return
      end

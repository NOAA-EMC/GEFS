      subroutine setlats_a(lats_nodes_a,lats_nodes_ext,global_lats_a,
     &                   global_lats_ext,iprint,lonsperlat)
cc
      use resol_def
      use layout1
      use mpi_def
      implicit none
cc
      integer              lats_nodes_a(nodes)
cc
      integer              global_lats_a(latg)
      integer              lats_nodes_ext(nodes)
      integer        global_lats_ext(latg+2*jintmx+2*nypt*(nodes-1))
cc
      integer              iprint,opt,ifin,nodesio
cc
      integer              lonsperlat(latg)
cc
      integer              ijk,jcount,jpt,lat,lats_sum,node,i,j
      integer              ILATPE,ngrptg,ngrptl,ipe,irest,idp
cc
      integer,allocatable :: lats_hold(:,:)
cc
      allocate ( lats_hold(latg,nodes) )
cc
      iprint=1
      iprint=0
      OPT=1
      lats_nodes_a=0
      if (liope) then
         if (icolor.eq.2) then
           nodesio=1
         else
           nodesio=nodes
         endif
      else
         nodesio=nodes
      endif
cc
      ngrptg=0
      do lat=1,latg
         if (opt.eq.1) then
           ifin=lonsperlat(lat)
         elseif(opt.eq.2) then
           ifin=lonf
         endif
         do i=1,ifin
            ngrptg=ngrptg+1
         enddo
      enddo
!
!     distribution of the grid
      ILATPE=ngrptg/nodesio
      ngrptl=0
      ipe=0
      irest=0
      idp=1
      do lat=1,latg
         if (opt.eq.1) then
           ifin=lonsperlat(lat)
         elseif(opt.eq.2) then
           ifin=lonf
         endif
          ngrptl=ngrptl+ifin
       if (ngrptl*nodesio.le.ngrptg+irest) then
           lats_nodes_a(ipe+1) = lats_nodes_a(ipe+1) + 1
           lats_hold(idp,ipe+1)=lat
           idp=idp+1
       else
          ipe=ipe+1
          if (ipe.le.nodesio) lats_hold(1,ipe+1) = lat
          idp=2
          irest=irest+ngrptg-(ngrptl-ifin)*nodesio
          ngrptl=ifin
          lats_nodes_a(ipe+1) = lats_nodes_a(ipe+1) + 1
       endif
      enddo
!!
      do node=1,nodesio
        if (nodesio.eq.1) then
          lats_nodes_ext(node)=lats_nodes_a(node)+2*jintmx
        else
          if (node.eq.1.or.node.eq.nodesio) then
           lats_nodes_ext(node)=lats_nodes_a(node)+jintmx+nypt
          else
           lats_nodes_ext(node)=lats_nodes_a(node)+2*nypt
          endif
        endif
      enddo
cc........................................................
cc
      jpt=0
      do node=1,nodesio
         if ( lats_nodes_a(node) .gt. 0 ) then
            do jcount=1,lats_nodes_a(node)
               global_lats_a(jpt+jcount) = lats_hold(jcount,node)
            enddo
         endif
         jpt=jpt+lats_nodes_a(node)
      enddo
cc
      jpt=0
      do node=1,nodesio
       if (nodesio.eq.1) then
        do i=1,jintmx
          global_lats_ext(i)=global_lats_a(1)
        enddo
        do i=1,jintmx
          global_lats_ext(jintmx+latg+i)=global_lats_a(latg)
        enddo
        do i=1,latg
          global_lats_ext(i+jintmx)=global_lats_a(i)
        enddo
       else
        do jcount=1,lats_nodes_a(node)
         global_lats_ext(jpt+jintmx+jcount+2*nypt*(node-1))=
     &               global_lats_a(jpt+jcount)
        enddo
        if (node.eq.1) then
         do i=1,jintmx
           global_lats_ext(i)=global_lats_a(1)
         enddo
         do i=1,nypt
           global_lats_ext(jintmx+lats_nodes_a(node)+i)=
     &               global_lats_a(lats_nodes_a(node))+i
         enddo
        elseif (node.eq.nodesio) then
         do i=1,jintmx
           global_lats_ext(latg+jintmx+2*nypt*(nodesio-1)+i)=
     &                    global_lats_a(latg)
         enddo
         do i=nypt,1,-1
           global_lats_ext(jpt+jintmx+2*nypt*(node-1)-i+1)=
     &                    global_lats_a(jpt)-i+1
         enddo
        else
         do i=nypt,1,-1
           global_lats_ext(jpt+jintmx+2*nypt*(node-1)-i+1)=
     &                    global_lats_a(jpt)-i+1
         enddo
         do i=1,nypt
         global_lats_ext(jpt+jintmx+2*nypt*(node-1)+
     &                    lats_nodes_a(node)+i)=
     &              global_lats_a(jpt+lats_nodes_a(node))+i
         enddo
        endif
       endif
        jpt=jpt+lats_nodes_a(node)
      enddo
cc
      if ( iprint .ne. 1 ) return
cc
      jpt=0
      do node=1,nodesio
         if ( lats_nodes_a(node) .gt. 0 ) then
            print 600
            lats_sum=0
            do jcount=1,lats_nodes_a(node)
               lats_sum=lats_sum + lonsperlat(global_lats_a(jpt+jcount))
               print 700, node-1,
     x                    node,    lats_nodes_a(node),
     x                    jpt+jcount, global_lats_a(jpt+jcount),
     x                     lonsperlat(global_lats_a(jpt+jcount)),
     x                    lats_sum
            enddo
         endif
         jpt=jpt+lats_nodes_a(node)
      enddo
cc
      print 600
cc
  600 format ( ' ' )
cc
  700 format (  'setlats  me=', i4,
     x          '  lats_nodes_a(',  i4, ' )=', i4,
     x          '  global_lats_a(', i4, ' )=', i4,
     x          '  lonsperlat=', i5,
     x          '  lats_sum=',   i6 )
cc
      deallocate ( lats_hold )
cc
      return
      end
      subroutine setlats_r(lats_nodes_r,global_lats_r,iprint,lonsperlar)
cc
      use resol_def
      use layout1
      use mpi_def
      implicit none
cc
      integer              lats_nodes_r(nodes)
cc
      integer              global_lats_r(latr)
cc
      integer              iprint,opt,ifin,nodesio
cc
      integer              lonsperlar(latr)
cc
      integer              ijk,jcount,jpt,lat,lats_sum,node,i,j
      integer              ILATPE,ngrptg,ngrptl,ipe,irest,idp
cc
      integer,allocatable :: lats_hold(:,:)
cc
      allocate ( lats_hold(latr,nodes) )
cc
      OPT=1
      lats_nodes_r=0
      if (liope) then
         if (icolor.eq.2) then
           nodesio=1
         else
           nodesio=nodes
         endif
      else
         nodesio=nodes
      endif
cc
      ngrptg=0
      do lat=1,latr
         if (opt.eq.1) then
           ifin=lonsperlar(lat)
         elseif(opt.eq.2) then
           ifin=lonr
         endif
         do i=1,ifin
            ngrptg=ngrptg+1
         enddo
      enddo
!
!     distribution of the grid
      ILATPE=ngrptg/nodesio
      ngrptl=0
      ipe=0
      irest=0
      idp=1
      do lat=1,latr
         if (opt.eq.1) then
           ifin=lonsperlar(lat)
         elseif(opt.eq.2) then
           ifin=lonr
         endif
          ngrptl=ngrptl+ifin
       if (ngrptl*nodesio.le.ngrptg+irest) then
           lats_nodes_r(ipe+1) = lats_nodes_r(ipe+1) + 1
           lats_hold(idp,ipe+1)=lat
           idp=idp+1
       else
          ipe=ipe+1
          if (ipe.le.nodesio) lats_hold(1,ipe+1) = lat
          idp=2
          irest=irest+ngrptg-(ngrptl-ifin)*nodesio
          ngrptl=ifin
          lats_nodes_r(ipe+1) = lats_nodes_r(ipe+1) + 1
       endif
      enddo
!!
      jpt=0
      do node=1,nodesio
         if ( lats_nodes_r(node) .gt. 0 ) then
            do jcount=1,lats_nodes_r(node)
               global_lats_r(jpt+jcount) = lats_hold(jcount,node)
            enddo
         endif
         jpt=jpt+lats_nodes_r(node)
      enddo
cc
cc
      if ( iprint .ne. 1 ) return
cc
      jpt=0
      do node=1,nodesio
         if ( lats_nodes_r(node) .gt. 0 ) then
            print 600
            lats_sum=0
            do jcount=1,lats_nodes_r(node)
               lats_sum=lats_sum + lonsperlar(global_lats_r(jpt+jcount))
               print 700, node-1,
     x                    node,    lats_nodes_r(node),
     x                    jpt+jcount, global_lats_r(jpt+jcount),
     x                     lonsperlar(global_lats_r(jpt+jcount)),
     x                    lats_sum
            enddo
         endif
         jpt=jpt+lats_nodes_r(node)
      enddo
cc
      print 600
cc
  600 format ( ' ' )
cc
  700 format (  'setlats_r  me=', i4,
     x          '  lats_nodes_r(',  i4, ' )=', i4,
     x          '  global_lats_r(', i4, ' )=', i4,
     x          '  lonsperlar=', i5,
     x          '  lats_sum=',   i6 )
cc
      deallocate ( lats_hold )
cc
      return
      end

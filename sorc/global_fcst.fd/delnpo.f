      subroutine delnpo(qo,edphi,odlam,epse,epso,
     x                  ls_node)
cc

cc
      use resol_def
      use layout1
      use physcons, rerth => con_rerth
      implicit none
cc
cc    input q is in ibm triang. order
cc    output  is in ibm triang. order
cc
      real(kind=kind_evod)    qo(len_trio_ls,2)
      real(kind=kind_evod) edphi(len_trie_ls,2)
      real(kind=kind_evod) odlam(len_trio_ls,2)
cc
      real(kind=kind_evod)  epse(len_trie_ls)
      real(kind=kind_evod)  epso(len_trio_ls)
cc
      integer              ls_node(ls_dim,3)
cc
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
cc
      integer              l,locl,n
cc
      integer              indev,indev1,indev2
      integer              indod,indod1,indod2
      integer              inddif
cc
      real(kind=kind_evod) aa,r1mn,rl,rnp2
cc
      real(kind=kind_evod) cons0     !constant
      real(kind=kind_evod) cons1     !constant
      real(kind=kind_evod) cons2     !constant
cc
      integer              indlsev,jbasev
      integer              indlsod,jbasod
cc
      include 'function2'
cc
cc
cc......................................................................
cc
cc
      CALL countperf(0,13,0.)
!!
      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
      cons2 = 2.d0     !constant
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasod=ls_node(locl,3)
         indod1 = indlsod(L+1,L)
         if (mod(L,2).eq.mod(jcap+1,2)) then
            indod2 = indlsod(jcap  ,L)
         else
            indod2 = indlsod(jcap+1,L)
         endif
cc
         rl = l
         do indod = indod1 , indod2
cc          dlam(l,n)= i*l*q(l,n)
cc
            odlam(indod,1) = -rl * qo(indod,2)
            odlam(indod,2) =  rl * qo(indod,1)
cc
         enddo
cc
      enddo
cc
cc......................................................................
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         jbasod=ls_node(locl,3)
         indev1 = indlsev(L,L) + 1
         if (mod(L,2).eq.mod(jcap+1,2)) then
            indev2 = indlsev(jcap+1,L)
         else
            indev2 = indlsev(jcap  ,L)
         endif
         indod1 = indlsod(l+1,l)
         inddif = indev1 - indod1
cc
         r1mn = -l - 1
         do indev = indev1 , indev2
cc
                   edphi(indev,1) =
     1      r1mn *  epse(indev)   * qo(indev-inddif,1)
cc
                   edphi(indev,2) =
     1      r1mn *  epse(indev)   * qo(indev-inddif,2)
cc
            r1mn = r1mn - cons2     !constant
         enddo
cc
      enddo
cc
cc......................................................................
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
cc
         edphi(indlsev(l,l),1) = cons0     !constant
         edphi(indlsev(l,l),2) = cons0     !constant
cc
cc
      enddo
cc
cc......................................................................
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         jbasod=ls_node(locl,3)
         indev1 = indlsev(L,L)
         if (mod(L,2).eq.mod(jcap+1,2)) then
            indev2 = indlsev(jcap+1,L) - 1
         else
            indev2 = indlsev(jcap  ,L) - 1
         endif
         indod1 = indlsod(l+1,l)
         inddif = indev1 - indod1
cc
         rnp2 = l+2
         do indev = indev1 , indev2
cc
                   edphi(indev,1)      = edphi(indev       ,1) +
     1      rnp2 *  epso(indev-inddif) *    qo(indev-inddif,1)
cc
                   edphi(indev,2)      = edphi(indev       ,2) +
     1      rnp2 *  epso(indev-inddif) *    qo(indev-inddif,2)
cc
            rnp2 = rnp2 + cons2     !constant
         enddo
cc
      enddo
cc
cc......................................................................
cc
      aa=cons1/rerth     !constant
cc
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         jbasod=ls_node(locl,3)
         indev1 = indlsev(L,L)
         indod1 = indlsod(L+1,L)
         if (mod(L,2).eq.mod(jcap+1,2)) then
            indev2 = indlsev(jcap+1,L)
            indod2 = indlsod(jcap  ,L)
         else
            indev2 = indlsev(jcap  ,L)
            indod2 = indlsod(jcap+1,L)
         endif
         do indod = indod1 , indod2
cc
            odlam(indod,1) = odlam(indod,1) * aa
            odlam(indod,2) = odlam(indod,2) * aa
cc
         enddo
cc
         do indev = indev1 , indev2
cc
            edphi(indev,1) = edphi(indev,1) * aa
            edphi(indev,2) = edphi(indev,2) * aa
cc
         enddo
cc
cc
      enddo
cc
      CALL countperf(1,13,0.)
!!
      return
      end

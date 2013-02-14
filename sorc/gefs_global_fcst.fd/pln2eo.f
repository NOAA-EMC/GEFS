      subroutine pln2eo_a(plnev_a,plnod_a,epse,epso,colrad_a,
     x                    ls_node,num_lat)
cc
      use resol_def
      use layout1
      implicit none
cc
      real(kind=kind_dbl_prec) plnev_a(len_trie_ls,latg2)
      real(kind=kind_dbl_prec) plnod_a(len_trio_ls,latg2)
cc
      real(kind=kind_dbl_prec)    epse(len_trie_ls)
      real(kind=kind_dbl_prec)    epso(len_trio_ls)
cc
      real(kind=kind_dbl_prec) colrad_a(latg2)
cc
      integer                  ls_node(ls_dim,3)
cc
      integer                  num_lat
cc
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
cc
      integer                  l,lat,locl,max_l,n
cc
      integer                  indev
      integer                  indod
cc
      real(kind=kind_dbl_prec) a,alp1,alp2,alp3,b
      real(kind=kind_dbl_prec) cos2,fl,prod,sinlat
cc
      real(kind=kind_dbl_prec) alp10(0:jcap)
cc
      real(kind=kind_dbl_prec) cons0,cons0p5,cons1,cons2,cons3    !constant
cc
cc
      integer                  indlsev,jbasev
      integer                  indlsod,jbasod
cc
      include 'function2'
cc
cc
      cons0=0.0d0       !constant
      cons0p5=0.5d0     !constant
      cons1=1.0d0       !constant
      cons2=2.0d0       !constant
      cons3=3.0d0       !constant
cc
cc
      max_l=-1
      do locl=1,ls_max_node
         max_l = max ( max_l, ls_node(locl,1) )
      enddo
cc
cc
      do lat=1,num_lat
cc
         sinlat = cos(colrad_a(lat))
         cos2=cons1-sinlat*sinlat           !constant
         a=cons1                            !constant
         b=cons0                            !constant
         prod=cons1                         !constant
         do l=0,max_l
            alp10(l)=sqrt(cons0p5*prod)     !constant
            a=a+cons2                       !constant
            b=b+cons2                       !constant
            prod=prod*cos2*a/b
         enddo
cc
         do locl=1,ls_max_node
                 l=ls_node(locl,1)
            jbasev=ls_node(locl,2)
            jbasod=ls_node(locl,3)
            n=l
            fl=l
            alp1=alp10(l)
            indev=indlsev(n  ,l)
            indod=indlsod(n+1,l)
            plnev_a(indev       ,lat)=alp1
            alp2=sqrt(cons2*fl+cons3)*sinlat*alp1     !constant
            plnod_a(indod         ,lat)=alp2
cc
            do n=l+2,jcap+1
               if(mod(n+l,2).eq.0) then
                  indev=indev+1
                  alp3=(sinlat*alp2-epso(indod)*alp1)
     x                             /epse(indev)
                  plnev_a(indev,lat)=alp3
               else
                  indod=indod+1
                  alp3=(sinlat*alp2-epse(indev)*alp1)
     x                             /epso(indod)
                  plnod_a(indod,lat)=alp3
               endif
               alp1=alp2
               alp2=alp3
            enddo
cc
         enddo
cc
      enddo
cc
      return
      end
      subroutine pln2eo_r(plnev_r,plnod_r,epse,epso,colrad_r,
     x                    ls_node,num_lat)
cc
      use resol_def
      use layout1
      implicit none
c
      real(kind=kind_dbl_prec) plnev_r(len_trie_ls,latr2)
      real(kind=kind_dbl_prec) plnod_r(len_trio_ls,latr2)
cc
      real(kind=kind_dbl_prec)    epse(len_trie_ls)
      real(kind=kind_dbl_prec)    epso(len_trio_ls)
cc
      real(kind=kind_dbl_prec) colrad_r(latr2)
cc
      integer                  ls_node(ls_dim,3)
cc
      integer                  num_lat
cc
cc
      integer                  l,lat,locl,max_l,n
cc
      integer                  indev
      integer                  indod
cc
      real(kind=kind_dbl_prec) a,alp1,alp2,alp3,b
      real(kind=kind_dbl_prec) cos2,fl,prod,sinlat
cc
      real(kind=kind_dbl_prec) alp10(0:jcap)
cc
      real(kind=kind_dbl_prec) cons0,cons0p5,cons1,cons2,cons3    !constant
cc
cc
      integer                  indlsev,jbasev
      integer                  indlsod,jbasod
cc
      include 'function2'
cc
cc
      cons0=0.0d0       !constant
      cons0p5=0.5d0     !constant
      cons1=1.0d0       !constant
      cons2=2.0d0       !constant
      cons3=3.0d0       !constant
cc
cc
      max_l=-1
      do locl=1,ls_max_node
         max_l = max ( max_l, ls_node(locl,1) )
      enddo
cc
cc
      do lat=1,num_lat
cc
         sinlat = cos(colrad_r(lat))
         cos2=cons1-sinlat*sinlat           !constant
         a=cons1                            !constant
         b=cons0                            !constant
         prod=cons1                         !constant
         do l=0,max_l
            alp10(l)=sqrt(cons0p5*prod)     !constant
            a=a+cons2                       !constant
            b=b+cons2                       !constant
            prod=prod*cos2*a/b
         enddo
cc
         do locl=1,ls_max_node
                 l=ls_node(locl,1)
            jbasev=ls_node(locl,2)
            jbasod=ls_node(locl,3)
            n=l
            fl=l
            alp1=alp10(l)
            indev=indlsev(n  ,l)
            indod=indlsod(n+1,l)
            plnev_r(indev       ,lat)=alp1
            alp2=sqrt(cons2*fl+cons3)*sinlat*alp1     !constant
            plnod_r(indod         ,lat)=alp2
cc
            do n=l+2,jcap+1
               if(mod(n+l,2).eq.0) then
                  indev=indev+1
                  alp3=(sinlat*alp2-epso(indod)*alp1)
     x                             /epse(indev)
                  plnev_r(indev,lat)=alp3
               else
                  indod=indod+1
                  alp3=(sinlat*alp2-epse(indev)*alp1)
     x                             /epso(indod)
                  plnod_r(indod,lat)=alp3
               endif
               alp1=alp2
               alp2=alp3
            enddo
cc
         enddo
cc
      enddo
cc
      return
      end

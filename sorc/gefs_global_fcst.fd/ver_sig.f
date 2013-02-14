      subroutine amhmtm(del,sv,am)
cc
      use machine , only : kind_phys,kind_rad
cc
      use resol_def
      use physcons, rerth => con_rerth, rd => con_rd
      implicit none
cc
      integer  i,j,k,le
cc
      real(kind=kind_evod) det
cc
      integer  lll(levs),mmm(levs)
cc
      real(kind=kind_evod) del(levs),sv(levs),
     2 am(levs,levs),hm(levs,levs),tm(levs,levs)
      real(kind=kind_evod) si(levp1)
      real(kind=kind_evod)  rmu(levs),rnu(levs),rho(levs)
cc
      real(kind=kind_evod) cons0,cons1     !constant
cc
      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
cc
c
!     print 200,del(1),del(levs)
200   format(1h ,' amhmtm del(1) del(levs)  ',e12.3,2x,e12.3,2x,i3)
c
      si(1)=cons1     !constant
      do 4 k=1,levs
      si(k+1)=si(k)-del(k)
4     continue
c
      do 1 k=1,levm1
      rho(k)= log(si(k)/si(k+1))
1     continue
      rho(levs)=cons0     !constant
c
      do 2 k=1,levs
      rmu(k)=cons1-si(k+1)*rho(k)/del(k)      !constant
2     continue
c
      do 3 k=1,levm1
      rnu(k+1)=-cons1+si(k)*rho(k)/del(k)     !constant
3     continue
      rnu(1)=cons0              !constant
c
      do 5 j=1,levs
      do 5 i=1,levs
      hm(i,j) = cons0           !constant
      tm(i,j) = cons0           !constant
5     continue
      do 6 le=1,levs
      hm(le,le) = cons1         !constant
      tm(le,le)=rd*rmu(le)
6     continue
c
      do 20 i=1,levm1
           hm(i+1,i)=-cons1     !constant
           tm(i+1,i)=rd*rnu(i+1)
20    continue
c
!     print*,'matrix hm'
c     call printa(hm,levs)
 
!     print*,'matrix tm'
c     call printa(tm,levs)
c
      call iminv (hm, levs, det, lll, mmm)
c     call printa(hm,levs)
      do 88 i=1,levs
      do 88 j=1,levs
      am(i,j) = cons0      !constant
      do 8 k=1,levs
      am(i,j) = am(i,j) + hm(i,k)*tm(k,j)
8     continue
88    continue
!     print*,'matrix hm**-1*tm'
c     call printa(am,levs)
c
c     here is a good place to divide by a*a  for laplacian
c     store am in tm and divide am
      do 10 i=1,levs
      do 10 j=1,levs
      tm(i,j) = am(i,j)
      hm(i,j) = am(i,j)
      am(i,j) = am(i,j) / (rerth * rerth)
10    continue
c     print*,'old tm'
c     call printa(hm,levs)
c     call printa(am,levs)
c     call printa(tm,levs)
      call iminv(tm,levs,det,lll,mmm)
c     call printa(tm,levs)
      do 9 le=1,levs
9     sv(le) = -del(le)
      do 11 le=1,levm1
11    continue
c     call printb(sv,levs)
c     call printb(p1,levs)
c     call printb(p2,levs)
c     print 333
333   format(1h0,'shalom  new  amhmtm')
      return
      end
      subroutine bmdi_sig(ci,bn)
cc
      use machine , only : kind_phys,kind_rad

      use namelist_def , only : ref_temp
      use resol_def
      use physcons, rerth => con_rerth, rd => con_rd, cp => con_cp
      implicit none
cc
      integer              i,j,k,n,n2
cc
      real(kind=kind_evod) bm,r,rcp
cc
      real(kind=kind_evod) ci(0:levs)
      real(kind=kind_evod) t(levs),t1(levs),rm(levs),
     . rn(levs),b(levs,levs),s(levs,0:levs),d(0:levs,levs),ds(levs),
     . sig(0:levs),rho(levs),p(levs,levs),bn(levs,levs),sum(levs)
cc
      real(kind=kind_evod) cons0,cons0p5,cons1,cons300     !constant
cc
      n = levs
      r = rd
      rcp = r / cp
      cons0   =   0.d0      !constant
      cons0p5 =   0.5d0     !constant
      cons1   =   1.d0      !constant
      cons300 = 300.d0      !constant
cc
!%%%%%%%%%%%%%%%%%%%%%%%%%%%
      do 5 k=0,levs
      sig(k)=cons1-ci(levS -k)          !constant
5     continue
!%%%%%%%%%%%%%%%%%%%%%%%%%%%
cc
      do i=1,n
        ds(i)=sig(i)-sig(i-1)
      end do
cc    read*,ds
c   reference profile t:
      do k=1,n
!       t(k)=cons300     !constant
        t(k)=ref_temp
      end do
c   sigma on layer interfaces:
c     sig(0)=0
c     do i=1,n
c       sig(i)=sig(i-1)+ds(i)
c     end do
c__ b matrix = s matrix * d matrix + p matrix
c   d matrix:
      do i=0,n
        do j=1,i
          d(i,j)=-(cons1-sig(i))*ds(j)     !constant
        end do
        do j=i+1,n
          d(i,j)=sig(i)*ds(j)
        end do
      end do
!     do i=0,n
c       print '(3e20.13)',(d(i,j),j=1,n)
!     end do
c   rm,rn computation (weights for hydr. eq. and conv. term):
      rho(1)=cons0     !constant
      do i=2,n
        rho(i)=log(sig(i)/sig(i-1))
      end do
      do k=1,n
        rm(k)=cons1-sig(k-1)*rho(k)/ds(k)     !constant
      end do
      rn(n)=cons0                             !constant
      do k=2,n
        rn(k-1)=sig(k)*rho(k)/ds(k)-cons1     !constant
      end do
c   reference profile on interfaces:
      do k=1,n-1
ctsc:   t1(k)=(rm(k)*(1+rcp*rm(k))*t(k)+rn(k)*(1-rcp*rn(k))*t(k+1))/
ctsc:>  (rm(k)+rn(k))
cecmwf:
        t1(k)=cons0p5*(t(k)+t(k+1))     !constant
      end do
      t1(n)=cons0                       !constant
c   s matrix:
      do i=1,n
        do j=0,n
          s(i,j)=cons0                  !constant
        end do
      end do
      do i=1,n
        s(i,i)=-(t1(i)-t(i))/ds(i)+rcp*rm(i)*t(i)/ds(i)
      end do
      do i=2,n
        s(i,i-1)=-(t(i)-t1(i-1))/ds(i)+rcp*rn(i-1)*t(i)/ds(i)
      end do
c   p matrix:
      do i=1,n
        do j=1,n
          p(i,j)=-rcp*t(i)*ds(j)
        end do
      end do
c   b matrix:
      do i=1,n
        do j=1,n
          b(i,j)=p(i,j)
          do k=0,n
            b(i,j)=b(i,j)+s(i,k)*d(k,j)
          end do
        end do
      end do
c   sums:
      do i=1,n
        sum(i)=cons0     !constant
        do j=1,n
          sum(i)=sum(i)+b(i,j)
        end do
      end do
c   output:
      bm=cons0           !constant
c     print*, ' '
c     print*,' sums:'
c     print*, ' '
c     print '(1p5e12.5)',sum
      do i=1,n
        do j=1,n
          if (abs(b(i,j)).gt.bm) bm=abs(b(i,j))
        end do
      end do
      do i=1,n
        do j=1,n
          bn(i,j)=b(i,j)/bm
        end do
      end do
c     print*, ' '
c     print*, '  b matrix normalized (left half, right half):'
c     print*, ' '
      n2=n/2
c     print '(9f7.3)',((bn(i,j),j=1,n2),i=1,n)
c     print*, ' '
c     print '(9f7.3)',((bn(i,j),j=n2+1,n),i=1,n)
      do k=1,n
         do i=1,n
           bn(k,i)=b(n+1-k,n+1-i)
         enddo
      enddo
      return
      end
      subroutine printa(a,jcap1)
cc
      use machine
      implicit none
cc
      integer              i,j,jcap1
cc
      real(kind=kind_evod) r
cc
      real(kind=kind_evod) a(jcap1,jcap1)
cc
cc    r = -1.e+20     !constant
      r = -1.d+20     !constant
cc
      do 1 i=1,jcap1
      do 1 j=1,jcap1
      if (abs(a(i,j)).gt.r) r=abs(a(i,j))
1     continue
      print 100, r
100   format (1h0, 'scale of matrix  =', e12.5)
      do 2 i=1,jcap1
      do 2 j=1,jcap1
      a(i,j) = a(i,j) / r
2     continue
      do 3 i=1,jcap1
      print 101, (a(i,j), j=1,jcap1)
101   format (1x, 18(f6.3, 1x))
3     continue
      do 4 i=1,jcap1
      do 4 j=1,jcap1
      a(i,j) = a(i,j) * r
4     continue
      return
      end
      subroutine printb(a,jcap1)
cc
      use machine
      implicit none
cc
      integer              j,jcap1
cc
      real(kind=kind_evod) r
cc
      real(kind=kind_evod) a(jcap1)
cc
      real(kind=kind_evod) cons0     !constant
cc
      cons0 = 0.d0     !constant
cc
cc    r = -1.e+20      !constant
      r = -1.d+20      !constant
cc
      do 1 j=1,jcap1
      if (abs(a(j)).gt.r) r=abs(a(j))
1     continue
      print 100, r
100   format (1h0, 'scale of vector  =', e12.5)
cc    if(r.eq.0.e0 )return     !constant
      if(r.eq.cons0)return     !constant
      do 2 j=1,jcap1
      a(j) = a(j) / r
2     continue
      print 101, (a(j), j=1,jcap1)
101   format (1x, 16(f6.3, 1x))
      do 4 j=1,jcap1
      a(j) = a(j) * r
4     continue
      return
      end
      subroutine get_cd_sig(am,bm,dt,tov,sv)
      use machine , only : kind_phys
cc
      use resol_def
      use matrix_sig_def
      use physcons, rerth => con_rerth, rd => con_rd
      implicit none

      integer              i,j,k,n,nn
      real(kind=kind_evod) dt,raa,rnn1
      real(kind=kind_evod) am(levs,levs),bm(levs,levs)
      real(kind=kind_evod) xm(levs,levs),ym(levs,levs)
      real(kind=kind_evod) rim(levs,levs)
      real(kind=kind_evod) sv(levs),tov(levs)
!!!!  real(kind=kind_evod) dm205(jcap1,levs,levs)
      real(kind=kind_evod) ddd(jcap1),ppp(jcap1),rrr(jcap1)
      integer              lu(levs),mu(levs)
      real(kind=kind_evod) cons0,cons1     !constant

      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
      raa = rd/(rerth*rerth)
      do 250 k=1,levs
      do 200 j=1,levs
      rim(j,k)=cons0     !constant
200   continue
250   continue
 
      do 1 k=1,levs
      tor_sig(k)=(rd/(rerth*rerth))*tov(k)
      rim(k,k) = cons1     !constant
1     continue
!..................................................................
      do 2000 nn=1,jcap1
 
      n = nn-1
      rnn1 =       n*(n+1)
 
 
!-------------------------------------------------------
      do 10 i=1,levs
 
      do  9 j=1,levs
      ym(i,j) = tov(i)*sv(j)*raa
9     continue
 
      do 11 k=1,levs
      do 19 j=1,levs
      ym(i,j) = ym(i,j) + am(i,k)*bm(k,j)
19    continue
11    continue
 
10    continue
!-------------------------------------------------------
 
      do 12 i=1,levs
      do 12 j=1,levs
      xm(i,j) = rnn1*dt*dt*ym(i,j)
12    continue
 
      do 14 i=1,levs
      do 13 j=1,levs
      dm205(nn,i,j) = rim(i,j) - xm(i,j)
13    continue
14    continue
 
2000  continue
!..................................................................
      call matinv(dm205,jcap1,levs,ddd,ppp,rrr)
      do 23 nn=1,jcap1
      do 22 i=1,levs
      do 21 j=1,levs
      D_m(i,j,nn)=dm205(nn,i,j)
21    continue
22    continue
23    continue
100   format(1h ,'completed pure sicdif preparation getcd1 dt=',f7.1)
      return
      end

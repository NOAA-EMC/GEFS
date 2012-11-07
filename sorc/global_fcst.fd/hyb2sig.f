      subroutine hyb2sig(njeff,nsize_ar, pgr,sl,del,si,slk)
 
      use machine , only : kind_phys
 
      use resol_def
      use coordinate_def						! hmhj
      use physcons, rk => con_rocp
      implicit none
 
      real(kind=kind_phys)    sl(         levs),   del(         levs)
      real(kind=kind_phys)   prsl(         levs),prdel(         levs)
      real(kind=kind_phys)   prsi(         levs+1)
      real(kind=kind_phys)  si(         levp1),slk(         levs)
      real(kind=kind_phys)  pgr
      real(kind=kind_phys) rkr,rk1,sumdel,dif
!     real(kind=kind_phys) rk,rkr,rk1,sumdel,dif
 
      integer njeff,nsize_ar
      integer levshc(nsize_ar)
      integer iq,ilat,me
      integer i,k
 
!     print *,' enter hyb2sig_fd ' 		! hmhj

      do k=1,levp1
         si(levs+2-k)= ak5(k)/pgr+bk5(k) ! si are now sigmas
      enddo
 
         sumdel=0.
 
      do k=1,levs
         del(k)= si(k)-si(k+1)
         sumdel=sumdel+del(k)
      enddo
 
!     rk = rd/cp
      rk1 = rk + 1.e0
      rkr = 1.0/rk
 
      do k=1,levs
         dif = si(k)**rk1 - si(k+1)**rk1
         dif = dif / (rk1*(si(k)-si(k+1)))
         sl(k) = dif**rkr
      enddo
 
      do k=1,levs
         slk(k)=sl(k)**rk
      enddo
 
          i=1
         me=0
      if(me.eq.0) return
 
250     format('ilat iq=',i4,2x,i5,'sumdel(i)=',e12.3)
 
251     format('ilat=',i4,2x,'iq=',i5,2x,
     &    '=',e12.3,2x,'levshc(i)=',i5,' me=',i3)
 
        if(ilat.lt.3)then
 
         write(200,250)ilat,iq,sumdel
         write(200,251)ilat,iq,me
 
       do k=1,levp1
       write(200,150) ak5(k),bk5(k),si(k),pgr
150    format('ak5(k)=',e12.3,2x,'bk5(k)=',e12.3,2x,'si(i,k)=',e12.3,
     & 'p=',e12.3)
       enddo
 
         do k=1,levs
          if(me.eq.0)then
          write(200,300)k,sl(k),del(k)
          endif
300    format('k sl del=',i2,2x,e12.3,2x,e12.3)
         enddo
 
        endif
 
!     print *,' leave hyb2sig_fd ' 		! hmhj

      return
      end

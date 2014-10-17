      subroutine sig2press(njeff,nsize_ar,pgr,sl,si,slk,sik,prsi,prsl
     &,                    prsik, prslk)
 
      use machine , only : kind_phys
      use resol_def
      use coordinate_def
      use physcons, rk => con_rocp
      implicit none
 
      integer njeff,nsize_ar
      real(kind=kind_phys)    sl(levs),   si(levp1)
      real(kind=kind_phys)    slk(levs),  sik(levp1), pgrk(nsize_ar)
      real(kind=kind_phys) prsl(nsize_ar,levs), prslk(nsize_ar,levs)
      real(kind=kind_phys) prsi(nsize_ar,levs+1), prsik(nsize_ar,levs+1)
      real(kind=kind_phys)  pgr(nsize_ar)
      real (kind=kind_phys), parameter :: PT01=0.01
 
      integer iq,ilat,me
      integer i,k
 
!     sik(levs+1) = (si(levs+1)*0.01) ** rk
!     do k=1,levs
!       slk(k) = (sl(k)*0.01) ** rk
!       sik(k) = (si(k)*0.01) ** rk
!     enddo
      do i=1,njeff
         prsi(i,levs+1)  = si(levs+1)*pgr(i)      ! prsi are now pressures
         pgrk(i)         = (pgr(i)*pt01) ** rk
         prsik(i,levs+1) = sik(levs+1) * pgrk(i)
      enddo
      do k=1,levs
        do i=1,njeff
          prsi(i,k)  = si(k)*pgr(i)               ! prsi are now pressures
          prsl(i,k)  = sl(k)*pgr(i)
          prsik(i,k) = sik(k) * pgrk(i)
          prslk(i,k) = slk(k) * pgrk(i)
        enddo
      enddo
 
 
      return
      end

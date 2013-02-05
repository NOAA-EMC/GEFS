        subroutine updown_gc(sl,zoncoef)
        use resol_def
        implicit none
        integer k,kd,ku
        real(kind=kind_evod) ysk(levs),sl(levs),zoncoef(levs)
        real(kind=kind_evod)  xd(levs),xu(levs),sf(levs)
 

!       print *,' enter getysk_gc_fd '
 
        DO k=1,levs
          KD=MAX(k-1,1)
          KU=MIN(k+1,levs)
          SF(k)=1./(sl(KU)-sl(KD))/SQRT(2.)
        ENDDO
 
 
          xu(levs)=zoncoef(levs)
        do k=1,levs-1
          xu(k)=zoncoef(k+1)
        enddo
        xd(1)=zoncoef(1)
        do k=2,levs
          xd(k)=zoncoef(k-1)
        enddo
 
      do k=1,levs
        zoncoef(k)=(xu(k)-xd(k))*sf(k)/101.316
      enddo
 
!       print *,' leave getysk_gc_fd '

      return
      end
 
 

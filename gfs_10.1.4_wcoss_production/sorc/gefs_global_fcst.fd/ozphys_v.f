      SUBROUTINE OZPHYS (IX, IM, LEVS, KO3, DT, OZI, OZO, TIN, PO3,
     &                   PRSL, PRDOUT, pl_coeff, DELP, LDIAG3D,
     &                   LGGFS3D, OZP,me)
!
!     This code assumes that both PRSL and PO3 are from bottom to top
!     as are all other variables
!
      USE MACHINE , ONLY : kind_phys
      use physcons, only : grav => con_g
      implicit none
!
      real, parameter :: gravi=1.0/grav
      integer IM, IX, LEVS, KO3, pl_coeff,me
      real(kind=kind_phys) OZI(IX,LEVS),   OZO(IX,LEVS), PO3(KO3),
     &                     PRSL(IX,LEVS),  TIN(IX,LEVS), DELP(IX,LEVS),
     &                     PRDOUT(IX,KO3,pl_coeff),
     &                     OZP(IX,LEVS,pl_coeff),  DT
!
      integer k,kmax,kmin,l,I,j
      logical              LDIAG3D, LGGFS3D, flg(im)
      real(kind=kind_phys) pmax, pmin, tem, temp
      real(kind=kind_phys) WK1(IM), WK2(IM), WK3(IM), PROD(IM,pl_coeff),
     &                     OZIB(IM),  COLO3(IM,LEVS+1)
!
      if (pl_coeff .gt. 2) then
        colo3(:,levs+1) = 0.0
        do l=levs,1,-1
          do i=1,im
            colo3(i,l) = colo3(i,l+1) + ozi(i,l) * delp(i,l) * gravi
          enddo
        enddo
      endif
!
      DO L=1,levs
        PMIN =  1.0E10
        PMAX = -1.0E10
!
        DO I=1,IM
          WK1(I) = log(prsl(I,l))
          PMIN   = MIN(WK1(I), PMIN)
          PMAX   = MAX(WK1(I), PMAX)
          prod(i,:) = 0.0
        ENDDO
        kmax = 1
        kmin = 1
        do K=1,KO3-1
          if (pmin .lt. po3(k)) kmax = k
          if (pmax .lt. po3(k)) kmin = k
        enddo
!
        do K=kmin,kmax
          temp = 1.0 / (po3(k) - po3(k+1))
          do i=1,IM
            flg(i) = .false.
            if (wk1(i) .lt. po3(k) .and. wk1(i) .ge. po3(k+1)) then
              flg(i) = .true.
              wk2(i) = (wk1(i) - po3(k+1)) * temp
              wk3(i) = 1.0 - wk2(i)
            endif
          enddo
          do j=1,pl_coeff
            do i=1,IM
              if (flg(i)) then
                prod(i,j)  = wk2(i) * prdout(i,k,j)
     &                     + wk3(i) * prdout(i,k+1,j)
              endif
            enddo
          enddo
        enddo
!
        do j=1,pl_coeff
          do i=1,IM
            if (wk1(i) .lt. po3(ko3)) then
              prod(i,j) = prdout(i,ko3,j)
            endif
            if (wk1(i) .ge. po3(1)) then
              prod(i,j) = prdout(i,1,j)
            endif
          enddo
        enddo
        if (pl_coeff .eq. 2) then 
          do i=1,IM
            OZIB(I)   = OZI(I,L)             ! NO FILLING
            ozo(i,l)  = (OZIB(I) + prod(i,1)*dt) / (1.0 + prod(i,2)*dt)
          enddo
!
          IF (LDIAG3D .or. LGGFS3D) then     !     Ozone change diagnostics
            DO I=1,IM
              OZP(I,L,1) = OZP(I,L,1) + PROD(I,1)*DT
              OZP(I,L,2) = OZP(I,L,2) + (OZO(I,L) - OZIB(I))
            ENDDO
          ENDIF
        endif
        if (pl_coeff .eq. 4) then 
          do i=1,IM
            OZIB(I)  = OZI(I,L)            ! NO FILLING
            tem      = prod(i,1) + prod(i,3)*tin(i,l)
     &                           + prod(i,4)*colo3(i,l+1)
!     if (me .eq. 0) print *,'ozphys tem=',tem,' prod=',prod(i,:)
!    &,' ozib=',ozib(i),' l=',l,' tin=',tin(i,l),'colo3=',colo3(i,l+1)
            ozo(i,l) = (OZIB(I)  + tem*dt) / (1.0 + prod(i,2)*dt)
          enddo
          IF (LDIAG3D .or. LGGFS3D) then     !     Ozone change diagnostics
            DO I=1,IM
              OZP(I,L,1) = OZP(I,L,1) + PROD(I,1)*DT
              OZP(I,L,2) = OZP(I,L,2) + (OZO(I,L) - OZIB(I))
              OZP(I,L,3) = OZP(I,L,3) + PROD(I,3)*tin(i,l)*DT
              OZP(I,L,4) = OZP(I,L,4) + PROD(I,4)*colo3(i,l+1)*DT
            ENDDO
          ENDIF
        endif
      enddo                                ! Vertical Loop
!
      RETURN
      END

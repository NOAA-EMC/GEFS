      subroutine dcyc2t3_pre_rad(ix,im,levs,solhr,slag,
     &                   sinlab,coslab,sdec,cdec,
     &                   xlon,czmn,sfcdlw,sfcnsw,tf,
     &                   sfcdsw,dswsfc,                 ! FOR SEA-ICE - XW Nov04
     &                   tsea,tsflw,swh,hlw,
!yth  add xcosz as output for sunshine time calc.  mar/08
!    &                   dlwsfc,ulwsfc,slrad,tau,xmu)
     &                   dlwsfc,ulwsfc,slrad,tau,xmu,xcosz)
      use machine     , only : kind_phys
      use physcons, pi => con_pi, sbc => con_sbc,jcal=>con_JCAL
      implicit none
      integer              levs,im,ix
      real(kind=kind_phys) cdec,cnwatt,hsigma,sdec,slag,solhr
      real(kind=kind_phys) sinlab(im)  , coslab(im), xlon(im),
     &                     czmn(im),     sfcdlw(im), sfcnsw(im),
     &                     tf(im),       tsea(im),   tsflw(im),
     &                     dlwsfc(im),   ulwsfc(im), slrad(im),
!yth  add xcosz as output var
!    &                     xmu(im)
     &                     xmu(im), xcosz(im)
      real(kind=kind_phys) swh(ix,levs), hlw(ix,levs), tau(im,levs)
!c-- XW: FOR SEA-ICE Nov04
C  ADD SFCDSW (INPUT) & DSWSFC (OUTPUT)
      real(kind=kind_phys) sfcdsw(im),   dswsfc(im)
!c-- XW: END SEA-ICE
      integer              i, k
      real(kind=kind_phys) cns,ss,cc,ch,sdlw, tem
      PARAMETER           (CNWATT=-jcal*1.E4/60.)


      slag=0.
      hlw=0.  
      swh=0.
      sfcnsw=0.
      sfcdlw=350./cnwatt
!sela

      cns = pi*(solhr-12.)/12.+slag
      do i=1,im
!sela   ss     = sinlab(i) * sdec
!sela   cc     = coslab(i) * cdec
!sela   ch     = cc * cos(xlon(i)+cns)
!sela   xmu(i) = ch + ss
        SS=0.5
        CC=0.5
        CH=0.5
!yth mar/08
        XMU(i)=CH+SS
        CZMN(i)=0.1

      enddo
      do i=1,im
        xcosz(i) = xmu(i)
        if(xmu(i).gt.0.01.and.czmn(i).gt.0.01) then
          xmu(i) = xmu(i) / czmn(i)
        else
          xmu(i) = 0.
        endif
        tem       = tf(i) / tsflw(i)
        tem       = tem * tem
        dlwsfc(i) = sfcdlw(i) * tem * tem
        slrad(i)  = sfcnsw(i)*xmu(i) - dlwsfc(i)
        dswsfc(i) = sfcdsw(i)*xmu(i)                    ! FOR SEA-ICE - XW Nov04
        tem       = tsea(i) * tsea(i)
        ulwsfc(i) = sbc * tem * tem
      enddo
      do k=1,levs
        do i=1,im
          tau(i,k) = tau(i,k) + swh(i,k)*xmu(i) + hlw(i,k)
        enddo
      enddo
      return
      end

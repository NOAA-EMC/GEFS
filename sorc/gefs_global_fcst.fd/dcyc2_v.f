CFPP$ NOCONCUR R
      SUBROUTINE DCYC2T3(IX,IM,LEVS,SOLHR,SLAG,
     &                   SINLAB,COSLAB,SDEC,CDEC,
     &                   XLON,CZMN,SFCDLW,SFCNSW,TF,
     &                   SFCDSW,DSWSFC,                 ! FOR SEA-ICE - XW Nov04
     &                   TSEA,TSFLW,SWH,HLW,
     &                   DLWSFC,ULWSFC,SLRAD,TAU,XMU,xcosz)
!
      USE MACHINE     , ONLY : kind_phys
      USE PHYSCONS, PI => con_PI, SBC => con_SBC
      implicit none
!
!     include 'constant.h'
!
      integer              levs,IM,IX
      real(kind=kind_phys) cdec,cnwatt,hsigma,sdec,slag,solhr
      real(kind=kind_phys) SINLAB(IM)  , COSLAB(IM), XLON(IM),
     &                     CZMN(IM),     SFCDLW(IM), SFCNSW(IM),
     &                     TF(IM),       TSEA(IM),   TSFLW(IM),
     &                     DLWSFC(IM),   ULWSFC(IM), SLRAD(IM),
!yth add cosine of zenith angle as output for sunshine time calc.  3/08
!    &                     XMU(IM)
     &                     XMU(IM), xcosz(IM)
      real(kind=kind_phys) SWH(IX,LEVS), HLW(IX,LEVS), TAU(IM,LEVS)
!c-- XW: FOR SEA-ICE Nov04
C  ADD SFCDSW (INPUT) & DSWSFC (OUTPUT)
      real(kind=kind_phys) SFCDSW(IM),   DSWSFC(IM)
!c-- XW: END SEA-ICE
!     PARAMETER           (HSIGMA=SBC,CNWATT=-con_JCAL*1.E4/60.)
      integer              I, K
      real(kind=kind_phys) cns,ss,cc,ch,sdlw, tem
C-----------------------------------------------------------------------
C  COMPUTE COSINE OF SOLAR ZENITH ANGLE FOR BOTH HEMISPHERES.
      CNS = PI*(SOLHR-12.)/12.+SLAG
      DO I=1,IM
        SS     = SINLAB(I) * SDEC
        CC     = COSLAB(I) * CDEC
        CH     = CC * COS(XLON(I)+CNS)
        XMU(I) = CH + SS
      ENDDO
C       XMU=(SINLAB*SDEC)
C    1        +(COSLAB*CDEC)*COS(XLON+CNS)
CC    DO I=1,LON2
C  NORMALIZE BY AVERAGE VALUE OVER RADIATION PERIOD FOR DAYTIME.
      DO I=1,IM
        xcosz(i) = xmu(i)
        IF(XMU(I).GT.0.0001.AND.CZMN(I).GT.0.0001) THEN
          XMU(I) = XMU(I) / CZMN(I)
        ELSE
          XMU(I)   = 0.
        ENDIF
C  ADJUST LONGWAVE FLUX AT SURFACE TO ACCOUNT FOR T CHANGES IN LAYER 1.
!       SDLW      = SFCDLW(I)*(TF(I)/TSFLW(I))**4
        TEM       = TF(I) / TSFLW(I)
        TEM       = TEM * TEM
        DLWSFC(I) = SFCDLW(I) * TEM * TEM
C  RETURN NET SURFACE RADIATIVE FLUX.
!       SLRAD(I)  = SFCNSW(I)*XMU(I) + SDLW
        SLRAD(I)  = SFCNSW(I)*XMU(I) - DLWSFC(I)
        DSWSFC(I) = SFCDSW(I)*XMU(I)                    ! FOR SEA-ICE - XW Nov04
C  RETURN DOWNWARD AND UPWARD LONGWAVE FLUX AT GROUND, RESPECTIVELY.
!       DLWSFC(I) = SDLW*CNWATT
!       ULWSFC(I) = HSIGMA*TSEA(I)**4
        TEM       = TSEA(I) * TSEA(I)
        ULWSFC(I) = SBC * TEM * TEM
      ENDDO
C  ADD RADIATIVE HEATING TO TEMPERATURE TENDENCY
      DO K=1,LEVS
        DO I=1,IM
          TAU(I,K) = TAU(I,K) + SWH(I,K)*XMU(I) + HLW(I,K)
        ENDDO
      ENDDO
      RETURN
      END

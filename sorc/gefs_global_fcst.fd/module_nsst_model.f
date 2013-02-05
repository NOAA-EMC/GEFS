module ocean_model
!$$$  module documentation block
!                .      .    .                                       .
! module:    ocean_model         oceanic model to forward the oceanic state and the coefficients to calculate the sensitivities.
!                                 at present, diurnal warming model, sub-layer cooling model 
!   prgmmr: Xu Li          org: np22                date: 2007-05-21
!
! abstract:  diurnal warming model, sub-layer cooling model, coefficients to calculate sensitivities of Tz to T_ref, support subroutines

  USE MACHINE , ONLY : kind_phys
  USE FUNCPHYS, ONLY : fpvs
  USE physcons, only: cp_a => con_cp,rd => con_rd
  USE resol_def, only: nr_nsst,nf_nsst
  use layout1
  USE module_nsst_parameters, ONLY : cp_w,nu=>vis_w, kw => tc_w,visw,smallnumber, &
                                    Von,hvap,sigma_r,gray,t0K,rho0_w,radian, &
                                    omg_m,omg_r,omg_rot, &
                                    omega        ! ang vel of earth    (1/s)

  USE module_nsst_water_prop, ONLY: sw_rad,sw_rad_Aw,sw_rad_upper,sw_rad_upper_Aw,sw_rad_skin,grv
  PRIVATE
  PUBLIC :: cool_skin, warm_layer, Jacobi_Temp


CONTAINS
  !
  !======================
  !
 subroutine cool_skin(ustar_a,alpha,beta,rho_w,rho_a,Ts,F_nsol,F_sol_0,sss,evap,sinlat,deltaT_c,z_c)
!
! Upper ocean cool-skin parameterizaion, Fairall et al, 1996. 
!
! INPUT: 
! ustar_a : atmosphreic friction velocity at the air-sea interface (m/s)
! F_nsol  : the "nonsolar" part of the surface heat flux (W/m^s)
! sss     : ocean upper mixed layer salinity (ppu)
! evap    : latent heat flux (W/M^2)
! F_sol_0 : solar radiation at the ocean surface (W/m^2)
! rho_a   : atmospheric density
! rho_w   : oceanic density
! Ts      : oceanic surface temperature
! alpha   : thermal expansion coefficient
! beta    : saline contraction coefficient
! sinlat  : sine of latitude
! 
! OUTPUT:
! z_c:      molecular sublayer (cool-skin) thickness (m) 
! deltaT_c: cool-skin temperature correction (degrees K) 
! 
  implicit none
  real(kind=kind_phys), parameter  :: z_c_max=0.01,z_c_ini=0.001,ustar_a_min = 0.056
  real(kind=kind_phys), intent(in) :: ustar_a,F_nsol,evap,sss,F_sol_0,rho_a,rho_w,alpha,beta,sinlat 
  real(kind=kind_phys), intent(out):: z_c,deltaT_c
! declare local variables
  real(kind=kind_phys)  :: xi,Hb,ustar1_a,bigc,grav,deltaF,Le,Ts,fxp

  grav = grv(sinlat)
  Le = (2.501-.00237*Ts)*1e6

  z_c=z_c_ini                 ! initial quess

  ustar1_a=max(ustar_a,ustar_a_min)

  CALL sw_rad_skin(z_c,fxp)
  deltaF=F_sol_0*fxp

  Hb=alpha*(F_nsol-DeltaF) +beta*sss*cp_w*evap/Le
  bigc=16*grav*cp_w*(rho_w*nu)**3/(rho_a*rho_a*kw*kw)
  if ( Hb > 0 ) then 
    xi=6./(1+(bigc*Hb/ustar1_a**4)**0.75)**0.3333333
  else
    xi=6.0
  endif
  z_c=min(z_c_max,xi*nu/(SQRT(rho_a/rho_w)*ustar1_a ))

  CALL sw_rad_skin(z_c,fxp)
  deltaF=F_sol_0*fxp
  deltaF=F_nsol - deltaF
  if ( deltaF > 0 ) then
    deltaT_c= deltaF * z_c / kw
  else
    deltaT_c=0.
    z_c=0.
  endif

 end subroutine cool_skin
  !
  !======================
  !

 subroutine warm_layer(alpha,beta,sss,rho_w,F_sol_0,F_nsol,tau,    &
                       lflx,rain,Qrain,Ts,dtime,sinlat,solar_time, &
                       ifd,time_old,time_ins,I_Sw,I_Q,I_Qrain,     &
                       I_M,I_Tau,dt_warm,z_w,                      & 
!                      ocnr1,ocnf1,                                &
                       kdt,lprint)
!
! Upper ocean daily warm layer parameterizaion, Fairall et al, 1996. 
!
! INPUT: 
! alpha       : thermal expansion coefficient (K^-1) 
! beta        : saline contraction coefficient (ppt^-1)
! sss         : sea water salinity (ppt)
! rho_w       : sea water density (kg m^-3)
! F_sol_0     : solar radiation at the ocean surface (W/m^2)
! F_nsol      : the "nonsolar" part of the surface heat flux (W/m^2)
! tau         : wind stress (N/M^2)
! lflx        : latent heat flux (w/m^2)
! rain        : rainfall (kg/m^2/s)
! Qrain       : sensible heat flux by rainfall (W/M^2)
! Ts          : sea surface temperature (K)
! dtime       : timestep
! sinlat      : sine of latitude
! solar_time  : local time
! kdt         : counts for time steps
! 
! InOut:
! z_w         : diurnal warming layer depth                       : z_w
! dt_warm     : diurnal warming amount                            : dt_warm
!
! ocnr1(1:11) : the variables for restart
! ocnr1( 1)   : index of time integral started mode                : ifd
! ocnr1( 2)   : solar time at previous time                        : time_old
! ocnr1( 3)   : the period of time integral                        : time_ins
! ocnr1( 4)   : time integral of solar radiation flux              : I_Sw
! ocnr1( 5)   : time integral of non-solar heat flux               : I_Q
! ocnr1( 6)   : time integral of rain caused sensibal heat flux    : I_Qrain
! ocnr1( 7)   : time integral of mass flux S(E-P)                  : I_M
! ocnr1( 8)   : time integral of momentum flux                     : I_Tau
! ocnr1( 9)   : time integral of d(I_Sw)/d(z_w)                    : I_Sw_Zw
! ocnr1(10)   : time integral of d(I_Q)/d(Ts)                      : I_Q_Ts
! ocnr1(11)   : time integral of d(I_M)/d(Ts)                      : I_M_Ts

! ocnf1(1)    : Reference temperature                              : T_ref
! ocnf1(2)    : Sub-layer cooling amount                           : dt_cool
! ocnf1(3)    : Sub-layer cooling thickness                        : z_c
! ocnf1(4)    : diurnal warming amount                             : dt_warm
! ocnf1(5)    : diurnal warming layer depth                        : z_w
! ocnf1(6)    : W_0 (time integral)
! ocnf1(7)    : W_d (time integral)
! ocnf1(8)    : C_0 (current time step)
! ocnf1(9)    : C_d (current time step)

  implicit none
  integer,              parameter                           :: niter_max=5
  real(kind=kind_phys), parameter                           :: &
      Ri_c=0.65               & ! critical bulk  Richardson number (Fairall et al, 1996, p.1300) 
     ,eps_z=0.01              & ! criteria to finish iterations for z_w
     ,z_w_max=20.0            & ! max warm layer thickness
     ,z_w_min=0.2             & ! max warm layer thickness
     ,solar_time_6am=21600.0  & ! solar time at 6am local
     ,Q_warm_min=25.0         & ! minimal warming to start generate the warm layer ( = 50 in Fairall et al 1996)
     ,tau_min=0.0031            ! minimal wind stress (~ 1.6 m/s of 10m-wind), no need in Fairall, due to ug accounted for

! Input/Output variables

  real(kind=kind_phys), intent(in) :: alpha,beta,sss,rho_w,F_sol_0,F_nsol,tau,&
                                      lflx,rain,Qrain,Ts,solar_time,sinlat,dtime
  integer, intent(in) :: kdt
  logical, intent(in) :: lprint

! real(kind=kind_phys), intent(inout) :: ocnr1(nr_ocn)
! real(kind=kind_phys), intent(inout) :: ocnf1(nf_ocn)

! Local variables
  integer :: niter,i
  real(kind=kind_phys) :: ifd,time_old,time_ins,I_Sw,I_Q,I_Qrain,I_M,I_Tau
  real(kind=kind_phys) :: dt_warm,z_w,z_w_prev,dt_warm0,z_w0
  real(kind=kind_phys) :: I_heat,fxp,coeff,coeff1,coeff2,z_w_tmp,F_sol,Q_warm,&
                          RF_warm,Q_joule,TAU_warm,M_warm,timestep,coriolis,Le

  z_w_prev = z_w
  dt_warm0 = dt_warm
  z_w0     = z_w
 
  coeff    = SQRT(2.*Ri_c) 

! alpha       : thermal expansion coefficient (K^-1) 
! beta        : saline contraction coefficient (ppt^-1)
! sss         : sea water salinity (ppt)
! rho_w       : sea water density (kg m^-3)
! F_sol_0     : solar radiation at the ocean surface (W/m^2)
! F_nsol      : the "nonsolar" part of the surface heat flux (W/m^2)
! tau         : wind stress (N/M^2)
! lflx        : latent heat flux (w/m^2)
! rain        : rainfall (kg/m^2/s)
! Qrain       : sensible heat flux by rainfall (W/M^2)
! Ts          : sea surface temperature (K)
! dtime       : timestep
! sinlat      : sine of latitude
! solar_time  : local time

! if (lprint) print *,' dt_warm0=',dt_warm0,' solar_time=',solar_time,&
! ' rain=',rain,' qrain=',qrain,' lflx=',lflx,' F_sol_0=',F_sol_0,&
! 'F_nsol=',F_nsol,' Ts=',Ts,' alpha=',alpha,' beta=',beta,' sss=',sss,&
! ' rho_w=',rho_w,' tau=',tau,' ifd=',ifd,' time_old=',time_old


  if (solar_time > solar_time_6am .and. ifd == 0.0 ) then 
    dt_warm = 0.0        ! initialize diurnal warming amount to be zero:
                         ! 6 am too late to start the first day
    z_w     = z_w_max    ! assign warming depth as z_w_max meters 
  else
    ifd = 1.0

!   if (solar_time < time_old) then     ! solar_time and time_old : 0 ~ 24
    if (abs(solar_time -21600.) < dtime) then  ! solar_time and time_old : 0 ~ 24
                                      ! just pass the midnight (start of a new day)
      ifd      = 1.0                  ! count of the start of a new day
      time_ins = 0.0                  ! initialize the integral time to be zero
      I_Sw     = 0.0
      I_Q      = 0.0
      I_Qrain  = 0.0
      I_heat   = 0.0                  ! initialize net heat to be zero
      I_Tau    = 0.0                  ! initialize wind stress to be zero 
      I_M      = 0.0                  ! initialize mass exchange (E-P) to be zero 
      dt_warm  = 0.0                  ! initialize diurnal warming amount to be zero
      z_w      = z_w_max              ! assign warming depth as z_w_max

    else
!     Get the first guess (or initial value) of the absorbed solar radiation
!     in warm layer. Depends on if the time integral has started or not
!
      if( time_ins == 0.0 ) THEN 
        F_sol = 0.5*F_sol_0          ! implies a shallow heating layer to start the integration
      else
        call sw_rad(z_w_prev,fxp)
        F_sol = F_sol_0*fxp
      endif

      timestep = solar_time - time_old                   ! in seconds

      if ( ifd == 1.0 ) timestep = dtime

      Le        = (2.501-.00237*Ts)*1e6
      Q_warm    = (F_sol-F_nsol)*timestep
      RF_warm   = Qrain*timestep
      TAU_warm  = max(tau_min,tau)*timestep
      M_warm    = sss*(lflx/Le-rain)*timestep/rho0_w
!
!     skip: the heat is not strong enough to set up the onset of a diurnal warming
!
      IF (Q_warm/timestep < Q_warm_min .and. time_ins == 0.0) THEN   ! this should cover the period from midnight to the onset time of diurnal warming
        dt_warm = 0.0            ! initialize diurnal warming amount to be zero
        z_w     = z_w_max        ! assign warming depth as 19 meters (z_w_max)
      else                       ! there exists a warm layer
!
!                                  calculate warm layer depth
!
        time_ins = time_ins + timestep
        I_Tau    = I_Tau  + TAU_warm
        I_M      = I_M + M_warm
        I_heat   = I_Sw - I_Q - omg_r*I_Qrain

        if ( I_heat+Q_warm > 0.0 ) then
          coeff1  = rho_w*alpha*grv(sinlat)/cp_w
          coeff2  = beta*grv(sinlat)*rho_w**2

          z_w_tmp = z_w_prev

          iters: do niter=1,niter_max
            CALL sw_rad(z_w_tmp,fxp)
            F_sol   = F_sol_0*fxp

            Q_joule = (F_sol-F_nsol)*timestep - RF_warm

            if ( I_heat+Q_joule > 0.0 ) THEN
!             z_w = MIN(z_w_max,coeff*I_Tau/SQRT(coeff1*(I_heat+Q_joule)+coeff2*omg_m*I_M) )
              z_w = MIN(z_w_max,coeff*I_Tau/SQRT(coeff1*(I_heat+Q_joule)) )
              z_w = max(z_w,z_w_min)
            endif

            if (ABS(z_w - z_w_tmp) < eps_z .and. z_w/=z_w_max .and. niter<5) exit iters
            z_w_tmp = z_w
          end do iters

          if (niter == niter_max) then
            WRITE(*,*) 'ERROR in warm_layer: iterations do not converge'
            STOP
          endif
        else
          F_sol   = 0.75*F_sol_0
          z_w     = z_w_max
!         Q_joule = (F_sol-F_nsol)*timestep-RF_warm
          Q_joule = (F_sol-F_nsol)*timestep
        endif                                 ! IF ( I_heat+Q_warm > 0.0 ) THEN

        I_heat   = I_heat   + Q_joule
        I_Sw     = I_Sw     + F_sol*timestep
        I_Q      = I_Q      + F_nsol*timestep
        I_Qrain  = I_Qrain  + RF_warm

        if (I_heat <= 0.0  ) then
          dt_warm = 0.
          z_w     = z_w_max
        else

!         if ( (solar_time >= 21.*3600. .and. solar_time < 24.*3600.) ) then
!           I_heat = I_heat*(24.*3600. - solar_time)/10800.
!         endif

!         z_w     = MIN(z_w_max,coeff*I_Tau/SQRT(coeff1*I_heat+coeff2*omg_m*I_M) )
          z_w     = MIN(z_w_max,coeff*I_Tau/SQRT(coeff1*I_heat) )
          z_w     = max(z_w,z_w_min)
          dt_warm = 2.*I_heat/(rho_w*cp_w*z_w)

!         if (lprint) print *,' dt_warm=',dt_warm,' i_heat=',i_heat,' rho_w=',&
!                     rho_w,' cp_w=',cp_w,' z_w=',z_w
        endif

      endif           ! IF (Q_warm/timestep < Q_warm_min .and. time_ins == 0.0) THEN : start to accumulate the heat
    endif             ! IF (solar_time < time_old) THEN: midnight reset
  endif             ! IF (solar_time > solar_time_6am .and. ifd == 0.0 ) THEN: too late to start the first day 

! if ( mod(kdt,60) == 1 .and. dt_warm >= 2.50 ) then
!   write(*,'(a,I5,F3.0,3F8.0,2F11.0,F8.2,2F8.1,F7.4,4F7.2)') &
!            'warming : ',kdt,ifd,time_old,solar_time,time_ins,I_Sw,I_Q,I_Tau,F_sol_0,F_nsol,tau,dt_warm0,z_w0,dt_warm,z_w
! endif

  end subroutine warm_layer


  subroutine Jacobi_Temp(alpha,beta,sss,Ta,Ps,Qa,ustar_a,rho_w,rho_a,rain,lflx,sflx,dlwrf,ulwrf,Rns,Ts, &
                         timestep,sinlat,loc_time,Hs_Ts,Hl_Ts,RF_Ts, &
                         I_Sw,I_Q,I_M,I_Sw_Zw,I_Q_Ts,I_M_Ts, &
                         dt_cool,z_c,dt_warm,z_w, &
                         c_0,c_d,w_0,w_d, &
!                        ocnr1,ocnf1, &
                         kdt)
!
! Upper ocean deily warm layer parameterizaion (Fairall et al, 1996) and Jacobi (Xu Li, 2007) 
!
! INPUT: 
! alpha      : thermal expansion coefficient 
! beta       : saline contraction coefficient
! Ta         : surface atmospheric temperature (K)
! Ps         : surface atmospheric pressure (Pa)
! Qa         : surface atmospheric specific humidity (kg/kg)
! ustar_a    : friction velocity in the atmosphere (m/s)
! rho_w      : sea water density (kg/m^3)
! rho_a      : atmoshpere density (kg/m^3)
! rain       : rainfall (kg/m^2/s)
! lflx       : time mean value: latent heat flux (w/m^2)
! sflx       : time mean value: sensible heat flux (w/m^2)
! dlwrf      : time mean value: downward longwave radiation flux (w/m^2)
! ulwrf      : time mean value: upward longwave radiation flux (w/m^2)
! Rns        : time mean value: net solar radiation at the ocean surface (W/m^2)
! Ts         : sea surface temperature (K)
! timestep   : time step
! sinlat     : sine(latitude)
! loc_time   : solar time in hour
! Hs_Ts      : d(Hs)/d(Ts) = exchange coefficient for air-sea flux calculation
! Hl_Ts      : d(Hl)/d(Ts) 
! RF_Ts      : d(Qrain)/d(Ts) 
! kdt        : counts for time steps
! me         : cpu number
! ocnf1(2)   : Sub-layer cooling amount                   : dt_cool
! ocnf1(3)   : Sub-layer cooling thickness                : z_c
! ocnf1(4)   : diurnal warming amount                     : dt_warm
! ocnf1(5)   : diurnal warming layer depth                : z_w

! 
! InOut:
!        ocnr1( 4)     : time integral of solar radiation flux      : I_Sw
!        ocnr1( 5)     : time integral of non-solar heat flux       : I_Q
!        ocnr1( 6)     : time integral of mass flux S(E-P)          : I_M
!        ocnr1( 9)     : time integral of d(I_Sw)/d(z_w)            : I_Sw_Zw
!        ocnr1(10)     : time integral of d(I_Q)/d(Ts)              : I_Q_Ts
!        ocnr1(11)     : time integral of d(I_M)/d(Ts)              : I_M_Ts
! Output:
!        ocnf1(6)      : W_0 (time integral)
!        ocnf1(7)      : W_d (time integral)
!        ocnf1(8)      : C_0 (current time step)
!        ocnf1(9)      : C_d (current time step)


  IMPLICIT NONE
  real(kind=kind_phys), parameter   :: Rich=0.65,ustar_a_min = 0.07
  real(kind=kind_phys), intent(in)  :: alpha,beta,sss,Ta,Ps,Qa,rho_w,rho_a,ustar_a,rain,lflx,sflx,dlwrf,ulwrf,Rns, &
                                       Ts,timestep,sinlat,loc_time,Hs_Ts,Hl_Ts,RF_Ts
! real(kind=kind_phys), intent(inout) :: ocnf1(nf_ocn)
! real(kind=kind_phys), intent(inout) :: ocnr1(nr_ocn)
!
!
! Local variables declaring 
!
  integer :: niter,i,j,kdt,me
  real(kind=kind_phys) :: dt_cool    ! cooling amount
  real(kind=kind_phys) :: z_c        ! thickness of sub-layer
  real(kind=kind_phys) :: z_w        ! thickness of diurnal warming layer
  real(kind=kind_phys) :: dt_warm    ! diurnal warming at the sea surface
! real(kind=kind_phys) :: time_ins   ! time of integration done (Moorthi)
  real(kind=kind_phys) :: coeff,coeff1,F_sol,Q_warm
  real(kind=kind_phys) :: wj,es,es_liq,es_sol,a_liq,a_sol,b_liq,b_sol,Rnl_Ts,es_Ts,es_liq_Ts,es_sol_Ts,Q_Ts,H_Ts
  real(kind=kind_phys) :: a1,a2,a3,a4,A_c,B_c,H,Rnl,qout,dels,fxp,Qs
  real(kind=kind_phys) :: tcw,cc1,cc2,cc3,Hb,qcol,bigc,Le,dtemp,corioli,A_w,dwat,dtmp,alfac,wetc
  real(kind=kind_phys) :: Sw_Zw,M_Ts,Zw_Ts,I_H
  real(kind=kind_phys) :: I_Sw,I_Q,I_Qrain,I_M,I_Sw_Zw,I_Q_ts,I_M_Ts,C_0,C_d,W_0,W_d
  real(kind=kind_phys) :: Hs_Ts_tmp,Hl_Ts_tmp
!======================================================================================

  tcw = 0.6
  a1 = 0.065; a2 = 11.0; a3 = 6.6e-5; a4 = 8.0e-4
!*************** constants  for heat flux sensitivity to Ts ******
  a_liq = 5.286; b_liq = 25.12; a_sol = 0.5634; b_sol = 23.04
  Le = (2.501-.00237*Ts)*1e6
!
!
! Calculate the sensitivities of heat flux componets to Ts
!

! ========================================================================================================
!
     Rnl = ulwrf - dlwrf       ! positive = upward
     qout=Rnl+sflx+lflx

     Le = (2.501-.00237*Ts)*1e6
     Rnl_Ts = 4.0*gray*sigma_r*Ts**3       ! d(Rnl)/d(Ts)

!    if ( me > 70 .and. me < 100) then
!      write(*,'(a,4F12.6)') 'H_Ts: ',Hs_Ts, Hs_Ts_tmp,Hl_Ts,Hl_Ts_tmp
!    endif


     Q_Ts = Rnl_Ts+Hs_Ts+Hl_Ts+omg_r*RF_Ts

     Rnl = ulwrf - dlwrf       ! positive = upward
     qout=Rnl+sflx+lflx
     if ( z_c == 0.0 ) then
       C_0 = 0.0
       C_d = 0.0
     else

       dels=Rns*(.065+11*z_c-6.6e-5/z_c*(1-exp(-z_c/8.0e-4))) ! Eq.16 Shortwave

       qcol=qout-dels

       bigc=16.0*grv(sinlat)*cp_w*(rho_w*visw)**3/(tcw*tcw*rho_a*rho_a)
       cc1 = 6.0*visw/(tcw*max(ustar_a,ustar_a_min)*(rho_a/rho_w)**0.5)
       cc2 = bigc*alpha/max(ustar_a,ustar_a_min)**4
       cc3 = beta*sss*cp_w/(alpha*Le)
       A_c = (a2+a3/z_c**2-(a3/(a4*z_c)+a3/z_c**2)*exp(-z_c/a4))

       Hb = qcol+beta*sss*lflx*cp_w/(alpha*Le)

       if ( Hb > 0.0 ) then
         B_c = (Q_Ts+cc3*Hl_Ts)/(Rns*A_c-4.0*(cc1*tcw)**3*Hb**0.25/(cc2**0.75*z_c**4))
         C_0 = (z_c*Q_Ts+(qout-dels-Rns*A_c*z_c)*B_c)/tcw                     ! C_0
         C_d = (Rns*A_c*z_c*B_c-Q_Ts)/tcw                                    ! C_d
       else
         C_0 = z_c*Q_Ts/tcw                                                  ! C_0
         C_d = -Q_Ts/tcw                                                    ! C_d
       endif

     endif                      !  if ( z_c == 0.0 ) then

     IF ( dT_warm > 0.0 ) THEN                                                     ! diurnal warming layer exists

       CALL sw_rad_Aw(z_w,A_w)

       Sw_Zw = Rns*A_w/z_w
       M_Ts  = sss*Hl_Ts/(Le*rho0_w)

       I_H  = I_Sw - I_Q - omg_r*I_Qrain
       I_Sw_Zw = I_Sw_Zw + Sw_Zw*timestep
       I_Q_Ts  = I_Q_Ts  + Q_Ts*timestep
       I_M_Ts  = I_M_Ts  + M_Ts*timestep

       coeff = rho_w*cp_w*beta/alpha
       Zw_Ts = z_w*(I_Q_Ts-coeff*omg_m*I_M_Ts)/(2.0*I_H+z_w*I_Sw_Zw+2.0*coeff*omg_m*I_M_Ts)

       W_0 = (2.0/(rho0_w*cp_w*z_w))*((I_Sw_Zw-I_H/z_w)*Zw_Ts-I_Q_Ts)           ! W_0
       W_d = (2.0/(rho0_w*cp_w*z_w**2))*((2.0*I_H/z_w-I_Sw_Zw)*Zw_Ts+I_Q_Ts)    ! W_d

     ELSE                                  ! diurnal warming layer doesn't exist

        W_0 = 0.0
        W_d = 0.0

        I_Sw_Zw = 0.0
        I_Q_Ts  = 0.0
        I_M_Ts  = 0.0

     ENDIF                          ! if ( dT_warm > 0.0 )

    !
  end subroutine Jacobi_Temp
!==============================================================================================

 function qsee(ts,Pa)
   real :: ts,Pa,x,p,es
   x=ts
   p=Pa
   es=6.112*exp(17.502*x/(x+240.97))*.98*(1.0007+3.46e-6*p)
   qsee=es*621.97/(p-.378*es)
 end function

end module ocean_model

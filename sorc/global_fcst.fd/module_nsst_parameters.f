MODULE module_nsst_parameters
  USE machine, ONLY :  kind_phys &
       ,kind_rad ! for astronomy (date) calculations
  !
  ! air constants and coefficients from the atmospehric model
  USE physcons, ONLY: &
         eps     => con_eps   & 
       , cp_a    => con_cp    &   ! spec heat air @p    (J/kg/K)
       , epsm1   => con_epsm1 & 
       , hvap    => con_hvap  &   ! lat heat H2O cond   (J/kg)
       , sigma_r => con_sbc   &   ! stefan-boltzmann    (W/m2/K4)
       , grav    => con_g     &   ! acceleration due to gravity (kg/m/s^2)
       , omega   => con_omega &   ! ang vel of earth    (1/s)
       , rvrdm1  => con_FVirt &
       , rd      => con_RD    &
       , rocp    => con_rocp  &   ! R/cp          
       , pi      => con_pi
  !
  ! NOTE: take timestep from here later
  PUBLIC 
  REAL (kind=kind_phys), PARAMETER :: & 
       !
       ! general constants
        sec_in_day=86400. &
       ,sec_in_hour=3600. &
       ,Von=0.4   &                        ! von Karman's "constant"      !
       ,t0K=273.16  &                      !  Celsius to Kelvin
       ,gray=0.97 &
       ,omg_r =0.0 &
       ,omg_rot = 0.0 &
       ,omg_m = 0.0  &
!dbgz
       ,visw=1.e-6 &                       !m2/s kinematic viscosity water
       ,novalue=0 &
!       ,novalue=-1.0e+10 & 
       ,smallnumber=1.e-6 & 
!      ,timestep_oc=sec_in_day/24. &          ! time step in the ocean model (1 hours)
       ,timestep_oc=sec_in_day/8. &           ! time step in the ocean model (3 hours)
       ,radian=2.*pi/180. & 
       ! sea constants and coefficients
       !
       ,cp_w=4000.   &                     ! specific heat water (J/kg/K )
       ,rho0_w=1022.0 &                    ! density water (kg/m3 ) (or 1024.438)
       ,vis_w=1.e-6  &                     ! kinematic viscosity water (m2/s )
       ,tc_w=0.6    &                      ! Thermal conductivity water (W/m/K )
       ,capa_w =3950.0 &                   ! heat capacity of sea water      !
       !
       ! air constants and coefficients
       !
       ,thref =1.0e-3      ! reference value of specific volume (m**3/kg) 

!!$!============================================
!!$
!!$  ,lvapor=2.453e6 &        ! latent heat of vaporization NOTE: make it function of T ????? NOTE the same as hvap        
!!$       ,alpha=1 ! thermal expansion coefficient
!!$  ,beta ! saline contraction coefficient
!!$  ,cp=1 !=1 specific heat of sea water
!!$  ,g=1 ! acceleration due to gravity
!!$  ,kw=1 ! thermal conductivity of water
!!$  ,nu=1    !kinematic wiscosity
!!$  ,rho_w=1 !water density
!!$  ,rho_a=1 !air density
!!$  ,l_vapr=2.453e6
!!$  ,novalue=--1.0e+10
!!$
!!$c Factors
!!$      Beta=1.2     !Given as 1.25 in Fairall et al.(1996)
!!$      Von=0.4      ! von Karman's "constant"
!!$c      fdg=1.00     ! Fairall's LKB rr to von karman adjustment
!!$      fdg=1.00     !based on results from Flux workshop August 1995
!!$      toK=273.16   ! Celsius to Kelvin
!!$      twopi=3.14159*2.
!!$ 
!!$c Air constants and coefficients
!!$      Rgas=287.1                  !J/kg/K     gas const. dry air
!!$      xlv=(2.501-0.00237*TS)*1e+6  !J/kg  latent heat of vaporization at TS
!!$      Cpa=1004.67                 !J/kg/K specific heat of dry air (Businger 1982)
!!$      Cpv=Cpa*(1+0.84*Q)          !Moist air - currently not used (Businger 1982)
!!$      rhoa=P*100./(Rgas*(T+toK)*(1.+.61*Q)) !kg/m3  Moist air density ( " )
!!$      visa=1.326e-5*(1+6.542e-3*T+8.301e-6*T*T-4.84e-9*T*T*T)   !m2/s
!!$          !Kinematic viscosity of dry air - Andreas (1989) CRREL Rep. 89-11
!!$c 
!!$c Cool skin constants
!!$      al=2.1e-5*(ts+3.2)**0.79     !water thermal expansion coefft.
!!$      be=0.026                     !salinity expansion coefft.
!!$      cpw=4000.                    !J/kg/K specific heat water
!!$      rhow=1022.                   !kg/m3  density water
!!$      visw=1.e-6                   !m2/s kinematic viscosity water
!!$      tcw=0.6                      !W/m/K   Thermal conductivity water
!!$      bigc=16.*grav*cpw*(rhow*visw)**3/(tcw*tcw*rhoa*rhoa)
!!$      wetc=0.622*xlv*QS/(rgas*(TS+toK)**2) !correction for dq;slope of sat. vap.
!!$
!!$!
!!$! Functions
!!$
!!$
!!$  real,    parameter :: timestep=86400.    !integration time step, second
!!$
!!$  real, parameter    :: grav =9.81         !gravity, kg/m/s^2
!!$  real, parameter    :: capa =3950.0       !heat capacity of sea water
!!$  real, parameter    :: rhoref = 1024.438  !sea water reference density, kg/m^3
!!$  real   , parameter :: hslab=50.0         !slab ocean depth
!!$  real   , parameter :: bad=-1.0e+10
!!$  real   , parameter :: tmin=2.68E+02 
!!$  real   , parameter :: tmax=3.11E+02
!!$
!!$  real, parameter :: grav =9.81           !gravity, kg/m/s^2
!!$  real, parameter :: capa =3950.0         !heat capacity of sea water 
!!$  real, parameter :: rhoref = 1024.438    !sea water reference density, kg/m^3
!!$  real, parameter :: tmin=2.68E+02        !normal minimal temp
!!$  real, parameter :: tmax=3.11E+02        !normal max temp
!!$  real, parameter :: smin=1.0             !normal minimal salt
!!$  real, parameter :: smax=50.             !normal maximum salt
!!$  real, parameter :: visct=1.e-5          !viscocity for temperature diffusion
!!$  real, parameter :: viscs=1.e-5          !viscocity for salt diffusion
!!$
!!$
END MODULE module_nsst_parameters

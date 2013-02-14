 SUBROUTINE Sto_Per_Scheme_Step1(work_ini, work, w_step1, &
     TRIEO_LSTOT_SIZ4, Total_member, Cpl_Run_Calling_Number, Sto_Coef, CENTER)

! This subroutine is used to compute the first step of the 
! stochastic perturbation scheme, in which X_i_dot = T_i + S_i 
! and S_i ~ SUM(W_i,j P_j.
!-------------------------------------------------------------

! work_ini is the six hours ago spectral array.  work is the 
! current spectral array and the w_step1 is the step 1 output
! spectral array.
!------------------------------------------------------------


! In the current code, assume each ensemble member uses the same 
! number of the processors.
!---------------------------------------------------------------

 USE ESMF_Mod
 USE GEFS_Cpl_InternalState_ESMFMod
 INTEGER                                                             :: TRIEO_LSTOT_SIZ4
 INTEGER                                                             :: Total_member
 REAL(KIND=KIND_EVOD), DIMENSION(TRIEO_LSTOT_SIZ4, Total_member)     :: work_ini
 REAL(KIND=KIND_EVOD), DIMENSION(TRIEO_LSTOT_SIZ4, Total_member)     :: work 
 REAL(KIND=KIND_EVOD), DIMENSION(TRIEO_LSTOT_SIZ4, Total_member)     :: w_step1
!REAL(KIND=KIND_EVOD), DIMENSION(TRIEO_LSTOT_SIZ4, Total_member - 1) :: w_step1
 REAL(KIND=KIND_EVOD), DIMENSION(Total_member - 1, Total_member - 1) :: Sto_Coef
 INTEGER                                                             :: Cpl_Run_Calling_Number
 INTEGER,              DIMENSION(Total_member - 1)                   :: jp
 REAL(KIND=KIND_EVOD), DIMENSION(Total_member - 1)                   :: rp
!DHOU, added 03-28-2007
 REAL(KIND=KIND_EVOD), DIMENSION(TRIEO_LSTOT_SIZ4)                   :: w_mean
 INTEGER                                                             :: CENTER

! Working arrays.
!----------------
 REAL(KIND=KIND_EVOD), DIMENSION(TRIEO_LSTOT_SIZ4, Total_member)     :: work1_ini
 REAL(KIND=KIND_EVOD), DIMENSION(TRIEO_LSTOT_SIZ4, Total_member)     :: work1
 INTEGER                                                             :: i, j, k

! Put in the computation of the first step of the formulation of
! the stochastic perturbation scheme.
!---------------------------------------------------------------

!jp(1)            = Total_member - 2
!jp(2)            = Total_member - 1

!DO j = 3, Total_member - 1
!    jp(j) = j - 2
!END DO

!DO j = 3, Total_member - 1
!! DO j = 1, Total_member - 1
!    IF(MOD(j, 2) == 0) THEN
!        rp(j) = -1.0
!    ELSE
!        rp(j) =  1.0
!    END IF
!END DO

! For testing.
!-------------
!rp(1) = 0.0
!rp(2) = 0.0

!Sto_Coef(k,j) is the weight of the k'th perturabtion in member j's formulation 
 DO j = 1, Total_member - 1     !for the forcing of each member j
     DO i = 1, TRIEO_LSTOT_SIZ4
!        w_step1(i, j) = rp(j) * (work(i, jp(j)) - work_ini(i, jp(j)) &
!                              -  work(i, Total_member) + work_ini(i, Total_member))
         w_step1(i, j) = 0.0
          DO k=1, Total_member - 1    !contribution from each perturbation k
           w_step1(i, j) = w_step1(i, j) &
              + Sto_Coef(k, j) * (work(i, k) - work_ini(i, k) &
                               -  work(i, Total_member) + work_ini(i, Total_member)) 
          END DO
     END DO
 END DO

! New feature in B series  DHOU 03-28-2007
! Centralize the perturbations w_step1
! Find the average of the perturbations
IF (CENTER .eq. 1) THEN
 DO i = 1, TRIEO_LSTOT_SIZ4
     w_mean(i) = 0.0
     DO j = 1, Total_member - 1     !for the forcing of each member j
       w_mean(i) = w_mean(i) + w_step1(i, j)
     END DO
       w_mean(i) = w_mean(i)/float(Total_member - 1)
 END DO
! subtract the average from the perturbations
 DO i = 1, TRIEO_LSTOT_SIZ4
     DO j = 1, Total_member - 1     !for the forcing of each member j
       w_step1(i,j) = w_step1(i,j) - w_mean(i)
     END DO
 END DO
ENDIF

!rp1 = 0.1   !(Al, constant amplitude)
! Am,  linear varaition of amplitude
!rp1 = 0.15*(-Cpl_Run_Calling_Number+63)/62.0
! An,linear varaition of amplitud starting at 120h (n=20), reduce from 0.1 to 0.05 at n=63 
!rp1=0.1
!if ( Cpl_Run_Calling_Number .ge. 20 ) then
!rp1 = (0.1-0.05)*(63-Cpl_Run_Calling_Number)/(63.0 - 20.0) + 0.05
!endif
! Ap,linear varaition of amplitud starting at 120h (n=20), reduce from 0.1 to 0.01 at n=63 
!rp1=0.1
!if ( Cpl_Run_Calling_Number .ge. 20 ) then
!rp1 = (0.1-0.01)*(63-Cpl_Run_Calling_Number)/(63.0 - 20.0) + 0.01
!endif
! Aq,  Same as Ap but rp1 is reduced to 50% 
!rs_global=0.1
!if ( Cpl_Run_Calling_Number .ge. 20 ) then
!rs_global = (0.1-0.01)*(63-Cpl_Run_Calling_Number)/(63.0 - 20.0) + 0.01
!endif
!
!!rs_global=rs_global*1.0    ! Ap, Ba, Bb
! rs_global=-1.0*rs_global    ! Bc
! DO j = 1, Total_member - 1
!     DO i = 1, TRIEO_LSTOT_SIZ4
!         work(i, j) = work(i, j) + rs_global * w_step1(i, j)   !upto Bc
!!        w_step1(i, j) = rs_global * w_step1(i, j)
!!        work(i, j) = work(i, j) + w_step1(i, j)
!     END DO
! END DO

 END SUBROUTINE Sto_Per_Scheme_Step1

 SUBROUTINE Sto_Per_Scheme_Step1_2(work, w_step1,                         &
     TRIEO_LSTOT_SIZ4, Total_member, Cpl_Run_Calling_Number, RS_GLOBAL)

! This subroutine is used to compute the first step of the 
! stochastic perturbation scheme, in which X_i_dot = T_i + S_i 
! and S_i ~ SUM(W_i,j P_j.
!-------------------------------------------------------------

! work_ini is the six hours ago spectral array.  work is the 
! current spectral array and the w_step1 is the step 1 output
! spectral array.
!------------------------------------------------------------


! In the current code, assume each ensemble member uses the same 
! number of the processors.
!---------------------------------------------------------------

 USE ESMF_Mod
 USE GEFS_Cpl_InternalState_ESMFMod

 INTEGER                                                             :: TRIEO_LSTOT_SIZ4
 INTEGER                                                             :: Total_member
 REAL(KIND=KIND_EVOD), DIMENSION(TRIEO_LSTOT_SIZ4, Total_member)     :: work 
 REAL(KIND=KIND_EVOD), DIMENSION(TRIEO_LSTOT_SIZ4, Total_member)     :: w_step1
 INTEGER                                                             :: Cpl_Run_Calling_Number

 REAL(KIND=KIND_EVOD)                                                :: RS_GLOBAL

!rp1 = 0.1   !(Al, constant amplitude)
! Am,  linear varaition of amplitude
!rp1 = 0.15*(-Cpl_Run_Calling_Number+63)/62.0
! An,linear varaition of amplitud starting at 120h (n=20), reduce from 0.1 to 0.05 at n=63 
!rp1=0.1
!if ( Cpl_Run_Calling_Number .ge. 20 ) then
!rp1 = (0.1-0.05)*(63-Cpl_Run_Calling_Number)/(63.0 - 20.0) + 0.05
!endif
! Ap,linear varaition of amplitud starting at 120h (n=20), reduce from 0.1 to 0.01 at n=63 
!rp1=0.1
!if ( Cpl_Run_Calling_Number .ge. 20 ) then
!rp1 = (0.1-0.01)*(63-Cpl_Run_Calling_Number)/(63.0 - 20.0) + 0.01
!endif
! Aq,  Same as Ap but rp1 is reduced to 50% 
!RS_GLOBAL=0.1
!if ( Cpl_Run_Calling_Number .ge. 20 ) then
!RS_GLOBAL = (0.1-0.01)*(63-Cpl_Run_Calling_Number)/(63.0 - 20.0) + 0.01
!endif
!!rp1=rp1*1.0    ! Ap, Ba, Bb
!S_GLOBAL=-1.0*RS_GLOBAL    ! Bc

 DO j = 1, Total_member - 1
     DO i = 1, TRIEO_LSTOT_SIZ4
         work(i, j) = work(i, j) + RS_GLOBAL * w_step1(i, j)   !upto Bc
!        w_step1(i, j) = RS_GLOBAL * w_step1(i, j)
!        work(i, j) = work(i, j) + w_step1(i, j)
     END DO
 END DO

 END SUBROUTINE Sto_Per_Scheme_Step1_2

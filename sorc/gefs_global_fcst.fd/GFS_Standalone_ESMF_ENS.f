!----------------------------------------------------------------------
!BOP
!
! !IROUTINE: GFS_StandAlone_ESMF --- Application main program to run the NCEP 
!                                    GFS system alone.
!
! !DESCRIPTION: This program uses the ESMF superstructure to run
!               the NCEP GFS modeling system.
!
! !REVISION HISTORY:
!
!  November 2004     W Yang              Initial code.
!  May      2005     W Yang              For the updated GFS version.
!  September 2005    W Yang              Concurrent version for the ensemble operational forecast.`
!  March     2006    S. Moorthi          New version of the model
!  February  2008    W. Yang             Modified to using the ESMF 3.1.0 library, change the multiple 
!                                        arrays of the ESMF states, ESMF clock, and ESMF time into the
!                                        single variables.
!
! !INTERFACE:
 PROGRAM GFS_StandAlone_ESMF

!
!USES:
!
! Use the ESMF library.
!----------------------
! Coupling insertion->
      USE ATM_cc, ONLY: MPI_COMM_Atmos
!<-Coupling insertion

 USE ESMF_Mod

! Use the GFS grid component module, 
! only the set service registration routine is public.
!-----------------------------------------------------
 USE GFS_GridComp_ESMFMod, ONLY: GFS_StandAlone_SetServices => SetServices
 USE GFS_ErrMsgMod

! This is some information required by the ESMF Log error utility.
!-----------------------------------------------------------------
#include "ESMF_LogMacros.inc"

 IMPLICIT none

!
!!ESMF DERIVED DATA TYPE ARRAYS:
!
 TYPE(ESMF_VM)                                       :: vm        ! the ESMF virtual machine, which contains and
                                                                  ! manages the computer CPU resource for the 
                                                                  ! ESMF grid components.
 TYPE(ESMF_GridComp), DIMENSION(:), ALLOCATABLE      :: gcGFS     ! the ESMF composite gridded 
                                                                  ! component.
  
 TYPE(ESMF_State)                                    :: impGFS    ! the ESMF interface import state.
 TYPE(ESMF_State)                                    :: expGFS    ! the ESMF interface export state.

 TYPE(ESMF_LOG)                                      :: logGFS    ! the ESMF Log Error object.
 TYPE(ESMF_Clock)                                    :: clock     ! the ESMF time management data 
                                                                  ! type clock.
 
! Define ESMF time manager variables.
!------------------------------------
 TYPE(ESMF_TimeInterval)                             :: timestep  ! the ESMF time step length
 TYPE(ESMF_Time)                                     :: startTime ! the ESMF starting time

 TYPE(ESMF_config)                                   :: Cf        ! ESMF config

 INTEGER              :: rc = ESMF_SUCCESS ! the running error signal.
 INTEGER              :: rcfinal           ! the final value of the running error signal.
 INTEGER              :: Total_member      ! the number of the ensemble run members.
 INTEGER              :: Member_Id
 INTEGER              :: i, tasks
 INTEGER, Allocatable :: pe_member(:)      ! number of pe per members

 CHARACTER(ESMF_MAXSTR)                            :: impStateName
 CHARACTER(ESMF_MAXSTR)                            :: expStateName
 CHARACTER(ESMF_MAXSTR), DIMENSION(:), ALLOCATABLE :: GridCompName

 character*20           :: PELAB

!
!!BEGIN GFS CODE.
!

! Initialize the final error signal.
!-----------------------------------
 rcfinal = ESMF_SUCCESS

!
!  Initialize the ESMF framework. 
!  ------------------------------

! Coupling insertion->
!        print*,'AM: to call MPI_INIT'
      call MPI_INIT(rc)
!        print*,'AM: back from MPI_INIT'
      call ATM_CMP_START
         print*,'AM: back from ATM_CMP_START',MPI_COMM_Atmos
!<-Coupling insertion

! Coupling deletion->
!CALL ESMF_Initialize(vm              = vm,                 &
!                     defaultCalendar = ESMF_CAL_GREGORIAN, & ! Set up the default calendar.
!                     defaultlogtype  = ESMF_LOG_MULTI,     & ! Define multiple log error output file,
!                                                             ! each task has its own log error output file.
!                     rc              = rc)
!<- Coupling deletion
! Coupling insertion->
 CALL ESMF_Initialize( &
                      defaultCalendar = ESMF_CAL_GREGORIAN,       & ! Set up the default calendar.
                      defaultlogtype  = ESMF_LOG_MULTI,           & ! Define multiple log error output file,
                                                              ! each task has its own log error output file.
                      mpiCommunicator = MPI_COMM_Atmos,           &
                      vm = vm,                                    &
                      rc              = rc)

      print*,'AM: back from ESMF_Initialize, rc=',rc
!<-Coupling insertion

!  Open the Log Error file and Set Log parameters.
!-------------------------------------------------

!Question here, if no such lines, no error mesage output to the default log?
!---------------------------------------------------------------------------

! When user does not want to use the ESMF default Log, use it to open a user defined Log.
!----------------------------------------------------------------------------------------
 CALL ESMF_LogOpen (logGFS, 'GFS_Error_Log.txt',            &
                    logtype = ESMF_LOG_MULTI, rc = rc)
! Coupling insertion->
!     print*,'AM: back from ESMF_LogOpen'
!<-Coupling insertion

! Set up the user defined Log.
!-----------------------------
 CALL ESMF_LogSet(log         = logGFS,             &
                  verbose     = ESMF_TRUE,          &
                  flush       = ESMF_TRUE,          &
                  rootOnly    = ESMF_FALSE,         &
                  halt        = ESMF_LOG_HALTERROR, & ! It means that the job will be stopped
                                                      ! when error happens.
                  maxElements = 10,                 & ! Maximum number of elements in the log
                                                      ! before printing them to the log file.
                  rc          = rc)
! Coupling insertion->
!     print*,'AM: back from ESMF_LogSet'
!<-Coupling insertion
!
!!CREATE THE ESMF GFS GRID COMPONENT, USING SAME LAYOUT AS APPLICATION:
!

! Print out information to the log file as a mark to show where the job running is now.
!--------------------------------------------------------------------------------------
 CALL ESMF_LogWrite("Create the GFS Grid Component",            &
                    ESMF_LOG_INFO, rc = rc)
! Coupling insertion->
!     print*,'AM: back from ESMF_LogWrite'
!<-Coupling insertion

!
! Load the config_file to the config

 Cf       = ESMF_ConfigCreate(rc = rc)
 CALL ESMF_ConfigLoadFile(Cf, 'gfs_namelist.rc', rc = rc)
! Coupling insertion->
!     print*,'AM: back from ESMF_ConfigLoadFile'
!<-Coupling insertion

! To get the total number of the ensemble members.
! If run stand alone, Total_member = 1.
!-------------------------------------------------
!CALL GetTotalMember(Total_member,Cf,rc)

 rc       = ESMF_SUCCESS
 CALL ESMF_ConfigGetAttribute(Cf, Total_member,                    &
                              label = 'TOTAL_MEMBER:', rc    = rc)
! Coupling insertion->
!     print*,'AM: back from ESMF_ConfigGetAttribute'
!<-Coupling insertion

     CALL ERR_MSG3(rc,'Get the Number of the Total Ensemble Members',rcfinal)

! Aloocate arrays.
!-----------------
 ALLOCATE(gcGFS       (Total_member))
 ALLOCATE(GridCompName(Total_member))
 ALLOCATE(pe_member   (Total_member))

!SET UP CHARACTERS PARAMETERS:
!-----------------------------

! Coupling insertion->
!     call MPI_COMM_SIZE(MPI_COMM_Atmos,tasks,rc)
!<-Coupling insertion
!<-Coupling replacement  CALL ESMF_VMGet(vm, peCount = tasks, rc = rc)
  CALL ESMF_VMGet(vm, peCount = tasks, rc = rc)

 rc       = ESMF_SUCCESS
 DO i = 1, Total_member
 
!                    Set State Names
 
   WRITE(GridCompName(i), '("GFS_STAND_ALONE GRID COMPONENT",I2.2)') i

!                    Get pe_members

   write(PELAB,'("PE_MEMBER",I2.2,":")') i
   CALL ESMF_ConfigGetAttribute(Cf, pe_member(i), label = PELAB, rc = rc)
! Coupling insertion->
!     print*,'AM: back from ESMF_ConfigGetAttribute(Cf, pe_member(i)...'
!<-Coupling insertion
   if (pe_member(i) == 0) pe_member(i) = tasks / Total_Member
 ENDDO

 WRITE(impStateName, '("GFS Import State")')
 WRITE(expStateName, '("GFS Export State")')

!!CREATE THE ESMF GFS GRID COMPONENT, USING SAME LAYOUT AS APPLICATION:
!---------------------------------------------------------------------
 CALL ESMF_LogWrite("Create the GFS Grid Component", &
                    ESMF_LOG_INFO, rc = rc)
! Coupling insertion->
!     print*,'AM: to call GridCompCreate'
!<-Coupling insertion

 CALL GridCompCreate(vm, gcGFS, GridCompName, Cf,                 &
                         Total_Member, pe_member, Member_Id, rc)
! Coupling insertion->
!     print*,'AM: back from GridCompCreate'
!<-Coupling insertion

     CALL ERR_MSG3(rc,'Create Grid Component',rcfinal)

!REGISTER THE GFS GRID COMPONENT:
!--------------------------------
 CALL ESMF_LogWrite("Run Set Services of the GFS Grid Component", &
                    ESMF_LOG_INFO, rc = rc)
! Coupling insertion->
!     print*,'AM: back from ESMF_LogWrite("Run Set Services...'
!<-Coupling insertion

 CALL GFS_SetServices(gcGFS, Total_Member, rc)
! Coupling insertion->
!     print*,'AM: back from GFS_SetServices'
!<-Coupling insertion

     CALL ERR_MSG3(rc,'Grid Component Set Services',rcfinal)

!
!!CREATE THE IMPORT AND EXPORT ESMF STATES:
!
 CALL ESMF_LogWrite("Create the Import ESMF State",               &
                     ESMF_LOG_INFO, rc = rc)
! Coupling insertion->
!     print*,'AM: back from ESMF_LogWrite("Create the Import...'
!<-Coupling insertion
 impGFS = ESMF_StateCreate (statename = impStateName,             &
                            statetype = ESMF_STATE_IMPORT,        &
                            rc        = rc)

     CALL ERR_MSG3(rc,'Create the Import ESMF State',rcfinal)

 CALL ESMF_LogWrite("Create the Export ESMF State",               &
                    ESMF_LOG_INFO, rc = rc)
 expGFS = ESMF_StateCreate (statename = expStateName,             &
                            statetype = ESMF_STATE_EXPORT,        &
                            rc        = rc)

     CALL ERR_MSG3(rc,'Create the Export ESMF State',rcfinal)

! Set up the ESMF clock.  At this step just use an arbitrary
! ESMF time and an arbitrary time step to set up it.
!-----------------------------------------------------------
 CALL ESMF_TImeSet(startTime, yy = 2007, rc = rc)

 CALL ERR_MSG3(rc,'Set up the ESMF startTime', rcfinal)

 CALL ESMF_TimeIntervalSet(timeStep, m = 10, rc = rc)

 CALL ERR_MSG3(rc,'Set up the ESMF timeStep', rcfinal)

 clock = ESMF_ClockCreate(name      = 'GFS_RUN_CLOCK', &
                          timeStep  = timeStep,        &
                          startTime = startTime,       &
                          rc        = rc)

 CALL ERR_MSG3(rc,'Create up the ESMF clock', rcfinal)

!

!
!!INITIALIZE THE GFS GRID COMPONENT:
!
 CALL ESMF_LogWrite("Calling the GFS Initialize - GFS_Standalone.F90",   &
                    ESMF_LOG_INFO, rc = rc)
! Coupling insertion->
!     print*,'AM: to call GFS_Initialize'
!<-Coupling insertion
 CALL GFS_Initialize (gcGFS, impGFS, expGFS, clock, Total_member, Member_Id, rc)
! Coupling insertion->
!     print*,'AM: back from GFS_Initialize'
!<-Coupling insertion

     CALL ERR_MSG3(rc,'Run the GFS Initialize',rcfinal)

 CALL ESMF_ConfigDestroy(Cf, rc = rc)     ! Destroy the parental config?
! Coupling insertion->
!     print*,'AM: back from ESMF_ConfigDestroy'
!<-Coupling insertion
!
!!RUNNING THE GFS GRID COMPONENT:
!
 CALL ESMF_LogWrite("Calling the GFS Run - GFS_Standalone.F90",         &
                    ESMF_LOG_INFO, rc = rc)
! Coupling insertion->
!     print*,'AM: to call GFS_Run'
!<-Coupling insertion
 CALL GFS_Run      (gcGFS, impGFS, expGFS, clock, Total_member, Member_Id, rc)
! Coupling insertion->
!     print*,'AM: back from GFS_Run'
!<-Coupling insertion

     CALL ERR_MSG3(rc,'Run the GFS RUN',rcfinal)

!
!!AFTER RUNNING, FINALIZE THE GFS GRID COMPONENT:
!
 CALL ESMF_LogWrite("Calling the GFS Finalize - in GFS_Standalone.F90",  &
                    ESMF_LOG_INFO, rc = rc)
 CALL GFS_Finalize (gcGFS, impGFS, expGFS, clock, Total_member, Member_Id, rc)

     CALL ERR_MSG3(rc,'Run the GFS Finalize',rcfinal)

!  Finalize the ESMF System.
!---------------------------
 CALL ESMF_Finalize()

 IF(rcfinal == ESMF_SUCCESS) THEN
     PRINT*, "PASS: GFS_Standalone.F90"
 ELSE
     PRINT*, "FAIL: GFS_Standalone.F90"
 END IF
!
!!END THE PROGRAM:
!
 END PROGRAM GFS_StandAlone_ESMF

!
!EOP
!-------------------------------------------------------------------------

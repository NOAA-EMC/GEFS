 SUBROUTINE GetTotalMember_EnsembleRunTime(Total_member, &
                                           Ens_sps,      &
                                           pe_member,    &
                                           hh_increase,  &
                                           hh_start,     &
                                           hh_final,     &
                                           rc)

 USE ESMF_Mod

 IMPLICIT none

 INTEGER,                        INTENT(out) :: Total_member
 INTEGER, DIMENSION(50),         INTENT(out) :: pe_member
 INTEGER,                        INTENT(out) :: hh_increase
 INTEGER,                        INTENT(out) :: hh_start
 INTEGER,                        INTENT(out) :: hh_final
 INTEGER,                        INTENT(out) :: rc
 LOGICAL,                        INTENT(out) :: Ens_sps
 TYPE(ESMF_VM)                               :: vm
 TYPE(ESMF_Config)                           :: Cf
 CHARACTER(ESMF_MAXSTR)                      :: Cf_fname
 CHARACTER(12)                               :: PELAB
 INTEGER                                     :: tasks
 INTEGER                                     :: i

 CALL ESMF_VMGetGLobal(vm, rc = rc)
 CALL ESMF_VMGet(vm, petCount = tasks, rc = rc)

 rc       = ESMF_SUCCESS
 Cf       = ESMF_ConfigCreate(rc = rc)
 Cf_fname = 'gfs_namelist.rc'

 CALL ESMF_ConfigLoadFile(Cf, Cf_fname, rc = rc)

 CALL ESMF_ConfigGetAttribute(Cf,                      &
                              Total_member,            &
                              label = 'TOTAL_MEMBER:', &
                              rc    = rc)
 CALL ESMF_ConfigGetAttribute(Cf,                      &
                              Ens_sps,                 &
                              label = 'ENS_SPS:',      &
                              rc    = rc)

 pe_member = 0
 DO i = 1, Total_member
   WRITE(PELAB,'("PE_MEMBER",I2.2,":")') i
   CALL ESMF_ConfigGetAttribute(Cf, pe_member(i), label = PELAB, rc = rc)
   if (pe_member(i) == 0) pe_member(i) = tasks / Total_Member
 ENDDO

 CALL ESMF_ConfigGetAttribute(Cf,                      &
                              hh_increase,             &
                              label = 'HH_INCREASE:',  &
                              rc    = rc)

 CALL ESMF_ConfigGetAttribute(Cf,                      &
                              hh_start,                &
                              label = 'HH_START:',     &
                              rc    = rc)

 CALL ESMF_ConfigGetAttribute(Cf,                      &
                              hh_final,                &
                              label = 'HH_FINAL:',     &
                              rc    = rc)

 CALL ESMF_ConfigDestroy(Cf, rc = rc)

 END SUBROUTINE GetTotalMember_EnsembleRunTime





 SUBROUTINE SetGcStateNames(GridCompName, &
                            CplCompName,  &
                            impStateName, &
                            expStateName, &
                            impGEFSName,  &
                            expGEFSName,  &
                            Total_member)

 USE ESMF_Mod

 IMPLICIT none

 INTEGER,                                         INTENT(in)  :: Total_member
 CHARACTER(ESMF_MAXSTR), DIMENSION(Total_member), INTENT(out) :: GridCompName
 CHARACTER(ESMF_MAXSTR),                          INTENT(out) :: CplCompName
 CHARACTER(ESMF_MAXSTR),                          INTENT(out) :: impStateName
 CHARACTER(ESMF_MAXSTR),                          INTENT(out) :: expStateName
 CHARACTER(ESMF_MAXSTR),                          INTENT(out) :: impGEFSName
 CHARACTER(ESMF_MAXSTR),                          INTENT(out) :: expGEFSName

 INTEGER :: i

 WRITE(CplCompName,  1000)
 WRITE(impStateName, 2000)
 WRITE(expStateName, 3000)
 WRITE(impGEFSName,  4000)
 WRITE(expGEFSName,  5000)

 DO i = 1, Total_member
     WRITE(GridCompName(i), 6000) i
 END DO

1000 FORMAT('GFS Coupler Grid Component Name')
2000 FORMAT('GFS Import State')
3000 FORMAT('GFS Export State')
4000 FORMAT('GEFS Import State')
5000 FORMAT('GEFS Export State')
6000 FORMAT('GFS GRID COMPONENT', I2.2)

 END SUBROUTINE SetGcStateNames




 SUBROUTINE GridCompCreate(vm, gcGFS, GridCompName, Cf,             &
                           Total_Member, pe_member, Member_Id,rc)

 USE ESMF_Mod

 IMPLICIT none

 INTEGER, intent(in) :: Total_Member, pe_member(Total_member)
!
 TYPE(ESMF_VM),                                   INTENT(inout) :: vm    ! the ESMF virtual machine.
 TYPE(ESMF_config),                               INTENT(inout) :: Cf    ! ESMF config
 TYPE(ESMF_GridComp),    DIMENSION(Total_member), INTENT(out)   :: gcGFS
 CHARACTER(ESMF_MAXSTR), DIMENSION(Total_member), INTENT(in)    :: GridCompName
 INTEGER,                                         INTENT(out)   :: rc, Member_Id

 INTEGER, POINTER :: petlist(:, :)
 INTEGER          :: npe, me, i, pe_max

 rc = ESMF_SUCCESS

!  Get the number of provided tasks.
!-----------------------------------
 CALL ESMF_VMGet(vm, petCount = npe, localPet = me, rc = rc)

!
 pe_max = 1
 do i=1,Total_member
  pe_max = max(pe_max,pe_member(i))
 enddo

!  Set up the Pet List.
!----------------------
ALLOCATE(petlist(pe_max, Total_member))

! Create the the pet list and the local communicator for each ensemble member.
!-----------------------------------------------------------------------------
CALL SetUp_Member_Communicator(petlist, Total_Member, pe_member,  &
			       pe_max, Member_Id, me)

DO i = 1, Total_member
!gcGFS(i) = ESMF_GridCompCreate (vm,                                     &
gcGFS(i) = ESMF_GridCompCreate (                                         &
                             name         = GridCompName(i),             &
			     gridcomptype = ESMF_ATM,                    &
			     petList      = petlist(1:pe_member(i), i),  &
                             config       = Cf,                          &
			     rc           = rc)
END DO

END SUBROUTINE GridCompCreate



SUBROUTINE GFS_SetServices(gcGFS, Total_Member, rc)

USE ESMF_Mod
USE GFS_GridComp_ESMFMod, ONLY: GFS_StandAlone_SetServices => SetServices

IMPLICIT none

INTEGER, INTENT(in)                                         :: Total_member
TYPE(ESMF_GridComp), DIMENSION(Total_member), INTENT(inout) :: gcGFS
INTEGER,                                      INTENT(out)   :: rc
INTEGER :: i

rc   = ESMF_SUCCESS

DO i = 1, Total_member
 CALL ESMF_GridCompSetServices(gcGFS(i),  GFS_StandAlone_SetServices, rc)
END DO

END SUBROUTINE GFS_SetServices





SUBROUTINE GFS_Initialize(gcGFS, impGFS, expGFS, clock,        &
                          Total_Member, Member_Id, rc)

USE ESMF_Mod
USE GFS_GridComp_ESMFMod, ONLY: GFS_StandAlone_SetServices => SetServices

IMPLICIT none

INTEGER, intent(in) :: Total_Member, Member_Id
TYPE(ESMF_GridComp), DIMENSION(Total_member), INTENT(inout) :: gcGFS
TYPE(ESMF_State),                             INTENT(inout) :: impGFS   
TYPE(ESMF_State),                             INTENT(inout) :: expGFS  
TYPE(ESMF_Clock),                             INTENT(inout) :: clock  
INTEGER,                                      INTENT(out)   :: rc
INTEGER                                                     :: i

rc   = ESMF_SUCCESS
DO i = 1, Total_member
 IF(Member_Id == i) THEN
   CALL ESMF_GridCompInitialize (gcGFS(i),                       &
	 	  	         importstate = impGFS,           &
			         exportstate = expGFS,           &
			         clock       = clock,            &
			         phase       = ESMF_SINGLEPHASE, &
			         rc          = rc)
 END IF
END DO

END SUBROUTINE GFS_Initialize





SUBROUTINE GFS_Run(gcGFS, impGFS, expGFS, clock,         &
   	           Total_Member, Member_Id, rc)

USE ESMF_Mod
USE GFS_GridComp_ESMFMod, ONLY: GFS_StandAlone_SetServices => SetServices

IMPLICIT none

INTEGER, intent(in) :: Total_Member, Member_Id
TYPE(ESMF_GridComp), DIMENSION(Total_member), INTENT(inout) :: gcGFS
TYPE(ESMF_State),                             INTENT(inout) :: impGFS
TYPE(ESMF_State),                             INTENT(inout) :: expGFS
TYPE(ESMF_Clock),                             INTENT(inout) :: clock
INTEGER,                                      INTENT(out)   :: rc
INTEGER                                                     :: i

rc   = ESMF_SUCCESS
DO i = 1, Total_member
  IF(Member_Id == i) THEN
   CALL ESMF_GridCompRun (gcGFS(i),                       &
	  		  importstate = impGFS,           &
			  exportstate = expGFS,           &
			  clock       = clock,            &
			  phase       = ESMF_SINGLEPHASE, &
			  rc          = rc)
  END IF
END DO

END SUBROUTINE GFS_Run





SUBROUTINE GFS_Finalize(gcGFS, impGFS, expGFS, clock,         &
		 Total_Member, Member_Id, rc)

USE ESMF_Mod
USE GFS_GridComp_ESMFMod, ONLY: GFS_StandAlone_SetServices => SetServices

IMPLICIT none

INTEGER, intent(in) :: Total_Member, Member_Id
TYPE(ESMF_GridComp), DIMENSION(Total_member), INTENT(inout) :: gcGFS
TYPE(ESMF_State),                             INTENT(inout) :: impGFS
TYPE(ESMF_State),                             INTENT(inout) :: expGFS
TYPE(ESMF_Clock),                             INTENT(inout) :: clock
INTEGER,                                      INTENT(out)   :: rc
INTEGER                                                     :: i

rc   = ESMF_SUCCESS
DO i = 1, Total_member
 IF(Member_Id == i) THEN
   CALL ESMF_GridCompFinalize (gcGFS(i),                       &
			       importstate = impGFS,           &
			       exportstate = expGFS,           &
			       clock       = clock,            &
			       phase       = ESMF_SINGLEPHASE, &
			       rc          = rc)
 END IF
END DO

END SUBROUTINE GFS_Finalize





SUBROUTINE SetUp_Member_Communicator(petlist, Total_Member, pe_member,  &
			             pe_max,  Member_Id, me)

!
IMPLICIT none

 INTEGER, intent(in) :: Total_Member, pe_max, pe_member(Total_Member)
 INTEGER, DIMENSION(pe_max, Total_member), intent(out) :: petlist 
 INTEGER, intent(out)                                  :: Member_Id

 INTEGER :: me
 INTEGER :: i, j, i1

 i1 = 0
 DO j = 1, Total_member
     DO i = 1, pe_member(j)
         petlist(i, j) = i1
         IF(me == i1) THEN
             Member_Id = j
         END IF
         i1 = i1+1
     END DO
 END DO

 END SUBROUTINE SetUp_Member_Communicator

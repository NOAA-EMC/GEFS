 SUBROUTINE GFS_InputFile2ImportState(gcGFS, impGFS, Int_State, grid1, grid2, grid3, grid4, rc)

! This subroutine reads the sigma file and surface file and put them 
! into the GFS ESMF Import state, to test the GFS ESMF code.

!
!!USES:
!

! Coupling insertion->
   USE ATM_cc, ONLY: MPI_COMM_Atmos
!     All occurences of MPI_COMM_WORLD in this unit are replaced
!     by MPI_COMM_Atmos to enable coupling
!<-Coupling insertion

 USE ESMF_Mod                 ! The ESMF library.
 USE GFS_InternalState_ESMFMod
 USE GFS_ESMFStateAddGetMod
 USE mpi_def

 USE sfcio_module

 IMPLICIT none

!
! !INPUT/OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------------

 TYPE(ESMF_GridComp),              INTENT(inout) :: gcGFS
 TYPE(ESMF_State),                 INTENT(inout) :: impGFS
 TYPE(GFS_InternalState),          INTENT(inout) :: Int_State
 INTEGER                                         :: rc         ! return code

 INTEGER                                         :: lnt2_s, lonr_s, latr_s

! Working variables and arrays.
!------------------------------
 TYPE(ESMF_Grid)                   :: grid1        ! the ESMF GRID TYPE ARRAY.
 TYPE(ESMF_Grid)                   :: grid2        ! the ESMF GRID TYPE ARRAY.
 TYPE(ESMF_Grid)                   :: grid3        ! the ESMF GRID TYPE ARRAY.
 TYPE(ESMF_Grid)                   :: grid4        ! the ESMF GRID TYPE ARRAY.

 TYPE(ESMF_VM)         :: vm
 TYPE(sfcio_head)      :: head
 TYPE(sfcio_data)      :: data
 INTEGER               :: rc1
 INTEGER               :: rcfinal
 INTEGER               :: nosig, nosfc, i, j, ii1(2)
 INTEGER               :: i1(2), idate2(4), idate1(5)

 REAL(KIND = kind_io4) :: bfhour
 REAL(KIND = kind_io4) :: buf(Int_State%lnt2)
 REAL(KIND = kind_io4) :: trisca_1(Int_State%lnt2, Int_State%levs)
 REAL(KIND = kind_io4) :: trisca_2(Int_State%lnt2, Int_State%levs)

 CHARACTER(5),          DIMENSION(:),    POINTER :: SMC_name, STC_name, SLC_name
 REAL(KIND = kind_io4), DIMENSION(:, :), POINTER :: sfcbuf1

 INTEGER, DIMENSION(Int_State%nodes, 2)          :: ijn_s, displs_s

 CALL ESMF_GridCompGet(gcGFS, vm = vm, rc = rc1)

 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS
 me      = Int_State%me

 lnt2_s = Int_State%lnt2_s
 lonr_s = Int_State%lonr_s
 latr_s = Int_State%latr_s
 i1     = Int_State%grid4_i1

 displs_s(1, :) = 0
 DO i = 1, Int_State%nodes
     IF(me == i-1) THEN
         ii1(1) = lnt2_s
         ii1(2) = lonr_s
     END IF
     CALL ESMF_VMBroadcast(vm, ii1, 2, i-1, blockingflag = ESMF_BLOCKING, rc = rc1)
     ijn_s(i, 1) = ii1(1)
     ijn_s(i, 2) = ii1(2)

     IF(ESMF_LogMsgFoundError(rc1, "VMBroadcast ijn_s.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When VMBroadcast ijn_s, i, rc = ', i, rc1
         rc1 = ESMF_SUCCESS
     END IF

     IF(i /= 1) THEN
         displs_s(i, 1) = displs_s(i-1, 1) + ijn_s(i-1, 1)
         displs_s(i, 2) = displs_s(i-1, 2) + ijn_s(i-1, 2)
     END IF
 END DO

 nosig = 61
 IF(me == 0) THEN 
     OPEN(nosig, FILE = 'siganl.2003061500', FORM = 'unformatted')
     REWIND nosig
     READ(nosig)
     READ(nosig) bfhour, idate2
     idate1(1)       = NINT(bfhour)
     DO i = 1, 4
         idate1(i+1) = idate2(i)
     END DO
 END IF
 
! idate1_im and idate1_ex:  (1) --- bfhour (integer), (2) - (5) --- idate.
!-------------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%idate1_import) THEN
     ALLOCATE(Int_State%idate1_ex(i1(1), i1(2)), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - idate1_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - idate1_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     CALL ESMF_VMBroadcast(vm, idate1, 5, 0, blockingflag = ESMF_BLOCKING, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "VMBroadcast idate1_ex.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When VMBroadcast idate1_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     Int_State%idate1_ex(1, :) = idate1
     PRINT*, 'in Int to impGFS, idate1_ex = ', Int_State%idate1_ex
     CALL AddF90ArrayToState(impGFS, grid4, 'DATE', &
         Int_State%idate1_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - DATE_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - DATE_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 END IF

 IF(me == 0) THEN 
     READ(nosig) buf
 END IF
 IF(Int_State%ESMF_Sta_List%z_import) THEN
     ALLOCATE(Int_State%z_ex(lnt2_s, 1), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - z_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - z_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     CALL mpi_scatterv(buf, ijn_s, displs_s,             &
          MPI_R_IO, Int_State%z_ex, lnt2_s, MPI_R_IO, 0, &
          MPI_COMM_Atmos, rc1)

     IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - z_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When MPI_Scatterving - z_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     CALL AddF90ArrayToState(impGFS, grid1, 'HS', &
          Int_State%z_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - HS_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - HS_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 END IF

 IF(me == 0) THEN 
     READ(nosig) buf
 END IF
 IF(Int_State%ESMF_Sta_List%ps_import) THEN
     ALLOCATE(Int_State%ps_ex(lnt2_s, 1), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - ps_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - ps_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     CALL mpi_scatterv(buf, ijn_s, displs_s,              &
          MPI_R_IO, Int_State%ps_ex, lnt2_s, MPI_R_IO, 0, &
          MPI_COMM_Atmos, rc1)

     IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - ps_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When MPI_Scatterving - ps_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     CALL AddF90ArrayToState(impGFS, grid1, 'PS', &
         Int_State%ps_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - PS_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - PS_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 END IF

 DO i = 1, Int_State%levs
     IF(me == 0) THEN 
         READ(nosig) buf
         trisca_1(:, i) = buf
     END IF
 END DO
 IF(Int_State%ESMF_Sta_List%temp_import) THEN
     ALLOCATE(Int_State%temp_ex(lnt2_s, Int_State%levs), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - temp_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - temp_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, Int_State%levs
         CALL mpi_scatterv(trisca_1(1, i), ijn_s, displs_s,              &
              MPI_R_IO, Int_State%temp_ex(1, i), lnt2_s, MPI_R_IO, 0,    &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - temp_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - temp_ex, rc = ', rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid2, 'T', &
         Int_State%temp_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - T_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - T_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 END IF

 DO i = 1, Int_State%levs
     IF(me == 0) THEN 
         READ(nosig) buf
         trisca_1(:, i) = buf
         READ(nosig) buf
         trisca_2(:, i) = buf
     END IF
 END DO
 IF(Int_State%ESMF_Sta_List%div_import) THEN
     ALLOCATE(Int_State%div_ex(lnt2_s, Int_State%levs), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - div_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - div_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, Int_State%levs
         CALL mpi_scatterv(trisca_1(1, i), ijn_s, displs_s,              &
              MPI_R_IO, Int_State%div_ex(1, i), lnt2_s, MPI_R_IO, 0,     &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - div_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - div_ex, rc = ', rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid2, 'D', &
         Int_State%div_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - D_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - D_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 END IF

 IF(Int_State%ESMF_Sta_List%vor_import) THEN
     ALLOCATE(Int_State%vor_ex(lnt2_s, Int_State%levs), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - vor_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - vor_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, Int_State%levs
         CALL mpi_scatterv(trisca_2(1, i), ijn_s, displs_s,              &
              MPI_R_IO, Int_State%vor_ex(1, i), lnt2_s, MPI_R_IO, 0,     &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - vor_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - vor_ex, rc = ', rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid2, 'Z', &
         Int_State%vor_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - Z_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - Z_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 END IF

 DO i = 1, Int_State%levs
     IF(me == 0) THEN 
         READ(nosig) buf
         trisca_1(:, i) = buf
     END IF
 END DO
 IF(Int_State%ESMF_Sta_List%q_import) THEN
     ALLOCATE(Int_State%q_ex(lnt2_s, Int_State%levs), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - q_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - q_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, Int_State%levs
         CALL mpi_scatterv(trisca_1(1, i), ijn_s, displs_s,              &
              MPI_R_IO, Int_State%q_ex(1, i), lnt2_s, MPI_R_IO, 0,       &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - q_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - q_ex, rc = ', rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO
     CALL AddF90ArrayToState(impGFS, grid2, 'SHUM', &
         Int_State%q_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - SHUM_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - SHUM_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 END IF

 DO i = 1, Int_State%levs
     IF(me == 0) THEN 
         READ(nosig) buf
         trisca_1(:, i) = buf
     END IF
 END DO
 IF(Int_State%ESMF_Sta_List%oz_import) THEN
     ALLOCATE(Int_State%oz_ex(lnt2_s, Int_State%levs), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - oz_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - oz_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, Int_State%levs
         CALL mpi_scatterv(trisca_1(1, i), ijn_s, displs_s,              &
              MPI_R_IO, Int_State%oz_ex(1, i), lnt2_s, MPI_R_IO, 0,      &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - oz_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - oz_ex, rc = ', rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid2, 'SOZ', &
         Int_State%oz_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - SOZ_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - SOZ_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 END IF

 IF(me == 0) THEN 
     DO i = 1, Int_State%levs
         READ(nosig) buf
         trisca_1(:, i) = buf
     END DO
     CLOSE (nosig)
 END IF

 IF(Int_State%ESMF_Sta_List%scld_import) THEN
     ALLOCATE(Int_State%scld_ex(lnt2_s, Int_State%levs), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - scld_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - scld_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, Int_State%levs
         CALL mpi_scatterv(trisca_1(1, i), ijn_s, displs_s,              &
              MPI_R_IO, Int_State%scld_ex(1, i), lnt2_s, MPI_R_IO, 0,    &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - scld_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - scld_ex, rc = ', rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid2, 'SCLD', &
         Int_State%scld_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - SCLD_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - SCLD_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 END IF

 nosfc = 61
 IF(me == 0) THEN 
     OPEN(nosfc, FILE = 'sfcanl.2003061500', FORM = 'unformatted')

     CALL sfcio_srhead(nosfc, head, rc1)

     IF(ESMF_LogMsgFoundError(rc1, " CALL sfcio_srhead")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Calling sfcio_srhead, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     CALL sfcio_aldata(head, data, rc1)

     IF(ESMF_LogMsgFoundError(rc1, " CALL sfcio_aldata")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Calling sfcio_aldata, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     CALL sfcio_srdata(nosfc, head, data, rc1)

     IF(ESMF_LogMsgFoundError(rc1, " CALL sfcio_srdata")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Calling sfcio_srdata, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     CLOSE(nosfc)
 END IF

 IF(Int_State%ESMF_Sta_List%sea_level_ice_mask_import) THEN
     ALLOCATE(Int_State%sea_level_ice_mask_ex(lonr_s, latr_s), stat = rc1)
     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - sea_level_ice_mask_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - sea_level_ice_mask_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%slmsk(1, i), ijn_s(1, 2), displs_s(1, 2),           &
              MPI_R_IO, Int_State%sea_level_ice_mask_ex(1, i), lonr_s, MPI_R_IO, 0, &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - sea_level_ice_mask_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - sea_level_ice_mask_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'SLMSK', &
         Int_State%sea_level_ice_mask_ex, rc = rc1)
     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - SLMSK_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - SLMSK_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%orography_import) THEN
     ALLOCATE(Int_State%orography_ex(lonr_s, latr_s), stat = rc1)
     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - orography_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - orography_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%orog(1, i), ijn_s(1, 2), displs_s(1, 2),              &
              MPI_R_IO, Int_State%orography_ex(1, i), lonr_s, MPI_R_IO, 0,            &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - orography_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - orography_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'OROG', Int_State%orography_ex, rc = rc1)
     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - OROG_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - OROG_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%t_skin_import) THEN
     ALLOCATE(Int_State%t_skin_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - t_skin_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - t_skin_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%tsea(1, i), ijn_s(1, 2), displs_s(1, 2),       &
              MPI_R_IO, Int_State%t_skin_ex(1, i), lonr_s, MPI_R_IO, 0,        &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - t_skin_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - t_skin_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'TSEA', &
         Int_State%t_skin_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - TSEA_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - TSEA_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%snow_depth_import) THEN
     ALLOCATE(Int_State%snow_depth_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - snow_depth_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - snow_depth_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%sheleg(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%snow_depth_ex(1, i), lonr_s, MPI_R_IO, 0,           &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - snow_depth_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - snow_depth_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'SHELEG', &
         Int_State%snow_depth_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - SHELEG_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - SHELEG_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%deep_soil_t_import) THEN
     ALLOCATE(Int_State%deep_soil_t_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - deep_soil_t_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - deep_soil_t_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%tg3(1, i), ijn_s(1, 2), displs_s(1, 2),               &
              MPI_R_IO, Int_State%deep_soil_t_ex(1, i), lonr_s, MPI_R_IO, 0,          &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - deep_soil_t_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - deep_soil_t_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'TG3', &
         Int_State%deep_soil_t_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - TG3_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - TG3_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%roughness_import) THEN
     ALLOCATE(Int_State%roughness_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - roughness_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - roughness_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%zorl(1, i), ijn_s(1, 2), displs_s(1, 2),              &
              MPI_R_IO, Int_State%roughness_ex(1, i), lonr_s, MPI_R_IO, 0,            &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - roughness_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - roughness_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'ZORL', &
         Int_State%roughness_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - ZORL_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - ZORL_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%albedo_visible_scattered_import) THEN
     ALLOCATE(Int_State%albedo_visible_scattered_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - albedo_visible_scattered_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - albedo_visible_scattered_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%alvsf(1, i), ijn_s(1, 2), displs_s(1, 2),                 &
              MPI_R_IO, Int_State%albedo_visible_scattered_ex(1, i), lonr_s, MPI_R_IO, 0, &
              MPI_COMM_Atmos, rc1)
    
         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - albedo_visible_scattered_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - albedo_visible_scattered_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'ALVSF', &
         Int_State%albedo_visible_scattered_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - ALVSF_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - ALVSF_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%albedo_visible_beam_import) THEN
     ALLOCATE(Int_State%albedo_visible_beam_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - albedo_visible_beam_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - albedo_visible_beam_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%alvwf(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%albedo_visible_beam_ex(1, i), lonr_s, MPI_R_IO, 0,  &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - albedo_visible_beam_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - albedo_visible_beam_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'ALVWF', &
         Int_State%albedo_visible_beam_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - ALVWF_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - ALVWF_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%albedo_nearIR_scattered_import) THEN
     ALLOCATE(Int_State%albedo_nearIR_scattered_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - albedo_nearIR_scattered_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - albedo_nearIR_scattered_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%alnsf(1, i), ijn_s(1, 2), displs_s(1, 2),                &
              MPI_R_IO, Int_State%albedo_nearIR_scattered_ex(1, i), lonr_s, MPI_R_IO, 0, &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - albedo_nearIR_scattered_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - albedo_nearIR_scattered_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'ALNSF', &
         Int_State%albedo_nearIR_scattered_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - ALNSF_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - ALNSF_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%albedo_nearIR_beam_import) THEN
     ALLOCATE(Int_State%albedo_nearIR_beam_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - albedo_nearIR_beam_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - albedo_nearIR_beam_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%alnwf(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%albedo_nearIR_beam_ex(1, i), lonr_s, MPI_R_IO, 0,   &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - albedo_nearIR_beam_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - albedo_nearIR_beam_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'ALNWF', &
         Int_State%albedo_nearIR_beam_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - ALNWF_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - ALNWF_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%vegetation_cover_import) THEN
     ALLOCATE(Int_State%vegetation_cover_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - vegetation_cover_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - vegetation_cover_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%vfrac(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%vegetation_cover_ex(1, i), lonr_s, MPI_R_IO, 0,     &
              MPI_COMM_Atmos, rc1)
    
         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - vegetation_cover_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - vegetation_cover_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'VFRAC', Int_State%vegetation_cover_ex, rc = rc1)
     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - VFRAC_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - VFRAC_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%canopy_water_import) THEN
     ALLOCATE(Int_State%canopy_water_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - canopy_water_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - canopy_water_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%canopy(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%canopy_water_ex(1, i), lonr_s, MPI_R_IO, 0,         &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - canopy_water_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - canopy_water_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'CANOPY', &
         Int_State%canopy_water_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - CANOPY_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - CANOPY_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%m10_wind_fraction_import) THEN
     ALLOCATE(Int_State%m10_wind_fraction_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - m10_wind_fraction_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - m10_wind_fraction_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%f10m(1, i), ijn_s(1, 2), displs_s(1, 2),              &
              MPI_R_IO, Int_State%m10_wind_fraction_ex(1, i), lonr_s, MPI_R_IO, 0,    &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - m10_wind_fraction_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - m10_wind_fraction_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'F10M', &
         Int_State%m10_wind_fraction_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - F10M_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - F10M_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%vegetation_type_import) THEN
     ALLOCATE(Int_State%vegetation_type_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - vegetation_type_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - vegetation_type_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%vtype(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%vegetation_type_ex(1, i), lonr_s, MPI_R_IO, 0,      &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - vegetation_type_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - vegetation_type_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'VTYPE', &
         Int_State%vegetation_type_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - VTYPE_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - VTYPE_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%soil_type_import) THEN
     ALLOCATE(Int_State%soil_type_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - soil_type_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - soil_type_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%stype(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%soil_type_ex(1, i), lonr_s, MPI_R_IO, 0,            &
              MPI_COMM_Atmos, rc1)
    
         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - soil_type_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - soil_type_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'STYPE', &
         Int_State%soil_type_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - STYPE_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - STYPE_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%zeneith_angle_facsf_import) THEN
     ALLOCATE(Int_State%zeneith_angle_facsf_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - zeneith_angle_facsf_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - zeneith_angle_facsf_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%facsf(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%zeneith_angle_facsf_ex(1, i), lonr_s, MPI_R_IO, 0,  &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - zeneith_angle_facsf_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - zeneith_angle_facsf_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'FACSF', &
         Int_State%zeneith_angle_facsf_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - FACSF_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - FACSF_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%zeneith_angle_facwf_import) THEN
     ALLOCATE(Int_State%zeneith_angle_facwf_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - zeneith_angle_facwf_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - zeneith_angle_facwf_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%facwf(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%zeneith_angle_facwf_ex(1, i), lonr_s, MPI_R_IO, 0,  &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - zeneith_angle_facwf_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - zeneith_angle_facwf_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'FACWF', &
         Int_State%zeneith_angle_facwf_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - FACWF_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - FACWF_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%uustar_import) THEN
     ALLOCATE(Int_State%uustar_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - uustar_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - uustar_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%uustar(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%uustar_ex(1, i), lonr_s, MPI_R_IO, 0,               &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - uustar_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - uustar_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'UUSTAR', Int_State%uustar_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - UUSTAR_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - UUSTAR_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%ffmm_import) THEN
     ALLOCATE(Int_State%ffmm_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - ffmm_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - ffmm_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%ffmm(1, i), ijn_s(1, 2), displs_s(1, 2),              &
              MPI_R_IO, Int_State%ffmm_ex(1, i), lonr_s, MPI_R_IO, 0,                 &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - ffmm_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - ffmm_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'FFMM', Int_State%ffmm_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - FFMM_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - FFMM_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%ffhh_import) THEN
     ALLOCATE(Int_State%ffhh_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - ffhh_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - ffhh_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%ffhh(1, i), ijn_s(1, 2), displs_s(1, 2),              &
              MPI_R_IO, Int_State%ffhh_ex(1, i), lonr_s, MPI_R_IO, 0,                 &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - ffhh_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - ffhh_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'FFHH', Int_State%ffhh_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - FFHH_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - FFHH_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%sea_ice_thickness_import) THEN
     ALLOCATE(Int_State%sea_ice_thickness_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - sea_ice_thickness_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - sea_ice_thickness_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%hice(1, i), ijn_s(1, 2), displs_s(1, 2),              &
              MPI_R_IO, Int_State%sea_ice_thickness_ex(1, i), lonr_s, MPI_R_IO, 0,    &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - sea_ice_thickness_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - sea_ice_thickness_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'SIH',                          &
                             Int_State%sea_ice_thickness_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - SIH_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - SIH_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%sea_ice_concentration_import) THEN
     ALLOCATE(Int_State%sea_ice_concentration_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - sea_ice_concentration_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - sea_ice_concentration_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%fice(1, i), ijn_s(1, 2), displs_s(1, 2),              &
              MPI_R_IO, Int_State%sea_ice_concentration_ex(1, i), lonr_s, MPI_R_IO, 0,&
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - sea_ice_concentration_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - sea_ice_concentration_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'SIC',                          &
                             Int_State%sea_ice_concentration_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - SIC_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - SIC_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%tprcp_import) THEN
     ALLOCATE(Int_State%tprcp_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - tprcp_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - tprcp_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%tprcp(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%tprcp_ex(1, i), lonr_s, MPI_R_IO, 0,                &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - tprcp_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - tprcp_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'TPRCP', Int_State%tprcp_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - TPRCP_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - TPRCP_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%srflag_import) THEN
     ALLOCATE(Int_State%srflag_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - srflag_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - srflag_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%srflag(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%srflag_ex(1, i), lonr_s, MPI_R_IO, 0,               &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - srflag_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - srflag_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'SRFLAG', Int_State%srflag_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - SRFLAG_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - SRFLAG_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%actual_snow_depth_import) THEN
     ALLOCATE(Int_State%actual_snow_depth_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - actual_snow_depth_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - actual_snow_depth_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%snwdph(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%actual_snow_depth_ex(1, i), lonr_s, MPI_R_IO, 0,    &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - actual_snow_depth_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - actual_snow_depth_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'SNWDPH', Int_State%actual_snow_depth_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - SNWDPH_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - SNWDPH_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%vegetation_cover_min_import) THEN
     ALLOCATE(Int_State%vegetation_cover_min_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - vegetation_cover_min_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - vegetation_cover_min_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%shdmin(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%vegetation_cover_min_ex(1, i), lonr_s, MPI_R_IO, 0, &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - vegetation_cover_min_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - vegetation_cover_min_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'VMN',                          &
                             Int_State%vegetation_cover_min_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - VMN_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - VMN_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%vegetation_cover_max_import) THEN
     ALLOCATE(Int_State%vegetation_cover_max_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - vegetation_cover_max_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - vegetation_cover_max_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%shdmax(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%vegetation_cover_max_ex(1, i), lonr_s, MPI_R_IO, 0, &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - vegetation_cover_max_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - vegetation_cover_max_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'VMX',                          &
                             Int_State%vegetation_cover_max_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - VMX_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - VMX_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%slope_type_import) THEN
     ALLOCATE(Int_State%slope_type_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - slope_type_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - slope_type_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%slope(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%slope_type_ex(1, i), lonr_s, MPI_R_IO, 0,           &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - slope_type_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - slope_type_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'SLP', Int_State%slope_type_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - SLP_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - SLP_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%snow_albedo_max_import) THEN
     ALLOCATE(Int_State%snow_albedo_max_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - snow_albedo_max_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - snow_albedo_max_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%snoalb(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%snow_albedo_max_ex(1, i), lonr_s, MPI_R_IO, 0,      &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - snow_albedo_max_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - snow_albedo_max_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'ABS', Int_State%snow_albedo_max_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - ABS_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - ABS_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%soil_t_import) THEN
     ALLOCATE(sfcbuf1(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the working array - sfcbuf1")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the working array - sfcbuf1, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     ALLOCATE(STC_name(Int_State%lsoil),     stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the working array - STC_name")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the working array - STC_name, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, Int_State%lsoil
         WRITE(STC_name(i), 1001) i
     END DO
1001 FORMAT ('STC_', i1)

     ALLOCATE(Int_State%soil_t_ex(lonr_s, latr_s, Int_State%lsoil), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - soil_t_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - soil_t_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, Int_State%lsoil
         DO j = 1, latr_s
             CALL mpi_scatterv(data%stc(1, j, i), ijn_s(1, 2), displs_s(1, 2),      &
                  MPI_R_IO, Int_State%soil_t_ex(1, j, i), lonr_s, MPI_R_IO, 0,      &
                  MPI_COMM_Atmos, rc1)

             IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - soil_t_ex")) THEN
                  rcfinal = ESMF_FAILURE
                  PRINT*, 'Error Happened When MPI_Scatterving - soil_t_ex, i, j, rc = ', i, j, rc1
                  rc1 = ESMF_SUCCESS
             END IF
         END DO
     
         sfcbuf1 = Int_State%soil_t_ex(:, :, i)
         CALL AddF90ArrayToState(impGFS, grid3, STC_name(i), sfcbuf1, rc = rc1)
         
         IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - soil_t_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When Creating ESMF State - soil_t_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     DEALLOCATE(sfcbuf1, STC_name)
 END IF

 IF(Int_State%ESMF_Sta_List%soil_mois_import) THEN
     ALLOCATE(sfcbuf1(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the working array - sfcbuf1")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the working array - sfcbuf1, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     ALLOCATE(SMC_name(Int_State%lsoil),     stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the working array - SMC_name")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the working array - SMC_name, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, Int_State%lsoil
         WRITE(SMC_name(i), 1002) i
     END DO
1002 FORMAT('SMC_', i1)

     ALLOCATE(Int_State%soil_mois_ex(lonr_s, latr_s, Int_State%lsoil), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - soil_mois_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - soil_mois_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, Int_State%lsoil
         DO j = 1, latr_s
             CALL mpi_scatterv(data%smc(1, j, i), ijn_s(1, 2), displs_s(1, 2),      &
                  MPI_R_IO, Int_State%soil_mois_ex(1, j, i), lonr_s, MPI_R_IO, 0,            &
                  MPI_COMM_Atmos, rc1)

             IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - soil_mois_ex")) THEN
                 rcfinal = ESMF_FAILURE
                 PRINT*, 'Error Happened When MPI_Scatterving - soil_mois_ex, i, j, rc = ', i, j, rc1
                 rc1 = ESMF_SUCCESS
             END IF
         END DO
     
         sfcbuf1 = Int_State%soil_mois_ex(:, :, i)
         CALL AddF90ArrayToState(impGFS, grid3, SMC_name(i), sfcbuf1, rc = rc1)
         
         IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - soil_mois_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When Creating ESMF State - soil_mois_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF

     END DO

     DEALLOCATE(sfcbuf1, SMC_name)
 END IF

 IF(Int_State%ESMF_Sta_List%liquid_soil_moisture_import) THEN
     ALLOCATE(sfcbuf1(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the working array - sfcbuf1")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the working array - sfcbuf1, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     ALLOCATE(SLC_name(Int_State%lsoil),     stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the working array - SLC_name")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the working array - SLC_name, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, Int_State%lsoil
         WRITE(SLC_name(i), 1003) i
     END DO
1003 FORMAT('SLC_', i1)

     ALLOCATE(Int_State%liquid_soil_moisture_ex(lonr_s, latr_s, Int_State%lsoil), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - liquid_soil_moisture_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - liquid_soil_moisture_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, Int_State%lsoil
         DO j = 1, latr_s
             CALL mpi_scatterv(data%slc(1, j, i), ijn_s(1, 2), displs_s(1, 2),      &
                  MPI_R_IO, Int_State%liquid_soil_moisture_ex(1, j, i),             &
                  lonr_s, MPI_R_IO, 0, MPI_COMM_Atmos, rc1)
    
             IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - liquid_soil_moisture_ex")) THEN
                 rcfinal = ESMF_FAILURE
                 PRINT*, 'Error Happened When MPI_Scatterving - liquid_soil_moisture_ex, i, j, rc = ', i, j, rc1
                 rc1 = ESMF_SUCCESS
             END IF
         END DO
     
         sfcbuf1 = Int_State%liquid_soil_moisture_ex(:, :, i)
         CALL AddF90ArrayToState(impGFS, grid3, SLC_name(i), sfcbuf1, rc = rc1)
         
         IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - liquid_soil_moisture_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When Creating ESMF State - liquid_soil_moisture_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF

     END DO

     DEALLOCATE(sfcbuf1, SLC_name)
 END IF

 IF(Int_State%ESMF_Sta_List%conv_cloud_cover_import) THEN
     ALLOCATE(Int_State%conv_cloud_cover_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - conv_cloud_cover_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - conv_cloud_cover_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%cv(1, i), ijn_s(1, 2), displs_s(1, 2),                &
              MPI_R_IO, Int_State%conv_cloud_cover_ex(1, i), lonr_s, MPI_R_IO, 0,     &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - conv_cloud_cover_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - conv_cloud_cover_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'CV', &
         Int_State%conv_cloud_cover_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - CV_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - CV_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%conv_cloud_base_import) THEN
     ALLOCATE(Int_State%conv_cloud_base_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - conv_cloud_base_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - conv_cloud_base_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%cvb(1, i), ijn_s(1, 2), displs_s(1, 2),               &
              MPI_R_IO, Int_State%conv_cloud_base_ex(1, i), lonr_s, MPI_R_IO, 0,      &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - conv_cloud_base_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - conv_cloud_base_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'CVB', &
         Int_State%conv_cloud_base_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - CVB_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - CVB_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(Int_State%ESMF_Sta_List%conv_cloud_top_import) THEN
     ALLOCATE(Int_State%conv_cloud_top_ex(lonr_s, latr_s), stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Internal State - conv_cloud_top_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Internal State - conv_cloud_top_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     DO i = 1, latr_s
         CALL mpi_scatterv(data%cvt(1, i), ijn_s(1, 2), displs_s(1, 2),               &
              MPI_R_IO, Int_State%conv_cloud_top_ex(1, i), lonr_s, MPI_R_IO, 0,       &
              MPI_COMM_Atmos, rc1)

         IF(ESMF_LogMsgFoundError(rc1, "MPI_Scatterv - conv_cloud_top_ex")) THEN
             rcfinal = ESMF_FAILURE
             PRINT*, 'Error Happened When MPI_Scatterving - conv_cloud_top_ex, i, rc = ', i, rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO

     CALL AddF90ArrayToState(impGFS, grid3, 'CVT', &
         Int_State%conv_cloud_top_ex, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create ESMF State - CVT_ex")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating ESMF State - CVT_ex, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 END IF

 IF(rcfinal == ESMF_SUCCESS) THEN
     PRINT*, "PASS: GFS_InternalState2ESMFExportState1"
 ELSE
     PRINT*, "FAIL: GFS_InternalState2ESMFExportState1"
 END IF

 rc = rcfinal

 END SUBROUTINE GFS_InputFile2ImportState





 SUBROUTINE Grid_ESMFCreate3(vm, myDeLayout, grid2, Int_State, rc)
!
!!USES:
!
 USE ESMF_Mod                 ! The ESMF library.
 USE GFS_InternalState_ESMFMod

 TYPE(ESMF_VM),           INTENT(inout) :: vm           ! the ESMF virtual machine.
 TYPE(ESMF_DELayout),     INTENT(inout) :: myDeLayout   ! the ESMF layout type array.
 TYPE(ESMF_Grid),         INTENT(inout) :: grid2        ! the ESMF GRID TYPE ARRAY.
 INTEGER, INTENT(out)                   :: rc
 TYPE(GFS_InternalState), INTENT(inout) :: Int_State

 INTEGER                           :: rc1
 INTEGER                           :: rcfinal

 INTEGER,            DIMENSION(2)  :: counts
 REAL(ESMF_KIND_R8), DIMENSION(2)  :: min, max

 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

! Create grid.
! Use uniform grid to represent the Gaussian grid
! since no dx, dy is needed.
!--------------------------------------------------
!counts(1)        = (Int_State%nam_gfs%jcap+1)*(Int_State%nam_gfs%jcap+2) ! Moorthi
!counts(2)        = Int_State%nam_gfs%levs                                ! Moorthi
 counts(1)        = (Int_State%jcap+1)*(Int_State%jcap+2)
 counts(2)        = Int_State%levs
 min(1)           = 1.0
 min(2)           = 1.0
 max(1)           = counts(1)
 max(2)           = counts(2)

     CALL ESMF_LogWrite("Create Grid2", ESMF_LOG_INFO, rc = rc1)

 grid2 = ESMF_GridCreateHorzXYUni(counts,                      &
              minGlobalCoordPerDim = min,                      &
              maxGlobalCoordPerDim = max,                      &
              horzstagger          = ESMF_GRID_HORZ_STAGGER_A, &
              name                 = "GFS grid2",              &
              rc                   = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create Grid2")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating Grid2, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     CALL ESMF_LogWrite("Distribute Grid2", ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_GridDistribute(grid2, delayout = myDeLayout, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Distribute Grid2")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Distributing Grid2, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(rcfinal == ESMF_SUCCESS) THEN
     PRINT*, "PASS: Grid_ESMFCreate3."
 ELSE
     PRINT*, "FAIL: Grid_ESMFCreate3."
 END IF

 rc = rcfinal

 END SUBROUTINE Grid_ESMFCreate3

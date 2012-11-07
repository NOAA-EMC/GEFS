!----------------------------------------------------------------------
! !MODULE: GFS_AddParameterToStateMod
!        --- Add required parameters to the GFS ESMF export state
!            for the ensemble coupler to do the spectral transform
!            for the stochastic perturbation scheme, the second step.
!
! !DESCRIPTION: Add all required parameters to the GFS ESMF export state.
!
! !REVISION HISTORY:
!
!  May      2007     Weiyu Yang Initial code.
!
!
! !INTERFACE:
!

 MODULE GFS_AddParameterToStateMod

 USE ESMF_Mod

 USE resol_def
 USE layout1
 USE mpi_def
 USE GFS_InternalState_ESMFMod

 REAL(KIND = kind_evod), DIMENSION(:), POINTER   :: work1a, work1b, work1c, work1d
 INTEGER,                DIMENSION(:), POINTER   :: work3
 TYPE(ESMF_LOGICAL)                              :: lslag_1

 IMPLICIT none

 CONTAINS

 SUBROUTINE AddParameterToState(State, Int_State, rc)

 TYPE(ESMF_State),                 INTENT(inout) :: State
 TYPE(GFS_InternalState), POINTER, INTENT(in)    :: Int_State
 INTEGER, OPTIONAL,                INTENT(out)   :: rc


 INTEGER                                         :: i, j, i1
 INTEGER                                         :: dimg
 INTEGER                                         :: rc1, rcfinal

 LOGICAL first
 DATA first /.true./
 SAVE first

 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

! One by one add the parameters to the GFS ESMF export state.
!------------------------------------------------------------
 CALL ESMF_AttributeSet(State, 'JCAP', jcap, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add JCAP to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding JCAP to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'JINTMX', jintmx, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add JINTMX to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding JINTMX to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'LS_DIM', ls_dim, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LS_DIM to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LS_DIM to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'LS_MAX_NODE', ls_max_node, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LS_MAX_NODE to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LS_MAX_NODE to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'MPI_R_MPI', MPI_R_MPI, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add MPI_R_MPI to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding MPI_R_MPI to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'LEVS', levs, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LEVS to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LEVS to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'LEVH', levh, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LEVH to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LEVH to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'LATGD', latgd, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LATGD to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LATGD to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'LATR', latr, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LATR to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LATR to the GFS export state, rc =', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'NODES_COMP', nodes, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add NODES_COMP to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding NODES_COMP to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'ME_COMP', me, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add ME_COMP to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding ME_COMP to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'MC_COMP', MC_COMP, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add MC_COMP to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding MC_COMP to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'LATL2', latr2, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LATL2 to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LATL2 to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'NVARS', lots, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add NVARS to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding NVARS to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(Int_State%lslag) THEN
     lslag_1 = ESMF_TRUE
 ELSE
     lslag_1 = ESMF_FALSE
 END IF

 CALL ESMF_AttributeSet(State, 'LSLAG', lslag_1, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LSLAG to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LSLAG to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'LATDIMS', lats_dim_a, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LATDIMS to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LATDIMS to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'LAT1S', jcap + 1, lat1s_r, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LAT1S to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LAT1S to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'LATL', latr, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LATL to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LATL to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 dimg = 0

 CALL ESMF_AttributeSet(State, 'LATS_NODE', lats_node_r, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LATS_NODE to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LATS_NODE to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'IPT_LATS_NODE', ipt_lats_node_r, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add IPT_LATS_NODE to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding IPT_LATS_NODE to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 print *,' latgd=',latgd,' lon_dims_r=',lon_dims_r(1:5)
!CALL ESMF_AttributeSet(State, 'LON_DIMS', latgd, lon_dims_r, rc = rc1)
 CALL ESMF_AttributeSet(State, 'LON_DIMS', latr, lon_dims_r, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LON_DIMS to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LON_DIMS to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'LONS_LAT', latr, Int_State%lonsperlar, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LONS_LAT to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LONS_LAT to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'LONDI', lonrx, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LONDI to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LONDI to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(first) ALLOCATE(work1a(len_trie_ls * latr2))

 i1 = 0
 DO j = 1, latr2
     DO i = 1, len_trie_ls
         i1 = i1 + 1
         work1a(i1) = Int_State%plnev_r(i, j)
     END DO
 END DO

 CALL ESMF_AttributeSet(State, 'PLNEV', len_trie_ls * latr2, work1a, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add PLNEV to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding PLNEV to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(first) ALLOCATE(work1b(len_trio_ls * latr2))

 i1 = 0
 DO j = 1, latr2
     DO i = 1, len_trio_ls
         i1 = i1 + 1
         work1b(i1) = Int_State%plnod_r(i, j)
     END DO
 END DO

 CALL ESMF_AttributeSet(State, 'PLNOD', len_trio_ls * latr2, work1b, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add PLNOD to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding PLNOD to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(first) ALLOCATE(work1c(len_trie_ls * latr2))

 i1 = 0
 DO j = 1, latr2
     DO i = 1, len_trie_ls
         i1 = i1 + 1
         work1c(i1) = Int_State%plnew_r(i, j)
     END DO
 END DO

 CALL ESMF_AttributeSet(State, 'PLNEW', len_trie_ls * latr2, work1c, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add PLNEW to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding PLNEW to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(first) ALLOCATE(work1d(len_trio_ls * latr2))

 i1 = 0
 DO j = 1, latr2
     DO i = 1, len_trio_ls
         i1 = i1 + 1
         work1d(i1) = Int_State%plnow_r(i, j)
     END DO
 END DO

 CALL ESMF_AttributeSet(State, 'PLNOW', len_trio_ls * latr2, work1d, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add PLNOW to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding PLNOW to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'LS_NODE', ls_dim * 3, Int_State%ls_node, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LS_NODE to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LS_NODE to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(first) ALLOCATE(work3(ls_dim * nodes))
 
 i1 = 0
 DO j = 1, nodes
     DO i = 1, ls_dim
         i1 = i1 + 1
         work3(i1) = Int_State%ls_nodes(i, j)
     END DO
 END DO

 CALL ESMF_AttributeSet(State, 'LS_NODES', ls_dim * nodes, work3, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LS_NODES to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LS_NODES to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'MAX_LS_NODES', nodes, Int_State%max_ls_nodes, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add MAX_LS_NODES to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding MAX_LS_NODES to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'LATS_NODES', nodes, Int_State%lats_nodes_r, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add LATS_NODES to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding LATS_NODES to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'GLOBAL_LATS', latr + dimg, Int_State%global_lats_r, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Add GLOBAL_LATS to the GFS export state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Adding GLOBAL_LATS to the GFS export state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'EPSEDN', len_trie_ls, Int_State%epsedn, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Set EPSEDN to the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Setting EPSEDN to the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'EPSODN', len_trio_ls, Int_State%epsodn, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Set EPSODN to the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Setting EPSODN to the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'EPSE', len_trie_ls, Int_State%epse, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Set EPSE to the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Setting EPSE to the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'EPSO', len_trio_ls, Int_State%epso, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Set EPSO to the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Setting EPSO to the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'SNNP1EV', len_trie_ls, Int_State%snnp1ev, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Set SNNP1EV to the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Setting SNNP1EV to the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeSet(State, 'SNNP1OD', len_trio_ls,Int_State%snnp1od, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Set SNNP1OD to the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Setting SNNP1OD to the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 
 IF(first) first = .false.

 IF(rcfinal == ESMF_SUCCESS) THEN
     PRINT*, "PASS: GFS_AddParameterToStateMod.f"
 ELSE
     PRINT*, "FAIL: GFS_AddParameterToStateMod.f"
 END IF

 IF(PRESENT(rc)) THEN
     rc = rcfinal
 END IF

 END SUBROUTINE AddParameterToState

 END MODULE GFS_AddParameterToStateMod

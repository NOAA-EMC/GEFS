!----------------------------------------------------------------------
! !MODULE: GEFS_GetParameterFromStateMod
!        --- Get required parameters from the GEFS Coupler ESMF import state
!            for the ensemble coupler to do the spectral transform
!            for the stochastic perturbation scheme, the second step.
!
! !DESCRIPTION: Get all required parameters from the GEFS Cpl ESMF import state.
!
! !REVISION HISTORY:
!
!  May      2007     Weiyu Yang Initial code.
!
!
! !INTERFACE:
!

 MODULE GEFS_GetParameterFromStateMod

 USE ESMF_Mod
 USE GEFS_Cpl_InternalState_ESMFMod

 IMPLICIT none

 CONTAINS

 SUBROUTINE GEFS_GetParameterFromState(State, Int_State, rc)

 TYPE(ESMF_State),                      INTENT(inout) :: State
 TYPE(GEFS_Cpl_InternalState), POINTER, INTENT(inout) :: Int_State
 INTEGER, OPTIONAL,                     INTENT(out)   :: rc

 REAL(KIND = kind_evod), DIMENSION(:), POINTER        :: work1
 INTEGER,                DIMENSION(:), POINTER        :: work3

 INTEGER                                              :: i, j, i1
 INTEGER                                              :: rc1, rcfinal

 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

! One by one get the parameters from the GFS ESMF export state.
!--------------------------------------------------------------
 CALL ESMF_AttributeGet(State, 'JCAP', Int_State%jcap, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get JCAP from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting JCAP from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'JINTMX', Int_State%jintmx, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get JINTMX from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting JINTMX from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'LS_DIM', Int_State%ls_dim, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LS_DIM from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LS_DIM from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'LS_MAX_NODE', Int_State%ls_max_node, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LS_MAX_NODE from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LS_MAX_NODE from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'MPI_R_MPI', Int_State%MPI_R_MPI, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get MPI_R_MPI from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting MPI_R_MPI from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'LEVS', Int_State%levs, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LEVS from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LEVS from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'LEVH', Int_State%levh, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LEVH from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LEVH from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'LATGD', Int_State%latgd, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LATGD from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LATGD from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'LATR', Int_State%latr, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LATR from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LATR from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'NODES_COMP', Int_State%nodes_comp, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get NODES_COMP from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting NODES_COMP from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'ME_COMP', Int_State%me_comp, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get ME_COMP from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting ME_COMP from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'MC_COMP', Int_State%MC_COMP, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get MC_COMP from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting MC_COMP from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'LATL2', Int_State%latl2, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LATL2 from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LATL2 from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'NVARS', Int_State%nvars, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get NVARS from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting NVARS from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'LSLAG', Int_State%lslag_1, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LSLAG from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LSLAG from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(Int_State%lslag_1 == ESMF_TRUE) THEN
     Int_State%lslag = .true.
 ELSE
     Int_State%lslag = .false.
 END IF

 CALL ESMF_AttributeGet(State, 'LATDIMS', Int_State%latdims, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LATDIMS from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LATDIMS from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 NULLIFY(Int_State%lat1s)
 ALLOCATE(Int_State%lat1s(0 : Int_State%jcap))

 CALL ESMF_AttributeGet(State, 'LAT1S', Int_State%jcap + 1, Int_State%lat1s, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LAT1S from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LAT1S from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'LATL', Int_State%latl, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LATL from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LATL from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 Int_State%dimg = 0

 CALL ESMF_AttributeGet(State, 'LATS_NODE', Int_State%lats_node, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LATS_NODE from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LATS_NODE from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'IPT_LATS_NODE', Int_State%ipt_lats_node, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get IPT_LATS_NODE from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting IPT_LATS_NODE from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 NULLIFY(Int_State%lon_dims)
!ALLOCATE(Int_State%lon_dims(Int_State%latgd))
 ALLOCATE(Int_State%lon_dims(Int_State%latr))
 
!CALL ESMF_AttributeGet(State, 'LON_DIMS', Int_State%latgd, Int_State%lon_dims, rc = rc1)
 CALL ESMF_AttributeGet(State, 'LON_DIMS', Int_State%latr, Int_State%lon_dims, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LON_DIMS from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LON_DIMS from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 NULLIFY(Int_State%lons_lat)
 ALLOCATE(Int_State%lons_lat(Int_State%latl))

 CALL ESMF_AttributeGet(State, 'LONS_LAT', Int_State%latl, Int_State%lons_lat, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LONS_LAT from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LONS_LAT from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'LONDI', Int_State%londi, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LONDI from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LONDI from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 ALLOCATE(work1(Int_State%TRIE_LS_SIZE * Int_State%latl2))
 NULLIFY (Int_State%plnev)
 ALLOCATE(Int_State%plnev(Int_State%TRIE_LS_SIZE, Int_State%latl2))

 CALL ESMF_AttributeGet(State, 'PLNEV', Int_State%TRIE_LS_SIZE * Int_State%latl2, &
     work1, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get PLNEV from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting PLNEV from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 i1 = 0
 DO j = 1, Int_State%latl2
     DO i = 1, Int_State%TRIE_LS_SIZE
         i1 = i1 + 1
         Int_State%plnev(i, j) = work1(i1)
     END DO
 END DO

 DEALLOCATE(work1)

 ALLOCATE(work1(Int_State%TRIO_LS_SIZE * Int_State%latl2))
 NULLIFY (Int_State%plnod)
 ALLOCATE(Int_State%plnod(Int_State%TRIO_LS_SIZE, Int_State%latl2))

 CALL ESMF_AttributeGet(State, 'PLNOD', Int_State%TRIO_LS_SIZE * Int_State%latl2, &
     work1, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get PLNOD from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting PLNOD from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 i1 = 0
 DO j = 1, Int_State%latl2
     DO i = 1, Int_State%TRIO_LS_SIZE
         i1 = i1 + 1
         Int_State%plnod(i, j) = work1(i1)
     END DO
 END DO

 DEALLOCATE(work1)

 ALLOCATE(work1(Int_State%TRIE_LS_SIZE * Int_State%latl2))
 NULLIFY (Int_State%plnew)
 ALLOCATE(Int_State%plnew(Int_State%TRIE_LS_SIZE, Int_State%latl2))

 CALL ESMF_AttributeGet(State, 'PLNEW', Int_State%TRIE_LS_SIZE * Int_State%latl2, &
     work1, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get PLNEW from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting PLNEW from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 i1 = 0
 DO j = 1, Int_State%latl2
     DO i = 1, Int_State%TRIE_LS_SIZE
         i1 = i1 + 1
         Int_State%plnew(i, j) = work1(i1)
     END DO
 END DO

 DEALLOCATE(work1)

 ALLOCATE(work1(Int_State%TRIO_LS_SIZE * Int_State%latl2))
 NULLIFY (Int_State%plnow)
 ALLOCATE(Int_State%plnow(Int_State%TRIO_LS_SIZE, Int_State%latl2))

 CALL ESMF_AttributeGet(State, 'PLNOW', Int_State%TRIO_LS_SIZE * Int_State%latl2, &
     work1, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get PLNOW from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting PLNOW from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 i1 = 0
 DO j = 1, Int_State%latl2
     DO i = 1, Int_State%TRIO_LS_SIZE
         i1 = i1 + 1
         Int_State%plnow(i, j) = work1(i1)
     END DO
 END DO

 DEALLOCATE(work1)

 ALLOCATE(work3(Int_State%ls_dim * 3))
 NULLIFY (Int_State%ls_node)
 ALLOCATE(Int_State%ls_node(Int_State%ls_dim, 3))

 CALL ESMF_AttributeGet(State, 'LS_NODE', Int_State%ls_dim * 3, work3, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LS_NODE from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LS_NODE from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 i1 = 0
 DO j = 1, 3
     DO i = 1, Int_State%ls_dim
         i1 = i1 + 1
         Int_State%ls_node(i, j) = work3(i1)
     END DO
 END DO

 DEALLOCATE(work3)


 ALLOCATE(work3(Int_State%ls_dim * Int_State%nodes_comp))
 NULLIFY (Int_State%ls_nodes)
 ALLOCATE(Int_State%ls_nodes(Int_State%ls_dim, Int_State%nodes_comp))
 
 CALL ESMF_AttributeGet(State, 'LS_NODES', Int_State%ls_dim * Int_State%nodes_comp, &
     work3, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LS_NODES from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LS_NODES from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 i1 = 0
 DO j = 1, Int_State%nodes_comp
     DO i = 1, Int_State%ls_dim
         i1 = i1 + 1
         Int_State%ls_nodes(i, j) = work3(i1)
     END DO
 END DO

 DEALLOCATE(work3)

 NULLIFY (Int_State%max_ls_nodes)
 ALLOCATE(Int_State%max_ls_nodes(Int_State%nodes_comp))

 CALL ESMF_AttributeGet(State, 'MAX_LS_NODES', Int_State%nodes_comp, &
     Int_State%max_ls_nodes, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get MAX_LS_NODES from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting MAX_LS_NODES from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 NULLIFY (Int_State%lats_nodes)
 ALLOCATE(Int_State%lats_nodes(Int_State%nodes_comp))

 CALL ESMF_AttributeGet(State, 'LATS_NODES', Int_State%nodes_comp, &
     Int_State%lats_nodes, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get LATS_NODES from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LATS_NODES from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 NULLIFY (Int_State%global_lats)
 ALLOCATE(Int_State%global_lats(Int_State%latl + Int_State%dimg))

 CALL ESMF_AttributeGet(State, 'GLOBAL_LATS', Int_State%latl + Int_State%dimg, &
     Int_State%global_lats, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get GLOBAL_LATS from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting GLOBAL_LATS from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 NULLIFY (Int_State%epsedn)
 ALLOCATE(Int_State%epsedn(Int_State%TRIE_LS_SIZE))

 CALL ESMF_AttributeGet(State, 'EPSEDN', Int_State%TRIE_LS_SIZE, &
     Int_State%epsedn, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get EPSEDN from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting EPSEDN from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 NULLIFY (Int_State%epsodn)
 ALLOCATE(Int_State%epsodn(Int_State%TRIO_LS_SIZE))

 CALL ESMF_AttributeGet(State, 'EPSODN', Int_State%TRIO_LS_SIZE, &
     Int_State%epsodn, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get EPSODN from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting EPSODN from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 NULLIFY (Int_State%epse)
 ALLOCATE(Int_State%epse(Int_State%TRIE_LS_SIZE))

 CALL ESMF_AttributeGet(State, 'EPSE', Int_State%TRIE_LS_SIZE, &
     Int_State%epse, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get EPSE from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting EPSE from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 NULLIFY (Int_State%epso)
 ALLOCATE(Int_State%epso(Int_State%TRIO_LS_SIZE))

 CALL ESMF_AttributeGet(State, 'EPSO', Int_State%TRIO_LS_SIZE, &
     Int_State%epso, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get EPSO from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting EPSO from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 NULLIFY (Int_State%snnp1ev)
 ALLOCATE(Int_State%snnp1ev(Int_State%TRIE_LS_SIZE))

 CALL ESMF_AttributeGet(State, 'SNNP1EV', Int_State%TRIE_LS_SIZE, &
     Int_State%snnp1ev, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get SNNP1EV from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting SNNP1EV from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 NULLIFY (Int_State%snnp1od)
 ALLOCATE(Int_State%snnp1od(Int_State%TRIO_LS_SIZE))

 CALL ESMF_AttributeGet(State, 'SNNP1OD', Int_State%TRIO_LS_SIZE, &
     Int_State%snnp1od, rc = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, "Get SNNP1OD from the GEFS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting SNNP1OD from the GEFS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(rcfinal == ESMF_SUCCESS) THEN
     PRINT*, "PASS: GEFS_GetParameterFromStateMod.f"
 ELSE
     PRINT*, "FAIL: GEFS_GetParameterFromStateMod.f"
 END IF

 IF(PRESENT(rc)) THEN
     rc = rcfinal
 END IF

 END SUBROUTINE GEFS_GetParameterFromState

 END MODULE GEFS_GetParameterFromStateMod


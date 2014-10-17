 SUBROUTINE GEFS_Sto_Per_Scheme_Step2(Int_State, ZEROS03, USES1, GRIDUSE,       &
                 QADJUST, RS_GLOBAL, Jul_Day,  KEMAX, nreg,slat1, slat2, rc)
!
!DHOU, 10/17/2007  Added arguments 7 and 8, i.e. KEMAX(nreg) and nreg for regional rescaling
!DHOU, 10/17/2007  Added argument 6 i.e. RS_GLOBAL for global rescaling factor 
!DHOU  09/11/2007  Added Arguments
!ZEROS03, 0=No, 1=YES; Zero out arrays gz, dpdlam, dpdphi, uln, and vln as well as State 3
!S1USE, 0=Zero-out, 1=no-change, 2=replaced with S2, 3=replace S1 and s2 with 0.5(s1+S2).
!           For arrays related to state 1,zem to qm.
!GRIDUSE, 1=S1, 2=S2; Convert the state S1/S2 from spectral to grid and  and back to spec.
!QADJUST, 0=NO, 1=YES; Adjust the q array so that q>=0 everywhere.

! This subroutine is used to compute the second step of the
! stochastic perturbation scheme, in which it carries out the spectral 
! transform computation into the Gaussian grid space arrays, then
! computes the second step of the stochastic perturbation scheme that
! considering the local weighting influences.
!---------------------------------------------------------------------

! !REVISION HISTORY:
!
!  May 2007       Weiyu Yang Initial code for wave-grid conversion for model state .
!  Nov 2007       Dingchen Hou ddopted the code for global/regional rescaling as well as conversion, for model state
!                 or its perturbation.    
!-----------------------------------------

 USE ESMF_Mod
 USE GEFS_Cpl_InternalState_ESMFMod
 USE machine,  ONLY: kind_evod, kind_phys, kind_rad
 use physcons, only : pi => con_pi, FV => con_fvirt, rerth => con_rerth

!DHOU 04/16/2012 moved this line for ZEUS (following gw?) 
 IMPLICIT none
 INCLUDE 'mpif.h'

 TYPE(GEFS_Cpl_InternalState), INTENT(inout) :: Int_State
 INTEGER,                      INTENT(out)   :: rc
 INTEGER                                     :: ZEROS03,USES1,GRIDUSE,QADJUST,LSHIFT
 INTEGER,                      INTENT(in)    :: Jul_Day
 REAL(KIND = kind_evod)                      :: RS_GLOBAL,RS
 INTEGER,                      INTENT(in )   :: nreg
 INTEGER                                     :: ireg,imem,k500
 REAL(KIND = kind_evod)                      :: KEMAX(nreg)
 REAL(KIND = kind_evod)                      :: KER(3,15)
 REAL(KIND = kind_evod)                      :: slat1,slat2

 TYPE(ESMF_VM)                               :: vm_esmf
 INTEGER                                     :: i, j, k, l, mem
 INTEGER                                     :: ksd, ksplam, kspphi
 INTEGER                                     :: ksq, ksr,    kst
 INTEGER                                     :: ksu, ksv,    ksz
 INTEGER                                     :: kso, ksc
 INTEGER                                     :: lan, lat,    lon_dim
 INTEGER                                     :: lon_lat

 REAL(KIND = kind_evod)                      :: atem, keavg

 REAL(KIND = kind_evod)                      :: vorm, divm, tm
 REAL(KIND = kind_evod)                      :: qm,   ozm,  clwm
 REAL(KIND = kind_evod)                      :: um,   vm
 REAL(KIND = kind_evod)                      :: psm,  dpdlamm, dpdphim

 REAL(KIND = kind_evod)                      :: vors, divs, ts
 REAL(KIND = kind_evod)                      :: qs,   ozs,  clws 
 REAL(KIND = kind_evod)                      :: us,   vs
 REAL(KIND = kind_evod)                      :: pss,  dpdlams, dpdphis

 REAL(KIND = kind_rad)                       :: qmin
!REAL(KIND = kind_phys)                      :: fv
!REAL(KIND = kind_phys)                      :: rerth


 INTEGER                                     :: rc1
 INTEGER                                     :: rcfinal

 PARAMETER(qmin  = 1.0e-10)
!PARAMETER(fv    = 4.6150e+2 / 2.8705e+2 - 1.0)
!PARAMETER(rerth = 6.3712e+6)

 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

 CALL ESMF_VMGetGlobal(vm_esmf, rc=rc1)
 ksz     = 0 * Int_State%levs + 0 * Int_State%levh + 1
 ksd     = 1 * Int_State%levs + 0 * Int_State%levh + 1
 kst     = 2 * Int_State%levs + 0 * Int_State%levh + 1
 ksr     = 3 * Int_State%levs + 0 * Int_State%levh + 1
 kso     = 4 * Int_State%levs + 0 * Int_State%levh + 1
 ksc     = 5 * Int_State%levs + 0 * Int_State%levh + 1
 ksq     = 3 * Int_State%levs + 1 * Int_State%levh + 1
 ksplam  = 3 * Int_State%levs + 1 * Int_State%levh + 2
 kspphi  = 3 * Int_State%levs + 1 * Int_State%levh + 3
 ksu     = 3 * Int_State%levs + 1 * Int_State%levh + 4
 ksv     = 4 * Int_State%levs + 1 * Int_State%levh + 4

 IF (.NOT.ASSOCIATED(Int_State%trie_ls))                                               & 
 ALLOCATE(Int_State%trie_ls(Int_State%TRIE_LS_SIZE, 2, Int_State%TRIEO_VERTICAL_SIZE))
 IF (.NOT.ASSOCIATED(Int_State%trio_ls))                                               & 
 ALLOCATE(Int_State%trio_ls(Int_State%TRIO_LS_SIZE, 2, Int_State%TRIEO_VERTICAL_SIZE))

!Split TRIEO into TRIE and TRIO arrays
 k = 1
 DO l = 1, Int_State%TRIEO_VERTICAL_SIZE
     DO j = 1, 2
         DO i = 1, Int_State%TRIE_LS_SIZE
             Int_State%trie_ls( i, j, l) = Int_State%trieo_ls_max(k)
             k = k + 1
         END DO
         DO i = 1, Int_State%TRIO_LS_SIZE
             Int_State%trio_ls( i, j, l) = Int_State%trieo_ls_max(k)
             k = k + 1
         END DO
     END DO
 END DO
   
!PRINT *, "In the Ensemble Coupler, Step2, gz ",                  &
!Int_State%trio_ls(1, 1, Int_State%P_gz),Int_State%trie_ls(1, 1, Int_State%P_gz),Int_State%P_gz
!PRINT *, "In the Ensemble Coupler, Step2, gz ",                  &
!Int_State%trio_ls(1, 2, Int_State%P_gz),Int_State%trie_ls(1, 2, Int_State%P_gz),Int_State%P_gz

! ZERO OUT S0 arrays --- Arrays not related to the model state
IF (ZEROS03 .eq. 1) THEN
! State 0, gs array
 DO l = int_State%P_gz, Int_State%P_gz  
     DO j = 1, 2
         DO i = 1, Int_State%TRIE_LS_SIZE
             Int_State%trie_ls( i, j, l) = 0.0
         END DO
         DO i = 1, Int_State%TRIO_LS_SIZE
             Int_State%trio_ls( i, j, l) = 0.0 
         END DO
     END DO
 END DO
! State 0, arrays dpdlam, dpdphi, uln and vln
 DO l = int_State%P_dlam, Int_State%P_w - 1 
     DO j = 1, 2
         DO i = 1, Int_State%TRIE_LS_SIZE
             Int_State%trie_ls( i, j, l) = 0.0
         END DO
         DO i = 1, Int_State%TRIO_LS_SIZE
             Int_State%trio_ls( i, j, l) = 0.0 
         END DO
     END DO
 END DO
! State 3
 DO l = int_State%P_w, Int_State%P_zq  
     DO j = 1, 2
         DO i = 1, Int_State%TRIE_LS_SIZE
             Int_State%trie_ls( i, j, l) = 0.0
         END DO
         DO i = 1, Int_State%TRIO_LS_SIZE
             Int_State%trio_ls( i, j, l) = 0.0 
         END DO
     END DO
 END DO
ENDIF

! ZERO OUT S1 arrays  (model state 1, for t-1)
IF (USES1 .lt. 0 .or. USES1 .gt. 3 ) THEN
  PRINT *, 'INVALID VALUE of USES1=', USES1, 'FORCED STOP, CHECK STEP2'
  STOP
ENDIF
IF (USES1 .eq. 0) THEN
 DO l = int_State%P_zem, Int_State%P_qm  
     DO j = 1, 2
         DO i = 1, Int_State%TRIE_LS_SIZE
             Int_State%trie_ls( i, j, l) = 0.0
         END DO
         DO i = 1, Int_State%TRIO_LS_SIZE
             Int_State%trio_ls( i, j, l) = 0.0 
         END DO
     END DO
 END DO
ENDIF

!DHOU 09/06/2007 shift the arrays to process the t-1 time level instead of t level

IF (GRIDUSE .lt. 1 .or. GRIDUSE .gt. 2 ) THEN
  PRINT *, 'INVALID VALUE of GRIDUSE=', GRIDUSE, 'FORCED STOP, CHECK STEP2'
  STOP
ENDIF
IF (GRIDUSE .eq. 1 ) THEN
 LSHIFT = Int_State%P_q - Int_State%P_qm  
 DO l = int_State%P_ze, Int_State%P_q 
     DO j = 1, 2
         DO i = 1, Int_State%TRIE_LS_SIZE
             atem = Int_State%trie_ls( i, j, l) 
             Int_State%trie_ls( i, j, l) = Int_State%trie_ls(i, j, l-LSHIFT)
             Int_State%trie_ls(i, j, l-LSHIFT) = atem
         END DO
         DO i = 1, Int_State%TRIO_LS_SIZE
             atem = Int_State%trio_ls( i, j, l) 
             Int_State%trio_ls( i, j, l) = Int_State%trio_ls(i, j, l-LSHIFT)
             Int_State%trio_ls(i, j, l-LSHIFT) = atem
         END DO
     END DO
 END DO
ENDIF

!Start wave-grid conversion
 DO k = 1, Int_State%levs
     CALL GEFS_dezouv(Int_State%trie_ls(1, 1, Int_State%P_di  + k - 1), &
                      Int_State%trio_ls(1, 1, Int_State%P_ze  + k - 1), &
                      Int_State%trie_ls(1, 1, Int_State%P_uln + k - 1), &
                      Int_State%trio_ls(1, 1, Int_State%P_vln + k - 1), &
                      Int_State%epsedn,       Int_State%epsodn,         &
                      Int_State%snnp1ev,      Int_State%snnp1od,        &
                      Int_State%ls_node,      Int_State%jcap,           &
                      Int_State%TRIE_LS_SIZE, Int_State%TRIO_LS_SIZE,   &
                      Int_State%ls_dim,       Int_State%ls_max_node,    &
                      rerth)

     CALL GEFS_dozeuv(Int_State%trio_ls(1, 1, Int_State%P_di  + k - 1), &
                      Int_State%trie_ls(1, 1, Int_State%P_ze  + k - 1), &
                      Int_State%trio_ls(1, 1, Int_State%P_uln + k - 1), &
                      Int_State%trie_ls(1, 1, Int_State%P_vln + k - 1), &
                      Int_State%epsedn,       Int_State%epsodn,         &
                      Int_State%snnp1ev,      Int_State%snnp1od,        &
                      Int_State%ls_node,      Int_State%jcap,           &
                      Int_State%TRIE_LS_SIZE, Int_State%TRIO_LS_SIZE,   &
                      Int_State%ls_dim,       Int_State%ls_max_node,    &
                      rerth)
 END DO

 Int_State%dimg = 0

 IF (.NOT.ASSOCIATED(Int_State%four_gr1))                                               & 
 ALLOCATE(Int_State%four_gr1(Int_State%londi * Int_State%nvars, Int_State%latdims))
 IF (.NOT.ASSOCIATED(Int_State%four_gr2))                                               & 
 ALLOCATE(Int_State%four_gr2(Int_State%londi * Int_State%nvars, Int_State%latdims))

 CALL GEFS_sumfln(Int_State%trie_ls(1, 1, Int_State%P_ze),             &
                  Int_State%trio_ls(1, 1, Int_State%P_ze),             &
                  Int_State%lat1s,                                     &
                  Int_State%plnev, Int_State%plnod,                    &
                  Int_State%nvars, Int_State%ls_node, Int_State%latl2, &
                  Int_State%lslag, Int_State%latdims, Int_State%nvars, &
                  Int_State%four_gr1,                                  &
                  Int_State%ls_nodes,     Int_State%max_ls_nodes,      &
                  Int_State%lats_nodes,   Int_State%global_lats,       &
                  Int_State%lats_node,    Int_State%ipt_lats_node,     &
                  Int_State%lon_dims,     Int_State%dimg,              &
                  Int_State%lons_lat,     Int_State%londi,             &
                  Int_State%latl,         Int_State%jcap,              &
                  Int_State%TRIE_LS_SIZE, Int_State%TRIO_LS_SIZE,      &
                  Int_State%ls_dim,       Int_State%ls_max_node,       &
                  Int_State%MPI_R_MPI,    Int_State%levs,              &
                  Int_State%levh,         Int_State%latgd,             &
                  Int_State%nodes_comp,   Int_State%me_comp,           &
                  Int_State%MC_COMP)
!Finished Wave-grid conversion

!Convert the grid array (from gr1 to gr2) for zonal processing---A number of latitude on each task
 DO lan = 1, Int_State%lats_node
     lat     = Int_State%global_lats(Int_State%ipt_lats_node - 1 + lan)
     lon_dim = Int_State%lon_dims(lan)
     lon_lat = Int_State%lons_lat(lat)

     CALL FOUR2GRID_thread(Int_State%four_gr1(1, lan), Int_State%four_gr2(1, lan), &
                           lon_dim, lon_lat, Int_State%londi, Int_State%nvars,     &
                           lan, Int_State%me_comp)

! Adjust for q<0 grid points.
! If do spectral transform back to the spectral space, the results will be 
! changed and cannot be identical to the original spectral T and q fields.
!-------------------------------------------------------------------------
IF (QADJUST .eq. 1) THEN
     DO k = 1, Int_State%levs
         DO j = 1, lon_lat
             IF(Int_State%four_gr2(j + (ksr - 1) * lon_dim + lon_dim * (k - 1), lan) <= 0.0) &
                Int_State%four_gr2(j + (ksr - 1) * lon_dim + lon_dim * (k - 1), lan) = qmin

             Int_State%four_gr2(j + (kst - 1) * lon_dim + lon_dim * (k - 1), lan)=           &
                 Int_State%four_gr2(j + (kst - 1) * lon_dim + lon_dim * (k - 1), lan) /      &
                 (1.0 + fv * Int_State%four_gr2(j + (ksr - 1) * lon_dim + lon_dim * (k - 1), lan))
         END DO
     END DO
ENDIF

! converting ln(ps * 0.1) to (ps * 0.1).
!----------------------------------------------------------------------
     DO j = 1, lon_lat
          Int_State%four_gr2(j + (ksq - 1) * lon_dim, lan) &
              = EXP(Int_State%four_gr2(j + (ksq - 1) * lon_dim, lan))
     END DO
 END DO

! Put the Gaussian fileds into each control variable arrays.
! First, allocate all control variable arrays if not done yet
!-----------------------------------------------------------
 IF (.NOT.ASSOCIATED(Int_State%vor))                                            & 
 ALLOCATE(Int_State%vor   (Int_State%londi, Int_State%levs, Int_State%latdims))
 IF (.NOT.ASSOCIATED(Int_State%div))                                            & 
 ALLOCATE(Int_State%div   (Int_State%londi, Int_State%levs, Int_State%latdims))
 IF (.NOT.ASSOCIATED(Int_State%t))                                            & 
 ALLOCATE(Int_State%t     (Int_State%londi, Int_State%levs, Int_State%latdims))
 IF (.NOT.ASSOCIATED(Int_State%q))                                            & 
 ALLOCATE(Int_State%q     (Int_State%londi, Int_State%levs, Int_State%latdims))
 IF (.NOT.ASSOCIATED(Int_State%oz))                                            & 
 ALLOCATE(Int_State%oz    (Int_State%londi, Int_State%levs, Int_State%latdims))
 IF (.NOT.ASSOCIATED(Int_State%clw))                                            & 
 ALLOCATE(Int_State%clw   (Int_State%londi, Int_State%levs, Int_State%latdims))
 IF (.NOT.ASSOCIATED(Int_State%u))                                            & 
 ALLOCATE(Int_State%u     (Int_State%londi, Int_State%levs, Int_State%latdims))
 IF (.NOT.ASSOCIATED(Int_State%v))                                            & 
 ALLOCATE(Int_State%v     (Int_State%londi, Int_State%levs, Int_State%latdims))
 IF (.NOT.ASSOCIATED(Int_State%ps))                                            & 
 ALLOCATE(Int_State%ps    (Int_State%londi, Int_State%latdims))
 IF (.NOT.ASSOCIATED(Int_State%dpdlam))                                            & 
 ALLOCATE(Int_State%dpdlam(Int_State%londi, Int_State%latdims))
 IF (.NOT.ASSOCIATED(Int_State%dpdphi))                                            & 
 ALLOCATE(Int_State%dpdphi(Int_State%londi, Int_State%latdims))

! Set up the maximum and minumum initial values for max/min etc.
!-----------------------------------------------
 vorm    = -1e20
 divm    = -1e20
 tm      = -1e20
 qm      = -1e20
 ozm     = -1e20
 clwm    = -1e20
 um      = -1e20
 vm      = -1e20
 psm     = -1e20
 dpdlamm = -1e20
 dpdphim = -1e20

 vors    =  1e20
 divs    =  1e20
 ts      =  1e20
 qs      =  1e20
 ozs     =  1e20
 clws    =  1e20
 us      =  1e20
 vs      =  1e20
 pss     =  1e20
 dpdlams =  1e20
 dpdphis =  1e20

 DO j = 1, Int_State%lats_node

! lat is the real global latitue index number (1 for N pole).
! lon_lat is the number of longitudes at the "lat" latitude. Note that used grid is a reduce Gaussian grid.
!--------------------------------------------------------
     lat     = Int_State%global_lats(Int_State%ipt_lats_node - 1 + j)
     lon_lat = Int_State%lons_lat(lat)

     DO k = 1, Int_State%levs
         DO i = 1, lon_lat
             lon_dim                = Int_State%lon_dims(j)
             Int_State%vor(i, k, j) = Int_State%four_gr2(i + (ksz + k - 2) * lon_dim, j)
             Int_State%div(i, k, j) = Int_State%four_gr2(i + (ksd + k - 2) * lon_dim, j)
             Int_State%t  (i, k, j) = Int_State%four_gr2(i + (kst + k - 2) * lon_dim, j)
             Int_State%q  (i, k, j) = Int_State%four_gr2(i + (ksr + k - 2) * lon_dim, j)
             Int_State%oz (i, k, j) = Int_State%four_gr2(i + (kso + k - 2) * lon_dim, j)
             Int_State%clw(i, k, j) = Int_State%four_gr2(i + (ksc + k - 2) * lon_dim, j)
             Int_State%u  (i, k, j) = Int_State%four_gr2(i + (ksu + k - 2) * lon_dim, j)
             Int_State%v  (i, k, j) = Int_State%four_gr2(i + (ksv + k - 2) * lon_dim, j)

! Calculate the maximum and minumum of each variable fields.
!-----------------------------------------------------------
             vorm = MAX(vorm, Int_State%vor(i, k, j))
             divm = MAX(divm, Int_State%div(i, k, j))
             tm   = MAX(tm,   Int_State%t  (i, k, j))
             qm   = MAX(qm,   Int_State%q  (i, k, j))
             ozm  = MAX(ozm,  Int_State%oz (i, k, j))
             clwm = MAX(clwm, Int_State%clw(i, k, j))
             um   = MAX(um,   Int_State%u  (i, k, j))
             vm   = MAX(vm,   Int_State%v  (i, k, j))

             vors = MIN(vors, Int_State%vor(i, k, j))
             divs = MIN(divs, Int_State%div(i, k, j))
             ts   = MIN(ts,   Int_State%t  (i, k, j))
             qs   = MIN(qs,   Int_State%q  (i, k, j))
             ozs  = MIN(ozs,  Int_State%oz (i, k, j))
             clws = MIN(clws, Int_State%clw(i, k, j))
             us   = MIN(us,   Int_State%u  (i, k, j))
             vs   = MIN(vs,   Int_State%v  (i, k, j))
         END DO
     END DO

     DO i = 1, lon_lat
         lon_dim                = Int_State%lon_dims(j)
         Int_State%ps    (i, j) = Int_State%four_gr2(i + (ksq     - 1) * lon_dim, j)
         Int_State%dpdlam(i, j) = Int_State%four_gr2(i + (ksplam  - 1) * lon_dim, j)
         Int_State%dpdphi(i, j) = Int_State%four_gr2(i + (kspphi  - 1) * lon_dim, j)

! Calculate the maximum and minumum of each variable fields.
!-----------------------------------------------------------
         psm     = MAX(psm,     Int_State%ps    (i, j))
         dpdlamm = MAX(dpdlamm, Int_State%dpdlam(i, j))
         dpdphim = MAX(dpdphim, Int_State%dpdphi(i, j))

         pss     = MIN(pss,     Int_State%ps    (i, j))
         dpdlams = MIN(dpdlams, Int_State%dpdlam(i, j))
         dpdphis = MIN(dpdphis, Int_State%dpdphi(i, j))
     END DO
 END DO
 
!PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum VOR    = ', vorm, vors
!PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum DIV    = ', divm, divs
!PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum T      = ', tm,   ts  
!PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum q      = ', qm,   qs  
!PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum OZ     = ', ozm,  ozs 
!PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum CLW    = ', clwm, clws
!PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum U      = ', um,   us  
!PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum V      = ', vm,   vs  
!PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum Ps     = ', psm,  pss 
!PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum DpDlam = ', dpdlamm, dpdlams
!PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum DpDphi = ', dpdphim, dpdphis

! Now we get the Gaussian grid fields and can use them for reginal processing.
!--------------------------------------------------------------
 PRINT *, "RS_GLOBAL=",RS_GLOBAL
 IF (ABS(RS_GLOBAL).gt.0.999.and.ABS(RS_GLOBAL).lt.1.001.and.GRIDUSE.eq.2) THEN
!  Calculating regional rescaling factors fro KINETIC ENERGY at 500hPa

 IF (.NOT.ASSOCIATED(Int_State%KE_work))                                            & 
 ALLOCATE(Int_State%KE_work(Int_State%latmax, Int_State%Total_member))
 Int_State%KE_work = 0.0

!PRINT*,'In the Ensemble Coupler, BCST, latdims/Total_member=', Int_State%latdims, Int_State%Total_member
!PRINT*,'In the Ensemble Coupler, BCST, me/member_id=', Int_State%me, Int_State%member_id

!Identify the 500hPa level
 if (Int_State%levs .eq. 28) then
  k500=13 
 endif
 if (Int_State%levs .eq. 64) then
  k500=25 
 endif

!Calculate 500hPa Kineitic  Energy KE.
 DO j = 1, Int_State%lats_node
     lat     = Int_State%global_lats(Int_State%ipt_lats_node - 1 + j)
     lon_lat = Int_State%lons_lat(lat)
     keavg = 0.0
     DO i = 1, lon_lat
        keavg = keavg + Int_State%u(i, k500, j) * Int_State%u(i, k500, j)        &
                      + Int_State%v(i, k500, j) * Int_State%v(i, k500, j)
     END DO
        keavg = 0.5 * keavg / float(lon_lat)
        Int_State%KE_work(lat, Int_State%member_id(Int_State%mm1)) = keavg
 END DO

! Start global broadcast 
! Broadcasting the average KE over this latitude to other tasks
! PRINT*,'In the Ensemble Coupler, Start BCST, # of cpus', Int_State%nodes
 DO k = 1, Int_State%nodes    ! each task
  DO j = 1, Int_State%lats_node_global(k)    !each latitude
   CALL GEFS_bcst_global(Int_State%KE_work(Int_State%lats_global(j,k), Int_State%member_id(k)), k-1, rc1)
!  CALL ESMF_VMBarrier(vm_esmf, rc=rc1)
  END DO
 END DO
! PRINT*,'In the Ensemble Coupler, Finished BCST, '

! Print out KE_work
 IF(Int_State%me == 00) THEN
   PRINT *,'In the Ensemble Coupler, AFTER BCST, test KE_work'
  DO k = 1, Int_State%latmax
   write (*,'(1x,i4,f6.3,21f8.3)')  k,Int_State%slat_work(k),(Int_State%KE_work(k,l),l=1,Int_State%Total_member)
  END DO
 ENDIF

!DO k = 1, Int_State%latmax
!DO l = 1, Int_State%Total_member
! IF(Int_State%me == 00) THEN
!  PRINT*,'In the Ensemble Coupler, BCST, k/l/KEAVG=', k,l,Int_State%KE_work(k,l),Int_State%slat_work(k) 
! ENDIF
!END DO
!END DO

!Calculating regiobnal re-scaling factors from the KE_work array.
!PRINT *, 'DHHHTEST', RS_GLOBAL, GRIDUSE, KEMAX
!IF (ABS(RS_GLOBAL).gt.0.999.and.ABS(RS_GLOBAL).lt.1.001.and.GRIDUSE.eq.2) THEN
   CALL GET_SCALING_FACTORS(Int_State%KE_work,Int_State%latmax,Int_State%Total_member,Int_State%slat_worK,   &
                            KEMAX,KER,Int_State%factor1_work,nreg,slat1,slat2)
   IF(Int_State%me == 35) THEN
    PRINT*,'In the Ensemble Coupler, BCST, itest KER', Int_State%Cpl_Run_Calling_Number
    DO k = 1, nreg
     write (*,'(1x,A4,i4,15f8.3)')  'KER ',k,(KER(k,l),l=1,Int_State%Total_member) 
    END DO
    PRINT*,'In the Ensemble Coupler, BCST, itest FAW factor1_work',Int_State%Cpl_Run_Calling_Number
    DO k = 1, nreg
     write (*,'(1x,A4,i4,15f8.3)')  'FAW ',k,(Int_State%factor1_work(k,l),l=1,Int_State%Total_member)
    END DO
   ENDIF
!ENDIF

!   DO k = 1, nreg
!   DO l = 1, Int_State%Total_member
!    IF(Int_State%me == 00) THEN
!     PRINT*,'AFTER calling GET_SCALING_Factors region/member/KEMAX/KER/factor=',  &
!          k,l,KEMAX(k),KER(k,l),Int_State%factor1_work(k,l)
!    ENDIF
!   END DO
!   END DO

 DEALLOCATE(Int_State%KE_work)

 ENDIF !! (ABS(RS_GLOBAL).gt.0.999.and.ABS(RS_GLOBAL).lt.1.001.and.GRIDUSE.eq.2) 
!end of the if_block of 500hPa KE base rescaling factor calculation

! Do the Rescaling for all model variables except ps.
!------------------------------------------------------------------------------
! PRINT *, "RS_GLOBAL=",RS_GLOBAL

    imem = Int_State%member_id(Int_State%me)
 DO j = 1, Int_State%lats_node
   lat     = Int_State%global_lats(Int_State%ipt_lats_node - 1 + j)
   lon_lat = Int_State%lons_lat(lat)
! assigning rescaling factor, 3-belt or latitude-dependent
   IF (ABS(RS_GLOBAL).gt.0.999.and.ABS(RS_GLOBAL).lt.1.001.and.GRIDUSE.eq.2) THEN 
     if (Int_State%slat_work(lat).ge.slat1) then
      ireg = 1
     elseif (Int_State%slat_work(lat).ge.slat2) then
      ireg = 2
     else
      ireg = 3
     endif
! DHOU 12/10/2007, KE based, 3-belt rescaling
     RS = RS_GLOBAL * Int_State%factor1_work(ireg,Int_State%member_id(Int_State%mm1))
   ELSE
! DHOU 12/10/2007, Latitude-Julian_Day based rescaling
!    RS = RS_GLOBAL * ( 1.0 + Int_State%PARM3(1) * asin(Int_State%slat_work(lat)) * &
!        2.0/pi * cos( (Jul_Day-1)*pi/182.0 ) )    ! linear function of latitude
     RS = RS_GLOBAL * ( 1.0 + Int_State%PARM3(1) * Int_State%slat_work(lat) *       &
         cos( (Jul_Day-1)*pi/182.0 ) )  ! linear function of sin(latitude)
     IF(Int_State%me == 00) THEN
       PRINT *,'RS=', j,lat,Int_State%slat_work(lat),RS,Jul_Day
     ENDIF
   ENDIF

! Applying the rescaling factor
     DO k = 1, Int_State%levs
       DO i = 1, lon_lat
        lon_dim                = Int_State%lon_dims(j)
        Int_State%four_gr2(i + (ksz + k - 2) * lon_dim, j) = Int_State%vor(i, k, j)*RS
        Int_State%four_gr2(i + (ksd + k - 2) * lon_dim, j) = Int_State%div(i, k, j)*RS
        Int_State%four_gr2(i + (kst + k - 2) * lon_dim, j) = Int_State%t  (i, k, j)*RS
        Int_State%four_gr2(i + (ksr + k - 2) * lon_dim, j) = Int_State%q  (i, k, j)*RS
        Int_State%four_gr2(i + (kso + k - 2) * lon_dim, j) = Int_State%oz (i, k, j)*RS
        Int_State%four_gr2(i + (ksc + k - 2) * lon_dim, j) = Int_State%clw(i, k, j)*RS
        Int_State%four_gr2(i + (ksu + k - 2) * lon_dim, j) = Int_State%u  (i, k, j)*RS
        Int_State%four_gr2(i + (ksv + k - 2) * lon_dim, j) = Int_State%v  (i, k, j)*RS
       END DO
     END DO

     DO i = 1, lon_lat
       lon_dim                = Int_State%lon_dims(j)
       Int_State%four_gr2(i + (ksplam  - 1) * lon_dim, j) = Int_State%dpdlam(i, j)*RS
       Int_State%four_gr2(i + (kspphi  - 1) * lon_dim, j) = Int_State%dpdphi(i, j)*RS
       Int_State%four_gr2(i + (ksq     - 1) * lon_dim, j) = LOG(Int_State%ps(i, j))*RS
! NB: For surface pressure, convert to ln(ps * 0.1) before rescaling 
     END DO
 END DO   !(j = 1, Int_State%lats_node)

! RE-scaling Done!
! Transforming back from the Gaussian grid fields to the spectral space fields.
! and do the GLOBAL Rescaling for all model variables except ps.
!------------------------------------------------------------------------------
 DO lan = 1, Int_State%lats_node
     lat     = Int_State%global_lats(Int_State%ipt_lats_node - 1 + lan)
     lon_dim = Int_State%lon_dims(lan)
     lon_lat = Int_State%lons_lat(lat)

     CALL GRID2FOUR_thread(Int_State%four_gr2(1, lan), Int_State%four_gr1(1, lan), &
                           lon_dim, lon_lat, Int_State%londi, Int_State%nvars)
 END DO
 
 CALL GEFS_four2fln(Int_State%lslag,        Int_State%latdims,    Int_State%nvars,         &
                    Int_State%nvars,        Int_State%four_gr1,   Int_State%ls_nodes,      &
                    Int_State%max_ls_nodes, Int_State%lats_nodes, Int_State%global_lats,   &
                    Int_State%lon_dims,     Int_State%lats_node,  Int_State%ipt_lats_node, &
                    Int_State%dimg,         Int_State%lat1s,      Int_State%londi,         &
                    Int_State%latl,         Int_State%latl2,                               &
                    Int_State%trie_ls(1, 1, Int_State%P_ze),                               &
                    Int_State%trio_ls(1, 1, Int_State%P_ze),                               &
                    Int_State%plnew,        Int_State%plnow,      Int_State%ls_node,       &
                    Int_State%jcap,         Int_State%jintmx,                              &
                    Int_State%TRIE_LS_SIZE, Int_State%TRIO_LS_SIZE,                        &
                    Int_State%ls_dim,       Int_State%ls_max_node,                         &
                    Int_State%MPI_R_MPI,    Int_State%levs,       Int_State%latgd,         &
                    Int_State%nodes_comp,   Int_State%me_comp,    Int_State%MC_COMP)

 DO k = 1, Int_State%levs
     CALL GEFS_uveodz(Int_State%trie_ls(1, 1, Int_State%P_ze  + k - 1), &
                      Int_State%trio_ls(1, 1, Int_State%P_di  + k - 1), &
                      Int_State%trie_ls(1, 1, Int_State%P_uln + k - 1), &
                      Int_State%trio_ls(1, 1, Int_State%P_vln + k - 1), &
                      Int_State%epse,         Int_State%epso,           &
                      Int_State%ls_node,      Int_State%jcap,           &
                      Int_State%TRIE_LS_SIZE, Int_State%TRIO_LS_SIZE,   &
                      Int_State%ls_dim,       Int_State%ls_max_node,    &
                      rerth)

     CALL GEFS_uvoedz(Int_State%trio_ls(1, 1, Int_State%P_ze  + k - 1), &
                      Int_State%trie_ls(1, 1, Int_State%P_di  + k - 1), &
                      Int_State%trio_ls(1, 1, Int_State%P_uln + k - 1), &
                      Int_State%trie_ls(1, 1, Int_State%P_vln + k - 1), &
                      Int_State%epse,         Int_State%epso,           &
                      Int_State%ls_node,      Int_State%jcap,           &
                      Int_State%TRIE_LS_SIZE, Int_State%TRIO_LS_SIZE,   &
                      Int_State%ls_dim,       Int_State%ls_max_node,    &
                      rerth)
 END DO

!PRINT *, "In the Ensemble Coupler, Step2, dpdlam ",                  &
!Int_State%trio_ls(1, 1, Int_State%P_dlam),Int_State%trie_ls(1, 1, Int_State%P_dlam)
!PRINT *, "In the Ensemble Coupler, Step2, dpdphi ",                  &
!Int_State%trio_ls(1, 1, Int_State%P_dphi),Int_State%trie_ls(1, 1, Int_State%P_dphi)

!DHOU 09/06/2007 shift the arrays back to original position if it is shifted
IF (GRIDUSE .eq. 1 ) THEN
 LSHIFT = Int_State%P_q - Int_State%P_qm  
 DO l = int_State%P_ze, Int_State%P_q
     DO j = 1, 2
         DO i = 1, Int_State%TRIE_LS_SIZE
             atem = Int_State%trie_ls(i, j, l-LSHIFT) 
             Int_State%trie_ls( i, j, l-LSHIFT) = Int_State%trie_ls( i, j, l)
             Int_State%trie_ls( i, j, l) = atem
         END DO
         DO i = 1, Int_State%TRIO_LS_SIZE
             atem = Int_State%trio_ls(i, j, l-LSHIFT)
             Int_State%trio_ls( i, j, l-LSHIFT) = Int_State%trio_ls( i, j, l)
             Int_State%trio_ls( i, j, l) = atem
         END DO
     END DO
 END DO
ENDIF

! Replace State S2 with 0.5*(S1+S2) or Replace State S1 with S2
IF (USES1 .eq. 3 ) THEN
 LSHIFT = Int_State%P_q - Int_State%P_qm  
 DO l = int_State%P_ze, Int_State%P_q
     DO j = 1, 2
         DO i = 1, Int_State%TRIE_LS_SIZE
              Int_State%trie_ls(i, j, l) =                                     &   
         0.5*(Int_State%trie_ls(i, j, l-LSHIFT) + Int_State%trie_ls(i, j, l) )
         END DO
         DO i = 1, Int_State%TRIO_LS_SIZE
              Int_State%trio_ls(i, j, l) =                                     &   
         0.5*(Int_State%trio_ls(i, j, l-LSHIFT) + Int_State%trio_ls(i, j, l) )
         END DO
     END DO
 END DO
ELSEIF (USES1 .eq. 2 ) THEN
 LSHIFT = Int_State%P_q - Int_State%P_qm  
 DO l = int_State%P_ze, Int_State%P_q
     DO j = 1, 2
         DO i = 1, Int_State%TRIE_LS_SIZE
             Int_State%trie_ls( i, j, l-LSHIFT) = Int_State%trie_ls( i, j, l)
         END DO
         DO i = 1, Int_State%TRIO_LS_SIZE
             Int_State%trio_ls( i, j, l-LSHIFT) = Int_State%trio_ls( i, j, l)
         END DO
     END DO
 END DO
ENDIF

!DHOU, 09/06/2007  save the trio-ls and trie_ls back to trieo_ls_max, so
!                   that the change (if any) can be memorized. 
 k = 1
 DO l = 1, Int_State%TRIEO_VERTICAL_SIZE
     DO j = 1, 2
         DO i = 1, Int_State%TRIE_LS_SIZE
             Int_State%trieo_ls_max(k) = Int_State%trie_ls(i, j, l) 
             k = k + 1
         END DO
         DO i = 1, Int_State%TRIO_LS_SIZE
             Int_State%trieo_ls_max(k) = Int_State%trio_ls(i, j, l) 
             k = k + 1
         END DO
     END DO
 END DO
   
!PRINT *, "In the Ensemble Coupler, Step2, dpdlam ",                  &
!Int_State%trio_ls(1, 1, Int_State%P_dlam),Int_State%trie_ls(1, 1, Int_State%P_dlam)
!PRINT *, "In the Ensemble Coupler, Step2, dpdphi ",                  &
!Int_State%trio_ls(1, 1, Int_State%P_dphi),Int_State%trie_ls(1, 1, Int_State%P_dphi)
!PRINT *, "In the Ensemble Coupler, Step2, ZZZZZ ",                  &
!Int_State%trio_ls(1, 1, Int_State%P_dlam),Int_State%trie_ls(1, 4, Int_State%P_ze),Int_State%trio_ls(1, 5, Int_State%P_ze)
!PRINT *, "In the Ensemble Coupler, Step2, ZZZDD ",                  &
!Int_State%trio_ls(1, 1, Int_State%P_dlam),Int_State%trie_ls(1, 4, Int_State%P_di),Int_State%trio_ls(1, 5, Int_State%P_di)
!PRINT *, "In the Ensemble Coupler, Step2, ZZZUU ",                  &
!Int_State%trio_ls(1, 1, Int_State%P_dlam),Int_State%trie_ls(1, 4, Int_State%P_uln),Int_State%trio_ls(1, 5, Int_State%P_uln)
!PRINT *, "In the Ensemble Coupler, Step2, ZZZVV ",                  &
!Int_State%trio_ls(1, 1, Int_State%P_dlam),Int_State%trie_ls(1, 4, Int_State%P_vln),Int_State%trio_ls(1, 5, Int_State%P_vln)

 IF(rcfinal == ESMF_SUCCESS) THEN
     PRINT*, "PASS: GEFS_Sto_Per_Scheme_Step2_m."
 ELSE
     PRINT*, "FAIL: GEFS_Sto_Per_Scheme_Step2_m."
 END IF

!IF(PRESENT(rc)) THEN
     rc = rcfinal
!END IF

 END SUBROUTINE GEFS_Sto_Per_Scheme_Step2

 SUBROUTINE GET_SCALING_FACTORS(KE,nlat,nmember,slat,KEMAX,KER,factor1,nregion,slat1,slat2)
 USE machine,  ONLY: kind_evod
 INTEGER nlat,nmember,nregion
 REAL(KIND = kind_evod)                      :: KE(nlat,nmember)
 REAL(KIND = kind_evod)                      :: factor1(nregion,nmember) 
 REAL(KIND = kind_evod)                      :: slat(nlat),slat1,slat2 
 REAL(KIND = kind_evod)                      :: KEMAX(nregion) 
 REAL(KIND = kind_evod)                      :: KER(nregion,nmember) 
 REAL(KIND = kind_evod)                      :: WEIGHT(3) 
 REAL(KIND = kind_evod)                      :: coslat
 INTEGER i,j,k

 DO k=1,nmember
  DO j=1,nregion
    KER(j,k)=0.0
  ENDDO
 ENDDO
 DO k=1,nmember-1
  DO j=1,nregion
    WEIGHT(j)=0.0
  ENDDO 
  DO j=1,nlat
   coslat=sqrt(1-slat(j)**2)
   IF (slat(j).gt.slat1) then
    KER(1,k)=KER(1,k)+KE(j,k)*coslat
    WEIGHT(1)=WEIGHT(1)+coslat
   ELSEIF (slat(j).gt.slat2) then
    KER(2,k)=KER(2,k)+KE(j,k)*coslat
    WEIGHT(2)=WEIGHT(2)+coslat
   ELSE
    KER(3,k)=KER(3,k)+KE(j,k)*coslat
    WEIGHT(3)=WEIGHT(3)+coslat
   ENDIF
  ENDDO
  DO j=1,nregion
    IF (WEIGHT(j).lt.1.0E-5) THEN
     PRINT *, 'Weight=0 In GET_SCALING_FACTORS, forced to stop! for region/member',j,k
     STOP
    ENDIF
    KER(j,k)=KER(j,k)/WEIGHT(j)
  ENDDO 
  DO j=1,nregion
   if (KER(j,k).gt.KEMAX(j)) THEN
    factor1(j,k)=KEMAX(j)/KER(j,k)
   else
    factor1(j,k)=1.0
   endif
  ENDDO
 ENDDO

 RETURN
 END

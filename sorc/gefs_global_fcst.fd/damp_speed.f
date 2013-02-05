      SUBROUTINE damp_speed(DIVE,VORE,TEME,RTE,NDEXEV,
     X                      DIVO,VORO,TEMO,RTO,NDEXOD,
     X                      SL,SPDMAX,DELTIM,
     X                      LS_NODE)
!
      use resol_def
      use layout1
      use physcons, rerth => con_rerth
      IMPLICIT NONE
!
      REAL(KIND=KIND_EVOD)   DIVE(LEN_TRIE_LS,2)
      REAL(KIND=KIND_EVOD)   VORE(LEN_TRIE_LS,2)
      REAL(KIND=KIND_EVOD)   TEME(LEN_TRIE_LS,2)
      REAL(KIND=KIND_EVOD)    RTE(LEN_TRIE_LS,2,LEVS,ntrac)
      INTEGER              NDEXEV(LEN_TRIE_LS)
!
      REAL(KIND=KIND_EVOD)   DIVO(LEN_TRIO_LS,2)
      REAL(KIND=KIND_EVOD)   VORO(LEN_TRIO_LS,2)
      REAL(KIND=KIND_EVOD)   TEMO(LEN_TRIO_LS,2)
      REAL(KIND=KIND_EVOD)    RTO(LEN_TRIO_LS,2,LEVS,ntrac)
      INTEGER              NDEXOD(LEN_TRIO_LS)
!
      REAL(KIND=KIND_EVOD)     SL(LEVS)
      REAL(KIND=KIND_EVOD) SPDMAX
      REAL(KIND=KIND_EVOD) DELTIM
cc
      integer              ls_node(ls_dim,3)
cc
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
      INTEGER              IT,KD,KU,L,LOCL,N,N0
!
      INTEGER              INDEV
      INTEGER              INDOD
      integer              indev1,indev2
      integer              indod1,indod2
      REAL(KIND=KIND_EVOD) ALFA,ALFADT,BETA,COEF,factor,RK,RNCRIT,SF,TK
!
      REAL(KIND=KIND_EVOD) CONS0,CONS1,CONS1P009     !CONSTANT
      REAL(KIND=KIND_EVOD) CONS2,CONS2P5             !CONSTANT
!
      INTEGER              INDLSEV,JBASEV
      INTEGER              INDLSOD,JBASOD
!
      include 'function2'
!
!
      CALL countperf(0,13,0.)
!!
      CONS0     = 0.D0        !CONSTANT
      CONS1     = 1.D0        !CONSTANT
      CONS1P009 = 1.009D0     !CONSTANT
      CONS2     = 2.D0        !CONSTANT
      CONS2P5   = 2.5D0       !CONSTANT
!
!
      ALFA=CONS2P5                    !CONSTANT
      BETA=RERTH*CONS1P009/DELTIM     !CONSTANT
      ALFADT=ALFA*DELTIM/RERTH
!
!
! ......................................................................
!
!
         RNCRIT=BETA/SPDMAX
         IF (RNCRIT.LT.JCAP) THEN
!
            COEF=ALFADT*SPDMAX
!!          KD=MAX(K-1,1)
!!          KU=MIN(K+1,LEVS)
!!!!        SF=SL(K)/(SL(KU)-SL(KD))/SQRT(2.)        !CONSTANT
!!          SF=SL(K)/(SL(KU)-SL(KD))/SQRT(CONS2)     !CONSTANT
!!!!        TK=(TEME(1,1,KU)-TEME(1,1,KD))*SF
!!!!        TK=(TEME1(KU)-TEME1(KD))*SF
!!
!!!!        DO L = 0, JCAP
            DO LOCL=1,LS_MAX_NODE
                    l=ls_node(locl,1)
               jbasev=ls_node(locl,2)
               IF (L.EQ.0) THEN
                                 N0=2
                           ELSE
                                 N0=L
               ENDIF
               indev1 = indlsev(N0,L)
               if (mod(L,2).eq.mod(jcap+1,2)) then
                  indev2 = indlsev(jcap+1,L)
               else
                  indev2 = indlsev(jcap  ,L)
               endif
!!!!           DO N = N0, JCAP+1, 2
               DO INDEV = indev1 , indev2
!
                  IF    (NDEXEV(INDEV).GT.RNCRIT)       THEN
                 factor=cons1/(cons1+((NDEXEV(INDEV) -  RNCRIT)*COEF))
!
                      DIVE(INDEV,1)=DIVE(INDEV,1)*factor
                      DIVE(INDEV,2)=DIVE(INDEV,2)*factor
 
                      VORE(INDEV,1)=VORE(INDEV,1)*factor
                      VORE(INDEV,2)=VORE(INDEV,2)*factor
 
                      TEME(INDEV,1)=TEME(INDEV,1)*factor
                      TEME(INDEV,2)=TEME(INDEV,2)*factor
!
                  ENDIF
               ENDDO
            ENDDO
!
! ......................................................................
!
!!!!        DO L = 0, JCAP
            DO LOCL=1,LS_MAX_NODE
                    l=ls_node(locl,1)
               jbasod=ls_node(locl,3)
               indod1 = indlsod(L+1,L)
               if (mod(L,2).eq.mod(jcap+1,2)) then
                  indod2 = indlsod(jcap  ,L)
               else
                  indod2 = indlsod(jcap+1,L)
               endif
!!!!           DO N = L+1, JCAP+1, 2
               DO INDOD = indod1 , indod2
!
                  IF    (NDEXOD(INDOD).GT.RNCRIT)       THEN
                 factor=cons1/(cons1+((NDEXOD(INDOD) -  RNCRIT)*COEF))
!
                      DIVO(INDOD,1)=DIVO(INDOD,1)*factor
                      DIVO(INDOD,2)=DIVO(INDOD,2)*factor
 
                      VORO(INDOD,1)=VORO(INDOD,1)*factor
                      VORO(INDOD,2)=VORO(INDOD,2)*factor
 
                      TEMO(INDOD,1)=TEMO(INDOD,1)*factor
                      TEMO(INDOD,2)=TEMO(INDOD,2)*factor
!
                  ENDIF
               ENDDO
            ENDDO
!
! ......................................................................
!
            DO IT=1,NTRAC
!!!!           DO L = 0, JCAP
               DO LOCL=1,LS_MAX_NODE
                       l=ls_node(locl,1)
                  jbasev=ls_node(locl,2)
                  IF (L.EQ.0) THEN
                                    N0=2
                              ELSE
                                    N0=L
                  ENDIF
                  indev1 = indlsev(N0,L)
                  if (mod(L,2).eq.mod(jcap+1,2)) then
                     indev2 = indlsev(jcap+1,L)
                  else
                     indev2 = indlsev(jcap  ,L)
                  endif
!!!!              DO N = N0, JCAP+1, 2
                  DO INDEV = indev1 , indev2
!
                     IF   (NDEXEV(INDEV).GT.RNCRIT)       THEN
                 factor=cons1/(cons1+((NDEXEV(INDEV) -  RNCRIT)*COEF))
!
                        RTE(INDEV,1,1,IT)=RTE(INDEV,1,1,IT)*factor
                        RTE(INDEV,2,1,IT)=RTE(INDEV,2,1,IT)*factor
!
                     ENDIF
                  ENDDO
               ENDDO
!
! ......................................................................
!
!!!!           DO L = 0, JCAP
               DO LOCL=1,LS_MAX_NODE
                       l=ls_node(locl,1)
                  jbasod=ls_node(locl,3)
                  indod1 = indlsod(L+1,L)
                  if (mod(L,2).eq.mod(jcap+1,2)) then
                     indod2 = indlsod(jcap  ,L)
                  else
                     indod2 = indlsod(jcap+1,L)
                  endif
!
!!!!              DO N = L+1, JCAP+1, 2
                  DO INDOD = indod1 , indod2
!
                     IF   (NDEXOD(INDOD).GT.RNCRIT)       THEN
                 factor=cons1/(cons1+((NDEXOD(INDOD) -  RNCRIT)*COEF))
!
                        RTO(INDOD,1,1,IT)=RTO(INDOD,1,1,IT)*factor
                        RTO(INDOD,2,1,IT)=RTO(INDOD,2,1,IT)*factor
!
                     ENDIF
                  ENDDO
               ENDDO
!
            ENDDO
         ENDIF
!
!
      CALL countperf(1,13,0.)
!!
      RETURN
      END

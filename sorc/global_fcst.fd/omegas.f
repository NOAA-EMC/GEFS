      SUBROUTINE OMEGAST3(NJEFF,NSIZE_AR,NZ,
     1      DPHI,DLAM,UG,VG,DG,DEL,RCL,VVEL,PS,SL)
CFPP$ NOCONCUR R
C....   CODE LIFTED FROM POST (MCP1840) JUN 88--COMPUTES VVEL (CB/SEC)
C....    INPUT PS IN CB,OUTPUT VVEL IN CB/SEC
C....   DO LOOPS ALTERED FOR BETTER VECTORIZATION POSSIBILITIES..K.A.C.
cc
      USE MACHINE , ONLY : kind_phys
      use resol_def
      implicit none

!!
      integer              i,k,le,nz,NSIZE_AR,NJEFF
      real(kind=kind_phys) rcl
cc
      real(kind=kind_phys) DPHI(NSIZE_AR),DLAM(NSIZE_AR),PS(NSIZE_AR)
      real(kind=kind_phys)
     1 CG(NJEFF,NZ),UG(NSIZE_AR,NZ),VG(NSIZE_AR,NZ),
     2 DG(NSIZE_AR,NZ),DEL(NZ),SL(NZ)
C...   VVEL CONTAINS OMEGA IN LAYERS ON RETURN FROM SUBROUTINE...
      real(kind=kind_phys) VVEL(NSIZE_AR,NZ)
      real(kind=kind_phys) DB(NJEFF,NZ),CB(NJEFF,NZ),
     &                     DOT(NJEFF,NZ+1)
!!
      DO 1 K=1,NZ+1
CC      DO 1 LO=1,NX
        DO 49 i=1,NJEFF
          DOT(i,K) = 0.E0
   49 CONTINUE
    1 CONTINUE
C...  COMPUTE C=V(TRUE)*DEL(LN(PS)).DIVIDE BY COS FOR DEL COS FOR V
CC    DO 3 LO=1,NX
      DO 48 i=1,NJEFF
        DPHI(i)=DPHI(i)*RCL
        DLAM(i)=DLAM(i)*RCL
   48 CONTINUE
CC  3 CONTINUE
      DO 5 LE=1,NZ
        DO 50 i=1,NJEFF
CC      DO 4 LO=1,NX
          CG(i,LE)=UG(i,LE)*DLAM(i)+VG(i,LE)*DPHI(i)
CC  4   CONTINUE
   50 CONTINUE
    5 CONTINUE
CC    DO 10 LO=1,NX
      DO 51 i=1,NJEFF
        DB(i,1)=DEL(1)*DG(i,1)
        CB(i,1)=DEL(1)*CG(i,1)
   51 CONTINUE
CC 10 CONTINUE
!!
      DO 6 LE=1,NZ-1
        DO 52 i=1,NJEFF
CC      DO 6 LO=1,NX
          DB(i,LE+1)=DB(i,LE)+DEL(LE+1)*DG(i,LE+1)
          CB(i,LE+1)=CB(i,LE)+DEL(LE+1)*CG(i,LE+1)
   52 CONTINUE
    6 CONTINUE
!!
C...    SIGMA DOT COMPUTED ONLY AT INTERIOR INTERFACES
      DO 7 K=1,NZ-1
        DO 53 i=1,NJEFF
CC      DO 7 LO=1,NX
          DOT(i,K+1)=DOT(i,K)+DEL(K)
     1               *(DB(i,NZ)+CB(i,NZ)-DG(i,K)-CG(i,K))
   53 CONTINUE
    7 CONTINUE
!!
      DO 8 K=1,NZ
CC      DO 8 LO=1,NX
        DO 54 i=1,NJEFF
          VVEL(i,K)=  SL(K)*(CG(i,K)-CB(i,NZ)-DB(i,NZ))-
     1                0.5*(DOT(i,K+1)+DOT(i,K))
          VVEL(i,K)=VVEL(i,K)*PS(i)
CCC       VVEL(i,K)=VVEL(i,K)*PS(i)*10.
   54 CONTINUE
    8 CONTINUE
!!
      RETURN
      END

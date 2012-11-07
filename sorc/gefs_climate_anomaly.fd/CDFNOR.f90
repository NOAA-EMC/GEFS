!===================================================== CDFNOR.FOR
!     DOUBLE PRECISION FUNCTION CDFNOR(X,PARA)
FUNCTION CDFNOR(X,PARA)
!***********************************************************************
!*                                                                     *
!*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
!*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
!*                                                                     *
!*  J. R. M. HOSKING                                                   *
!*  IBM RESEARCH DIVISION                                              *
!*  T. J. WATSON RESEARCH CENTER                                       *
!*  YORKTOWN HEIGHTS                                                   *
!*  NEW YORK 10598, U.S.A.                                             *
!*                                                                     *
!*  VERSION 3            AUGUST 1996                                   *
!*                                                                     *
!*  Modified by Bo Cui:  Dec 14, 2006                                  *
!*     - converted from f77 to f90                                        *
!*     - change function name from ERF to ERFF because ERF works for   *
!*       f77 but not for f90, the error information is: The intrinsic  *
!*       procedure erf (with these arguments) is not permitted by the  *
!*       Fortran 90 standard                                           *
!*                                                                     *
!***********************************************************************
!
!  DISTRIBUTION FUNCTION OF THE STANDARD NORMAL DISTRIBUTION
!
!  OTHER ROUTINES USED: DERF
!
!     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!     DOUBLE PRECISION PARA(2)

IMPLICIT NONE

REAL    X
REAL    HALF
REAL    PARA(2)
REAL    CDFNOR
REAL    RTHALF
REAL    ERFF       

!DATA HALF/0.5D0/,RTHALF/0.70710 67811 86547 524D0/
DATA HALF/0.5D0/,RTHALF/0.707106781186547524D0/

!CDFNOR=HALF+HALF*DERF((X-PARA(1))/PARA(2)*RTHALF)

CDFNOR=HALF+HALF*ERFF((X-PARA(1))/PARA(2)*RTHALF)

RETURN
END

!===================================================== ERF.FOR
!     DOUBLE PRECISION FUNCTION ERF(X)
FUNCTION ERFF(X)
!***********************************************************************
!*                                                                     *
!*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
!*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
!*                                                                     *
!*  J. R. M. HOSKING                                                   *
!*  IBM RESEARCH DIVISION                                              *
!*  T. J. WATSON RESEARCH CENTER                                       *
!*  YORKTOWN HEIGHTS                                                   *
!*  NEW YORK 10598, U.S.A.                                             *
!*                                                                     *
!*  VERSION 3     AUGUST 1996                                          *
!*                                                                     *
!*  Modified by Bo Cui:  Dec 14, 2006                                  *
!*     - converted from f77 to f90                                        *
!*     - change function name from ERF to ERFF because ERF works for   *
!*       f77 but not for f90, the error information is: The intrinsic  *
!*       procedure erf (with these arguments) is not permitted by the  *
!*       Fortran 90 standard
!*     - define all the arries using REAL                                           *
!*                                                                     *
!*                                                                     *
!***********************************************************************
!
!  ERROR FUNCTION
!
!  BASED ON ALGORITHM 5666, J.F.HART ET AL. (1968) 'COMPUTER
!  APPROXIMATIONS'
!
!  ACCURATE TO 15 DECIMAL PLACES
!
IMPLICIT NONE
!IMPLICIT DOUBLE PRECISION (A-H, O-Z)

REAL ZERO,ONE,TWO,THREE,FOUR,P65
REAL P0,P1,P2,P3,P4,P5,P6,Q0,Q1,Q2,Q3,Q4,Q5,Q6,Q7,C1,C2,BIG
REAL XX,EXPNTL,ZZ,ERFF,X,CRIT

DATA ZERO/0D0/,ONE/1D0/,TWO/2D0/,THREE/3D0/,FOUR/4D0/,P65/0.65D0/
!
!         COEFFICIENTS OF RATIONAL-FUNCTION APPROXIMATION
!
DATA P0,P1,P2,P3,P4,P5,P6/0.2202068679123761D3,0.2212135961699311D3,0.1120792914978709D3,0.3391286607838300D2,0.6373962203531650D1,0.7003830644436881D0,0.3526249659989109D-1/

DATA Q0,Q1,Q2,Q3,Q4,Q5,Q6,Q7/0.4404137358247522D3,0.7938265125199484D3,0.6373336333788311D3,0.2965642487796737D3,0.8678073220294608D2,0.1606417757920695D2,0.1755667163182642D1,0.8838834764831844D-1/
!
!         C1 IS SQRT(2), C2 IS SQRT(2/PI)
!         BIG IS THE POINT AT WHICH ERFF=1 TO MACHINE PRECISION
!
DATA C1/1.414213562373095D0/
DATA C2/7.978845608028654D-1/
DATA BIG/6.25D0/,CRIT/5D0/
!
ERFF=ZERO
IF(X.EQ.ZERO)RETURN
XX=ABS(X)
IF(XX.GT.BIG)GOTO 20
EXPNTL=EXP(-X*X)
ZZ=ABS(X*C1)
IF(XX.GT.CRIT)GOTO 10
ERFF=EXPNTL*((((((P6*ZZ+P5)*ZZ+P4)*ZZ+P3)*ZZ+P2)*ZZ+P1)*ZZ+P0)/(((((((Q7*ZZ+Q6)*ZZ+Q5)*ZZ+Q4)*ZZ+Q3)*ZZ+Q2)*ZZ+Q1)*ZZ+Q0)
IF(X.GT.ZERO)ERFF=ONE-TWO*ERFF
IF(X.LT.ZERO)ERFF=TWO*ERFF-ONE
RETURN
!
10 ERFF=EXPNTL*C2/(ZZ+ONE/(ZZ+TWO/(ZZ+THREE/(ZZ+FOUR/(ZZ+P65)))))
IF(X.GT.ZERO)ERFF=ONE-ERFF
IF(X.LT.ZERO)ERFF=ERFF-ONE
RETURN
!
20 ERFF=ONE
IF(X.LT.ZERO)ERFF=-ONE
RETURN
END

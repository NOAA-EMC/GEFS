      FUNCTION ISRCHNE(N,X,INCX,TARGET)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!               .      .    .                                       .
!SUBPROGRAM:    ISRCHNE     Searches vector given a target
!  PRGMMR: gilbert          ORG: W/NP11    DATE: 99-02-11
!
!ABSTRACT: Searches a vector for the first element 
!          not equal to a target
!
!PROGRAM HISTORY LOG:
!  99-02-11  Gilbert
!
!USAGE:    index=ISRCHNE(n, x, incx, target)
!  INPUT ARGUMENT LIST:
!    n        - Number of elements to be searched
!    x        - Real or integer array of dimension (n-1) * |incx| + 1.
!               Array x contains the vector to be searched.
!    incx     - Increment between elements of the searched array.
!    target   - Value for which to search in the array.
!
!  OUTPUT VALUE
!    index  - Index of the first element equal or not equal to target.  If
!             target is not found, n+1 is returned.  If n <= 0, 0 is
!             returned.
!
!REMARKS: This code and documentation was taken directly from the 
!         man page for routine ISRCHNE on a CRAY UNICOS system.
!
!ATTRIBUTES:
!  LANGUAGE: Fortran
!
!$$$
      INTEGER X(*), TARGET
      J=1
      ISRCHNE=0
      IF(N.LE.0) RETURN
      IF(INCX.LT.0) J=1-(N-1)*INCX
      DO 100 I=1,N
         IF(X(J).NE.TARGET) EXIT
         J=J+INCX
  100 CONTINUE
  200 ISRCHNE=I
      RETURN
      END


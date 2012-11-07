 subroutine stop1
 include 'mpif.h'
 integer ierr
 call mpi_finalize(ierr)
 stop
 return
 end

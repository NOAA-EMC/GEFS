! From George V
!
! ifort -o ../../exec/zeus-poe zeus-poe.f90 -lmpi
! mpiexec_mpt -np $NTASKS   ./zeuspoe
!
  program zeuspoe
    use mpi
    character*232,  allocatable :: cmds(:)

    call mpi_init(ier)
    call mpi_comm_rank(mpi_comm_world,nrank,ier)
    call mpi_comm_size(mpi_comm_world,nsize,ier)

    open(10,file='cmdfile',form='formatted')
    allocate(cmds(0:nsize))

    do k=0,nsize-1
       read(10,101,end=99) cmds(k)
    end do
101 format(a232)
    
    print *,cmds(nrank), ' on TASK ',nrank
    call system(cmds(nrank))
    go to 100
99  continue
    print *,' EOF ON FILE cmdfile. should be at least ',nsize,' ENTRIES'
100 continue
    call mpi_finalize(ier)
    stop
  end program zeuspoe

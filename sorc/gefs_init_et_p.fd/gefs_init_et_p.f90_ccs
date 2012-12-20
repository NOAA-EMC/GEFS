!-------------------------------------------------------------------------
! Main program for doing ETR (Ensemble Transform with Rescaling)
!
!
! Abstract: Generates analysis perturbations from forecast perturbations 
!            by using ETR (Ensemble Transform with Rescaling) method 
! Wei et al.(2008), Tellus 60A, 62-79.          
! Wei et al.(2005), The Symposium Proceedings in a WMO Thorpex Pub.2005, 
!                   WMO TD No. 1237, WWRP Thorpex No.6, p227-230.
!  The ETR transformation uses the analysis variances to restrict the
!  analysis perturbation directions at every cycle. We also impose 
!  ST (simplex transformation, Wei et. al. 2006, Tellus 58A, 28-44.)
!  to center the transformed perturbations.         
! 
!  PROGRAM HISTORY LOG:
!
! 2004-08-24, Mozheng Wei, Initial code for ET/ETR for 10 ensemble members
!             only, with ET and ST imposed on all 10 perturbations.
!
! 2005-03-17, Mozheng Wei, do ETR and ST for 80 perturbations, then impose
!                       ST on the different 20 perturbations which will 
!                       used for long forecasts at different cycles.
!
! 2005-11-11, Mozheng Wei, do ETR and ST for 56 perturbations and ST for
!                     14 perturbations which are used for long forecasts.
!
! 2006-05-30, Mozheng Wei, first ETR became operational on May 30, 2006, 
!                 at NCEP (do ETR and ST for 56 perturbations and ST 
!                 for 14 perturbations which are used for long forecasts).
!
! 2006-07-03, Mozheng Wei, modified to use sigio modules extensively to 
!                          handle the new GFS hybrid sigma files. 
!
! 2007-03-30, Doris Pan, Mozheng Wei, first MPI version of ETR code.
!
! 2007-04-16, Mozheng Wei, set means of vorticity and divergence zero
!                          after ETR and rescaling. 
!
! 2007-05-07, Mozheng Wei, build a mask at top level and interpolated to
!                          all levels between 23 and 28.
!
! 2007-06-14, Mozheng Wei, modify the rescaling factor based on total 
!                      energy plus three separate tracers, interpolated to
!                      all levels between 28 and a lower adjustable level.
!                      Also the negative values of 3 tracers are zeroed.
!
! 2007-09-13, Mozheng Wei, the rescaling factor computed by choosing the 
!                       largest total energy (including three separate 
!                       tracers) between mxlev->levs, then interpolated 
!                       to all levels between levs and a lower adjustable 
!                       level nlevrs.
!
! 2007-11-19, Mozheng Wei, add if (itopres .eq. 0) then do the extra 
!                      rescaling on top levels, otherwise no rescaling on
!                      top levels
!
! 2010-06-28, Doris Pan, Mozheng Wei, 2nd MPI version that allows 2D 
!                   domain decomposition. The number of cpus must meet 
!                   the following criteria: 
!                   1. "npes" has to either evenly divide "nens", or be 
!                       evenly divided by "nens"
!                   2. AND, if npes>nens, nd3 must evenly divide 
!                    npes/nens  i.e., if nens=8,nd3=ilon*ilat*nrec1=10000,
!                    then valid npes picks are  1,2,4,8,16,32,40,
!                    80,160,320,400,800,...(not 24,48,etc.)
!
! 2010-09-25, Mozheng Wei, add rescaling factors for levels below 500mb,
!                     remov globamplm
!-----------------------------------------------------------------------
!
!*******************************************************************
!    MODULE VARIABLE_DEFINE
MODULE VARIABLE_DEFINE
!    This module includes the global variables needed to be defined
!******************************************************************
!
  character*12,dimension(80)::en,enn
  namelist/DATNAMES/en,enn
  character*12,dimension(:),allocatable::enlocal,ennlocal
!  character*8 label(4)

  
  INTEGER :: ierr,mype,npes,npe_x,npe_y
  INTEGER :: icontxt,myrow,mycol
  INTEGER :: nd3,nd3local,nens,nenslocal

  INTEGER :: levsp,levs,lrec,nrec,nlath,nmt
  INTEGER :: jcaps,ilats,ilons,lrecs,nsp,nrec1
  INTEGER :: npair,irec,iret,nlevrs,mxlev,nlevmask,itopres 
  INTEGER :: npair1,jcap,ilat,ilon,ntrac,inflag,icyc

  INTEGER :: nens_group_newid
  INTEGER :: MPI_Comm_nens_group,MPI_COMM_col_group,original_group,nens_group
  INTEGER :: MAXLDA, MAXSDA,MAXLDB,MAXSDB

  real :: globamplr,globamplg,contop,smax,b1,b2,b3
  namelist/namens/npair1,jcap,ilat,ilon,levs,ntrac,inflag,icyc,&
  globamplr,globamplg,itopres,contop,nlevrs,mxlev,nlevmask,smax,b1,b2,b3

  real*8 :: gam,gam1,scf,ftt,aa
  real*8 :: rtc, tb, te, tbb, tee, ttb, tte

  real*8,dimension(:),allocatable ::eig,aux2
  real*8,dimension(:,:),allocatable ::ss,rhz,vec2,vec3,vec,veclocal

  INTEGER,dimension(:),allocatable ::nens_group_rank
  real*8,dimension(:,:,:,:),allocatable ::gfcstlocal
  real*8,dimension(:,:),allocatable ::gf1,gC,gf1_fullrecord
  real*8,dimension(:,:,:),allocatable ::gridm,gridmlocal

  real,dimension(:,:,:),allocatable ::tge,wge,keflocal,kef
  real,dimension(:,:),allocatable ::gmask,kem,kemlocal

  dimension idate(4),ext1(44)
  real,dimension(:,:,:),allocatable ::g_ana
  real,dimension(:),allocatable :: cofo_1
  LOGICAL :: Decompose1D_OPT

END MODULE VARIABLE_DEFINE
!----------------------------------------------------------------------

program gefs_init_et_para
  USE sigio_module
  USE variable_define
  include "mpif.h"
  implicit none

  INTEGER :: i,i1,i2,i3,k,nn,n
  type(sigio_head) :: heada

  real*8 :: starttime,endtime,temp


  !--------------- Initialize MPI
  call MPI_Init(ierr)
  starttime = MPI_Wtime()

  if (ierr.ne.0) stop "ERROR: can't initialize mpi" 
  call MPI_Comm_Size(MPI_COMM_WORLD,npes,ierr)
  call MPI_Comm_Rank(MPI_COMM_WORLD,mype,ierr)


  !  npair1=7,jcap=126,ilat=190,ilon=384,levs=28,ntrac=3,inflag=3

  if (mype == 0) then
     call w3tagb ('gefs_init_et',2006,0120,0084,'Global Ensemble')
     print *, '  ----------  starting gefs_init_et_para  -------------'

     !--------------- Read namelist
     read(*,namens)
     print *, 'read npair1,jcap,ilat,ilon,levs,ntrac,inflag = ' &
          , npair1,jcap,ilat,ilon,levs,ntrac,inflag
     print *, 'read globamplr,globamplg = ' &
          , globamplr,globamplg
     print *, 'read contop,smax,b1,b2,b3 = ', contop,smax,b1,b2,b3
     print *, 'read nlevrs,mxlev,nlevmask,itopres = ', nlevrs, mxlev, &
          nlevmask, itopres
  endif

     CALL MPI_Bcast(npair1,1,MPI_integer,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(jcap,1,MPI_integer,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(ilat,1,MPI_integer,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(ilon,1,MPI_integer,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(levs,1,MPI_integer,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(ntrac,1,MPI_integer,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(inflag,1,MPI_integer,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(icyc,1,MPI_integer,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(nlevrs,1,MPI_integer,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(mxlev,1,MPI_integer,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(nlevmask,1,MPI_integer,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(itopres,1,MPI_integer,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(b1,1,MPI_real,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(b2,1,MPI_real,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(b3,1,MPI_real,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(globamplr,1,MPI_real,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(globamplg,1,MPI_real,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(contop,1,MPI_real,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Bcast(smax,1,MPI_real,0,MPI_COMM_WORLD,ierr)
     CALL MPI_Barrier(MPI_COMM_WORLD,ierr)

     !
     !      level=13 is about 500mb for 28-level model      
     !       nlevmask = 13
     !  for variable vertical resolution
     !       nlevmask = ( nlevmask * levs + 14 )  / 28
     !       print *, nlevmask,'= nlevmask calculated linearly'
     !
     !   define T62 resolution for writing out fcst and ana perts
     jcaps=62
     ilons=192
     ilats=94
     lrecs=(jcaps+1)*(jcaps+2)
     levsp=levs+1
     lrec=(jcap+1)*(jcap+2)
     nrec=2+(3+ntrac)*levs
     nrec1 = nrec - 1
     irec = 2 + levs + 2*nlevmask 
     !  # of gaussian lats in one hemisphere
     nlath = ilat/2 + 1
     nmt = ilon*ilat
     !
     !   if npair1=7, 14 will be used for long fcsts, total number of perts(nens=56)
     !  npair=4*npair1
     nsp=2*npair1
     nens=4*nsp
     nd3=ilon*ilat*nrec1
!--------------- print numbers
  if (mype == 0) then
     print *, 'nens, npes, nd3 = ',   nens, npes, nd3 
     print *, 'nsp, nmt, nrec1  = ',  nsp, nmt, nrec1 
  endif

!
!****** Sanity check for number of processors used (npes)
!****** The restriction of npes: it has to either evenly divide nens, or be evenly divided by nens. AND,
!****** if npes>nens, nd3 must be able to evenly divide (npes/nens) 

     if (npes > nens) then
        Decompose1D_OPT = .false.
        if (mod(npes,nens) /= 0 .OR. mod(nd3,npes/nens) /= 0) then
           if (mype==0) print *,'error in number of processor: if npes>nens, then npes must evenly divide nens and nd3 must evenly divide npes/nens'
           CALL MPI_ABORT(MPI_COMM_WORLD,ierr)
        endif
        npe_y=nens
        npe_x=npes/nens
     else
        Decompose1D_OPT = .true.
        if (mod(nens,npes) /= 0) then
           if (mype==0) print *,'error in number of processor: if npes<=nens, then nens must evenly divide npes'
           CALL MPI_ABORT(MPI_COMM_WORLD,ierr)
        endif
        npe_y=npes
        npe_x=1
     endif

     nd3local = nd3/npe_x
     nenslocal= nens/npe_y


!**************** Map the processors (mype) in row/column order (myrow,mycol)

! Get the value of icontxt BLCS uses for internal defauls
     CALL BLACS_GET(0,0,icontxt)
! Map the processors sequentially in row/column order; NProw - 1, NPcol - npes
     CALL BLACS_GRIDINIT(icontxt,'C',npe_x,npe_y)
! Obtain the process row and column index
     CALL BLACS_GRIDINFO(icontxt,npe_x,npe_y,myrow,mycol)


!------ If npes>nens, 2D decomposition is necessary. 
!** First, setting up a subgroup, nens_group, with myrow=0. Each PE in the nens_group reads the fcst input data 
     if (.not. Decompose1D_OPT) then
        call MPI_Comm_Group(MPI_COMM_WORLD,original_group,ierr)
        allocate(nens_group_rank(0:npe_y-1))
        do i=1,npe_y    
           nens_group_rank(i-1) = (i-1)*npe_x
        enddo
        call MPI_Group_Incl(original_group, npe_y, nens_group_rank, nens_group, ierr)
        call MPI_Comm_Create(MPI_COMM_WORLD,nens_group,MPI_COMM_nens_group,ierr)
        if (myrow==0) call MPI_Comm_rank(MPI_COMM_nens_group,nens_group_newid,ierr)


!** Second, partitioning the original group into col_comm subgroups 
        call MPI_Comm_Split(MPI_COMM_WORLD,mycol,myrow,MPI_COMM_col_group,ierr)
     endif


     if (mype == 0) then  
        print *, 'npair1,nsp,nd3,nd3local,nens,nenslocal = ', &
             npair1, nsp, nd3,nd3local, nens, nenslocal
        print *, 'npes, npe_x,npe_y, lrec,nrec,nrec1,nlath,nmt = ', &
             npes, npe_x, npe_y, lrec,nrec,nrec1,nlath,nmt 
        print *, 'irec, levsp  = ',  irec, levsp 
     endif

     !------ Readin data file names nenslocal*character(12)
     if (mype == 0) read(*,DATNAMES)
     

     allocate(enlocal(nenslocal),ennlocal(nenslocal))
     if (Decompose1D_OPT) then
        CALL MPI_SCATTER(en,12*nenslocal,MPI_character,enlocal,12*nenslocal,MPI_character,0,MPI_COMM_WORLD,ierr)
        CALL MPI_SCATTER(enn,12*nenslocal,MPI_character,ennlocal,12*nenslocal,MPI_character,0,MPI_COMM_WORLD,ierr)
     else
        if (myrow==0) CALL MPI_SCATTER(en,12*nenslocal,MPI_character,enlocal,12*nenslocal,MPI_character,0,MPI_COMM_nens_group,ierr)
        if (myrow==0) CALL MPI_SCATTER(enn,12*nenslocal,MPI_character,ennlocal,12*nenslocal,MPI_character,0,MPI_COMM_nens_GROUP,ierr)
     endif
     call MPI_barrier(MPI_COMM_WORLD,ierr)

     if (myrow == 0) then
        allocate(gridm(ilon,ilat,nrec1),gridmlocal(ilon,ilat,nrec1))
        allocate(gfcstlocal(ilon,ilat,nrec1,nenslocal),tge(ilon,ilat,levs),wge(ilon,ilat,levs))
        allocate(kemlocal(ilon,ilat),kem(ilon,ilat),keflocal(ilon,ilat,nenslocal))
        gfcstlocal = 0.0
        gridmlocal = 0.0
        kemlocal = 0.0

        DO i = 1,nenslocal
           print *,'--------Start read_fcst for mype',mype,' record #',i,'-----------'
           call sigio_sropen(91,enlocal(i),iret)
           if (iret.ne.0) print *,'Processor #',mype,'en number',i,'sigio_sropen failed,iret=',iret
           call read_fcst(jcap,lrec,nrec1,levs,nlath,ilat,ilon,&
                91,gfcstlocal(:,:,:,i),levsp)
           call sigio_sclose(91,iret)
           if (iret.ne.0) print *,'Processor #',mype,'en number',i,'sigio_sclose failed,iret=',iret
           print *,'--------Complet read_fcst for mype',mype,' record #',i,'-----------'

           gridmlocal(:,:,:) = gridmlocal(:,:,:) + gfcstlocal(:,:,:,i)/dble(nens)        

           tge = 0.0
           wge = 0.0
           print *,'--------Start write_ke for mype',mype,' record #',i,'-----------'
           call write_ke(jcap,lrec,nrec1,levs,ilon,ilat,gfcstlocal(:,:,:,i),tge,wge)
           print *,'--------Complet write_ke for mype',mype,' record #',i,'-----------'
           keflocal(:,:,i) = wge(:,:,nlevmask)
           kemlocal(:,:) = kemlocal(:,:) + wge(:,:,nlevmask)/dble(nens)   
        ENDDO
     endif

     if (Decompose1D_OPT) then
        CALL MPI_ALLREDUCE(gridmlocal,gridm,ilon*ilat*nrec1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
        CALL MPI_ALLREDUCE(kemlocal,kem,ilon*ilat,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)
     else
        if (myrow==0) CALL MPI_ALLREDUCE(gridmlocal,gridm,ilon*ilat*nrec1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_nens_group,ierr)
        if (myrow==0) CALL MPI_ALLREDUCE(kemlocal,kem,ilon*ilat,MPI_REAL,MPI_SUM,MPI_COMM_nens_group,ierr)
     endif


     !---  computes perts of KE fields
     if (myrow == 0) then
        DO i = 1,nenslocal
           keflocal(:,:,i) = keflocal(:,:,i) - kem(:,:)
           gfcstlocal(:,:,:,i) = gfcstlocal(:,:,:,i) - gridm(:,:,:)
        ENDDO
     endif
!!$  print *,'sum of gfcst',sum(gfcstlocal(:,:,:,1:4)),sum(gfcstlocal(:,:,:,5:8)),sum(gfcstlocal(:,:,:,1:8)),mype
!!$  CALL MPI_ALLREDUCE(sum(gfcstlocal),temp,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
!     if (mype == 0) print *,'sum of gridm',sum(gridm)

     !********************** GATHER keflocal into kef on PE0
     if (mype == 0) allocate (kef(ilon,ilat,nens))
     if (Decompose1D_OPT) then
        CALL MPI_GATHER(keflocal,nmt*nenslocal,MPI_real,kef,nmt*nenslocal,MPI_real,0,MPI_COMM_WORLD,ierr)
     else
        if (myrow==0) CALL MPI_GATHER(keflocal,nmt*nenslocal,MPI_real,kef,nmt*nenslocal,MPI_real,0,MPI_COMM_nens_group,ierr)
     endif
     if (myrow == 0) DEALLOCATE(gridmlocal,kemlocal,kem,gridm,keflocal)

     !************** Generate vec(nens,nens) on PE0 from kef 

     if (mype == 0 ) then
        print *,'--------Start Generate_vec--------------'
        print *,'sum of kef',sum(kef)
        print *,'kef',kef(1,3,:)
        CALL Generate_vec
        print *,'sum of vec',sum(vec)
        print *,'--------End Generate_vec--------------'
     endif


     !************** Readin g_ana
     allocate( cofo_1(lrec),g_ana(ilon,ilat,nrec1)) 

     print *,'--------Start Read_ana--------------'
     call sigio_sropen(90,'sanl.in',iret)
     call read_ana(90,jcap,lrec,nrec1,levs,levsp,ilat,ilon,cofo_1,g_ana,heada)
     call sigio_sclose(90,iret)
     print *,'--------End Read_ana--------------'

!     call MPI_barrier(MPI_COMM_WORLD,ierr)

!-----------------------------------------------------------
     ! Parallel version of matrix-matrix multiplication
     ! gf1(i.e., gfcst)(ilon*ilat*nrec1,nens) * vec(nens,nens) = gC (ilon*ilat*nrec1,nens)

     print *,'--------Start Compute_gC--------------'
     CALL Compute_gC
     print *,'--------Complete Compute_gC--------------'

!!$print *,'gf1',gf1(1,:)
!!$print *,'vec',vec(:,1)
!!$print *,'result gc',gc(1,1)
!!$print *,'calculation',dot_product(gf1(1,:),vec(:,1))
!!$
!!$  print *,'sum of gC',sum(gC)
!-----------------------------------------------------------

     if (Decompose1D_OPT) then
        DO i=1, nenslocal
           nn = 0
           do i3 = 1, nrec1
              do i2 = 1, ilat
                 do i1 = 1, ilon
                    nn = nn + 1  
                    gfcstlocal(i1,i2,i3,i) = gC(nn,i)
                 enddo
              enddo
           enddo
        ENDDO
     else
        if (myrow == 0) gf1_fullrecord = 0. 
        CALL MPI_GATHER(gC,nd3local,MPI_DOUBLE_PRECISION,gf1_fullrecord,nd3local,MPI_DOUBLE_PRECISION,0,MPI_COMM_col_group,ierr)
        if (myrow == 0) then
           DO i=1, nenslocal
              nn = 0
              do i3 = 1, nrec1
                 do i2 = 1, ilat
                    do i1 = 1, ilon
                       nn = nn + 1  
                       gfcstlocal(i1,i2,i3,i) = gf1_fullrecord(nn,i)
                    enddo
                 enddo
              enddo
           ENDDO
           DEALLOCATE(gf1_fullrecord)
        endif
     endif

     DEALLOCATE( gf1, gC)

     !  use one inflation factor scf to inflate the initial perts

     ftt = 0.8
     if (mype == 0) then
        call factor_t(ftt,nrec1,ilon,ilat,gfcstlocal(:,:,:,1),scf)
        print*,'scf',scf
     endif
     CALL MPI_BCAST(scf,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

     if (myrow == 0) then
        DO i=1, nenslocal
           gfcstlocal(:,:,:,i) = gfcstlocal(:,:,:,i)*scf

           !      impose a mask on the perturbations

           print *, 'calling mask_pert    '
           call sigio_sropen(90,'sanl.in',iret)
           print *, 'iret from sigio_sropen =', iret
           if (iret.ne.0) print *,'sigio_sropen failed,iret=',iret
     call mask_pert(90,jcap,lrec,nrec1,irec,levs,nlath,ilon,ilat,&
          gfcstlocal(:,:,:,i),globamplr,globamplg,itopres,contop,&
          nlevrs,mxlev,nlevmask,smax,b1,b2,b3)
           call sigio_sclose(90,iret)
           call sigio_swopen(92,ennlocal(i),iret)
           print *, 'iret from sigio_swopen =', iret
           call write_ana(92,gfcstlocal(:,:,:,i),jcap,lrec,nrec1,levs,nlevmask,&
                ilat,ilon,cofo_1,g_ana,heada)
           call sigio_sclose(92,iret)

        ENDDO
     endif

     deallocate( g_ana,cofo_1,veclocal,enlocal,ennlocal)
     if (myrow==0) deallocate(gfcstlocal)

     print *, '  ---------- end of gefs_init_et  -------------'
     if (mype == 0) then
        call w3tage ('gefs_init_et')
        print *, '  ----------  end of gefs_init_et on PE0  -------------'
     endif


     call blacs_gridexit(icontxt)
     CALL MPI_FINALIZE(ierr)
   end program gefs_init_et_para
!
!********************************************************
subroutine Generate_vec
!********************************************************


  Use variable_define
  implicit none
  integer :: i,j,n,k
  real :: dummy

  allocate (ss(nsp-1,nsp),gmask(ilon,ilat))

     ss = 0.0
     do j = 1, nsp
        do i = 1, nsp-1
           if (j .eq. (i+1)) then
              ss(i, j) = dble(i)/sqrt(dble(i*(i+1)))
           else if (j .le. i) then
              ss(i, j) = -1.0/sqrt(dble(i*(i+1)))
           else
              ss(i, j) = 0.0
           endif
        enddo
     enddo
     
!   Read in gmask in PE 0
     gmask = 0.0
     open(90, file='sanl.in',form='unformatted')
     open(48,err=544,form='unformatted')
     call read_mask(90,48,jcap,lrec,levs,ilon,ilat,gmask)
!     gmask(:,:) = globamplm*gmask(:,:)
     close(90)
     close(48)
     goto 545
544  continue
     print *, ' fail to open mask file 48 '
545  continue
     print *, ' mask file 48 is opened  '


     allocate (rhz(nmt,nens),eig(nens),aux2(nmt+2*nens))
     allocate (vec2(nens,nens),vec3(nens,nens),vec(nens,nens))
     
     rhz = 0
     do k = 1, nens
        n = 0
        do j = 1, ilat
           do i = 1, ilon
              n = n + 1
              rhz(n, k) = dble(kef(i,j,k))/(dsqrt(dble(nens-1))* &
                   dble(abs(gmask(i,j))))
           enddo
        enddo
     enddo

!  using SVD subroutine on rhz, singular values are in eig2,
!  original rhz will be destroyed !!!

     eig = 0.0
     vec2 = 0.0
     call dgesvf(11,rhz,nmt,dummy,1,0,eig,nmt,nens,aux2,nmt+2*nens) 
     do k = 1, nens
        eig(k) = eig(k)*eig(k)
        vec2(1:nens, k) = rhz(1:nens, k)
     enddo

!  write(99,*) 'eigvalues(r8,SVD) eig = ', eig
!        write(99,*) 'initial SVs (r8,SVD) vec2 = ', vec2

     print *, 'eigvalues(r8,SVD) eig = ', eig
  !        print *, 'initial SVs (r8,SVD) vec2 = ', vec2


  !  replace last Gamma value (0.0) with  1.0+vlaue, it doesn't affect result
  !  compute transformation matrix T = C Gamma^(-1/2)
  !  vec3(nens, nens)

     eig(nens) = eig(nens)+1.0
     vec3 = 0.
     do k = 1, nens
        vec3(:, k) = vec2(:, k)/dsqrt(eig(k))
     enddo


!  mutiply the T(nens, nens) by C^T(nens, nens), use T C^T
!  vec(nens, nens)

     vec = 0.0
     call dgemul(vec3,nens,'N',vec2,nens,'T',vec,nens,nens,nens,nens)

!   impose simplex transfer onto nsp(=14) perts only at different cycles

     if ( icyc  .eq. 0 ) then 
        vec3 = 0.0
        do j = 1, nsp-1
           do k = 1, nsp-1   
              do i = 1, nens
                 vec3(i,j) = vec3(i,j) + vec(i, k)*ss(k, j)
              enddo
           enddo
        enddo
        do j = 1, nsp-1
           do i = 1, nens
              vec(i,j) = vec3(i,j)
           enddo
        enddo
     else if ( icyc  .eq. 6 ) then 
        vec3 = 0.0
        do j = 1, nsp-1
           do k = 1, nsp-1   
              do i = 1, nens
                 vec3(i,j) = vec3(i,j) + vec(i, k+nsp)*ss(k, j)
              enddo
           enddo
        enddo
        do j = 1, nsp-1
           do i = 1, nens
              vec(i,j+nsp) = vec3(i,j)
           enddo
        enddo
     else if ( icyc  .eq. 12 ) then 
        vec3 = 0.0
        do j = 1, nsp-1
           do k = 1, nsp-1   
              do i = 1, nens
                 vec3(i,j) = vec3(i,j) + vec(i, k+2*nsp)*ss(k, j)
              enddo
           enddo
        enddo
        do j = 1, nsp-1
           do i = 1, nens
              vec(i,j+2*nsp) = vec3(i,j)
           enddo
        enddo
     else if ( icyc  .eq. 18 ) then 
        vec3 = 0.0
        do j = 1, nsp-1
           do k = 1, nsp-1   
              do i = 1, nens
                 vec3(i,j) = vec3(i,j) + vec(i, k+3*nsp)*ss(k, j)
              enddo
           enddo
        enddo
        do j = 1, nsp-1
           do i = 1, nens
              vec(i,j+3*nsp) = vec3(i,j)
           enddo
        enddo
     else
        print *, 'Cycling times not identified !!!! '
     endif
  !       write(99,*) 'updated T by 14-simplex, ve! = ', vec
  !       te= rtc( )
  !       te = te - tb
  !       print *, 'time do 14 simplex trans = ', te

     DEALLOCATE (ss,gmask,kef,rhz,eig,aux2,vec3,vec2)

   END subroutine Generate_vec

!********************************************************
subroutine Compute_gC
! This routine performs the matrix-matrix product in parallel A*B=C
! gf1/gfcst(ilon*ilat*nrec1,nens) * vec(nens,nens) = gC (ilon*ilat*nrec1,nens)
!********************************************************

  Use variable_define
  include "mpif.h"
  implicit none
  integer :: i,i1,i2,i3,nn,k
  integer :: mb, nb, nbrhs
  integer :: DESC_gf1(9),DESC_vec(9),DESC_gC(9)
  integer,external :: NUMROC
  
! PDGEMM performs matrix-matrix product
!   GLOBAL:   gC(nd3,nens) = beta*gC + alpha * gf1(nd3,nens) * vec (nens, nens)
!   USAGE:
! CALL PDGEMM(transa,transb,m, n, k, alpha, a, ia, ja, desc_a, b, ib, jb, desc_b, beta, c, ic, jc, desc_c)
!  transa/transb: N means gf1 and vec are used in the computation, not the transpose
!  m : number of rows of gf1 (m=nd3); n : number of columns of vec (n=nens);
!  k : number of columns of gf1 (k=nens)
!  a/b/c: local part of GLOBAL gf1/vec/gC
!  ia/ib: the row/column idex of the GLOBAL gf1

!! global size A(nd3,nens)* B(nens,nens) = C(nds,nens)
!! process array (npe_x,npe_y)
!! block size (mb,nb) * (nb,nbrhs) = (mb,nbrhs)
!! local arraysize gf1local(MAXLDA,MAXLDB) * veclocal(MAXLDB,MAXSDB) = gC(MAXLDA,MAXSDB) 


     mb = nd3local; nb = nenslocal; nbrhs = nenslocal
!  MAXLDA=nd3local; MAXSDA=nenslocal; MAXSDB=nenslocal
     MAXLDA=max(1,NUMROC(nd3,mb,myrow,0,npe_x))
     MAXSDA=max(1,NUMROC(nens,nb,mycol,0,npe_y))
     MAXLDB=max(1,NUMROC(nens,nb,myrow,0,npe_x))
     MAXSDB=max(1,NUMROC(nens,nbrhs,mycol,0,npe_y))

!------------------ Distribute global matrix Vec and gf1 to processors, generating Veclocal and gf1local 
!*********** First, scatter vec in PE0 to all other processors
     allocate(veclocal(MAXLDB,MAXSDB))
     if (Decompose1D_OPT) then
        CALL MPI_SCATTER(vec,nens*nenslocal,MPI_DOUBLE_PRECISION,veclocal,nens*nenslocal,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
        if (mype == 0) DEALLOCATE(vec)
     else
        if (mype /= 0) allocate (vec(nens,nens))
        CALL MPI_Bcast(vec,nens*nens,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
        veclocal=0.
        Do i=1,MAXLDB
           veclocal(i,1)=vec(myrow+1+(i-1)*npe_x,mycol+1)
        End Do
        DEALLOCATE(vec)
     endif

!************ Second, if 2D-Decomposition is true, scatter gf1(nd3,nenslocal) from PEs(myrow=0) to the rest of colum groups
     allocate(gC(nd3local,nenslocal),gf1(nd3local,nenslocal))
     if (Decompose1D_OPT) then
        do i=1, nenslocal
           nn = 0
           do i3 = 1, nrec1
              do i2 = 1, ilat
                 do i1 = 1, ilon
                    nn = nn + 1  
                    gf1(nn, i) = gfcstlocal(i1, i2, i3, i)
                 enddo
              enddo
           enddo
        enddo
     else    ! 2D-Decomposition, in which nenslocal=1
        if (myrow == 0) then
           allocate(gf1_fullrecord(nd3,nenslocal))
           do i=1, nenslocal
              nn = 0
              do i3 = 1, nrec1
                 do i2 = 1, ilat
                    do i1 = 1, ilon
                       nn = nn + 1  
                       gf1_fullrecord(nn, i) = gfcstlocal(i1, i2, i3, i)
                    enddo
                 enddo
              enddo
           enddo
        endif
        CALL MPI_SCATTER(gf1_fullrecord,nd3local,MPI_DOUBLE_PRECISION,gf1,nd3local,MPI_DOUBLE_PRECISION,0,MPI_COMM_col_group,ierr)
     endif
!print*,'gf1',mype,size(gf1,1),size(gf1,2),'numbers',gf1(1:4,1),gf1(size(gf1,2)/2+1:size(gf1,2)/2+4,1)


! define the "desc" parameter for arrays A, B, and C
     call desc_setup(DESC_gf1,nd3, nens,mb, nb,icontxt,MAXLDA)
     call desc_setup(DESC_vec,nens,nens,nb,nbrhs,icontxt,MAXLDB)
     call desc_setup(DESC_gC,nd3,nens,mb,nbrhs,icontxt,MAXLDA)

!!$     DESC_gf1(1) = 1              ! DTYPE : descriptor type
!!$     DESC_gf1(2) = icontxt        ! output of the BLACS_GRIDINIT call
!!$     DESC_gf1(3) = nd3            ! M : number of rows of global gf1
!!$     DESC_gf1(4) = nens           ! N : number of columns of global gf1
!!$     DESC_gf1(5) = nd3            ! MB : row block size
!!$     DESC_gf1(6) = nenslocal      ! NB : column block size
!!$     DESC_gf1(7) = 0              ! RSRC: the first row idex of the process row 
!!$     DESC_gf1(8) = 0              ! CSRC: the first column idex of the process column
!!$     DESC_gf1(9) = max(1,NUMROC(DESC_gf1(3),DESC_gf1(5),myrow,DESC_gf1(7),nprow))
!!$     
!!$     DESC_vec(1) = 1              ! DTYPE : descriptor type
!!$     DESC_vec(2) = icontxt        ! output of the BLACS_GRIDINIT call
!!$     DESC_vec(3) = nens           ! M : number of rows of global vec
!!$     DESC_vec(4) = nens           ! N : number of columns of global vec
!!$     DESC_vec(5) = nenslocal      ! MB : row block size
!!$     DESC_vec(6) = nenslocal      ! NB : column block size
!!$     DESC_vec(7) = 0              ! RSRC: the first row idex of the process row 
!!$     DESC_vec(8) = 0              ! CSRC: the first column idex of the process column
!!$     DESC_vec(9) = max(1,NUMROC(DESC_vec(3),DESC_vec(5),myrow,DESC_vec(7),nprow))
!!$     
!!$     DESC_gC(1) = 1               ! DTYPE : descriptor type
!!$     DESC_gC(2) = icontxt         ! output of the BLACS_GRIDINIT call
!!$     DESC_gC(3) = nd3             ! M : number of rows of global gC
!!$     DESC_gC(4) = nens            ! N : number of columns of global gC
!!$     DESC_gC(5) = nd3             ! MB : row block size
!!$     DESC_gC(6) = nenslocal       ! NB : column block size
!!$     DESC_gC(7) = 0               ! RSRC: the first row idex of the process row 
!!$     DESC_gC(8) = 0               ! CSRC: the first column idex of the process column
!!$     DESC_gC(9) = max(1,NUMROC(DESC_gC(3),DESC_gC(5),myrow,DESC_gC(7),nprow))
   

     CALL PDGEMM('N','N',nd3,nens,nens,1.0d0,gf1,1,1,DESC_gf1,veclocal,1,1,DESC_vec,0.0d0,gC,1,1,DESC_gC)

   END subroutine Compute_gC


!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   subroutine desc_setup(Adesc,m,n,mb,nb,icontext,LDA)
     implicit none
     integer m, n, mb, nb, icontext, LDA, Adesc(9)
     Adesc(1) = 1
     Adesc(2) = icontext
     Adesc(3) = m
     Adesc(4) = n
     Adesc(5) = mb
     Adesc(6) = nb
     Adesc(7) = 0
     Adesc(8) = 0
     Adesc(9) = LDA
     return
   end subroutine desc_setup

! --------------------------------------------------------------------
subroutine read_ana(ir,jcap,lrec,nrec1,levs,levsp,ilat,ilon,cofo_1,g_ana,head)

!    Mozheng Wei, 2004-10-15
!    Mozheng Wei, 2006-05-18, based on read_ana2.f, modified to read head 
!    using sigio_module prepared for hybrid GFS files 
! --------------------------------------------------------------------

  use sigio_module
  type(sigio_head):: head
  type(sigio_data):: data
  !  
  character*8 label(4)
  dimension idate(4)
  real,intent(out) ::  cofo_1(lrec),g_ana(ilon,ilat,nrec1)
  real :: cofo(lrec),grid(ilon,ilat)
  
  levsp=levs+1
!  print *, '  --- in et, starting read_ana.exe --'
  !
  !  use sigio_module to read
  call sigio_srhead(ir,head,iret)
  if (iret.ne.0) print *,'sigio_srhead failed,iret=',iret
  call sigio_aldata(head,data,iret)
  call sigio_srdata(ir,head,data,iret)
  if (iret.ne.0) print *,'sigio_srdata failed,iret=',iret
  fhour=head%fhour
  idate=head%idate
  !
  !
  write(6,1001) fhour,idate
  levs=head%levs
  ntrac=head%ntrac
  print *, ' levs, ntrac from head = ', levs, ntrac
!  print *, 'sl from head in read_ana = ', head%sl
  print *, 'jcap from head in read_ana = ', head%jcap

1001 format(5x,'fhour and idata= ',f10.1,2x,4i5)
  !
  !        fhr1 = head%fhour
  !        idate1=head%idate
  !        rewind(ir)
  !        read(ir) label
  !        read(ir) fhr1,(idate(i),i=1,4),
  !     &  (sigi(k),k=1,levsp),(sigl(k),k=1,levs),dummy1,ext1
  !
  !      reads orography
  !      read(ir) (cofo_1(j), j = 1, lrec)
  !
  !       do nr = 1, nrec1
  !          cofo = 0.0
  !          grid = 0.0
  !          read(ir) (cofo(j), j = 1, lrec)
  !          call sptez(0,jcap,4,ilon,ilat,cofo,grid,+1)
  !          g_ana(:,:,nr) = grid(:,:)
  !       enddo
  !
  ! use sigio_module
  cofo_1 = 0.0
  cofo_1(:) = data%hs(:)
  cofo(:) = data%ps(:)
  call sptez(0,jcap,4,ilon,ilat,cofo,grid,+1)
  g_ana(:,:,1) = grid(:,:)
  ! read temp
  print *, '---------------  In read_ana  --------  '
  do k = 1, levs
     grid = 0.0
     cofo(:) = data%t(:, k)
     call sptez(0,jcap,4,ilon,ilat,cofo,grid,+1)
     g_ana(:,:,k+1) = grid(:,:)
! print 
         if (k .eq. 1) then
         print *, 'Ana temp(read_ana)at k=1 ', grid(1:5,1:5)
         endif
         if (k .eq. 13) then
         print *, 'Ana temp(read_ana)at k=13 ', grid(1:5,1:5)
         endif
         if (k .eq. levs) then
         print *, 'Ana temp(read_ana)at k=28 ', grid(1:5,1:5)
         endif

  enddo
  ! div and vor
  do k = 1, levs
     grid = 0.0
     cofo(:) = data%d(:, k)
     call sptez(0,jcap,4,ilon,ilat,cofo,grid,+1)
     g_ana(:,:,2*k+levs) = grid(:,:)
     grid = 0.0
     cofo(:) = data%z(:, k)
     call sptez(0,jcap,4,ilon,ilat,cofo,grid,+1)
     g_ana(:,:,2*k+levs+1) = grid(:,:)
  enddo
  ! ntrac tracers
  ii = 0
  do n = 1, ntrac
     do k =  1, levs
        ii = ii + 1
        grid = 0.0
        cofo(:) = data%q(:, k, n)
        call sptez(0,jcap,4,ilon,ilat,cofo,grid,+1)
        g_ana(:,:,3*levs+1+ii) = grid(:,:)
     enddo
  enddo
  !
  call sigio_axdata(data,iret)
!  print *, '---in et, end of read_ana.exe ----'
  return
END subroutine read_ana

! --------------------------------------------------------------------
subroutine write_ana(iout,grida,jcap,lrec,nrec1,levs,nlevmask,ilat, &
                     ilon,cofo_1,g_ana,head)

  !    Mozheng Wei, 2004-10-15
  !    Mozheng Wei, 2006-05-18, based on write_ana2.f, modified to read head 
  !    using sigio_module prepared for hybrid GFS files 
! --------------------------------------------------------------------

  use sigio_module
  type(sigio_head):: head
  type(sigio_data):: data

  character*8 label(4)
  dimension idate(4)
  real cofo_1(lrec),g_ana(ilon,ilat,nrec1)
  real*8 grida(ilon,ilat,nrec1)
  real cofo(lrec),grid(ilon,ilat)

!  print *, '  -- in et, starting write_ana.exe --'
  levs=head%levs
  ntrac=head%ntrac
!  print *, 'sl from head in write_ana = ', head%sl
  print *, 'jcap from head in write_ana = ', head%jcap
  print *, ' levs, ntrac from head = ', levs, ntrac
  print *, 'idvc from head in write_ana = ', head%idvc
!  print *, 'ak from head in write_ana = ', head%ak
  call sigio_swhead(iout,head,iret)
  print *, 'iret from sigio_swhead =', iret
  call sigio_aldata(head,data,iret)
  print *, 'iret from sigio_aldata =', iret
  data%hs(:) = cofo_1(:)
  !
  cofo = 0.0
  grid = 0.0
  grid(:,:) = g_ana(:,:,1) + real(grida(:,:,1))
  cofo = 0.0
  call sptez(0,jcap,4,ilon,ilat,cofo,grid,-1)
  data%ps(:)=cofo(:)
  !
  !     write perturbed sigma file,  temp
  !
  print *, '---------------  In write_ana  --------  '
  do k = 1, levs
     cofo = 0.0
     grid = 0.0
     grid(:,:) = g_ana(:,:,k+1) + real(grida(:,:,k+1))
     cofo = 0.0
     call sptez(0,jcap,4,ilon,ilat,cofo,grid,-1)
     data%t(:, k)=cofo(:)
     ! print 
     if (k .eq. 1) then
        print *, 'Ana temp  at k=1 ', g_ana(1:5,1:5,k+1)
        print *, 'temp perts  at k=1 ',real(grida(1:5,1:5,k+1))
        print *, 'new Ana temp  at k=1 ',  grid(1:5,1:5)
     endif
     if (k .eq. nlevmask) then
        print *, 'Ana temp  at k=nlevmask ', g_ana(1:5,1:5,k+1)
        print *, 'temp perts  at k=nlevmask',real(grida(1:5,1:5,k+1))
        print *, 'new Ana temp  at k=nlevmask', grid(1:5,1:5)
     endif
     if (k .eq. levs) then
        print *, 'Ana temp  at k=levs ', g_ana(1:5,1:5,k+1)
        print *, 'temp perts  at k=levs ',real(grida(1:5,1:5,k+1))
        print *, 'new Ana temp  at k=levs ', grid(1:5,1:5)
     endif
     !
  enddo
!
!      div and vor
!
  do k = 1, levs
     cofo = 0.0
     grid = 0.0
     grid(:,:)=g_ana(:,:,2*k+levs)+real(grida(:,:,2*k+levs))
     cofo = 0.0
     call sptez(0,jcap,4,ilon,ilat,cofo,grid,-1)
     cofo(1) = 0.0
     cofo(2) = 0.0
     data%d(:, k)=cofo(:)
!
     cofo = 0.0
     grid = 0.0
     grid(:,:)=g_ana(:,:,2*k+levs+1)+real(grida(:,:,2*k+levs+1))
     cofo = 0.0
     call sptez(0,jcap,4,ilon,ilat,cofo,grid,-1)
     cofo(1) = 0.0
     cofo(2) = 0.0
     data%z(:, k)=cofo(:)
  enddo
!
!  ntrac tracers
!
  ii = 0
  do n = 1, ntrac
     do k = 1, levs
        ii = ii + 1
        cofo = 0.0
        grid = 0.0
        do i = 1, ilon
           do j = 1, ilat
              grid(i,j)=g_ana(i,j,ii+3*levs+1)+real(grida(i,j,ii+3*levs+1))
              if (grid(i, j) .lt. 0.0) then
                  grid(i, j) = 0.0
              endif
           enddo
        enddo
        cofo = 0.0
        call sptez(0,jcap,4,ilon,ilat,cofo,grid,-1)
        data%q(:, k, n)=cofo(:)
     enddo
  enddo
!
  call sigio_swdata(iout, head, data, iret)
  print *, 'iret from sigio_swdata =', iret
  call sigio_axdata(data,iret)
!  print *, '---in et, end of write_ana.exe ----'
  return
END subroutine write_ana

! ----------------------------------------------------------------
subroutine read_fcst(jcap,lrec,nrec1,levs,nlath,ilat,ilon,ir,grid,levsp)

  !  from read_sig_all, avoid dynamic array to save time
  !   Mozheng Wei, 2004-10-15
  !   Mozheng Wei, 2006-05-18, based on read_fcst2.f, modified to read head 
  !       and data using sigio_module.f, prepared for hybrid GFS files 
  ! ----------------------------------------------------------------


  use sigio_module
  type(sigio_head):: head
  type(sigio_data):: data
  !
  character*8 label(4),label2(4)
  dimension idate(4)
  real ext1(44)
  real*8,intent(out) :: grid(ilon,ilat,nrec1)
  !
  real sigi(levsp),sigl(levs),dummy1(201-levs-levsp),cofo(lrec),grid1(ilon,ilat)  
  !
!  print *, '  ----starting read_fcst.exe -------'
  !
  !  use sigio_module to read
  !
  call sigio_srhead(ir,head,iret)
  if (iret.ne.0) print *,'sigio_srhead failed,iret=',iret
  call sigio_aldata(head,data,iret)
  call sigio_srdata(ir,head,data,iret)
  if (iret.ne.0) print *,'sigio_srdata failed,iret=',iret
  fhour=head%fhour
  idate=head%idate
!  print *, 'sl from head in read_fcst = ', head%sl
!  print *, 'jcap from head in read_fcst = ', head%jcap
  !
  !
!  write(6,1001) fhour,idate
1001 format(5x,'fhour and idata= ',f10.1,2x,4i5)
  !        print *, ' levs, ntrac = ', levs, ntrac
  levs=head%levs
  ntrac=head%ntrac
  print *, ' levs, ntrac from head = ', levs, ntrac
  print *, ' lrec = ', lrec
  cofo = 0.0
  grid1 = 0.0
  grid = 0.0
  !
  ! use sigio_module
  !       print *, ' start reading ps '
  !       print *, 'data%ps =  ', data%ps(1:10)
  cofo(1:lrec) = data%ps(1:lrec)
  !       print *, ' start converting ps to grid points '
  call sptez(0,jcap,4,ilon,ilat,cofo,grid1,+1)
  grid(:,:,1) = dble(grid1(:,:))
  ! read temp
  do k = 1, levs
     !          print *, ' start reading t at level= ', k
     grid1 = 0.0
     cofo(:lrec) = data%t(:lrec, k)
     call sptez(0,jcap,4,ilon,ilat,cofo,grid1,+1)
     grid(:,:,k+1) = dble(grid1(:,:))
  enddo
  ! div and vor
  do k = 1, levs
     !           print *, ' start reading div and vor at level= ', k
     grid1 = 0.0
     cofo(:lrec) = data%d(:lrec, k)
     call sptez(0,jcap,4,ilon,ilat,cofo,grid1,+1)
     grid(:,:,2*k+levs) = dble(grid1(:,:))
     grid1 = 0.0
     cofo(:lrec) = data%z(:lrec, k)
     call sptez(0,jcap,4,ilon,ilat,cofo,grid1,+1)
     grid(:,:,2*k+levs+1) = dble(grid1(:,:))
  enddo
  ! ntrac tracers
  ii = 0
  do n = 1, ntrac
     do k =  1, levs
        !           print *, ' start reading tracer at n, k= ', n, k
        ii = ii + 1
        grid1 = 0.0
        cofo(:lrec) = data%q(:lrec, k, n)
        call sptez(0,jcap,4,ilon,ilat,cofo,grid1,+1)
        grid(:,:,3*levs+1+ii) = dble(grid1(:,:))
     enddo
  enddo
  call sigio_axdata(data,iret)
  !        close(ir)
!  print *, '-- in gefs_init_et, end of read_fcst.exe ----'

END subroutine read_fcst

!-----------------------------------------------------------------------

subroutine write_ke(jcap,lrec,nrec1,levs,ilon,ilat,gridd,tge,wge)

  !   by Mozheng Wei, 2004-10-15
  !
  !  write levs of grid point values of t, wind
!-----------------------------------------------------------------------

  real*8 gridd(ilon,ilat,nrec1)
  real tge(ilon,ilat,levs),wge(ilon,ilat,levs),grid(ilon,ilat),grid2(ilon,ilat)
  
  real cofo(lrec),cofo2(lrec),coft(lrec,nrec1+1) 
  real ug(ilon,ilat,levs),vg(ilon,ilat,levs) 
  
  !        real,dimension(:,:),allocatable ::grid3
  !        nrec = nrec1+1
  !        allocate(cofo(lrec),cofo2(lrec),coft(lrec,nrec) )
  
!  print *, '  starting write_ke.exe --'
  
  coft = 0.0
  do nr = 1, nrec1
     grid(:,:) = real(gridd(:,:,nr))
     cofo = 0.0
     call sptez(0,jcap,4,ilon,ilat,cofo,grid,-1)
     coft(:, nr+1)=cofo(:)
  enddo
  
  !       allocate( cofts(lrecs,nrec1+1), cofo2(lrecs)  )
  !  reduce resolution to T62
  !       cofts = 0.0
  !      do nr = 1, nrec1
  !          cofo(:) = coft(:, nr+1)
  !          call reduce(jcap,cofo,jcaps,cofo2)
  !          cofts(:, nr+1)=cofo2(:)
  !       enddo
  
  !        allocate(ug(ilon,ilat,levs),vg(ilon,ilat,levs) )
  cofo = 0.0
  grid = 0.0
  ! levs levels of temperature
  do  nr = 3, levs+2
     do j=1, lrec
        cofo(j)=coft(j, nr)
     enddo
     call sptez(0,jcap,4,ilon,ilat,cofo,grid,+1)
     tge(1:ilon,1:ilat,nr-2) = grid(1:ilon,1:ilat)
  enddo
  
  ! levs levels of divergence and vorticity
  
  nuv = 0
  do  nr = levs+3, 3*levs+1, 2
     do j = 1, lrec
        cofo(j)=coft(j, nr)
        cofo2(j)=coft(j, nr+1)
     enddo
     call sptezv(0,jcap,4,ilon,ilat,cofo,cofo2,grid,grid2,+1)
     nuv = nuv + 1
     ug(:,:,nuv) = grid(:,:)
     vg(:,:,nuv) = grid2(:,:)
  enddo
  
  do k = 1, nuv
     do j = 1, ilat
        do i = 1, ilon
           wge(i, j, k) = sqrt(ug(i,j,k)*ug(i,j,k) + vg(i,j,k)*vg(i,j,k))
        enddo
     enddo
  enddo
!  print *, '  ---- end of write_ke.exe -------'
  return
END subroutine write_ke

!---------------------------------------------------------
subroutine read_mask(ir,ig,jcap,lrec,levs,ilon,ilat,geogr)

  !   Mozheng Wei, 2004-10-15
  !   Mozheng Wei, 2006-05-18, modified to read head using sigio_module
  !                prepared for hybrid GFS files 
!---------------------------------------------------------


  use sigio_module
  type(sigio_head):: head
  character*8 label(4)
  dimension idate(4)
  complex cofilr(2016)
  real geoglr(192,94),geogr1(192,94),geogr2(192,94)
  real, intent(out) :: geogr(ilon,ilat)
  complex stra(lrec/2) 
  !       complex,dimension(:),allocatable::stra
  
  levsp=levs+1
  !  lrec/2 for complex stra;   lrec for real
  !       allocate( stra(lrec/2) )
  print *,' in gefs_init_et, read_mask.exe starts'
  
  write(6,688) jcap,ilat,ilon
688 format(1x,'scale truncation, lat, lon' ,3i5)
689 format(1x,'scale ir, iw',2i10)
    
!----------------------CHECK FOR DATE
  !      
  !       rewind(ir)
  !       read(ir) label
  !       print *, label
  !       read(ir) fhr,idate
  !       print *, 'fhr, idate = ', fhr,idate
  !       close(ir)
  !
  !   use sigio_module to read

  call sigio_srhead(ir,head,iret)
  call sigio_sclose(ir,iret)
  idate=head%idate
!  print *, 'sl from head in read_mask = ', head%sl
  print *, 'idvc from head in read_mask = ', head%idvc
  print *, 'jcap from head in read_mask = ', head%jcap
  
  m1=idate(2)
  jday=idate(3)
  print *, 'm1, jday = ', m1, jday
  
  if(jday.le.15)then
     m2=m1-1
     if(m2.eq.0)then
        m2=12
     endif
  endif
  
  if(jday.gt.15)then
     m2=m1+1
     if(m2.gt.12)then
        m2=1
     endif
  endif
  print *, 'm1, m2 = ', m1,m2
  
  wfac2=abs(float(jday)-15.0)/30.0
  wfac1=1.0-wfac2
  print *, 'wfac1, wfac2=', wfac1, wfac2
! check code begin
  rewind(ig)
  do nr = 1, 12
     print *,' reading GEOGR(mask) of month= ', nr
     read(ig) geogr1
     call range(geogr1,192,94)
  enddo
! check code end
  rewind(ig)
  do nr = 1, m1-1
     read(ig)
  enddo
  read(ig) geogr1
  rewind(ig)
  do nr = 1, m2-1
     read(ig)
  enddo
  read(ig) geogr2
  
  do j = 1 , 94
     do i = 1 , 192
        geoglr(i,j)=geogr1(i,j)*wfac1+geogr2(i,j)*wfac2
     enddo
  enddo
  print *,' mask file has been interpolated on T62 '
!       close(ig)
  
!-------------------CHECK FOR HIGH RESOLUTION

  if (jcap.ne.62)then
     print *,' range of low resolution GEOGR: world, nh, sh'
     call range(geoglr,192,94)
     call range(geoglr(1,1),192,47)
     call range(geoglr(1,48),192,47)
     CALL SPTEZ(0,62,4,192,94,cofilr,geoglr,-1)
     call fillf(cofilr,stra,62,jcap)
     CALL SPTEZ(0,jcap,4,ilon,ilat,stra,geogr,+1)
     print *,' in read_mask range of high resolution GEOGR'
     call range(geogr,ilon,ilat)
  else
     do j = 1 , 94
        do i = 1 , 192
           geogr(i,j)=geoglr(i,j)
        enddo
     enddo
     !    
     print *,' in read_mask:  range of low resolution GEOGR'
     call range(geoglr,192,94)
  endif
!--------------------------------------------------------------
544 continue
!       deallocate(stra)
  RETURN
end subroutine read_mask
!
! -----------------------------------------------------------------
!
subroutine mask_pert(ir,jcap,lrec,nrec1,irec,levs,nlath,ilon,ilat, &
grida,globamplr,globamplg,itopres,contop,nlevrs,mxlev,nlevmask,smax, &
b1,b2,b3)
!  
!   Mozheng Wei, 2004-10-15
!   Mozheng Wei, 2006-05-18, modified to read head using sigio_module
!                prepared for hybrid GFS files 
! -----------------------------------------------------------------

  use sigio_module
  type(sigio_head):: head
  type(sigio_data):: data
!      PARAMETER(smx=0.15,globampl=1.45)
  PARAMETER(smx=0.15)
!  
  character*8 label(4)
  dimension idate(4)
  complex cofilr(2016)
  real factor(2), ext(44)
  real geoglr(192,94), geogr1(192,94),geogr2(192,94)
  real*8 grida(ilon,ilat,nrec1)
!
!       real,dimension(:),allocatable ::sigi,sigl,dummy,zgeogr,zgrid
!       real,dimension(:,:),allocatable ::grid,gridk,grida2,gu,gv,
!     & geogr,gresc,geogl
!       complex,dimension(:),allocatable::cofi,str,stra
!       complex,dimension(:,:),allocatable::coft
!       real,dimension(:),allocatable::cofi,str,stra
!       real,dimension(:,:),allocatable::coft

  real sigi(levs+1),sigl(levs),dummy(200),&
       cofi(lrec),str(lrec),stra(lrec),grid(ilon,ilat),&
       grid1(ilon,ilat),grid2(ilon,ilat),cofo1(lrec),cofo2(lrec), &
       coft(lrec,nrec1+1),gridk(ilon,ilat),grida2(ilon,ilat), &
       gu(ilon,ilat),gv(ilon,ilat),geogr(ilon,ilat),res(ilon,ilat),  &
       gresc(ilon,ilat),geogl(ilon,ilat),zgeogr(ilat),zgrid(ilat), &
       q1(ilon,ilat),q2(ilon,ilat),q3(ilon,ilat),tem(ilon,ilat), &
       gridmax(ilon,ilat),gridmax1(ilon,ilat),resv(levs)

  levsp=levs+1
  nrec = nrec1+1
  print *,' starting mask_pert.exe'
  write(6,688) jcap,ilat,ilon
688 format(1x,'scale truncation, latitude, longjcude' ,3i5)
    
!---------------------------------CHECK FOR DATE
  !
  !       open(90, file='sanl.in', form='unformatted')
  !       rewind(90)
  !       read(90)label
  !       read(90)fhr,idate
  !       close(90)
  !
  !   use sigio_module to read
  call sigio_srhead(ir,head,iret)
  print *, 'iret from sigio_srhead =', iret
  if (iret.ne.0) print *,'sigio_srhead failed,iret=',iret
  fhr=head%fhour
  idate=head%idate
!  print *, 'sl from head in mask_pert = ', head%sl
  print *, 'idvc from head in mask_pert = ', head%idvc
  print *, 'jcap from head in mask_pert = ', head%jcap
  write(6,1001) fhr,idate
!       label=head%clabsig
!       sigi=head%si
!       sigl=head%sl
!       print *, 'label from head=', label
!       print *, 'label,sigi,sigl from head=', label,sigi,sigl
1001 format(5x,'ir ',f10.1,2x,4i5)
  
  m1=idate(2)
  jday=idate(3)
  
  if(jday.le.15)then
     m2=m1-1
     if(m2.eq.0)then
        m2=12
     endif
  endif
  
  if(jday.gt.15)then
     m2=m1+1
     if(m2.gt.12)then
        m2=1
     endif
  endif
  !       print *,m1,m2
  
  wfac2=abs(float(jday)-15.0)/30.0
  wfac1=1.0-wfac2
  print *, wfac1, wfac2
! check code begin
  open(48,err=544,form='unformatted')
  rewind(48)
  do nr=1,12
     read(48) geogr1
     print *,' range of input GEOGR for ',nr
     call range(geogr1,192,94)
  enddo
! check code end
  rewind(48)
  do nr=1,m1-1
     read(48)
  enddo
  read(48)geogr1
  rewind(48)
  do nr=1,m2-1
     read(48)
  enddo
  read(48) geogr2
  
  do j = 1 , 94
     do i = 1 , 192
        geoglr(i,j)=geogr1(i,j)*wfac1+geogr2(i,j)*wfac2
     enddo
  enddo
  
!------------------------------- CHECK FOR HIGH RESOLUTION

  if(jcap.ne.62)then
     print *,' range of low resolution GEOGR: world, nh, sh'
     call range(geoglr,192,94)
     call range(geoglr(1,1),192,47)
     call range(geoglr(1,48),192,47)
     CALL SPTEZ(0,62,4,192,94,cofilr,geoglr,-1)
     call fillf(cofilr,stra,62,jcap)
     CALL SPTEZ(0,jcap,4,ilon,ilat,stra,geogr,+1)
     print *,' in mask_pert range of high resolution GEOGR'
     call range(geogr,ilon,ilat)
  else
     do j = 1 , 94
        do i = 1 , 192
           geogr(i,j)=geoglr(i,j)
        enddo
     enddo
     !    
     print *,' in mask_pert:  range of low resolution GEOGR'
     call range(geoglr,192,94)
  endif
!-------------------------------Get latitudinal average of geogr
  do ila=1,ilat
     zgeogr(ila)=0.0
     do ilo=1,ilon
        zgeogr(ila)=zgeogr(ila)+(geogr(ilo,ila)*globamplr)/float(ilon)
     enddo
  enddo

  
  idirect = 0
  if(idirect.eq.0) then
     print *,' in mask_pert: reading ana orgraphy'

!   use sigio_module to read
!         call sigio_srohdc(ir,'sanl.in',head,data,iret)
!         call sigio_sropen(ir,'sanl.in',iret)

     call sigio_aldata(head,data,iret)
     print *, 'iret from sigio_aldata =', iret
     if (iret.ne.0) print *,'sigio_aldata failed,iret=',iret
     call sigio_srdata(ir,head,data,iret)
     print *, 'iret from sigio_srdata =', iret
     if (iret.ne.0) print *,'sigio_srdata failed,iret=',iret

!         open(90, file='sanl.in', form='unformatted')
!         rewind(90)
!         read(90) label
!         read(90) fhr, (idate(i),i=1,4),
!     &   (sigi(k),k=1,levsp),(sigl(k),k=1,levs),dummy,(ext(n),n=1,44)

!  use the analysis orgraphy
!         read(90) (cofi(J),J=1,lrec)
!         close(90)
     cofi = 0.0
     coft = 0.0
     cofi(:) = data%hs(:)
     do j = 1, lrec
        coft(j, 1) = cofi(j)
     enddo
     
     grid = 0.0
     do nr = 1, nrec1
        grid(:,:) = real(grida(:,:,nr))
        call sptez(0,jcap,4,ilon,ilat,cofi,grid,-1)
        coft(:, nr+1) = cofi(:)
     enddo
         
!  vorticity at 500mb
     cofi(:) = coft(:, irec)
!  divergence at 500mb
     str(:) = coft(:, irec-1)
     gu = 0.0
     gv = 0.0
     call sptezv(0,jcap,4,ilon,ilat,str,cofi,gu,gv,+1)
     !        gu, gv --> grid point values of u, v, 
     call kinendist(dx,gu,gv,grid,ilon,ilat)
     !        grid --> sqt of KE over grids
     print *,"in mask_pert,ave KE over all grids at 500mb= ",dx
     !        kinetic energy based geographical rescaling  *************
     stra = 0.0
     call sptez(0,jcap,4,ilon,ilat,stra,grid,-1)
     !        stra --> spectrum of sqt of KE at 500mb         
     str = 0.0
     call smooth(stra,str,1,smx,jcap)
     call sptez(0,jcap,4,ilon,ilat,str,grida2,+1)
     print *,"in mask_pert, grid values of sqt of KE at 500mb"
     call range(grida2,ilon,ilat)

!--------------------------------- get Latitudinal average

     do j = 1,ilat
        zgrid(j)=0.0
        do i = 1,ilon
           zgrid(j) = zgrid(j)+grida2(i,j)/float(ilon)
        enddo
     enddo
     
     do ila=1,ilat
        do ilo=1,ilon
           geogr(ilo,ila)=geogr(ilo,ila)*globamplr
           gresc(ilo,ila)=geogr(ilo,ila)/grida2(ilo,ila)
        enddo
     enddo
     do ila=1,ilat
        do ilo=1,ilon
           if(gresc(ilo,ila).gt.1.0) gresc(ilo,ila)=1.0
           geogl(ilo,ila)=gresc(ilo,ila)*grida2(ilo,ila)
        enddo
     enddo
     print *,' in mask_pert, range of gresc: scaling factors'
     call range(gresc,ilon,ilat)

     do ila=1,ilat
        do ilo=1,ilon
           gridk(ilo,ila)=grida2(ilo,ila)/geogl(ilo,ila)
        enddo
     enddo
     print *,' in mask_pert initial smoothed energy ratio '
     call range(gridk,ilon,ilat)
     do ila=1,ilat
        do ilo=1,ilon
           gridk(ilo,ila)=grida2(ilo,ila)/geogr(ilo,ila)
        enddo
     enddo
     print *,' in mask_pert initial smoothed energy ratio'
     call range(gridk,ilon,ilat)
     print *,' in mask_pert end pre ed range of geogr*globampl: global'
     call range(geogr,ilon,ilat)
  else
     factor(1)=dnh
     factor(2)=dsh
  endif

  iexpi = 0
  do nr=1,nrec
     cofi(:) = coft(:, nr) 
     call sptez(0,jcap,4,ilon,ilat,cofi,grid,+1)
     if(nr.eq.2) then
        if(iexpi.eq.1) then
           do  j=1,ilat
              do i=1,ilon
                 grid(i,j)=exp(grid(i,j))*10.0
              enddo
           enddo
        endif
     endif
     do j=1,ilat
        do i=1,ilon
           grid(i,j)=grid(i,j)*gresc(i,j)*globamplg
        enddo
     enddo
     
     if(nr.eq.irec) then 
        print *,' in mask_pert, rescaled 500 hPa vorticity:'
        call range(grid,ilon,ilat)
     endif
     ialogo=0
     if(nr.eq.2.and.ialogo.eq.1) then
        do j=1,ilat
           do  i=1,ilon
              grid(i,j)=alog((grid(i,j))/10.0)
           enddo
        enddo
     endif
     if (nr .ge. 2) then
        do j = 1, ilat
           do  i = 1, ilon
              grida(i,j,nr-1) = dble(grid(i,j))
           enddo
        enddo
     endif
  enddo
!
! extra rescaling for levels from  nlevmask (1.0) to bottom (smax) 
!
!  smax = 1.2
  resv = 1.0

  do k = 1, nlevmask
     resv(k) = smax + (float(k-1)*(1.0-smax))/float(nlevmask-1)
     do j = 1, ilat
        do i = 1, ilon
! temp       
          grida(i,j,k+1) =grida(i,j,k+1)*dble(resv(k))
! div and vor
          grida(i,j,2*k+levs)=grida(i,j,2*k+levs)*dble(resv(k))
          grida(i,j,2*k+levs+1)=grida(i,j,2*k+levs+1)*dble(resv(k))
! ntrac tracers
          grida(i,j,k+3*levs+1)=grida(i,j,k+3*levs+1)*dble(resv(k))
          grida(i,j,k+4*levs+1)=grida(i,j,k+4*levs+1)*dble(resv(k))
          grida(i,j,k+5*levs+1)=grida(i,j,k+5*levs+1)*dble(resv(k))
      enddo
    enddo
  enddo
  print *,"in mask_pert, levs, nlevmask,  = ", levs,nlevmask
  print *,"in mask_pert, resv(:)= ", resv
!
!  if (itopres .eq. 0) then do the extra rescaling on top levels
!  do rescaling on the top levels -------------------
!  find the max total energy gridmax from levels  mxlev -> levs 
!
   if (itopres .eq. 0) then
       print *,"in mask_pert, doing extra rescaling on top levels ===="
       gridmax = 0.0
       gridmax1 = 0.0
       do  k = mxlev,  levs
        cofo1 = 0.0
        grid1(:,:)=real(grida(:,:,2*k+levs))
        call sptez(0,jcap,4,ilon,ilat,cofo1,grid1,-1)
!
        cofo2 = 0.0
        grid2(:,:) = real(grida(:,:,2*k+levs+1))
        call sptez(0,jcap,4,ilon,ilat,cofo2,grid2,-1)
!   compute u, v
        call sptezv(0,jcap,4,ilon,ilat,cofo1,cofo2,grid1,grid2,+1)
!       grid1, grid2 --> grid point values of u, v, 
!       call kinendist(dx,grid1,grid2,grid,ilon,ilat)
!       grid --> sqt of KE over grids
!       print *,"in mask_pert,ave KE over all grids at level=28 ",dx
!
        tem(:,:)=real(grida(:,:,k+1))
        q1(:,:)=real(grida(:,:,k+3*levs+1))
        q2(:,:)=real(grida(:,:,k+4*levs+1))
        q3(:,:)=real(grida(:,:,k+5*levs+1))
        call tendist(grid1,grid2,tem,q1,q2,q3,b1,b2,b3,ilon,ilat,grid,dx)
!       grid --> sqt of total energy over grids
        print *,"in mask_pert,ave TE over all grids at level k= ",k, dx
!       total energy based geographical rescaling 
        gridmax = max(grid, gridmax1)
        gridmax1(:, :)  = gridmax(:, :)
       enddo  
!
        cofo1 = 0.0
        call sptez(0,jcap,4,ilon,ilat,cofo1,gridmax,-1)
!       cofo1 --> spectrum of sqt of TE at level 28         
        cofo2 = 0.0
        grid2 = 0.0
        call smooth(cofo1,cofo2,1,smx,jcap)
        call sptez(0,jcap,4,ilon,ilat,cofo2,grid2,+1)
        print *,"in mask_pert, grid values of sqt of KE at level 28"
        call range(grid2,ilon,ilat)
!
        do ila=1,ilat
           do ilo=1,ilon
               gresc(ilo,ila)=contop/grid2(ilo,ila)
	   enddo
        enddo
!
        do ila=1,ilat
           do ilo=1,ilon
              if(gresc(ilo,ila).gt.1.0)  gresc(ilo,ila)=1.0
           enddo
        enddo
        print *,' in mask_pert, for top levels: scaling factors'
	call range(gresc,ilon,ilat)
!
! rescaling for levels from levs to nlevrs which has rescaling 1.0
!
        do k = nlevrs, levs
           res = 0.0
           do j = 1, ilat
   	     do i = 1, ilon
                 res(i, j)=(float(k-nlevrs)*gresc(i,j) +  &
                 1.0*float(levs-k))/float(levs-nlevrs)
             enddo
           enddo
           do j = 1, ilat
              do i = 1, ilon
! temp       
              grida(i,j,k+1) =grida(i,j,k+1)*dble(res(i,j))
! div and vor
              grida(i,j,2*k+levs)=grida(i,j,2*k+levs)*dble(res(i,j))
              grida(i,j,2*k+levs+1)=grida(i,j,2*k+levs+1)*dble(res(i,j))
! ntrac tracers
              grida(i,j,k+3*levs+1)=grida(i,j,k+3*levs+1)*dble(res(i,j))
              grida(i,j,k+4*levs+1)=grida(i,j,k+4*levs+1)*dble(res(i,j))
              grida(i,j,k+5*levs+1)=grida(i,j,k+5*levs+1)*dble(res(i,j))
              enddo
           enddo
        enddo
!
!
endif
!
!
544 continue
  close(48)
  call sigio_axdata(data,iret)
  !       deallocate(dummy,cofi,str,stra,grid,gridk,grida2,gu,gv,geogr,
  !     & gresc,geogl,zgeogr,zgrid,coft)
  RETURN
end subroutine mask_pert

! --------------------------------------------------------------------
subroutine factor_t(ftt,nrec1,ilon,ilat,grida,scf)

!   by Mozheng Wei, 2004-10-15
!  compute rescaling factor scf  -> output
!  get the temp at lowest level nr=3-1=2 and get the scaling factor
!  nr= 15-1 = 14 for Temp at 500mb
! --------------------------------------------------------------------

  real*8 grida(ilon,ilat,nrec1)
  real*8 ftt,scf,trms
!  real grid(ilon,ilat)
  
  nt = 14
  print *, ' in gefs_init_et, starting factor_t.exe '
!  grid = 0.0
  trms = 0.0
!  grid(:,:) = grida(:,:,nt)
  do j = 1, ilat
     do  i = 1, ilon
        trms = trms + grida(i,j,nt)*grida(i,j,nt)
     enddo
  enddo
  trms = sqrt(trms/(ilat*ilon))
  scf = ftt/trms
      
end subroutine factor_t

! ----------------------------------------------------------------

subroutine inflate(scf,nrec1,ilon,ilat,grida)

!   by Mozheng Wei, 2004-10-15
!  input rescaling factor-> scf,   scale the perts grida
! ----------------------------------------------------------------

  real*8 grida(ilon,ilat,nrec1)
  real*8 scf
  
!!$!  input rescaling factor-> scf,   scale the perts grida
!!$
!!$  do  j = 1, ilat
!!$     do i = 1, ilon
!!$        grida(i, j, 1) = grida(i, j, 1)*scf
!!$!              grida(i, j, 1) = alog((grida(i,j,1))/10.0)
!!$     enddo
!!$  enddo
!!$
!!$  do nr = 2, nrec1
!!$     do j = 1, ilat
!!$        do  i = 1, ilon
!!$           grida(i, j, nr) = grida(i, j, nr)*scf
!!$        enddo
!!$     enddo
!!$  enddo

grida = grida * scf

end subroutine inflate

! ----------------------------------------------------------------

subroutine read_sig_all(jcap,lrec,nrec1,levs,nlath,ilat,ilon,ir,grid,irec)
  
  !   by Mozheng Wei, 2003-10-15
  
  character*8 label(4),label2(4)
  dimension idate1(4),idate2(4),idate(4)
  real ext1(44)
  real*8 grid(ilon,ilat,nrec1)
  real,allocatable::cofo(:)
  !      	complex,allocatable::cofo(:)
  
  real,dimension(:),allocatable ::sigi,sigl,dummy1
  real,dimension(:,:),allocatable ::grid1
  
  levsp=levs+1
  allocate( sigi(levsp),sigl(levs),dummy1(201-levs-levsp),cofo(lrec),grid1(ilon,ilat)  )
  print *, '  ---- in et, starting read_sig_all.exe -------'
  rewind(ir)
  read(ir) label
  read(ir) fhr1,(idate1(i),i=1,4),(sigi(k),k=1,levsp),(sigl(k),k=1,levs),dummy1,ext1
  write(6,1001) fhr1,idate1
1001 format(5x,'ir ',f10.1,2x,4i5)
  id=1
  write(6,1333) id,ext1(6),ext1(13),ext1(14),ext1(15),ext1(16)
1333 format(1x,i3,' = file id',5f9.2,' = ext(6,13,14,15,16)')
  
  !     determine irec to specify a sigma level near 500hpa
  
  diffmin=1.0e20
  kdiffmin=0
  do k=1,levs
     diff=abs(sigl(k) - 0.5)
     if ( diffmin .gt. diff ) then
        diffmin=diff
        kdiffmin=k
     endif
  enddo
  if ( (kdiffmin .le. 0) .or. (kdiffmin .gt. levs ) ) then
     print *,' read_sig_all, bad kdiffmin = ', kdiffmin
     print *,' levs = ', levs
     print *,' k, sigl(k) = ',(k, sigl(k), k = 1, levs)
  endif
  irec=2+levs+2*kdiffmin
  print *,' in read_sig_all, irec = ',irec
  
  cofo = 0.0
  grid1 = 0.0
  grid = 0.0
  read(ir) (cofo(J),J=1,lrec)
  !       starts from surface pressure
  do nr = 1, nrec1
     read(ir) (cofo(J),J=1,lrec)
     call sptez(0,jcap,4,ilon,ilat,cofo,grid1,+1)
     grid(:,:,nr) = dble(grid1(:,:))
  enddo
  close(ir)
  deallocate( sigi,sigl,dummy1,cofo,grid1 )
  print *, '-- in et, end of read_sig_all.exe ----'
  return
END subroutine read_sig_all

!------------------------------------------------------------------

subroutine print_sig(jcap,lrec,nrec,levs,nlath,ilon,ilat,coft)

  !   by Mozheng Wei, 2003-10-15

!  printing T, U, V at lev=1, levs given coft
!------------------------------------------------------------------
  
  real coft(lrec,nrec)
  
  real cofo(lrec),cofo2(lrec),grid(ilon,ilat),&
       grid2(ilon,ilat),ug(ilon,ilat,levs),vg(ilon,ilat,levs),&
       tg(ilon,ilat,levs)
  real*8 grid3(ilon,ilat)
  print *, '  ---- in et, starting print_sig.exe -------'
  cofo = 0.0
  grid = 0.0
! levs levels of temperature
  do  nr = 3, levs+2
     do j=1, lrec
        cofo(j)=coft(j, nr)
     enddo
     call sptez(0,jcap,4,ilon,ilat,cofo,grid,+1)
     tg(1:ilon,1:ilat,nr-2) = grid(1:ilon,1:ilat)
  enddo
  write(99,*) 'range of T at lev=1 is: '
  grid3(:,:) = dble(tg(:,:, 1))
  write(99,*) ' T(1:5,1:5) at lev=1 is: ', grid3(1:5, 1:5)
  call range2(grid3,ilon,ilat,dmin,dmax,avg,adev,sdev,skew)
  write(99,*) ' dmin,dmax,avg,adev,sdev,skew = ',&
       dmin,dmax,avg,adev,sdev,skew
  write(99,*) 'range of T at lev=levs is: '
  grid3(:,:) = dble(tg(:,:, levs))
  write(99,*) ' T(1:5,1:5) at lev=levs is:', grid3(1:5, 1:5)
  call range2(grid3,ilon,ilat,dmin,dmax,avg,adev,sdev,skew)
  write(99,*) ' dmin,dmax,avg,adev,sdev,skew = ',&
       dmin,dmax,avg,adev,sdev,skew
  
  ! levs levels of divergence and vorticity
  
  nuv = 0
  do  nr = levs+3, 3*levs+1, 2
     do j = 1, lrec
        cofo(j)=coft(j, nr)
        cofo2(j)=coft(j, nr+1)
     enddo
     call sptezv(0,jcap,4,ilon,ilat,cofo,cofo2,grid,grid2,+1)
     nuv = nuv + 1
     ug(:,:,nuv) = grid(:,:)
     vg(:,:,nuv) = grid2(:,:)
  enddo
  
  write(99,*) 'range of U at lev=1 is: '
  grid3(:,:) = dble(ug(:,:,1))
  write(99,*) ' U(1:5,1:5) at lev=1 is: ', grid3(1:5, 1:5)
  call range2(grid3,ilon,ilat,dmin,dmax,avg,adev,sdev,skew)
  write(99,*) ' dmin,dmax,avg,adev,sdev,skew = ',&
       dmin,dmax,avg,adev,sdev,skew
  write(99,*) 'range of U at lev=levs is: '
  grid3(:,:) = dble(ug(:,:, levs))
  write(99,*) ' U(1:5,1:5) at lev=levs is: ', grid3(1:5, 1:5)
  call range2(grid3,ilon,ilat,dmin,dmax,avg,adev,sdev,skew)
  write(99,*) ' dmin,dmax,avg,adev,sdev,skew = ',&
       dmin,dmax,avg,adev,sdev,skew
  write(99,*) 'range of V at lev=1 is: '
  grid3(:,:) = dble(vg(:,:,1))
  write(99,*) ' V(1:5,1:5) at lev=1 is: ', grid3(1:5, 1:5)
  call range2(grid3,ilon,ilat,dmin,dmax,avg,adev,sdev,skew)
  write(99,*) ' dmin,dmax,avg,adev,sdev,skew = ',&
       dmin,dmax,avg,adev,sdev,skew
  write(99,*) 'range of V at lev=levs is: '
  grid3(:,:) = dble(vg(:,:, levs))
  write(99,*) ' V(1:5,1:5) at lev=levs is: ', grid3(1:5, 1:5)
  call range2(grid3,ilon,ilat,dmin,dmax,avg,adev,sdev,skew)
  write(99,*) ' dmin,dmax,avg,adev,sdev,skew = ',&
       dmin,dmax,avg,adev,sdev,skew
  
  
  print *, '  ---- in et, end of print_sig.exe -------'
  return
END subroutine print_sig

! --------------------------------------------------------------------

subroutine print_pert(jcap,lrec,nrec1,levs,nlath,ilon,ilat,grida)

  !   by Mozheng Wei, 2003-10-15
! --------------------------------------------------------------------

  real*8 grida(ilon,ilat,nrec1)
  
  real cofo(lrec),grid(ilon,ilat),coft(lrec,nrec1+1) 
  print *, '  -in et, starting print_pert.exe --'
  
  coft = 0.0
  do nr = 1, nrec1
     grid(:,:) = real(grida(:,:,nr))
     cofo = 0.0
     call sptez(0,jcap,4,ilon,ilat,cofo,grid,-1)
     coft(:, nr+1)=cofo(:)
  enddo
  print *, 'range of Pert T, U, V ==============: '
  write(99,*) 'range of Pert T, U, V ==============: '
  write(99,*) ' ---------------------------------------- '
  call print_sig(jcap,lrec,nrec1+1,levs,nlath,ilon,ilat,coft)
  print *, '---in et, end of print_pert.exe ----'
  return
END subroutine print_pert

! ----------------------------------------------------------------

subroutine write_pert(jcap,lrec,nrec1,levs,ilon,&
     ilat,gridd,tg,wg,jcaps,lrecs,ilons,ilats)
  
  !   by Mozheng Wei, 2003-10-15
  
  !  write levs of grid point values of t, wind
  
  real gridd(ilon,ilat,nrec1)
  real tg(ilons,ilats,levs),wg(ilons,ilats,levs)
  
  real,allocatable::cofo(:),cofo2(:)
  real,allocatable::coft(:,:),cofts(:,:)
  real,dimension(:,:),allocatable ::grid
  
  allocate( cofo(lrec),grid(ilon,ilat),coft(lrec,nrec1+1),&
       cofts(lrecs,nrec1+1), cofo2(lrecs)  )
  print *, '  -in et, starting write_pert.exe --'
  
  coft = 0.0
  do nr = 1, nrec1
     grid(:,:) = real(gridd(:,:,nr))
     cofo = 0.0
     call sptez(0,jcap,4,ilon,ilat,cofo,grid,-1)
     coft(:, nr+1)=cofo(:)
  enddo
  !  reduce resolution to T62
  cofts = 0.0
  do nr = 1, nrec1
     cofo(:) = coft(:, nr+1)
     call reduce(jcap,cofo,jcaps,cofo2)
     cofts(:, nr+1)=cofo2(:)
  enddo
  call write_tuv(jcaps,lrecs,nrec1+1,levs,ilons,ilats,cofts,tg, wg)
  deallocate(cofo,cofo2,coft,cofts,grid)
  print *, '---in et, end of write_pert.exe ----'
  return
END subroutine write_pert

! ---------------------------------------------------------------

subroutine write_tuv(jcap,lrec,nrec,levs,ilon,ilat,coft,tg,wg)

  !   by Mozheng Wei, 2003-10-15
  
  !  write T, wind (W) at lev=1 ... levs given coft
  
  real coft(lrec,nrec)
  real tg(ilon,ilat,levs),wg(ilon,ilat,levs)
  !
  real,allocatable::cofo(:),cofo2(:)
  real,allocatable::ug(:,:,:),vg(:,:,:)
  
  real,dimension(:,:),allocatable ::grid,grid2
  !        real,dimension(:,:),allocatable ::grid3
  
  allocate(cofo(lrec),cofo2(lrec),grid(ilon,ilat),&
       grid2(ilon,ilat),ug(ilon,ilat,levs),vg(ilon,ilat,levs) )
  print *, '  ---- in et, starting write_tuv.exe -------'
  cofo = 0.0
  grid = 0.0
  ! levs levels of temperature
  do  nr = 3, levs+2
     do j=1, lrec
        cofo(j)=coft(j, nr)
     enddo
     call sptez(0,jcap,4,ilon,ilat,cofo,grid,+1)
     tg(1:ilon,1:ilat,nr-2) = grid(1:ilon,1:ilat)
  enddo
  
  ! levs levels of divergence and vorticity
  
  nuv = 0
  do  nr = levs+3, 3*levs+1, 2
     do j = 1, lrec
        cofo(j)=coft(j, nr)
        cofo2(j)=coft(j, nr+1)
     enddo
     call sptezv(0,jcap,4,ilon,ilat,cofo,cofo2,grid,grid2,+1)
     nuv = nuv + 1
     ug(:,:,nuv) = grid(:,:)
     vg(:,:,nuv) = grid2(:,:)
  enddo
  
  do k = 1, nuv
     do i = 1, ilon
        do j = 1, ilat
           wg(i, j, k) = sqrt(ug(i,j,k)*ug(i,j,k) + vg(i,j,k)*vg(i,j,k))
        enddo
     enddo
  enddo
  deallocate(cofo,cofo2,grid,grid2,ug,vg)
  
  print *, '  ---- in et, end of write_tuv.exe -------'
  return
END subroutine write_tuv

! ----------------------------------------------------------------
subroutine reduce(jcap,cofb,jcaps,cofs) 
    !   by Mozheng Wei, 2004-10-15
    !       cofb has jcap --> cofs with jcaps which is smaller
  real cofb((jcap+1)*(jcap+2))
  real cofs((jcaps+1)*(jcaps+2))
  
  inb = 0
  ins = 0
  do m = 0, jcap
     do n = m, jcap
        inb = inb + 1
        if (n .le. jcaps) then
           ins = ins + 1
           cofs(2*ins-1) = cofb(2*inb-1)
           cofs(2*ins) = cofb(2*inb)
        endif
     enddo
  enddo
  return
end subroutine reduce
! -------------------------------------------------------------------

subroutine tw_pert(ilon,ilat,levs,nens,tgf,wgf,tga,wga)
  
  !   by Mozheng Wei, 2003-10-15
  
  real tgf(ilon,ilat,levs,nens), wgf(ilon,ilat,levs,nens)
  real tga(ilon,ilat,levs,nens), wga(ilon,ilat,levs,nens)

  real,dimension(:,:,:),allocatable ::tga3,wga3,tgf3,wgf3,tsc3,wsc3
  real,dimension(:,:),allocatable ::tga2,wga2,tsc2,wsc2
  real,dimension(:),allocatable ::tga1,wga1,tsc1,wsc1

  allocate( tga3(ilon,ilat,levs), wga3(ilon,ilat,levs),&
       tgf3(ilon,ilat,levs), wgf3(ilon,ilat,levs),&
       tsc3(ilon,ilat,levs), wsc3(ilon,ilat,levs),&
       tga2(ilon,ilat),wga2(ilon,ilat),tga1(levs),wga1(levs),&
       tsc2(ilon,ilat),wsc2(ilon,ilat),tsc1(levs),wsc1(levs) )
  
  ! get the temp at lowest level and get the scaling factor
  
  print *, ' in gefs_init_et, starting tw_pert.exe '

  open(52, file='tw_pert_3d', form='unformatted')
  rewind(52)
  write(52) tga
  write(52) wga
  write(52) tgf
  write(52) wgf
  close(52)
  
  !  rms values over different members
  
  tga3 = 0.0 
  tgf3 = 0.0
  wga3 = 0.0 
  wgf3 = 0.0
  tsc3 = 0.0
  wsc3 = 0.0
  do k = 1, levs
     do i = 1, ilon
        do j = 1, ilat
           trm = 0.0
           trm2 = 0.0
           wrm = 0.0
           wrm2 = 0.0
           do m = 1, nens
              trm=trm   + tga(i,j,k,m)*tga(i,j,k,m)
              trm2=trm2 + tgf(i,j,k,m)*tgf(i,j,k,m)
              wrm=wrm   + wga(i,j,k,m)*wga(i,j,k,m)
              wrm2=wrm2 + wgf(i,j,k,m)*wgf(i,j,k,m)
           enddo
           tga3(i,j,k)=sqrt(trm/float(nens))
           tgf3(i,j,k)=sqrt(trm2/float(nens))
           wga3(i,j,k)=sqrt(wrm/float(nens))
           wgf3(i,j,k)=sqrt(wrm2/float(nens))
!  scaling ratios over fcsts 
           tsc3(i,j,k)=tga3(i,j,k)/tgf3(i,j,k)
           wsc3(i,j,k)=wga3(i,j,k)/wgf3(i,j,k)
        enddo
     enddo
  enddo
  !         write(99,*) 'tw_part_2d, tga3(1:10,1:10,13)=', 
  !     &                            tga3(1:10,1:10,13)
  !         write(99,*) 'tw_part_2d, tgf3(1:10,1:10,13)=', 
  !     &                            tgf3(1:10,1:10,13)
  !         write(99,*) 'tw_part_2d, wga3(1:10,1:10,13)=', 
  !     &                            wga3(1:10,1:10,13)
  !         write(99,*) 'tw_part_2d, wgf3(1:10,1:10,13)=', 
  !     &                            wgf3(1:10,1:10,13)
  !         write(99,*) 'tw_part_2d, tsc3(1:10,1:10,13)=', 
  !     &                            tsc3(1:10,1:10,13)
  !         write(99,*) 'tw_part_2d, wsc3(1:10,1:10,13)=', 
  !     &                            wsc3(1:10,1:10,13)
  
  !  horizontal distribution, average over different levels
  
  tga2 = 0.0
  wga2 = 0.0
  tsc2 = 0.0
  wsc2 = 0.0
  do i = 1, ilon
     do j = 1, ilat
        tav = 0.0
        wav = 0.0
        tav2 = 0.0
        wav2 = 0.0
        do k = 1, levs
           tav = tav + tga3(i,j,k)
           wav = wav + wga3(i,j,k)
           tav2 = tav2 + tsc3(i,j,k)
           wav2 = wav2 + wsc3(i,j,k)
        enddo
        tga2(i,j) = tav/float(levs)
        wga2(i,j) = wav/float(levs)
        tsc2(i,j) = tav2/float(levs)
        wsc2(i,j) = wav2/float(levs)
     enddo
  enddo
  
  open(55, file='tw_pert_2d', form='unformatted')
  rewind(55)
  write(55) tga2
  write(55) wga2
  write(55) tsc2
  write(55) wsc2
  close(55)
       
  write(99,*) 'tw_part_2d, tga2(1:5,1:5)=', tga2(1:5,1:5)
  write(99,*) 'tw_part_2d, wga2(1:5,1:5)=', wga2(1:5,1:5)
  write(99,*) 'tw_part_2d, tsc2(1:5,1:5)=', tsc2(1:5,1:5)
  write(99,*) 'tw_part_2d, wsc2(1:5,1:5)=', wsc2(1:5,1:5)
  !        write(99,*) 'tw_part_2d, tsc2 =', tsc2
  !        write(99,*) 'tw_part_2d, wsc2 =', wsc2
  !
  !  vertical distribution, average over grid points
  
  tga1 = 0.0
  wga1 = 0.0
  tsc1 = 0.0
  wsc1 = 0.0
  do k = 1, levs
     tav = 0.0
     wav = 0.0
     tav2 = 0.0
     wav2 = 0.0
     do i = 1, ilon
        do j = 1, ilat
           tav = tav + tga3(i,j,k)
           wav = wav + wga3(i,j,k)
           tav2 = tav2 + tsc3(i,j,k)
           wav2 = wav2 + wsc3(i,j,k)
        enddo
     enddo
     tga1(k) = tav/float(ilat*ilon)
     wga1(k) = wav/float(ilat*ilon)
     tsc1(k) = tav2/float(ilat*ilon)
     wsc1(k) = wav2/float(ilat*ilon)
  enddo
  !    
  open(56, file='tw_pert_z')
  rewind(56)
  write(56,11) (tga1(k), k=1, levs)
  write(56,11) (wga1(k), k=1, levs)
  write(56,11) (tsc1(k), k=1, levs)
  write(56,11) (wsc1(k), k=1, levs)
  close(56)
11 format(g13.6,1x)

  deallocate(tga3,wga3,tgf3,wgf3,tsc3,wsc3,tga2,wga2,tsc2,&
       wsc2,tga1,wga1,tsc1,wsc1)
  
  print *, ' in gefs_init_et, end of tw_pert.exe '
  return
end subroutine tw_pert

!-----------------------------------------------------------------

subroutine range2(var,nlon,nlat,dmin,dmax,avg,adev,sdev,skew)
  !	  dimension var(nlon,nlat)
  real*8 var(nlon,nlat)
  real var2(nlon,nlat),dmin,dmax,avg,adev,sdev,skew
  ! uses srange instead of doing its own calculation
  var2(:,:) = real(var(:,:))
  nlonlat=nlon*nlat
  call srange2(var2,nlonlat,dmin,dmax,avg,adev,sdev,skew) 
  return
end subroutine range2


subroutine srange2(var,nlat,dmin,dmax,avg,adev,sdev,skew)
  dimension var(nlat)
  !         real var(nlat),dmin,dmax,avg,adev,sdev,skew
  !    produces range, mean, avg dev, std dev, skew
  ptsn=nlat
  sa=0.0
  dmin=1.e40
  dmax=-1.e40
  do j=1,nlat
     sa=sa+var(j)
     dmin=min(dmin,var(j))
     dmax=max(dmax,var(j))
  enddo
  avg=sa/ptsn
  sl=0.0
  sv=0.0
  do j=1,nlat
     sl=sl+abs(var(j)-avg)
     sv=sv+(var(j)-avg)**2
  enddo
  adev=sl/ptsn
  sdev=sqrt(sv/(ptsn-1))
  if (sdev.gt.0.0) then
     ss=0.0
     do j=1,nlat
        devn=(var(j)-avg)/sdev
        ss=ss+devn**3
     enddo
     skew=ss/ptsn
  else
     skew=0.0
  endif
  !          print *,dmin,dmax,avg,adev,sdev,skew
  return
end subroutine srange2

subroutine range(var,nlon,nlat)
  dimension var(nlon,nlat)
! uses srange instead of doing its own calculation
  nlonlat=nlon*nlat
  call srange(var,nlonlat) 
  return
end subroutine range

!---------------------------------------------
subroutine srange(var,nlat)
  dimension var(nlat)
! produces range, mean, avg dev, std dev, skew
  ptsn=nlat
  sa=0.0
  dmin=1.e40
  dmax=-1.e40
  do j=1,nlat
     sa=sa+var(j)
     dmin=min(dmin,var(j))
     dmax=max(dmax,var(j))
  enddo
  avg=sa/ptsn
  sl=0.0
  sv=0.0
  do j=1,nlat
     sl=sl+abs(var(j)-avg)
     sv=sv+(var(j)-avg)**2
  enddo
  adev=sl/ptsn
  sdev=sqrt(sv/(ptsn-1))
  if (sdev.gt.0.0) then
     ss=0.0
     do j=1,nlat
        devn=(var(j)-avg)/sdev
        ss=ss+devn**3
     enddo
     skew=ss/ptsn
  else
     skew=0.0
  endif
  print *,dmin,dmax,avg,adev,sdev,skew
  return
end subroutine srange


! ------------------------------------------------------------------
subroutine read_ana2(ir,jcap,lrec,nrec1,levs,levsp,ilat,ilon,&
     cofo_1,g_ana,label,idate,fhr1,sigi,sigl,dummy1,ext1)
  
  !    by Mozheng Wei, 2004-10-15
!-------------------------------------------------------------------  
  character*8 label(4)
  dimension idate(4)
  real cofo_1(lrec),g_ana(ilon,ilat,nrec1),sigi(levsp), &
       sigl(levs),dummy1(201-levs-levsp),ext1(44)
  
  real cofo(lrec),grid(ilon,ilat)
  
  levsp=levs+1
  print *, '  --- in et, starting read_ana.exe --'
  rewind(ir)
  read(ir) label
  read(ir) fhr1,(idate(i),i=1,4),&
       (sigi(k),k=1,levsp),(sigl(k),k=1,levs),dummy1,ext1
  
  !      reads orography
  cofo_1 = 0.0
  read(ir) (cofo_1(j), j = 1, lrec)
  do nr = 1, nrec1
     cofo = 0.0
     grid = 0.0
     read(ir) (cofo(j), j = 1, lrec)
     call sptez(0,jcap,4,ilon,ilat,cofo,grid,+1)
     g_ana(:,:,nr) = grid(:,:)
  enddo
  close(ir)
  print *, '---in et, end of read_ana.exe ----'
  return
END subroutine read_ana2

! ----------------------------------------------------------------------

subroutine write_ana2(iout,grida,ir,jcap,lrec,nrec1,levs,levsp,&
     ilat,ilon,cofo_1,g_ana,label,idate,fhr1,sigi,sigl,dummy1,ext1)
  
  !    by Mozheng Wei, 2004-10-15
  
  character*8 label(4)
  dimension idate(4)
  real cofo_1(lrec),g_ana(ilon,ilat,nrec1),sigi(levsp),&
       sigl(levs),dummy1(201-levs-levsp),ext1(44)
  real*8 grida(ilon,ilat,nrec1)
  real cofo(lrec),grid(ilon,ilat)
  
  
  levsp=levs+1
  print *, '  -- in et, starting write_ana.exe --'
  write(iout) label
  write(iout) fhr1,(idate(i),i=1,4), &
       (sigi(k),k=1,levsp),(sigl(k),k=1,levs),dummy1,ext1
  
  !     write perturbed sigma file
  
  write(iout) (cofo_1(j), j = 1, lrec)
  do nr = 1, nrec1
     cofo = 0.0
     grid = 0.0
     grid(:,:) = g_ana(:,:,nr) + real(grida(:,:,nr))
     cofo = 0.0
     call sptez(0,jcap,4,ilon,ilat,cofo,grid,-1)
     write(iout) (cofo(j), j = 1, lrec)
  enddo
  close(iout)
  print *, '---in et, end of write_ana.exe ----'
  return
END subroutine write_ana2

!-----------------------------------------------------------------------

subroutine read_fcst2(jcap,lrec,nrec1,levs,nlath,ilat,ilon,&
     ir,grid,irec,levsp)

  !  by Mozheng Wei, 2004-10-15

  !  from read_sig_all, avoid dynamic array to save time
  character*8 label(4),label2(4)
  dimension idate1(4),idate2(4),idate(4)
  real ext1(44)
  real grid(ilon,ilat,nrec1)

  real sigi(levsp),sigl(levs),dummy1(201-levs-levsp),&
       cofo(lrec),grid1(ilon,ilat)  

  print *, '  ----starting read_fcst.exe -------'
  rewind(ir)
  read(ir) label
  read(ir) fhr1,(idate1(i),i=1,4),(sigi(k),k=1,levsp),(sigl(k),k=1,levs),dummy1,ext1
  write(6,1001) fhr1,idate1
1001 format(5x,'ir ',f10.1,2x,4i5)
  id=1
  write(6,1333) id,ext1(6),ext1(13),ext1(14),ext1(15),ext1(16)
1333 format(1x,i3,' = file id',5f9.2,' = ext(6,13,14,15,16)')

  !    to save time, remove this, set irec value
  !     determine irec to specify a sigma level near 500hpa
  
  !        diffmin=1.0e20
  !        kdiffmin=0
  !       do k=1,levs
  !	  diff=abs(sigl(k) - 0.5)
  !	  if ( diffmin .gt. diff ) then
  !	     diffmin=diff
  !	     kdiffmin=k
  !	  endif
  !        enddo
  !        if ( (kdiffmin .le. 0) .or. (kdiffmin .gt. levs ) ) then
  !	  print *,' read_fcst, bad kdiffmin = ', kdiffmin
  !	  print *,' levs = ', levs
  !	  print *,' k, sigl(k) = ',(k, sigl(k), k = 1, levs)
  !        endif
  !        irec=2+levs+2*kdiffmin
  !        print *,' in read_fcst, irec = ',irec
  
  cofo = 0.0
  grid1 = 0.0
  grid = 0.0
  read(ir) (cofo(J),J=1,lrec)
  !       starts from surface pressure
  do nr = 1, nrec1
     read(ir) (cofo(J),J=1,lrec)
     call sptez(0,jcap,4,ilon,ilat,cofo,grid1,+1)
     grid(:,:,nr) = dble(grid1(:,:))
  enddo
  close(ir)
  print *, '-- in gefs_init_et, end of read_fcst.exe ----'
  return
END subroutine read_fcst2

! ---------------------------------------------------------------------
!  subroutine to compute kinetic energy of u&v gridded fields
!    point by point; grid1,grid2-input, grido=output
!    square root of kinen taken for gridded output
! ---------------------------------------------------------------------

subroutine kinendist(d,grid1,grid2,grido,nlon,nlat)
  dimension grid1(nlon,nlat),grid2(nlon,nlat),grido(nlon,nlat)
  ds=0.
  ns=0
  do i=1,nlat
     if(i.le.nlat/2) then
        xlat=float(nlat)/2.-float(i)
     else
        xlat=float(i)-float(nlat)/2.
     endif
     weight=cos((xlat*2./float(nlat))*1.570796)
     do j=1,nlon
        ds=ds+(grid1(j,i)**2+grid2(j,i)**2)*weight/2.
        !      ds=ds+(grid1(jj,i)**2+grid2(jj,i)**2)*weight/2.
        grido(j,i)=sqrt(grid1(j,i)**2+grid2(j,i)**2)
        ns=ns+1
     enddo
     d=ds/float(ns)
  end do
end subroutine kinendist
!
! -------------------------------------------------------------------------
!   subroutine to compute total  energy of u & v T, q1, q2, q3 gridded fields
!   point by point; grid=output
!   square root of total energy taken for gridded output
!----------------------------------------------------------------------------
subroutine tendist(grid1,grid2,tem,q1,q2,q3,b1,b2,b3,nlon,nlat,grid,d)
  dimension grid1(nlon,nlat),grid2(nlon,nlat),grid(nlon,nlat)
  dimension tem(nlon,nlat),q1(nlon,nlat),q2(nlon,nlat)
  dimension q3(nlon,nlat)
! alp = Cp/Tr-->1004.0/251.0=4.0 for temp coef in total energy
  PARAMETER(alp=4.0)
  ds=0.0
  ns=0
  do i = 1, nlat
     if(i.le.nlat/2) then
        xlat=float(nlat)/2.-float(i)
     else
        xlat=float(i)-float(nlat)/2.
     endif
     weight=cos((xlat*2./float(nlat))*1.570796)
     do j = 1, nlon
        ds = ds+(grid1(j,i)**2+grid2(j,i)**2 +  &
        alp*tem(j,i)**2 + b1*q1(j,i)**2 + b2*q2(j,i)**2 + &
        b3*q3(j,i)**2)*weight/2.0
!
        grid(j,i)=sqrt(grid1(j,i)**2+grid2(j,i)**2+   &
        alp*tem(j,i)**2 + b1*q1(j,i)**2 + b2*q2(j,i)**2 + b3*q3(j,i)**2)
        ns=ns+1
     enddo
  enddo
  d=ds/float(ns)
  return
  end
!
! ---------------------------------------------------------------------
!     subroutine fill(jcapi,jcapo,inp1,inp2)
!     parameter(nrec=142,levs=28,levsp=29)
!     cofi has jcapi triangular truncation that is smaller than cofo 
!        output
!     subroutine fills up with zeros
!     cofo except lower triangular, where it gets cofi values.
! ---------------------------------------------------------------------

subroutine fill(jcapi,jcapo,nrec,levs,inp1,inp2)
  complex cofi((jcapi+1)*(jcapi+2)/2),cofo((jcapo+1)*(jcapo+2)/2)
  dimension idate(4)
  dimension ext(44)
  character*8 label(4)

  allocatable dphi(:)
  allocatable dlam(:)
  allocatable dummy(:)

  levsp=levs+1
  allocate(dphi(levsp))
  allocate(dlam(levs))
  allocate(dummy(201-levs-levsp))


  write(6,322) jcapi,jcapo
322 format(1x,'fill coeffs from ',i3,' to ',i3)

  lreci=(jcapi+1)*(jcapi+2)/2
  lreco=(jcapo+1)*(jcapo+2)/2
  write(6,333) lreci,lreco
333 format(1x,'record lengths ',2i7)

  open(inp1,form='unformatted')
  open(inp2,form='unformatted')

  rewind(inp1)
  rewind(inp2)

  read(inp1)label
  read(inp1)fhr,(idate(n),n=1,4),(dphi(k),k=1,levsp),(dlam(k),k=1,levs),&
       dummy,ext

  ext(1)=jcapo
  ext(13)=2
  ext(14)=2
  write(inp2)label
  write(inp2)fhr,(idate(n),n=1,4),(dphi(k),k=1,levsp),(dlam(k),k=1,levs),&
       dummy,ext

  do  nr = 1 , nrec
     read(inp1)(cofi(i),i=1,lreci)
     indi=0
     indo=0
     do j=1,lreco
        cofo(j)=cmplx(0.,0.)
     enddo
     do  m=0,jcapo
        do n=m,jcapo
           indo=indo+1
           if(n.le.jcapi) then
              indi=indi+1
              cofo(indo)=cofi(indi)
           else
              cofo(indo)=cmplx(0.,0.)
           endif
        enddo
     enddo
     write(inp2)(cofo(i),i=1,lreco)
  enddo
  close(inp1)
  close(inp2)
  deallocate(dphi)
  deallocate(dlam)
  deallocate(dummy)
end subroutine fill

!---------------------------------------------
subroutine smooth(cofi,cofo,iord,sizeofsm,jcap)
  !    this subroutine performs jim purser's smoother
  !  cofi  input spher. coeff., triang. trunc jcap
  !    cofo  output spher. coeff., triang. trunc jcap
  !  iord  order of smoothing; 1=normal-shape, 7=approx. box shape
  !  sizeofsm size of area used to smooth in earth radius in radians 
  !          (1.57=90deg);
  !       eg, .1 for +-9deg dropoff (st dev) in smoothing
  !---------------------------------------------
  dimension xcof(7,7),xkappa(jcap+1),acof(7)
  complex   cofi((jcap+1)*(jcap+2)/2),cofo((jcap+1)*(jcap+2)/2)
  data xcof /1.,0.,0.,0.,0.,0.,0.,&
       2.,-1.,0.,0.,0.,0.,0.,&
       6.,-6.,1.,0.,0.,0.,0.,&
       24.,-36.,12.,-1.,0.,0.,0.,&
       120.,-240.,120,-20.,1.,0.,0.,&
       720.,-1800.,1200.,-3000.,30.,-1.,0.,&
       5040.,-15120.,12600.,-4200.,630.,-42.,1./
  data acof /1.,2.,6.,24.,120.,720.,5040./
  allocatable funct(:)
  allocate(funct(jcap+1))
  xsize=sizeofsm
  print'('' xsize='',e12.6,i5)',xsize,iord
  do i=1,jcap+1
     funct(i)=0.0
  enddo
  do ii=1,jcap+1
     i=ii-1
     xkappa(ii)=(xsize*xsize)*float(i)*float(i+1)
     !      write(6,78) ii,xkappa(ii)
     do  j=1,iord
        if(j.eq.1) then
           funct(ii)=funct(ii)+xcof(j,iord)/acof(iord)
        else
           funct(ii)=funct(ii)+(xcof(j,iord)/&
                acof(iord))*(xkappa(ii)**(j-1))
        endif
     enddo
     if(xkappa(ii).gt.100.) xkappa(ii)=20.
     funct(ii)=funct(ii)*exp(-xkappa(ii))
     !	   funct(ii)=funct(ii)*((2.72)**(-xkappa(ii)))
     !      write(6,77) ii,funct(ii)
77   format(1x,'ii,funct(ii)',i5,f12.2)
78   format(1x,'ii,xkappa(ii)',i5,f12.2)
  enddo

  indo=0
  do m=0,jcap
     do n=m,jcap
        indo=indo+1
        !	  write(6,67) m,n,indo
67      format(1x,'m,n,indo=',3i10)
        !	  cofo(indo)=funct(m+1)*cofi(indo)
        cofo(indo)=funct(n+1)*cofi(indo)
     enddo
  enddo
  !	  write(6,66)
66 format(1x,'smoothing finished')
  deallocate(funct)
  return
END subroutine smooth

!---------------------------------------------
subroutine fillf(cofi,cofo,jcapi,jcapo)
  !	  parameter(nrec=142,levs=28,levsp=29)
  !     cofi has jcapi triangular truncation that is smaller than cofo 
  !        output
  !     subroutine fills array up with zero
  !   cofo except lower triangular, where it gets cofi values.
  !---------------------------------------------
  complex cofi((jcapi+1)*(jcapi+2)/2)
  complex cofo((jcapo+1)*(jcapo+2)/2)

  write(6,322) jcapi,jcapo
322 format(1x,'fill array from ',i3,' to ',i3)

  indi=0
  indo=0
  do m=0,jcapo
     do n=m,jcapo
        indo=indo+1
        if(n.le.jcapi) then
           indi=indi+1
           cofo(indo)=cofi(indi)
        else
           cofo(indo)=cmplx(0.,0.)
        endif
     enddo
  enddo

end subroutine fillf


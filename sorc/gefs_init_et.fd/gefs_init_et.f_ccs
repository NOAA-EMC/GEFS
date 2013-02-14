!
!  Main program for doing ET (Ensemble Transform)
!
        program gefs_init_et
!
!  Abstract: Generates analysis perturbations from forecast perturbations by 
!            using ET (Ensemble Transform)  method (Wei et al. 2006b).  
!            The ET transformation uses the analysis variances to restrict the
!            analysis perturbation directions at every cycle. We also impose 
!            ST (simplex transformation, Wei et. al. 2006, Tellus 58A, 28-44.) 
!            to center the transformed perturbations.         
! 
!  PROGRAM HISTORY LOG:
!
!  2004-08-24, Mozheng Wei, Initial code for ET for 10 ensemble members only.
!                           with ET and ST imposed on all 10 perturbations.
!
!  2005-03-17, Mozheng Wei, do ET and ST for 80 perturbations, then impose
!                           ST on the different 20 perturbations which will 
!                           used for long forecasts at different cycles.
!
!  2005-11-11, Mozheng Wei, do ET and ST for 56 perturbations and ST for 14
!                           perturbations which are used for long forecasts.
!
!  2006-05-30, Mozheng Wei, first became operational on May 30, 2006, at NCEP 
!                           (do ET and ST for 56 perturbations and ST for 14
!                           perturbations which are used for long forecasts).
!
!  2006-07-03, Mozheng Wei, modified to use sigio modules extensively to 
!                           handle the new GFS hybrid sigma files.
!
!  2007-04-16, Mozheng Wei, set means of vorticity and divergence zero
!                           after ET and rescaling. 
!
!  2007-05-07, Mozheng Wei, build a mask at top level and interpolated to
!                           all levels between 23 and 28.
!
!
        use sigio_module
        type(sigio_head):: heada
        character*12,dimension(50)::en1,en2,enn1,enn2
        namelist/namens/npair1,jcap,ilat,ilon,levs,ntrac,inflag,icyc,
     &  globamplm,globamplr,globamplg,contop
        namelist/DATNAMES/en1,en2,enn1,enn2

        real*8 gam,gam1,scf,ftt,aa

        real*8,dimension(:),allocatable ::aux2,eig,eig2,eig3,gv,gga
        real*8,dimension(:,:),allocatable ::rhz,vec,vec2,vec3,gf1,
     &  ss, sst

        real*8,dimension(:,:,:),allocatable ::gridm,gridf,grida,grid1,
     &  gridas,gridfs
        real*8,dimension(:,:,:,:),allocatable ::gfcst
c
        real,dimension(:,:,:,:),allocatable ::tgf,wgf,tga,wga
        real,dimension(:,:),allocatable ::gmask,kem
        real,dimension(:,:,:),allocatable ::tg,wg,kef,tge,wge
c
        character*8 label(4)
        dimension idate(4),ext1(44)
        real,dimension(:,:,:),allocatable ::g_ana
        real,dimension(:),allocatable :: cofo_1,sigi,sigl,dummy1
        
        real(8) rtc, tb, te, tbb, tee, ttb, tte

       ifhruse=24
       ipair=5
       jcap=126
       ilat=190
       ilon=384
       levs=28
       ntrac=3
       inflag=3
       call w3tagb ('gefs_init_et',2006,0120,0084,'Global Ensemble')
       print *, '  ----------  starting gefs_init_et  -------------'
c  read namelist
       read(5,namens)
       print *, 'read npair1,jcap,ilat,ilon,levs,ntrac,inflag,'
     & , npair1,jcap,ilat,ilon,levs,ntrac,inflag
       print *, 'read globamplm,globamplr,globamplg'
     & , globamplm,globamplr,globamplg
c    use real cofo 
       levsp=levs+1
       lrec=(jcap+1)*(jcap+2)
       nrec=2+(3+ntrac)*levs
       nrec1 = nrec - 1 
c  # of gaussian lats in one hemisphere
       nlath = ilat/2 + 1
c    
c      level=13 is about 500mb for 28-level model      
       n500 = 13
c  for variable vertical resolution
       n500 = ( n500 * levs + 14 )  / 28
       print *, n500,'= n500 calculated linearly'
       nmt = ilon*ilat
c
c  define T62 resolution for writing out fcst and ana perts
       jcaps=62
       ilons=192
       ilats=94
       lrecs=(jcaps+1)*(jcaps+2)
c
c      file names of data
c
       read(5,DATNAMES)
c
c       print *, 'en1 = ', en1
c       print *, 'en2 = ', en2
c       print *, 'enn1 = ', enn1
c       print *, 'enn2 = ', enn2
c
       write(6,322) globamplm,globamplr,globamplg
322    format(1x,f12.2,'=globamplm',f12.2,'=globamplr',
     *           f12.2,'=globamplg')
       write(6,333) ipair,ifhruse,inflag
333    format(1x,i4,'=ipair',i5,'=ifhruse',
     *    i4,'=inflag',
     *    8x,'resolution parameters:')
       write(6,344) jcap,ilat,ilon,lrec,levs,ntrac,nrec,icyc
344    format(1x,i5,'=jcap',i6,'=ilat',i6,'=ilon',i8,'=lrec'
     *     ,i5,'=levs',i5,'=ntrac',i8,'=nrec',i2,'=icyc')
c    
       open(99, file='checkup_et.dat')
c
c
c   if npair1=7, 14 will be used for long fcsts, total number of perts(nens=56)
c    (npair = nens/2 = 28 )    
       npair=4*npair1
       nsp=2*npair1
       nens=4*nsp
c
       print *, 'et, npair1,npair,nsp,nens = ', npair1, npair, 
     & nsp, nens
c
       allocate(gridm(ilon,ilat,nrec1),grid1(ilon,ilat,nrec1),
     & gridf(ilon,ilat,nrec1),grida(ilon,ilat,nrec1),gmask(ilon,ilat),
     & gfcst(ilon,ilat,nrec1,nens) )
c     & gridas(ilon,ilat,nrec1), gridfs(ilon,ilat,nrec1) )
c     & tg(ilons,ilats,levs),wg(ilons,ilats,levs),
c     & tgf(ilons,ilats,levs,nens),wgf(ilons,ilats,levs,nens),
c     & tga(ilons,ilats,levs,nens),wga(ilons,ilats,levs,nens) )
c
       allocate( kem(ilon,ilat),kef(ilon,ilat,nens),rhz(nmt,nens),
     & vec(nens,nens),vec2(nens,nens),vec3(nens,nens),eig(nens),
     & tge(ilon,ilat,levs),wge(ilon,ilat,levs),
     & eig2(nens),eig3(nens),aux2(nmt+2*nens),
     & ss(nsp-1,nsp),sst(nsp-1, nsp-1) )
c
c    build (nsp-1) x nsp simplex matrix
c
       ss = 0.0
       do i = 1, nsp-1
          do j = 1, nsp
             if (j .eq. (i+1)) then
                ss(i, j) = dble(i)/sqrt(dble(i*(i+1)))
             else if (j .le. i) then
                ss(i, j) = -1.0/sqrt(dble(i*(i+1)))
             else
                ss(i, j) = 0.0
             endif
          enddo
       enddo
c
c check sum of columns 
c
c        write(99,*) 'check the sum of each row for SS = '
c        do i = 1, nsp-1
c          aa = 0.0
c          do j = 1, nsp
c             aa =  aa + ss(i, j)
c          enddo
c          write(99,*) '# of row in SS, sum = ', i,  aa
c        enddo
c        sst = 0.0
c        call dgemul(ss,nsp-1,'N',ss,nsp-1,'T',sst,nsp-1,nsp-1,
c     &  nsp,nsp-1)
c        do i = 1, nsp-1
c          do j = 1, nsp-1
c             do k = 1, nsp   
c                sst(i,j) = sst(i,j) + ss(i, k)*ss(j, k)
c             enddo
c          enddo
c        enddo
c        write(99,*) ' check if  U U^T  =  I '
c        do i = 1, nsp-1
c           write(99,*) 'U U^T, i, sst(:,i) = ', i, sst(:, i)
c        enddo
c        deallocate( sst )
c
c    use the control to get date and read the mask file
c    gmask is grid point values of mask
c       print *, '-in et, read_mask to read mask'
       gmask = 0.0
       open(90, file='sanl.in',form='unformatted')
       open(48,err=544,form='unformatted')
       call read_mask(90,48,jcap,lrec,levs,ilon,ilat,gmask)
       gmask(:,:) = globamplm*gmask(:,:)
       close(90)
       close(48)
       goto 545
544    continue
       print *, ' fail to open mask file 48 '
545    continue
       print *, ' mask file 48 is opened  '
c
c  also compute the mean KE field, instead of KE of mean field
c  define gfcst to hold all fcst fields to avoid IOs
c
c
c       tb = rtc( )
       gfcst = 0.0
       gridm = 0.0
       kem = 0.0
       kef = 0.0
       do i=1, npair
          grid1 = 0.0
!          open(91, file=en1(i),form='unformatted')
!          call read_sig_all(jcap,lrec,nrec1,levs,nlath,ilat,ilon,
!     &    91,grid1,irec)
!          close(91)
          call sigio_sropen(91,en1(i),iret)
          if (iret.ne.0) print *,'sigio_sropen failed,iret=',iret
          call read_fcst(jcap,lrec,nrec1,levs,nlath,ilat,ilon,
     &    91,grid1,irec,levsp)
          call sigio_sclose(91,iret)
          if (iret.ne.0) print *,'sigio_sclose failed,iret=',iret
          gridm(:,:,:) = gridm(:,:,:) + grid1(:,:,:)/dble(nens)        
          gfcst(:,:,:,2*i-1) = grid1(:,:,:)
c
          tge = 0.0
          wge = 0.0
          call write_ke(jcap,lrec,nrec1,levs,ilon,ilat,grid1,tge,wge)
          kef(:,:,2*i-1) = wge(:,:,n500)
          kem(:,:) = kem(:,:) + wge(:,:,n500)/dble(nens)   
c
          grid1 = 0.0
!          open(92, file=en2(i),form='unformatted')
!          call read_sig_all(jcap,lrec,nrec1,levs,nlath,ilat,ilon,
!     &    92,grid1,irec)
!          close(92)
          call sigio_sropen(92,en2(i),iret)
          if (iret.ne.0) print *,'sigio_sropen failed,iret=',iret
          call read_fcst(jcap,lrec,nrec1,levs,nlath,ilat,ilon,
     &    92,grid1,irec,levsp)
          call sigio_sclose(92,iret)
          if (iret.ne.0) print *,'sigio_sclose failed,iret=',iret
          gridm(:,:,:) = gridm(:,:,:) + grid1(:,:,:)/dble(nens)        
          gfcst(:,:,:,2*i) = grid1(:,:,:)
c
          tge = 0.0
          wge = 0.0
          call write_ke(jcap,lrec,nrec1,levs,ilon,ilat,grid1,tge,wge)
          kef(:,:,2*i) = wge(:,:,n500)
          kem(:,:) = kem(:,:) + wge(:,:,n500)/dble(nens)   
       enddo
c       te = rtc( )
c       te = te - tb
c       print *, ' time for mean KE field  = ', te
c
c
c  computes perts of KE fields
c
       do i=1, npair
c          print *, 'computes perts of KE fields'
          kef(:,:,2*i-1) = kef(:,:,2*i-1) - kem(:,:)
          kef(:,:,2*i) =   kef(:,:,2*i)  -  kem(:,:)
       enddo
c
c save fcst perts of T, wind at all levels for all members
c
c       if ( 1 .eq. 0 ) then
c       tb = rtc( )
c       tgf = 0.0
c       wgf = 0.0
c
         do i=1, npair
c          grid1 = 0.0
c          open(91, file=en1(i),form='unformatted')
c          call read_sig_all(jcap,lrec,nrec1,levs,nlath,ilat,ilon,
c     &    91,grid1,irec)
c          gridf(:,:,:) = grid1(:,:,:) - gridm(:,:,:)
c          close(91)
          gridf(:,:,:) = gfcst(:,:,:,2*i-1) - gridm(:,:,:)
c   use gfcst as fcst perts
          gfcst(:,:,:,2*i-1) = gridf(:,:,:)
c
c          tg = 0.0
c          wg = 0.0
c          call  write_pert(jcap,lrec,nrec1,levs,ilon,
c     &    ilat,gridf,tg,wg,jcaps,lrecs,ilons,ilats)
c          tgf(:,:,:,2*i-1) = tg(:,:,:)
c          wgf(:,:,:,2*i-1) = wg(:,:,:)
 
c          grid1 = 0.0
c          open(92, file=en2(i),form='unformatted')
c          call read_sig_all(jcap,lrec,nrec1,levs,nlath,ilat,ilon,
c     &    92,grid1,irec)
c          gridf(:,:,:) = grid1(:,:,:) - gridm(:,:,:)
c          close(92)
          gridf(:,:,:) = gfcst(:,:,:,2*i) - gridm(:,:,:)
c   use gfcst as fcst perts
          gfcst(:,:,:,2*i) = gridf(:,:,:)
c          tg = 0.0
c          wg = 0.0
c          call  write_pert(jcap,lrec,nrec1,levs,ilon,
c     &    ilat,gridf,tg,wg,jcaps,lrecs,ilons,ilats)
c          tgf(:,:,:,2*i) = tg(:,:,:)
c          wgf(:,:,:,2*i) = wg(:,:,:)
         enddo
c       endif
c       te = rtc( )
c       te = te - tb
c       print *, 'time to compute perts only = ', te
c
c  rhz(nmt, nens)
c
c       tb = rtc( )
        print *, 'building rhz for SVD'
        rhz = 0.0
        do k = 1, nens
          n = 0
          do i = 1, ilon
            do j = 1, ilat
              n = n + 1
              rhz(n, k) = dble(kef(i,j,k))/(dsqrt(dble(nens-1))*
     &                    dble(abs(gmask(i,j))))
            enddo
          enddo
        enddo
c
c  using SVD subroutine on rhz, singular values are in eig2,
c  original rhz will be destroyed !!!
c
        eig = 0.0
        eig2 = 0.0
        vec2 = 0.0
        call dgesvf(11,rhz,nmt,dummy,1,0,eig2,nmt,nens,aux2,nmt+2*nens) 
        do k = 1, nens
            eig(k) = eig2(k)*eig2(k)
            vec2(1:nens, k) = rhz(1:nens, k)
        enddo
c
        write(99,*) 'eigvalues(r8,SVD) eig = ', eig
c        write(99,*) 'initial SVs (r8,SVD) vec2 = ', vec2

        print *, 'eigvalues(r8,SVD) eig = ', eig
c        print *, 'initial SVs (r8,SVD) vec2 = ', vec2

c
c  replace last Gamma value (0.0) with  1.0+vlaue, it doesn't affect result
c  compute transformation matrix T = C Gamma^(-1/2)
c  vec3(nens, nens)
c
        eig(nens) = eig(nens)+1.0
        vec3 = 0.0
        do k = 1, nens
           do j = 1, nens
              vec3(j, k) = vec2(j, k)/dsqrt(eig(k))
           enddo
        enddo
c 
c        write(99,*) 'updated trans matrix T, vec3 = ', vec3
c
c  mutiply the T(nens, nens) by C^T(nens, nens), use T C^T
c  vec(nens, nens)
        vec = 0.0
        call dgemul(vec3,nens,'N',vec2,nens,'T',vec,nens,nens,
     &  nens,nens)
c        write(99,*) 'final updated T by C^T, vec = ', vec
c        print *, 'final updated T by C^T, vec = ', vec
c       te = rtc( )
c       te = te - tb
c       print *, 'time for SVD part = ', te

c
c   impose simplex transfer onto nsp(=14) perts only at different cycles
c
c       tb = rtc( )
       if ( icyc  .eq. 0 ) then 
           vec3 = 0.0
           do i = 1, nens
              do j = 1, nsp-1
                do k = 1, nsp-1   
                  vec3(i,j) = vec3(i,j) + vec(i, k)*ss(k, j)
                enddo
              enddo
           enddo
           do i = 1, nens
              do j = 1, nsp-1
                  vec(i,j) = vec3(i,j)
              enddo
           enddo
       else if ( icyc  .eq. 6 ) then 
           vec3 = 0.0
           do i = 1, nens
              do j = 1, nsp-1
                do k = 1, nsp-1   
                  vec3(i,j) = vec3(i,j) + vec(i, k+nsp)*ss(k, j)
                enddo
              enddo
           enddo
           do i = 1, nens
              do j = 1, nsp-1
                  vec(i,j+nsp) = vec3(i,j)
              enddo
           enddo
       else if ( icyc  .eq. 12 ) then 
           vec3 = 0.0
           do i = 1, nens
              do j = 1, nsp-1
                do k = 1, nsp-1   
                  vec3(i,j) = vec3(i,j) + vec(i, k+2*nsp)*ss(k, j)
                enddo
              enddo
           enddo
           do i = 1, nens
              do j = 1, nsp-1
                  vec(i,j+2*nsp) = vec3(i,j)
              enddo
           enddo
       else if ( icyc  .eq. 18 ) then 
           vec3 = 0.0
           do i = 1, nens
              do j = 1, nsp-1
                do k = 1, nsp-1   
                  vec3(i,j) = vec3(i,j) + vec(i, k+3*nsp)*ss(k, j)
                enddo
              enddo
           enddo
           do i = 1, nens
              do j = 1, nsp-1
                  vec(i,j+3*nsp) = vec3(i,j)
              enddo
           enddo
       else
           print *, 'Cycling times not identified !!!! '
       endif
c       write(99,*) 'updated T by 14-simplex, vec = ', vec
c       te= rtc( )
c       te = te - tb
c       print *, 'time do 14 simplex trans = ', te
c
c
        allocate( cofo_1(lrec),g_ana(ilon,ilat,nrec1),sigi(levsp),
     &  sigl(levs),dummy1(201-levs-levsp) )
!       open(90, file='sanl.in',form='unformatted')
!       call read_ana(90,jcap,lrec,nrec1,levs,levsp,ilat,ilon,
!     & cofo_1,g_ana,label,idate,fhr1,sigi,sigl,dummy1,ext1)
!       close(90)
!
        call sigio_sropen(90,'sanl.in',iret)
        call read_ana(90,jcap,lrec,nrec1,levs,levsp,ilat,ilon,
     &  cofo_1,g_ana,heada)
        call sigio_sclose(90,iret)
!
       if ( 1 .eq. 0 ) then

c sum of forecast perts
       gridfs = 0.0
       do n = 1,  nens
          gridfs(:,:,:) = gridfs(:,:,:) + gfcst(:,:,:, n) 
       enddo
c check if the sum of fcst perts is 0
c
        write(99,*) 'member # 2n-1 = ', 2*n-1
        write(99,*) 'irec-1:  vorticity at 500mb --- '
        write(99,*) 'sum of fcst perts gridfs(1:5,1:5,irec-1)=',
     &  gridfs(1:5,1:5,irec-1)
        write(99,*) 'sum of fcst perts gridfs(1:5,1:5,1)logP=',
     &  gridfs(1:5,1:5,1)
        write(99,*) '2:  temp at bottom level --- '
        write(99,*) 'sum of fcst perts gridfs(1:5,1:5,2),T(1)=',
     &  gridfs(1:5,1:5,2)
        write(99,*) '14:  temp at 500mb --- '
        write(99,*) 'sum of fcst perts gridfs(1:5,1:5,14),T(13)=',
     &  gridfs(1:5,1:5,14)
        write(99,*) 'levs+1:  temp at top level --- '
        write(99,*) 'sum of fcst perts gridfs(1:5,1:5,levs+1),
     &  T(levs)=',  gridfs(1:5,1:5, levs+1)
        deallocate( gridfs )

        endif

c

       ftt = 0.8
       gam = 1.0
c       gridas = 0.0
c       tga = 0.0
c       wga = 0.0

       nd3 = ilon*ilat*nrec1
       allocate( gf1(nd3,npair), gv(npair), gga(nd3)  )
       gf1 = 0.0
       gv = 0.0
       gga = 0.0 
       do n = 1,  npair
        tbb = rtc( ) 
        write(99,*) ' -- final pert analysis, pair n = ', n
        write(99,*) ' -------------------------------------  '
        print *, ' -- final pert analysis, pair n = ', n
        print *, ' -------------------------------------  '

c        tb = rtc( ) 
        grida = 0.0
        gf1 = 0.0
        gv = 0.0
        do i=1, npair
          gv(i) = vec(2*i-1, 2*n-1)
          nn = 0
          do i3 = 1, nrec1
             do i2 = 1, ilat
               do i1 = 1, ilon
                 nn = nn + 1  
                 gf1(nn, i) = gfcst(i1, i2, i3, 2*i-1)
               enddo
             enddo
          enddo
        enddo
        gga = 0.0
        call dgemx(nd3,npair,gam,gf1,nd3,gv,1,gga,1)
c
        gf1 = 0.0
        gv = 0.0
        do i=1, npair
          gv(i) = vec(2*i, 2*n-1)
          nn = 0
          do i3 = 1, nrec1
             do i2 = 1, ilat
               do i1 = 1, ilon
                 nn = nn + 1  
                 gf1(nn, i) = gfcst(i1, i2, i3, 2*i)
               enddo
             enddo
          enddo
        enddo
        call dgemx(nd3,npair,gam,gf1,nd3,gv,1,gga,1)
c
        nn = 0
        do i3 = 1, nrec1
           do i2 = 1, ilat
              do i1 = 1, ilon
                 nn = nn + 1  
                 grida(i1, i2, i3) = gga(nn) 
               enddo
             enddo
         enddo
c              
c        te= rtc( )
c        te = te - tb
c        print *, 'time for 1st pair loop of analysis = ', te
c
c       writes scaled analysis perturbations
c
c        print *, '-in et, writing 1st pair of ana pert '
c        write(99,*) 'writing analysis perts, member 2n-1= ',2*n-1
c        write(99,*)  ' ----------- before inflation-----------  '
c        print *, 'writing analysis perts, member 2n-1= ',2*n-1
c        print *,  ' ----------- before inflation-----------  '
c        tb = rtc( )
c        call print_pert(jcap,lrec,nrec1,levs,nlath,ilon,ilat,grida)
c        te= rtc( )
c        te = te - tb
c        print *, 'time for print_pert.exe = ', te

c
c -----------------------------------------------------------------
c
        if (n .eq. 1) then
          call factor_t(ftt,nrec1,ilon,ilat,grida,scf)
        endif
        write(99,*) 'rms_T, inflation factor = ', ftt, scf
        print *, 'rms_T, inflation factor = ', ftt, scf
c
c  use one inflation factor scf to inflate the initial perts
c
c        tb = rtc( )
        call inflate(scf,nrec1,ilon,ilat,grida)
c
c      impose a mask on the perturbations
c
c       contop = 5.0,  input from parm
        print *, 'calling mask_pert    '
        call sigio_sropen(90,'sanl.in',iret)
        print *, 'iret from sigio_sropen =', iret
        if (iret.ne.0) print *,'sigio_sropen failed,iret=',iret
        call mask_pert(90,jcap,lrec,nrec1,irec,levs,nlath,ilon,
     &   ilat,grida,globamplr,globamplg,contop)
        call sigio_sclose(90,iret)
!
c
c        te= rtc( )
c        te = te - tb
c        print *, 'time for inflate and mask_pert = ', te

c        write(99,*)  ' -------- after inflation --------------  '
c        print *, ' -------- after inflation --------------  '
c        call print_pert(jcap,lrec,nrec1,levs,nlath,ilon,ilat,grida)
c
c save analysis perts of T, wind at all levels for all members
c
c        gridas(:,:,:) = gridas(:,:,:) + grida(:,:,:) 
c        tg = 0.0
c        wg = 0.0
c        call  write_pert(jcap,lrec,nrec1,levs,ilon,
c     &  ilat,grida,tg,wg,jcaps,lrecs,ilons,ilats)
c        tga(:,:,:,2*n-1) = tg(:,:,:)
c        wga(:,:,:,2*n-1) = wg(:,:,:)
c
!        open(92, file=enn1(n),form='unformatted')
!        call write_ana(92,grida,ir,jcap,lrec,nrec1,levs,levsp,
!     &  ilat,ilon,cofo_1,g_ana,label,idate,fhr1,sigi,sigl,dummy1,ext1)
!        close(92)
!
        call sigio_swopen(92,enn1(n),iret)
        print *, 'iret from sigio_swopen =', iret
        call write_ana(92,grida,jcap,lrec,nrec1,levs,
     &  ilat,ilon,cofo_1,g_ana,heada)
        call sigio_sclose(92,iret)
!
c
c       write the 2nd in the pair -----------------------------------
c
c        tb = rtc( ) 
        grida = 0.0
        gf1 = 0.0
        gv = 0.0
        do i=1, npair
          gv(i) = vec(2*i-1, 2*n)
          nn = 0
          do i3 = 1, nrec1
             do i2 = 1, ilat
               do i1 = 1, ilon
                 nn = nn + 1  
                 gf1(nn, i) = gfcst(i1, i2, i3, 2*i-1)
               enddo
             enddo
          enddo
        enddo
        gga = 0.0
        call dgemx(nd3,npair,gam,gf1,nd3,gv,1,gga,1)
c
        gf1 = 0.0
        gv = 0.0
        do i=1, npair
          gv(i) = vec(2*i, 2*n)
          nn = 0
          do i3 = 1, nrec1
             do i2 = 1, ilat
               do i1 = 1, ilon
                 nn = nn + 1  
                 gf1(nn, i) = gfcst(i1, i2, i3, 2*i)
               enddo
             enddo
          enddo
        enddo
        call dgemx(nd3,npair,gam,gf1,nd3,gv,1,gga,1)
c
        nn = 0
        do i3 = 1, nrec1
           do i2 = 1, ilat
              do i1 = 1, ilon
                 nn = nn + 1  
                 grida(i1, i2, i3) = gga(nn) 
               enddo
             enddo
         enddo
c              
c        te= rtc( )
c        te = te - tb
c        print *, 'time for 2nd pair loop of analysis = ', te
c       writes ensemble members with scaled perturbations
c
c        print *, '-in et, writing 2nd pair of ana perts'
c        write(99,*) 'writing analysis perts, member 2n= ',2*n
c        write(99,*)  ' ------------ before inflation --------  '
c        print *, ' ------------ before inflation --------  '
c        print *, 'writing analysis perts, member 2n= ',2*n
c        call print_pert(jcap,lrec,nrec1,levs,nlath,ilon,ilat,grida)
c
c 
c  use one inflation factor scf to inflate the initial perts
c
        call inflate(scf,nrec1,ilon,ilat,grida)
c
c impose a mask on the perturbations
c
        print *, 'calling mask_pert    '
        call sigio_sropen(90,'sanl.in',iret)
        print *, 'iret from sigio_sropen =', iret
        if (iret.ne.0) print *,'sigio_sropen failed,iret=',iret
        call mask_pert(90,jcap,lrec,nrec1,irec,levs,nlath,ilon,
     &   ilat,grida,globamplr,globamplg,contop)
        call sigio_sclose(90,iret)
c
c        write(99,*)  ' ------- after inflation -------------  '
c        print *,  ' ------- after inflation -------------  '
c        call print_pert(jcap,lrec,nrec1,levs,nlath,ilon,ilat,grida)
c        gridas(:,:,:) = gridas(:,:,:) + grida(:,:,:) 
c
c save analysis perts of T, wind at all levels for all members
c
c        tg = 0.0
c        wg = 0.0
c        call  write_pert(jcap,lrec,nrec1,levs,ilon,
c     &  ilat,grida,tg,wg,jcaps,lrecs,ilons,ilats)
c        tga(:,:,:,2*n) = tg(:,:,:)
c        wga(:,:,:,2*n) = wg(:,:,:)
c
!        open(92, file=enn2(n),form='unformatted')
!        call write_ana(92,grida,ir,jcap,lrec,nrec1,levs,levsp,
!     &  ilat,ilon,cofo_1,g_ana,label,idate,fhr1,sigi,sigl,dummy1,ext1)
!        close(92)
!
        call sigio_swopen(92,enn2(n),iret)
        print *, 'iret from sigio_swopen =', iret
        call write_ana(92,grida,jcap,lrec,nrec1,levs,
     &  ilat,ilon,cofo_1,g_ana,heada)
        call sigio_sclose(92,iret)
!
        tee= rtc( )
        tee = tee - tbb
        print *, 'time for one big loop of analysis = ', tee

       enddo
c
c write analysis perts distributions and ratios over fcst perts
c
c        write(99,*) 'before tw_pert, ilats,ilons,levs,nens = ',
c     &                               ilats,ilons,levs,nens
c        call tw_pert(ilons,ilats,levs,nens,tgf,wgf,tga,wga)
c
c  print sum of analysis perts
c
c        write(99,*) 'writing sum of final analysis perts'
c        write(99,*) '------------------------------------'
c        call print_pert(jcap,lrec,nrec1,levs,nlath,ilon,ilat,gridas)
c check if the sum of analysis perts is 0
c
c        write(99,*) 'irec-1:  vorticity at 500mb --- '
c        write(99,*) 'sum of ana perts gridas(1:5,1:5,irec-1)=',
c     &  gridas(1:5,1:5,irec-1)
c        write(99,*) 'sum of ana perts gridas(1:5,1:5,1)logP=',
c     &  gridas(1:5,1:5,1)
c        write(99,*) '2:  temp at bottom level --- '
c        write(99,*) 'sum of ana perts gridas(1:5,1:5,2),T(1)=',
c     &  gridas(1:5,1:5,2)
c        write(99,*) '14:  temp at 500mb --- '
c        write(99,*) 'sum of ana perts gridas(1:5,1:5,14),T(13)=',
c     &  gridas(1:5,1:5,14)
c        write(99,*) 'levs+1:  temp at top level --- '
c        write(99,*) 'sum of ana perts gridas(1:5,1:5,levs+1),
c     &  T(levs)=',  gridas(1:5,1:5, levs+1)
c
c
       deallocate( kem,kef,rhz,vec,vec2,vec3,eig,eig2,eig3,aux2)  
       deallocate( gridm,grid1,gridf,grida,gmask,tge,wge,gfcst,
     & gf1,gv,gga)
c     & tg,wg,tgf,wgf,tga,wga,tge,wge,gfcst,gf1,gv,gga)
c
       close(99)
       print *, '  ---------- end of gefs_init_et  -------------'
       call w3tage ('gefs_init_et')
       stop
       end
c
c --------------------------------------------------------------------
c
       subroutine read_ana(ir,jcap,lrec,nrec1,levs,levsp,ilat,ilon,
     & cofo_1,g_ana,head)
c
c    Mozheng Wei, 2004-10-15
c    Mozheng Wei, 2006-05-18, based on read_ana2.f, modified to read head 
c    using sigio_module prepared for hybrid GFS files 
c
       use sigio_module
       type(sigio_head):: head
       type(sigio_data):: data
!  
        character*8 label(4)
        dimension idate(4)
        real cofo_1(lrec),g_ana(ilon,ilat,nrec1)
        real cofo(lrec),grid(ilon,ilat)
c
        levsp=levs+1
        print *, '  --- in et, starting read_ana.exe --'
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
!        print *, 'sl from head in read_ana = ', head%sl
        print *, 'jcap from head in read_ana = ', head%jcap

1001    format(5x,'fhour and idata= ',f10.1,2x,4i5)
!       to save time, remove this, set irec value
        irec =  56
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
       do k = 1, levs
           grid = 0.0
           cofo(:) = data%t(:, k)
           call sptez(0,jcap,4,ilon,ilat,cofo,grid,+1)
           g_ana(:,:,k+1) = grid(:,:)
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
       print *, '---in et, end of read_ana.exe ----'
       return
       END
c
c --------------------------------------------------------------------
c
       subroutine write_ana(iout,grida,jcap,lrec,nrec1,levs,
     & ilat,ilon,cofo_1,g_ana,head)
c
c    Mozheng Wei, 2004-10-15
c    Mozheng Wei, 2006-05-18, based on write_ana2.f, modified to read head 
c    using sigio_module prepared for hybrid GFS files 
c
       use sigio_module
       type(sigio_head):: head
       type(sigio_data):: data

        character*8 label(4)
        dimension idate(4)
        real cofo_1(lrec),g_ana(ilon,ilat,nrec1)
        real*8 grida(ilon,ilat,nrec1)
        real cofo(lrec),grid(ilon,ilat)
c
        print *, '  -- in et, starting write_ana.exe --'
        levs=head%levs
        ntrac=head%ntrac
!        print *, 'sl from head in write_ana = ', head%sl
        print *, 'jcap from head in write_ana = ', head%jcap
        print *, ' levs, ntrac from head = ', levs, ntrac
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
       do k = 1, levs
          cofo = 0.0
          grid = 0.0
          grid(:,:) = g_ana(:,:,k+1) + real(grida(:,:,k+1))
          call sptez(0,jcap,4,ilon,ilat,cofo,grid,-1)
          data%t(:, k)=cofo(:)
! print 
         if (k .eq. 1) then
         write(99,*) 'Ana temp  at k=1 ', g_ana(1:5,1:5,k+1)
         write(99,*) 'temp perts  at k=1 ',real(grida(1:5,1:5,k+1))
         endif
         if (k .eq. 13) then
         write(99,*) 'Ana temp  at k=13 ', g_ana(1:5,1:5,k+1)
         write(99,*) 'temp perts  at k=13 ',real(grida(1:5,1:5,k+1))
         endif
         if (k .eq. levs) then
         write(99,*) 'Ana temp  at k=28 ', g_ana(1:5,1:5,k+1)
         write(99,*) 'temp perts  at k=28 ',real(grida(1:5,1:5,k+1))
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
          call sptez(0,jcap,4,ilon,ilat,cofo,grid,-1)
          cofo(1) = 0.0
          cofo(2) = 0.0
          data%d(:, k)=cofo(:)
!
          cofo = 0.0
          grid = 0.0
          grid(:,:)=g_ana(:,:,2*k+levs+1)+real(grida(:,:,2*k+levs+1))
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
          grid(:,:)=g_ana(:,:,ii+3*levs+1)+real(grida(:,:,ii+3*levs+1))
            call sptez(0,jcap,4,ilon,ilat,cofo,grid,-1)
            data%q(:, k, n)=cofo(:)
         enddo
       enddo
!
       call sigio_swdata(iout, head, data, iret)
       print *, 'iret from sigio_swdata =', iret
       call sigio_axdata(data,iret)
       print *, '---in et, end of write_ana.exe ----'
       return
       END
c
c ----------------------------------------------------------------
c
       subroutine read_fcst(jcap,lrec,nrec1,levs,nlath,ilat,ilon,
     &  ir,grid,irec,levsp)
c
c  from read_sig_all, avoid dynamic array to save time
!   Mozheng Wei, 2004-10-15
!   Mozheng Wei, 2006-05-18, based on read_fcst2.f, modified to read head 
!       and data using sigio_module.f, prepared for hybrid GFS files 
       use sigio_module
       type(sigio_head):: head
       type(sigio_data):: data
!
        character*8 label(4),label2(4)
        dimension idate(4)
        real ext1(44)
        real*8 grid(ilon,ilat,nrec1)
!
        real sigi(levsp),sigl(levs),dummy1(201-levs-levsp),
     &  cofo(lrec),grid1(ilon,ilat)  
!
        print *, '  ----starting read_fcst.exe -------'
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
!        print *, 'sl from head in read_fcst = ', head%sl
        print *, 'jcap from head in read_fcst = ', head%jcap
!
!
        write(6,1001) fhour,idate
1001    format(5x,'fhour and idata= ',f10.1,2x,4i5)
!       to save time, remove this, set irec value
        irec =  56
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
        print *, '-- in gefs_init_et, end of read_fcst.exe ----'
        return
        END
c
c-----------------------------------------------------------------------
c
        subroutine write_ke(jcap,lrec,nrec1,levs,ilon,ilat,
     &  gridd,tge,wge)
c
c   by Mozheng Wei, 2004-10-15
c
c  write levs of grid point values of t, wind
c
        real*8 gridd(ilon,ilat,nrec1)
        real tge(ilon,ilat,levs),wge(ilon,ilat,levs),grid(ilon,ilat),
     &  grid2(ilon,ilat)
c
        real cofo(lrec),cofo2(lrec),coft(lrec,nrec1+1) 
        real ug(ilon,ilat,levs),vg(ilon,ilat,levs) 
c
c        real*8,dimension(:,:),allocatable ::grid3
c        nrec = nrec1+1
c        allocate(cofo(lrec),cofo2(lrec),coft(lrec,nrec) )
c
        print *, '  starting write_ke.exe --'
c
       coft = 0.0
       do nr = 1, nrec1
          grid(:,:) = real(gridd(:,:,nr))
          cofo = 0.0
          call sptez(0,jcap,4,ilon,ilat,cofo,grid,-1)
          coft(:, nr+1)=cofo(:)
       enddo
c
c       allocate( cofts(lrecs,nrec1+1), cofo2(lrecs)  )
c  reduce resolution to T62
c       cofts = 0.0
c      do nr = 1, nrec1
c          cofo(:) = coft(:, nr+1)
c          call reduce(jcap,cofo,jcaps,cofo2)
c          cofts(:, nr+1)=cofo2(:)
c       enddo
c
c        allocate(ug(ilon,ilat,levs),vg(ilon,ilat,levs) )
        cofo = 0.0
        grid = 0.0
c levs levels of temperature
       do  nr = 3, levs+2
          do j=1, lrec
              cofo(j)=coft(j, nr)
          enddo
          call sptez(0,jcap,4,ilon,ilat,cofo,grid,+1)
          tge(1:ilon,1:ilat,nr-2) = grid(1:ilon,1:ilat)
       enddo
c
c levs levels of divergence and vorticity
c
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
c
       do k = 1, nuv
          do i = 1, ilon
             do j = 1, ilat
                wge(i, j, k) = sqrt(ug(i,j,k)*ug(i,j,k) + 
     &                        vg(i,j,k)*vg(i,j,k))
             enddo
          enddo
       enddo
       print *, '  ---- end of write_ke.exe -------'
       return
       END
c
c---------------------------------------------------------
c
       subroutine read_mask(ir,ig,jcap,lrec,levs,ilon,ilat,geogr)
c
c   Mozheng Wei, 2004-10-15
c   Mozheng Wei, 2006-05-18, modified to read head using sigio_module
c                prepared for hybrid GFS files 
c      irec=56 for ~500 hPa vorticity
       use sigio_module
       type(sigio_head):: head
       character*8 label(4)
       dimension idate(4)
       complex cofilr(2016)
       real geoglr(192,94),geogr1(192,94),geogr2(192,94),
     & geogr(ilon,ilat)
       complex stra(lrec/2) 
c       complex,dimension(:),allocatable::stra
c
       levsp=levs+1
c  lrec/2 for complex stra;   lrec for real
c       allocate( stra(lrec/2) )
       print *,' in gefs_init_et, read_mask.exe starts'
c
       write(6,688) jcap,ilat,ilon
688    format(1x,'scale truncation, lat, lon' ,3i5)
689    format(1x,'scale ir, iw',2i10)
c    
c---------------------------------------------------------------
c      CHECK FOR DATE
c---------------------------------------------------------------
c      
c       rewind(ir)
c       read(ir) label
c       print *, label
c       read(ir) fhr,idate
c       print *, 'fhr, idate = ', fhr,idate
c       close(ir)
!
!   use sigio_module to read
       call sigio_srhead(ir,head,iret)
       call sigio_sclose(ir,iret)
       idate=head%idate
!       print *, 'sl from head in read_mask = ', head%sl
       print *, 'jcap from head in read_mask = ', head%jcap
!
       m1=idate(2)
       jday=idate(3)
       print *, 'm1, jday = ', m1, jday
c
       if(jday.le.15)then
          m2=m1-1
          if(m2.eq.0)then
	     m2=12
          endif
       endif
c
       if(jday.gt.15)then
          m2=m1+1
          if(m2.gt.12)then
	     m2=1
	  endif
       endif
       print *, 'm1, m2 = ', m1,m2
c
       wfac2=abs(float(jday)-15.0)/30.0
       wfac1=1.0-wfac2
       print *, 'wfac1, wfac2=', wfac1, wfac2
c check code begin
       rewind(ig)
       do nr = 1, 12
          print *,' reading GEOGR(mask) of month= ', nr
          read(ig) geogr1
	  call range(geogr1,192,94)
       enddo
c check code end
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
c
       do j = 1 , 94
          do i = 1 , 192
	     geoglr(i,j)=geogr1(i,j)*wfac1+geogr2(i,j)*wfac2
	  enddo
       enddo
       print *,' mask file has been interpolated on T62 '
c       close(ig)
c
c---------------------------------------------------------------
c      CHECK FOR HIGH RESOLUTION
c---------------------------------------------------------------
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
c    
           print *,' in read_mask:  range of low resolution GEOGR'
           call range(geoglr,192,94)
       endif
c--------------------------------------------------------------
544    continue
c       deallocate(stra)
       RETURN
       end
c
c -----------------------------------------------------------------
c
       subroutine mask_pert(ir,jcap,lrec,nrec1,irec,levs,nlath,ilon,
     &  ilat,grida,globamplr,globamplg,contop)
c
c   Mozheng Wei, 2004-10-15
c   Mozheng Wei, 2006-05-18, modified to read head using sigio_module
c                prepared for hybrid GFS files 
c
       use sigio_module
       type(sigio_head):: head
       type(sigio_data):: data
c      PARAMETER(smx=0.15,globampl=1.45)
       PARAMETER(smx=0.15)
c      irec=56 for ~500 hPa vorticity
C
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
c
       real sigi(levs+1),sigl(levs),dummy(200),
     & cofi(lrec),str(lrec),stra(lrec),grid(ilon,ilat),
     & grid1(ilon,ilat),grid2(ilon,ilat),cofo1(lrec),cofo2(lrec),
     & coft(lrec,nrec1+1),gridk(ilon,ilat),grida2(ilon,ilat),
     & gu(ilon,ilat),gv(ilon,ilat),geogr(ilon,ilat),res(ilon,ilat),
     & gresc(ilon,ilat),geogl(ilon,ilat),zgeogr(ilat),zgrid(ilat)
c
       levsp=levs+1
       nrec = nrec1+1
       print *,' starting mask_pert.exe'
       write(6,688) jcap,ilat,ilon
688    format(1x,'scale truncation, latitude, longjcude' ,3i5)
c    
c---------------------------------------------------------------
c      CHECK FOR DATE
c---------------------------------------------------------------
c      
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
!       print *, 'sl from head in mask_pert = ', head%sl
       print *, 'jcap from head in mask_pert = ', head%jcap
       write(6,1001) fhr,idate
!       label=head%clabsig
!       sigi=head%si
!       sigl=head%sl
!       print *, 'label from head=', label
!       print *, 'label,sigi,sigl from head=', label,sigi,sigl
1001     format(5x,'ir ',f10.1,2x,4i5)
!
       m1=idate(2)
       jday=idate(3)
c
       if(jday.le.15)then
          m2=m1-1
          if(m2.eq.0)then
	     m2=12
          endif
       endif
c
       if(jday.gt.15)then
          m2=m1+1
          if(m2.gt.12)then
	     m2=1
	  endif
       endif
c       print *,m1,m2
c
       wfac2=abs(float(jday)-15.0)/30.0
       wfac1=1.0-wfac2
       print *, wfac1, wfac2
c check code begin
       open(48,err=544,form='unformatted')
       rewind(48)
       do nr=1,12
          read(48) geogr1
          print *,' range of input GEOGR for ',nr
	  call range(geogr1,192,94)
       enddo
c check code end
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
c
       do j = 1 , 94
          do i = 1 , 192
	     geoglr(i,j)=geogr1(i,j)*wfac1+geogr2(i,j)*wfac2
	  enddo
       enddo
c
c---------------------------------------------------------------
c      CHECK FOR HIGH RESOLUTION
c---------------------------------------------------------------
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
c    
           print *,' in mask_pert:  range of low resolution GEOGR'
           call range(geoglr,192,94)
       endif
c--------------------------------------------------------------
c  Get latitudinal average of geogr
       do ila=1,ilat
          zgeogr(ila)=0.0
          do ilo=1,ilon
          zgeogr(ila)=zgeogr(ila)+(geogr(ilo,ila)*globamplr)/float(ilon)
          enddo
       enddo     
c-----------------------------------------------------------------------------
C
       idirect = 0
       if(idirect.eq.0) then
         print *,' in mask_pert: reading ana orgraphy'
!
!   use sigio_module to read
!         call sigio_srohdc(ir,'sanl.in',head,data,iret)
!         call sigio_sropen(ir,'sanl.in',iret)
!
         call sigio_aldata(head,data,iret)
         print *, 'iret from sigio_aldata =', iret
         if (iret.ne.0) print *,'sigio_aldata failed,iret=',iret
         call sigio_srdata(ir,head,data,iret)
         print *, 'iret from sigio_srdata =', iret
         if (iret.ne.0) print *,'sigio_srdata failed,iret=',iret
!
!
!         open(90, file='sanl.in', form='unformatted')
!         rewind(90)
!         read(90) label
!         read(90) fhr, (idate(i),i=1,4),
!     &   (sigi(k),k=1,levsp),(sigl(k),k=1,levs),dummy,(ext(n),n=1,44)
!
!  use the analysis orgraphy
!         read(90) (cofi(J),J=1,lrec)
!         close(90)
         cofi = 0.0
         coft = 0.0
         cofi(:) = data%hs(:)
         do j = 1, lrec
            coft(j, 1) = cofi(j)
         enddo
c
         grid = 0.0
         do nr = 1, nrec1
            grid(:,:) = real(grida(:,:,nr))
            call sptez(0,jcap,4,ilon,ilat,cofi,grid,-1)
            coft(:, nr+1) = cofi(:)
         enddo
c    
c  vorticity at 500mb
         cofi(:) = coft(:, irec)
c  divergence at 500mb
         str(:) = coft(:, irec-1)
         gu = 0.0
         gv = 0.0
         call sptezv(0,jcap,4,ilon,ilat,str,cofi,gu,gv,+1)
c        gu, gv --> grid point values of u, v, 
         call kinendist(dx,gu,gv,grid,ilon,ilat)
c        grid --> sqt of KE over grids
         print *,"in mask_pert,ave KE over all grids at 500mb= ",dx
c        kinetic energy based geographical rescaling  *************
         stra = 0.0
         call sptez(0,jcap,4,ilon,ilat,stra,grid,-1)
c        stra --> spectrum of sqt of KE at 500mb         
         str = 0.0
         call smooth(stra,str,1,smx,jcap)
         call sptez(0,jcap,4,ilon,ilat,str,grida2,+1)
         print *,"in mask_per, grid values of sqt of KE at 500mb"
         call range(grida2,ilon,ilat)
c
ccc-------------------------------------------------------------------
c
c      get Latitudinal average
c
	 do j = 1,ilat
            zgrid(j)=0.0
            do i = 1,ilon
               zgrid(j) = zgrid(j)+grida2(i,j)/float(ilon)
            enddo
         enddo
c
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
c
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
	   print *,' in mask_pert end pre ed range of geogr*globampl: 
     &           global'
	   call range(geogr,ilon,ilat)
       else
         factor(1)=dnh
         factor(2)=dsh
       endif
c
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
c
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
544    continue
!
!  do rescaling on the top level -------------------
!
        k = levs
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
        call kinendist(dx,grid1,grid2,grid,ilon,ilat)
!       grid --> sqt of KE over grids
        print *,"in mask_pert,ave KE over all grids at level=28 ",dx
!       kinetic energy based geographical rescaling 
        cofo1 = 0.0
        call sptez(0,jcap,4,ilon,ilat,cofo1,grid,-1)
!       cofo1 --> spectrum of sqt of KE at level 28         
        cofo2 = 0.0
        grid2 = 0.0
        call smooth(cofo1,cofo2,1,smx,jcap)
        call sptez(0,jcap,4,ilon,ilat,cofo2,grid2,+1)
        print *,"in mask_per, grid values of sqt of KE at level 28"
        call range(grid2,ilon,ilat)
!
        do ila=1,ilat
           do ilo=1,ilon
!              geogr(ilo,ila)=geogr(ilo,ila)
!              gresc(ilo,ila)=contop*geogr(ilo,ila)/grid2(ilo,ila)
               gresc(ilo,ila)=contop/grid2(ilo,ila)
	   enddo
        enddo
!
        do ila=1,ilat
           do ilo=1,ilon
              if(gresc(ilo,ila).gt.1.0)  gresc(ilo,ila)=1.0
           enddo
        enddo
        print *,' in mask_pert, at level=28: scaling factors'
	call range(gresc,ilon,ilat)
!
! rescaling for levs=28
!
        k = levs
        do j=1,ilat
   	   do i=1,ilon
! temp       
           grida(i,j,k+1) =grida(i,j,k+1)*dble(gresc(i,j))
! div and vor
           grida(i,j,2*k+levs)=
     &     grida(i,j,2*k+levs)*dble(gresc(i,j))
           grida(i,j,2*k+levs+1)=
     &     grida(i,j,2*k+levs+1)*dble(gresc(i,j))
! ntrac tracers
           do n = 1, ntrac
              grida(i,j,n*levs+3*levs+1)=
     &        grida(i,j,n*levs+3*levs+1)*dble(gresc(i,j))
           enddo
           enddo
        enddo
!
! rescaling for levs-1=27
!
        res=0.0
        do j=1,ilat
   	   do i=1,ilon
               res(i, j)=(4.0*gresc(i,j)+1.0)/5.0
           enddo
        enddo
!
        k = levs-1
        do j=1,ilat
   	   do i=1,ilon
! temp       
           grida(i,j,k+1) =grida(i,j,k+1)*dble(res(i,j))
! div and vor
           grida(i,j,2*k+levs)=
     &     grida(i,j,2*k+levs)*dble(res(i,j))
           grida(i,j,2*k+levs+1)=
     &     grida(i,j,2*k+levs+1)*dble(res(i,j))
! ntrac tracers
           do n = 1, ntrac
              grida(i,j,n*levs-1+3*levs+1)=
     &        grida(i,j,n*levs-1+3*levs+1)*dble(res(i,j))
           enddo
           enddo
        enddo
!
! rescaling for levs-2=26
!
        res=0.0
        do j=1,ilat
   	   do i=1,ilon
               res(i, j)=(3.0*gresc(i,j)+2.0)/5.0
           enddo
        enddo
!
        k = levs-2
        do j=1,ilat
   	   do i=1,ilon
! temp       
           grida(i,j,k+1) =grida(i,j,k+1)*dble(res(i,j))
! div and vor
           grida(i,j,2*k+levs)=
     &     grida(i,j,2*k+levs)*dble(res(i,j))
           grida(i,j,2*k+levs+1)=
     &     grida(i,j,2*k+levs+1)*dble(res(i,j))
! ntrac tracers
           do n = 1, ntrac
              grida(i,j,n*levs-2+3*levs+1)=
     &        grida(i,j,n*levs-2+3*levs+1)*dble(res(i,j))
           enddo
           enddo
        enddo
!
! rescaling for levs-3=25
!
        res=0.0
        do j=1,ilat
   	   do i=1,ilon
               res(i, j)=(2.0*gresc(i,j)+3.0)/5.0
           enddo
        enddo
!
        k = levs-3
        do j=1,ilat
   	   do i=1,ilon
! temp       
           grida(i,j,k+1) =grida(i,j,k+1)*dble(res(i,j))
! div and vor
           grida(i,j,2*k+levs)=
     &     grida(i,j,2*k+levs)*dble(res(i,j))
           grida(i,j,2*k+levs+1)=
     &     grida(i,j,2*k+levs+1)*dble(res(i,j))
! ntrac tracers
           do n = 1, ntrac
              grida(i,j,n*levs-3+3*levs+1)=
     &        grida(i,j,n*levs-3+3*levs+1)*dble(res(i,j))
           enddo
           enddo
        enddo
!
! rescaling for levs-4=24
!
        res=0.0
        do j=1,ilat
   	   do i=1,ilon
               res(i, j)=(gresc(i,j)+4.0)/5.0
           enddo
        enddo
!
        k = levs-4
        do j=1,ilat
   	   do i=1,ilon
! temp       
           grida(i,j,k+1) =grida(i,j,k+1)*dble(res(i,j))
! div and vor
           grida(i,j,2*k+levs)=
     &     grida(i,j,2*k+levs)*dble(res(i,j))
           grida(i,j,2*k+levs+1)=
     &     grida(i,j,2*k+levs+1)*dble(res(i,j))
! ntrac tracers
           do n = 1, ntrac
              grida(i,j,n*levs-4+3*levs+1)=
     &        grida(i,j,n*levs-4+3*levs+1)*dble(res(i,j))
           enddo
           enddo
        enddo
!
!
       close(48)
       call sigio_axdata(data,iret)
!       deallocate(dummy,cofi,str,stra,grid,gridk,grida2,gu,gv,geogr,
!     & gresc,geogl,zgeogr,zgrid,coft)
       RETURN
       end
c
c --------------------------------------------------------------------
c
       subroutine factor_t(ftt,nrec1,ilon,ilat,grida,scf)
c
c   by Mozheng Wei, 2004-10-15
c
       real*8 grida(ilon,ilat,nrec1)
       real*8 ftt,scf,trms
       real*8 grid(ilon,ilat)
c
c  compute rescaling factor scf  -> output
c  get the temp at lowest level nr=3-1=2 and get the scaling factor
c  nr= 15-1 = 14 for Temp at 500mb
         nt = 14
         print *, ' in gefs_init_et, starting factor_t.exe '
         grid = 0.0
         trms = 0.0
         grid(:,:) = grida(:,:,nt)
         do j = 1, ilat
           do  i = 1, ilon
                trms = trms + grid(i,j)*grid(i,j)
           enddo
         enddo
         trms = dsqrt(trms/float(ilat*ilon))
         scf = ftt/trms
c    
       RETURN
       end
c
c ----------------------------------------------------------------
c
       subroutine inflate(scf,nrec1,ilon,ilat,grida)
c
c   by Mozheng Wei, 2004-10-15
c
       real*8 grida(ilon,ilat,nrec1)
       real*8 scf
c
c  input rescaling factor-> scf,   scale the perts grida
c
         do  j = 1, ilat
           do i = 1, ilon
              grida(i, j, 1) = grida(i, j, 1)*scf
c              grida(i, j, 1) = alog((grida(i,j,1))/10.0)
           enddo
         enddo
c
         do nr = 2, nrec1
            do j = 1, ilat
              do  i = 1, ilon
                 grida(i, j, nr) = grida(i, j, nr)*scf
              enddo
            enddo
         enddo
c    
       RETURN
       end
c
c ----------------------------------------------------------------
c
       subroutine read_sig_all(jcap,lrec,nrec1,levs,nlath,ilat,ilon,
     &  ir,grid,irec)
c
c   by Mozheng Wei, 2003-10-15
c
        character*8 label(4),label2(4)
        dimension idate1(4),idate2(4),idate(4)
        real ext1(44)
        real*8 grid(ilon,ilat,nrec1)
      	real,allocatable::cofo(:)
c      	complex,allocatable::cofo(:)
c
        real,dimension(:),allocatable ::sigi,sigl,dummy1
        real,dimension(:,:),allocatable ::grid1
c
        levsp=levs+1
        allocate( sigi(levsp),sigl(levs),dummy1(201-levs-levsp),
     &  cofo(lrec),grid1(ilon,ilat)  )
        print *, '  ---- in et, starting read_sig_all.exe -------'
        rewind(ir)
        read(ir) label
        read(ir) fhr1,(idate1(i),i=1,4),
     *  (sigi(k),k=1,levsp),(sigl(k),k=1,levs),dummy1,ext1
        write(6,1001) fhr1,idate1
1001    format(5x,'ir ',f10.1,2x,4i5)
        id=1
        write(6,1333) id,ext1(6),ext1(13),ext1(14),ext1(15),ext1(16)
1333    format(1x,i3,' = file id',5f9.2,' = ext(6,13,14,15,16)')
c
c     determine irec to specify a sigma level near 500hpa
c
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
c
        cofo = 0.0
        grid1 = 0.0
        grid = 0.0
        read(ir) (cofo(J),J=1,lrec)
c       starts from surface pressure
        do nr = 1, nrec1
            read(ir) (cofo(J),J=1,lrec)
            call sptez(0,jcap,4,ilon,ilat,cofo,grid1,+1)
            grid(:,:,nr) = dble(grid1(:,:))
        enddo
        close(ir)
        deallocate( sigi,sigl,dummy1,cofo,grid1 )
        print *, '-- in et, end of read_sig_all.exe ----'
        return
        END
c
c------------------------------------------------------------------
c
       subroutine print_sig(jcap,lrec,nrec,levs,nlath,ilon,ilat,
     &  coft)
c
c   by Mozheng Wei, 2003-10-15
c
c  printing T, U, V at lev=1, levs given coft
c
        real coft(lrec,nrec)
c
        real cofo(lrec),cofo2(lrec),grid(ilon,ilat),
     &  grid2(ilon,ilat),ug(ilon,ilat,levs),vg(ilon,ilat,levs),
     &  tg(ilon,ilat,levs)
        real*8 grid3(ilon,ilat)
        print *, '  ---- in et, starting print_sig.exe -------'
        cofo = 0.0
        grid = 0.0
c levs levels of temperature
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
           write(99,*) ' dmin,dmax,avg,adev,sdev,skew = ',
     &               dmin,dmax,avg,adev,sdev,skew
       write(99,*) 'range of T at lev=levs is: '
       grid3(:,:) = dble(tg(:,:, levs))
       write(99,*) ' T(1:5,1:5) at lev=levs is:', grid3(1:5, 1:5)
       call range2(grid3,ilon,ilat,dmin,dmax,avg,adev,sdev,skew)
           write(99,*) ' dmin,dmax,avg,adev,sdev,skew = ',
     &               dmin,dmax,avg,adev,sdev,skew
c
c levs levels of divergence and vorticity
c
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
c
       write(99,*) 'range of U at lev=1 is: '
       grid3(:,:) = dble(ug(:,:,1))
       write(99,*) ' U(1:5,1:5) at lev=1 is: ', grid3(1:5, 1:5)
       call range2(grid3,ilon,ilat,dmin,dmax,avg,adev,sdev,skew)
           write(99,*) ' dmin,dmax,avg,adev,sdev,skew = ',
     &               dmin,dmax,avg,adev,sdev,skew
       write(99,*) 'range of U at lev=levs is: '
       grid3(:,:) = dble(ug(:,:, levs))
       write(99,*) ' U(1:5,1:5) at lev=levs is: ', grid3(1:5, 1:5)
       call range2(grid3,ilon,ilat,dmin,dmax,avg,adev,sdev,skew)
           write(99,*) ' dmin,dmax,avg,adev,sdev,skew = ',
     &               dmin,dmax,avg,adev,sdev,skew
       write(99,*) 'range of V at lev=1 is: '
       grid3(:,:) = dble(vg(:,:,1))
       write(99,*) ' V(1:5,1:5) at lev=1 is: ', grid3(1:5, 1:5)
       call range2(grid3,ilon,ilat,dmin,dmax,avg,adev,sdev,skew)
           write(99,*) ' dmin,dmax,avg,adev,sdev,skew = ',
     &               dmin,dmax,avg,adev,sdev,skew
       write(99,*) 'range of V at lev=levs is: '
       grid3(:,:) = dble(vg(:,:, levs))
       write(99,*) ' V(1:5,1:5) at lev=levs is: ', grid3(1:5, 1:5)
       call range2(grid3,ilon,ilat,dmin,dmax,avg,adev,sdev,skew)
           write(99,*) ' dmin,dmax,avg,adev,sdev,skew = ',
     &               dmin,dmax,avg,adev,sdev,skew
c
c
       print *, '  ---- in et, end of print_sig.exe -------'
       return
       END
c
c --------------------------------------------------------------------
c
       subroutine print_pert(jcap,lrec,nrec1,levs,nlath,
     &  ilon,ilat,grida)
c
c   by Mozheng Wei, 2003-10-15
c
        real*8 grida(ilon,ilat,nrec1)
c
       real cofo(lrec),grid(ilon,ilat),coft(lrec,nrec1+1) 
        print *, '  -in et, starting print_pert.exe --'
c
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
       END
c
c ----------------------------------------------------------------
c
        subroutine write_pert(jcap,lrec,nrec1,levs,ilon,
     &  ilat,gridd,tg,wg,jcaps,lrecs,ilons,ilats)
c
c   by Mozheng Wei, 2003-10-15
c
c  write levs of grid point values of t, wind
c
        real*8 gridd(ilon,ilat,nrec1)
        real tg(ilons,ilats,levs),wg(ilons,ilats,levs)
c
        real,allocatable::cofo(:),cofo2(:)
        real,allocatable::coft(:,:),cofts(:,:)
        real,dimension(:,:),allocatable ::grid
c
        allocate( cofo(lrec),grid(ilon,ilat),coft(lrec,nrec1+1),
     &  cofts(lrecs,nrec1+1), cofo2(lrecs)  )
        print *, '  -in et, starting write_pert.exe --'
c
       coft = 0.0
       do nr = 1, nrec1
          grid(:,:) = real(gridd(:,:,nr))
          cofo = 0.0
          call sptez(0,jcap,4,ilon,ilat,cofo,grid,-1)
          coft(:, nr+1)=cofo(:)
       enddo
c  reduce resolution to T62
       cofts = 0.0
       do nr = 1, nrec1
          cofo(:) = coft(:, nr+1)
          call reduce(jcap,cofo,jcaps,cofo2)
          cofts(:, nr+1)=cofo2(:)
       enddo
       call write_tuv(jcaps,lrecs,nrec1+1,levs,ilons,ilats,
     & cofts,tg, wg)
       deallocate(cofo,cofo2,coft,cofts,grid)
       print *, '---in et, end of write_pert.exe ----'
       return
       END
c
c ---------------------------------------------------------------
c
       subroutine write_tuv(jcap,lrec,nrec,levs,ilon,ilat,
     &  coft,tg,wg)
c
c   by Mozheng Wei, 2003-10-15
c
c  write T, wind (W) at lev=1 ... levs given coft
c
        real coft(lrec,nrec)
        real tg(ilon,ilat,levs),wg(ilon,ilat,levs)
c
      	real,allocatable::cofo(:),cofo2(:)
        real,allocatable::ug(:,:,:),vg(:,:,:)
c
        real,dimension(:,:),allocatable ::grid,grid2
c        real*8,dimension(:,:),allocatable ::grid3
c
        allocate(cofo(lrec),cofo2(lrec),grid(ilon,ilat),
     &  grid2(ilon,ilat),ug(ilon,ilat,levs),vg(ilon,ilat,levs) )
        print *, '  ---- in et, starting write_tuv.exe -------'
        cofo = 0.0
        grid = 0.0
c levs levels of temperature
       do  nr = 3, levs+2
          do j=1, lrec
              cofo(j)=coft(j, nr)
          enddo
          call sptez(0,jcap,4,ilon,ilat,cofo,grid,+1)
          tg(1:ilon,1:ilat,nr-2) = grid(1:ilon,1:ilat)
       enddo
c
c levs levels of divergence and vorticity
c
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
c
       do k = 1, nuv
          do i = 1, ilon
             do j = 1, ilat
                wg(i, j, k) = sqrt(ug(i,j,k)*ug(i,j,k) + 
     &                            vg(i,j,k)*vg(i,j,k))
             enddo
          enddo
       enddo
       deallocate(cofo,cofo2,grid,grid2,ug,vg)
c
       print *, '  ---- in et, end of write_tuv.exe -------'
       return
       END
c
c ----------------------------------------------------------------
        subroutine reduce(jcap,cofb,jcaps,cofs) 
c
c   by Mozheng Wei, 2004-10-15
c
c       cofb has jcap --> cofs with jcaps which is smaller
	real cofb((jcap+1)*(jcap+2))
	real cofs((jcaps+1)*(jcaps+2))
c
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
	end
c -------------------------------------------------------------------
c
       subroutine tw_pert(ilon,ilat,levs,nens,tgf,wgf,tga,wga)
c
c   by Mozheng Wei, 2003-10-15
c
       real tgf(ilon,ilat,levs,nens), wgf(ilon,ilat,levs,nens)
       real tga(ilon,ilat,levs,nens), wga(ilon,ilat,levs,nens)

       real,dimension(:,:,:),allocatable ::tga3,wga3,tgf3,wgf3,
     & tsc3,wsc3
       real,dimension(:,:),allocatable ::tga2,wga2,tsc2,wsc2
       real,dimension(:),allocatable ::tga1,wga1,tsc1,wsc1

       allocate( tga3(ilon,ilat,levs), wga3(ilon,ilat,levs),
     & tgf3(ilon,ilat,levs), wgf3(ilon,ilat,levs),
     & tsc3(ilon,ilat,levs), wsc3(ilon,ilat,levs),
     & tga2(ilon,ilat),wga2(ilon,ilat),tga1(levs),wga1(levs),
     & tsc2(ilon,ilat),wsc2(ilon,ilat),tsc1(levs),wsc1(levs) )
c
c get the temp at lowest level and get the scaling factor
c
       print *, ' in gefs_init_et, starting tw_pert.exe '

        open(52, file='tw_pert_3d', form='unformatted')
        rewind(52)
        write(52) tga
        write(52) wga
        write(52) tgf
        write(52) wgf
        close(52)
c
c  rms values over different members
c
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
c  scaling ratios over fcsts 
                  tsc3(i,j,k)=tga3(i,j,k)/tgf3(i,j,k)
                  wsc3(i,j,k)=wga3(i,j,k)/wgf3(i,j,k)
               enddo
            enddo
         enddo
c         write(99,*) 'tw_part_2d, tga3(1:10,1:10,13)=', 
c     &                            tga3(1:10,1:10,13)
c         write(99,*) 'tw_part_2d, tgf3(1:10,1:10,13)=', 
c     &                            tgf3(1:10,1:10,13)
c         write(99,*) 'tw_part_2d, wga3(1:10,1:10,13)=', 
c     &                            wga3(1:10,1:10,13)
c         write(99,*) 'tw_part_2d, wgf3(1:10,1:10,13)=', 
c     &                            wgf3(1:10,1:10,13)
c         write(99,*) 'tw_part_2d, tsc3(1:10,1:10,13)=', 
c     &                            tsc3(1:10,1:10,13)
c         write(99,*) 'tw_part_2d, wsc3(1:10,1:10,13)=', 
c     &                            wsc3(1:10,1:10,13)
c
c  horizontal distribution, average over different levels
c
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
c
        open(55, file='tw_pert_2d', form='unformatted')
        rewind(55)
        write(55) tga2
        write(55) wga2
        write(55) tsc2
        write(55) wsc2
        close(55)
c     
        write(99,*) 'tw_part_2d, tga2(1:5,1:5)=', tga2(1:5,1:5)
        write(99,*) 'tw_part_2d, wga2(1:5,1:5)=', wga2(1:5,1:5)
        write(99,*) 'tw_part_2d, tsc2(1:5,1:5)=', tsc2(1:5,1:5)
        write(99,*) 'tw_part_2d, wsc2(1:5,1:5)=', wsc2(1:5,1:5)
c        write(99,*) 'tw_part_2d, tsc2 =', tsc2
c        write(99,*) 'tw_part_2d, wsc2 =', wsc2
c
c  vertical distribution, average over grid points
c
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
c    
        open(56, file='tw_pert_z')
        rewind(56)
        write(56,11) (tga1(k), k=1, levs)
        write(56,11) (wga1(k), k=1, levs)
        write(56,11) (tsc1(k), k=1, levs)
        write(56,11) (wsc1(k), k=1, levs)
        close(56)
11      format(g13.6,1x)

       deallocate(tga3,wga3,tgf3,wgf3,tsc3,wsc3,tga2,wga2,tsc2,
     & wsc2,tga1,wga1,tsc1,wsc1)
c
       print *, ' in gefs_init_et, end of tw_pert.exe '
       return
       end
c
c-----------------------------------------------------------------
c
	  subroutine range2(var,nlon,nlat,dmin,dmax,avg,adev,
     &    sdev,skew)
c	  dimension var(nlon,nlat)
          real*8 var(nlon,nlat)
          real var2(nlon,nlat),dmin,dmax,avg,adev,sdev,skew
c uses srange instead of doing its own calculation
          var2(:,:) = real(var(:,:))
          nlonlat=nlon*nlat
          call srange2(var2,nlonlat,dmin,dmax,avg,adev,sdev,skew) 
	  return
	  end
c
c
	  subroutine srange2(var,nlat,dmin,dmax,avg,adev,sdev,skew)
	  dimension var(nlat)
c         real*8 var(nlat),dmin,dmax,avg,adev,sdev,skew
c    produces range, mean, avg dev, std dev, skew
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
c          print *,dmin,dmax,avg,adev,sdev,skew
	  return
	  end
cc
	  subroutine range(var,nlon,nlat)
	  dimension var(nlon,nlat)
c uses srange instead of doing its own calculation
          nlonlat=nlon*nlat
          call srange(var,nlonlat) 
	  return
	  end
c
	  subroutine srange(var,nlat)
	  dimension var(nlat)
c produces range, mean, avg dev, std dev, skew
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
	  end
c
c
c ------------------------------------------------------------------
       subroutine read_ana2(ir,jcap,lrec,nrec1,levs,levsp,ilat,ilon,
     & cofo_1,g_ana,label,idate,fhr1,sigi,sigl,dummy1,ext1)
c
c    by Mozheng Wei, 2004-10-15
c
        character*8 label(4)
        dimension idate(4)
        real cofo_1(lrec),g_ana(ilon,ilat,nrec1),sigi(levsp),
     &  sigl(levs),dummy1(201-levs-levsp),ext1(44)
c
        real cofo(lrec),grid(ilon,ilat)
c
        levsp=levs+1
        print *, '  --- in et, starting read_ana.exe --'
        rewind(ir)
        read(ir) label
        read(ir) fhr1,(idate(i),i=1,4),
     &  (sigi(k),k=1,levsp),(sigl(k),k=1,levs),dummy1,ext1
c
c      reads orography
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
       END
c
c ----------------------------------------------------------------------
c
       subroutine write_ana2(iout,grida,ir,jcap,lrec,nrec1,levs,levsp,
     & ilat,ilon,cofo_1,g_ana,label,idate,fhr1,sigi,sigl,dummy1,ext1)
c
c    by Mozheng Wei, 2004-10-15
c
        character*8 label(4)
        dimension idate(4)
        real cofo_1(lrec),g_ana(ilon,ilat,nrec1),sigi(levsp),
     &  sigl(levs),dummy1(201-levs-levsp),ext1(44)
        real*8 grida(ilon,ilat,nrec1)
        real cofo(lrec),grid(ilon,ilat)
c
c
        levsp=levs+1
        print *, '  -- in et, starting write_ana.exe --'
        write(iout) label
        write(iout) fhr1,(idate(i),i=1,4),
     &  (sigi(k),k=1,levsp),(sigl(k),k=1,levs),dummy1,ext1
c
c     write perturbed sigma file
c
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
       END
c
c-----------------------------------------------------------------------
c
       subroutine read_fcst2(jcap,lrec,nrec1,levs,nlath,ilat,ilon,
     &  ir,grid,irec,levsp)
c
c  by Mozheng Wei, 2004-10-15
c
c  from read_sig_all, avoid dynamic array to save time
        character*8 label(4),label2(4)
        dimension idate1(4),idate2(4),idate(4)
        real ext1(44)
        real*8 grid(ilon,ilat,nrec1)
c
        real sigi(levsp),sigl(levs),dummy1(201-levs-levsp),
     &  cofo(lrec),grid1(ilon,ilat)  
c
        print *, '  ----starting read_fcst.exe -------'
        rewind(ir)
        read(ir) label
        read(ir) fhr1,(idate1(i),i=1,4),
     *  (sigi(k),k=1,levsp),(sigl(k),k=1,levs),dummy1,ext1
        write(6,1001) fhr1,idate1
1001    format(5x,'ir ',f10.1,2x,4i5)
        id=1
        write(6,1333) id,ext1(6),ext1(13),ext1(14),ext1(15),ext1(16)
1333    format(1x,i3,' = file id',5f9.2,' = ext(6,13,14,15,16)')
c
c    to save time, remove this, set irec value
        irec =  56
c     determine irec to specify a sigma level near 500hpa
c
c        diffmin=1.0e20
c        kdiffmin=0
c       do k=1,levs
c	  diff=abs(sigl(k) - 0.5)
c	  if ( diffmin .gt. diff ) then
c	     diffmin=diff
c	     kdiffmin=k
c	  endif
c        enddo
c        if ( (kdiffmin .le. 0) .or. (kdiffmin .gt. levs ) ) then
c	  print *,' read_fcst, bad kdiffmin = ', kdiffmin
c	  print *,' levs = ', levs
c	  print *,' k, sigl(k) = ',(k, sigl(k), k = 1, levs)
c        endif
c        irec=2+levs+2*kdiffmin
c        print *,' in read_fcst, irec = ',irec
c
        cofo = 0.0
        grid1 = 0.0
        grid = 0.0
        read(ir) (cofo(J),J=1,lrec)
c       starts from surface pressure
        do nr = 1, nrec1
            read(ir) (cofo(J),J=1,lrec)
            call sptez(0,jcap,4,ilon,ilat,cofo,grid1,+1)
            grid(:,:,nr) = dble(grid1(:,:))
        enddo
        close(ir)
        print *, '-- in gefs_init_et, end of read_fcst.exe ----'
        return
        END
c
c ---------------------------------------------------------------------
c  subroutine to compute kinetic energy of u&v gridded fields
c    point by point; grid1,grid2-input, grido=output
c    square root of kinen taken for gridded output
c  
       subroutine kinendist(d,grid1,grid2,grido,nlon,nlat)
       dimension grid1(nlon,nlat),grid2(nlon,nlat),grido(nlon,nlat)
       ds=0.
       ns=0
       do 333 i=1,nlat
       if(i.le.nlat/2) then
       xlat=float(nlat)/2.-float(i)
       else
       xlat=float(i)-float(nlat)/2.
       endif
       weight=cos((xlat*2./float(nlat))*1.570796)
       do 444 j=1,nlon
       ds=ds+(grid1(j,i)**2+grid2(j,i)**2)*weight/2.
c      ds=ds+(grid1(jj,i)**2+grid2(jj,i)**2)*weight/2.
       grido(j,i)=sqrt(grid1(j,i)**2+grid2(j,i)**2)
       ns=ns+1
 444   continue
 333   continue
       d=ds/float(ns)
       return
       end
c     subroutine fill(jcapi,jcapo,inp1,inp2)
      subroutine fill(jcapi,jcapo,nrec,levs,inp1,inp2)
c     parameter(nrec=142,levs=28,levsp=29)
c     cofi has jcapi triangular truncation that is smaller than cofo 
c        output
c     subroutine fills up with zeros
c     cofo except lower triangular, where it gets cofi values.
      complex cofi((jcapi+1)*(jcapi+2)/2),cofo((jcapo+1)*(jcapo+2)/2)
      dimension idate(4)
      dimension ext(44)
      character*8 label(4)
c
	  allocatable dphi(:)
	  allocatable dlam(:)
	  allocatable dummy(:)
c
	  levsp=levs+1
	  allocate(dphi(levsp))
	  allocate(dlam(levs))
	  allocate(dummy(201-levs-levsp))
c
c
          write(6,322) jcapi,jcapo
 322      format(1x,'fill coeffs from ',i3,' to ',i3)
c
          lreci=(jcapi+1)*(jcapi+2)/2
          lreco=(jcapo+1)*(jcapo+2)/2
          write(6,333) lreci,lreco
 333      format(1x,'record lengths ',2i7)
c
          open(inp1,form='unformatted')
          open(inp2,form='unformatted')
c
	  rewind(inp1)
	  rewind(inp2)
c
	  read(inp1)label
	  read(inp1)fhr,(idate(n),n=1,4),
     &(dphi(k),k=1,levsp),(dlam(k),k=1,levs),
     &dummy,ext
c
          ext(1)=jcapo
          ext(13)=2
          ext(14)=2
	  write(inp2)label
	  write(inp2)fhr,(idate(n),n=1,4),
     &(dphi(k),k=1,levsp),(dlam(k),k=1,levs),
     &dummy,ext
c
	  do 999  nr = 1 , nrec
	  read(inp1)(cofi(i),i=1,lreci)
	  indi=0
	  indo=0
c	  do 10 j=1,lreco
c	  cofo(j)=cmplx(0.,0.)
c10   continue
	  do 15 m=0,jcapo
	  do 14 n=m,jcapo
  	    indo=indo+1
	    if(n.le.jcapi) then
	      indi=indi+1
	      cofo(indo)=cofi(indi)
	    else
	      cofo(indo)=cmplx(0.,0.)
	    endif
 14   continue
 15   continue
	  write(inp2)(cofo(i),i=1,lreco)
 999  continue
          close(inp1)
          close(inp2)
	  deallocate(dphi)
	  deallocate(dlam)
	  deallocate(dummy)
	  return
	  end
      subroutine smooth(cofi,cofo,iord,sizeofsm,jcap)
c    this subroutine performs jim purser's smoother
c  cofi  input spher. coeff., triang. trunc jcap
c  cofo  output spher. coeff., triang. trunc jcap
c  iord  order of smoothing; 1=normal-shape, 7=approx. box shape
c  sizeofsm size of area used to smooth in earth radius in radians 
c          (1.57=90deg);
c       eg, .1 for +-9deg dropoff (st dev) in smoothing
      dimension xcof(7,7),xkappa(jcap+1),acof(7)
      complex   cofi((jcap+1)*(jcap+2)/2),cofo((jcap+1)*(jcap+2)/2)
      data xcof /1.,0.,0.,0.,0.,0.,0.,
     *            2.,-1.,0.,0.,0.,0.,0.,
     *	          6.,-6.,1.,0.,0.,0.,0.,
     *            24.,-36.,12.,-1.,0.,0.,0.,
     *            120.,-240.,120,-20.,1.,0.,0.,
     *            720.,-1800.,1200.,-3000.,30.,-1.,0.,
     *            5040.,-15120.,12600.,-4200.,630.,-42.,1./
	   data acof /1.,2.,6.,24.,120.,720.,5040./
	   allocatable funct(:)
	   allocate(funct(jcap+1))
	   xsize=sizeofsm
c	   print'('' xsize='',e12.6,i5)',xsize,iord
	   do 300 i=1,jcap+1
	   funct(i)=0.0
 300   continue
 	   do 100 ii=1,jcap+1
	   i=ii-1
	   xkappa(ii)=(xsize*xsize)*float(i)*float(i+1)
c      write(6,78) ii,xkappa(ii)
	   do 110 j=1,iord
	   if(j.eq.1) then
	    funct(ii)=funct(ii)+xcof(j,iord)/acof(iord)
	   else
	    funct(ii)=funct(ii)+(xcof(j,iord)/
     *                 acof(iord))*(xkappa(ii)**(j-1))
	   endif
 110   continue
	   if(xkappa(ii).gt.100.) xkappa(ii)=20.
 	   funct(ii)=funct(ii)*exp(-xkappa(ii))
c	   funct(ii)=funct(ii)*((2.72)**(-xkappa(ii)))
c      write(6,77) ii,funct(ii)
 77    format(1x,'ii,funct(ii)',i5,f12.2)
 78    format(1x,'ii,xkappa(ii)',i5,f12.2)
 100   continue
c	   go to 555
c		
	  indo=0
	  do 15 m=0,jcap
	  do 14 n=m,jcap
	  indo=indo+1
c	  write(6,67) m,n,indo
 67   format(1x,'m,n,indo=',3i10)
c	  cofo(indo)=funct(m+1)*cofi(indo)
 	  cofo(indo)=funct(n+1)*cofi(indo)
 14   continue
 15   continue
c	  write(6,66)
 555  continue
 66   format(1x,'smoothing finished')
      deallocate(funct)
      return
      END
c
	  subroutine fillf(cofi,cofo,jcapi,jcapo)
c	  parameter(nrec=142,levs=28,levsp=29)
c     cofi has jcapi triangular truncation that is smaller than cofo 
c        output
c     subroutine fills array up with zero
c     cofo except lower triangular, where it gets cofi values.
	  complex cofi((jcapi+1)*(jcapi+2)/2)
	  complex cofo((jcapo+1)*(jcapo+2)/2)
c
          write(6,322) jcapi,jcapo
 322      format(1x,'fill array from ',i3,' to ',i3)
c
	  indi=0
	  indo=0
	  do 15 m=0,jcapo
	  do 14 n=m,jcapo
	    indo=indo+1
	    if(n.le.jcapi) then
	      indi=indi+1
	      cofo(indo)=cofi(indi)
	    else
	      cofo(indo)=cmplx(0.,0.)
	    endif
 14   continue
 15   continue
	  return
	  end
c

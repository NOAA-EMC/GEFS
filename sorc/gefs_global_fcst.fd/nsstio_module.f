!-------------------------------------------------------------------------------
module nsstio_module
!$$$  Module Documentation Block
!
! Module:    nsstio_module    API for global spectral oceanic file I/O
!   Prgmmr: Xu Li (modified from sfcio_module)         Org: w/nx23     date: 2007-10-26
!
! Abstract: This module provides an Application Program Interface
!   for performing I/O on the ocean restart file of the global oceanic diurnal warming and sub-layer cooling models.
!   Functions include opening, reading, writing, and closing as well as
!   allocating and deallocating data buffers sed in the transfers.
!   The I/O performed here is sequential.
!   The transfers are limited to header records or data records.
!   
! Program History Log:
!   2007-10-26  Xu Li
!   2008-03-25  Xu Li: add surface mask field
!   2008-05-23  Shrinivas Moorthi - Corrected bugs in nsstio_swdbta
!
! Public Variables:
!   nsstio_lhead1      Integer parameter length of first header record (=32)
!   nsstio_intkind     Integer parameter kind or length of passed integers (=4)
!   nsstio_realkind    Integer parameter kind or length of passed reals (=4)
!   nsstio_dblekind    Integer parameter kind or length of passed longreals (=8)
!   nsstio_realfill    Real(nsstio_realkind) fill value (=-9999.)
!   nsstio_dblefill    Real(nsstio_dblekind) fill value (=-9999.)
!
! Public Defined Types:
!   nsstio_head         Ocean file header information
!     clabnsst          Character(nsstio_lhead1) ON85 label
!     fhour             Real(nsstio_realkind) forecast hour
!     idate             Integer(nsstio_intkind)(4) initial date
!                       (hour, month, day, 4-digit year)
!     latb              Integer(nsstio_intkind) latitudes
!     lonb              Integer(nsstio_intkind) longitudes
!     ivo               Integer(nsstio_intkind) version number
!     lsea              Integer(nsstio_intkind) sea levels
!     irealf            Integer(sigio_intkind) floating point flag
!                       (=1 for 4-byte ieee, =2 for 8-byte ieee)
!     lpl               Integer(nsstio_intkind)(latb/2) lons per lat
!     zsea              Real(nsstio_realkind) sea depths (meter)
!
!   nsstio_data        Ocean file data fields
!     tref              Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       sea reference temperature in K
!     dt_cool           Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       sea surface cooling amount by sub-layer cooling effect
!     z_c               Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       sea sub-layer depth in m
!     dt_warm           Real(nsstio_realkind)(:,:,:) pointer to lonb*latb
!                       sea surface warming amount in K
!     z_w               Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       sea diurnal warming layer depth in m
!     C_0               Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr) in dimensionless
!     C_d               Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr) in m^-1 
!     W_0               Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr) in dimensionless
!     W_d               Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr) in m^-1
!     slmsk             Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       surface mask: 0 = water; 1 = land; 2 = ice

!     ifd               Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       index of time integral started mode: 0 = not yet; 1 = started already
!     time_old          Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       solar time at previous time in hours (0 -23)
!     time_ins          Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       the period of time integral since the onset of diurnal warming in seconds
!     I_Sw              Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       time integral of solar radiation flux in (W/M^2)S
!     I_Q               Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       time integral of non-solar heat flux in (W/M^2)S
!     I_Qrian           Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       time integral of sensible heat flux by rainfall in (W/M^2)S
!     I_M               Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       time integral of mass flux S(E-P) in (ppt)m
!     I_Tau             Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       time integral of momentum flux in (N/M^2)S
!     I_Sw_Zw           Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       time integral of d(I_Sw)/d(z_w) in (W/M^3)S
!     I_Q_Ts            Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       time integral of d(I_Q)/d(Ts) in (W/M^2/K)S
!     I_M_Ts            Real(nsstio_realkind)(:,:) pointer to lonb*latb
!                       time integral of d(I_M)/d(Ts) in (ppt)m/K
!                       
!   nsstio_dbta        Ocean file longreal data fields
!                       
! Public Subprograms:
!   nsstio_sropen      Open ocean file for sequential reading
!     lu                Integer(nsstio_intkind) input logical unit
!     cfname            Character(*) input filename
!     iret              Integer(nsstio_intkind) output return code
!
!   nsstio_swopen      Open ocean file for sequential writing
!     lu                Integer(nsstio_intkind) input logical unit
!     cfname            Character(*) input filename
!     iret              Integer(nsstio_intkind) output return code
!
!   nsstio_sclose      Close ocean file for sequential I/O
!     lu                Integer(nsstio_intkind) input logical unit
!     iret              Integer(nsstio_intkind) output return code
!
!   nsstio_srhead      Read header information with sequential I/O
!     lu                Integer(nsstio_intkind) input logical unit
!     head              Type(nsstio_head) output header information
!     iret              Integer(nsstio_intkind) output return code
!
!   nsstio_swhead      Write header information with sequential I/O
!     lu                Integer(nsstio_intkind) input logical unit
!     head              Type(nsstio_head) input header information
!     iret              Integer(nsstio_intkind) output return code
!
!   nsstio_alhead      Allocate head allocatables
!     head              Type(nsstio_head) input/output header information
!     iret              Integer(nsstio_intkind) output return code
!     latb              Integer(nsstio_intkind) optional latitudes
!     lsea             Integer(nsstio_intkind) optional sea levels
!
!   nsstio_aldata      Allocate data fields
!     head              Type(nsstio_head) input header information
!     data              Type(nsstio_data) output data fields
!     iret              Integer(nsstio_intkind) output return code
!
!   nsstio_axdata      Deallocate data fields
!     data              Type(nsstio_data) output data fields
!     iret              Integer(nsstio_intkind) output return code
!
!   nsstio_srdata      Read data fields with sequential I/O
!     lu                Integer(nsstio_intkind) input logical unit
!     head              Type(nsstio_head) input header information
!     data              Type(nsstio_data) output data fields
!     iret              Integer(nsstio_intkind) output return code
!
!   nsstio_swdata      Write data fields with sequential I/O
!     lu                Integer(nsstio_intkind) input logical unit
!     head              Type(nsstio_head) input header information
!     data              Type(nsstio_data) input data fields
!     iret              Integer(nsstio_intkind) output return code
!
!   nsstio_srohdc      Open, read header & data and close with sequential I/O
!     lu                Integer(nsstio_intkind) input logical unit
!     cfname            Character(*) input filename
!     head              Type(nsstio_head) output header information
!     data              Type(nsstio_data) output data fields
!     iret              Integer(nsstio_intkind) output return code
!
!   nsstio_swohdc      Open, write header & data and close with sequential I/O
!     lu                Integer(nsstio_intkind) input logical unit
!     cfname            Character(*) input filename
!     head              Type(nsstio_head) input header information
!     data              Type(nsstio_data) input data fields
!     iret              Integer(nsstio_intkind) output return code
!
!   nsstio_aldbta      Allocate longreal data fields
!     head              Type(nsstio_head) input header information
!     dbta              Type(nsstio_dbta) output longreal data fields
!     iret              Integer(nsstio_intkind) output return code
!
!   nsstio_axdbta      Deallocate longreal data fields
!     dbta              Type(nsstio_dbta) output longreal data fields
!     iret              Integer(nsstio_intkind) output return code
!
!   nsstio_srdbta      Read longreal data fields with sequential I/O
!     lu                Integer(nsstio_intkind) input logical unit
!     head              Type(nsstio_head) input header information
!     dbta              Type(nsstio_dbta) output longreal data fields
!     iret              Integer(nsstio_intkind) output return code
!
!   nsstio_swdbta      Write longreal data fields with sequential I/O
!     lu                Integer(nsstio_intkind) input logical unit
!     head              Type(nsstio_head) input header information
!     dbta              Type(nsstio_dbta) input longreal data fields
!     iret              Integer(nsstio_intkind) output return code
!
! Remarks:
!   (1) Here's the supported nsst file formats.
!       For ivo=200710 
!         Label containing
!           'GFS ','NSST ',ivo,nhead,ndata,reserved(3) (8 4-byte words)
!         Header records
!           lhead(nhead),ldata(ndata) (nhead+ndata 4-byte words)
!           fhour, idate(4), lonb, latb, lsea, irealf,
!             reserved(16)  (25 4-byte words)
!           lpl  (latb/2 4-byte words)
!           zsea  (lsea 4-byte words)
!         Data records
!           tref     (lonb*latb 4-byte words)
!           dt_cool  (lonb*latb 4-byte words)
!           z_c      (lonb*latb 4-byte words)
!           dt_warm  (lonb*latb 4-byte words)
!           z_w      (lonb*latb 4-byte words)
!           C_0      (lonb*latb 4-byte words)
!           C_d      (lonb*latb 4-byte words)
!           W_0      (lonb*latb 4-byte words)
!           W_d      (lonb*latb 4-byte words)
!           slmsk    (lonb*latb 4-byte words)
!           ifd      (lonb*latb 4-byte words)
!           time_old (lonb*latb 4-byte words)
!           time_ins (lonb*latb 4-byte words)
!           I_Sw     (lonb*latb 4-byte words)
!           I_Q      (lonb*latb 4-byte words)
!           I_Qrain  (lonb*latb 4-byte words)
!           I_M      (lonb*latb 4-byte words)
!           I_Tau    (lonb*latb 4-byte words)
!           I_Sw_Zw  (lonb*latb 4-byte words)
!           I_Q_Ts   (lonb*latb 4-byte words)
!           I_M_Ts   (lonb*latb 4-byte words)
!
!   (2) Possible return codes:
!          0   Successful call
!         -1   Open or close I/O error
!         -2   Header record I/O error or unrecognized version
!         -3   Allocation or deallocation error
!         -4   Data record I/O error
!         -5   Insufficient data dimensions allocated
!
! Examples:
!   (1) Read the entire ocean file 'nsstf24' and
!       print out the northernmost ocean temperature at greenwich.
!
!     use nsstio_module
!     type(nsstio_head):: head
!     type(nsstio_data):: data
!     call nsstio_srohdc(11,'nsstf24',head,data,iret)
!     print '(f8.2)',data%tref(1,1)
!     end
!
! Attributes:
!   Language: Fortran 90
!
!$$$
  implicit none
  private
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Variables
  integer,parameter,public:: nsstio_lhead1=32
  integer,parameter,public:: nsstio_intkind=4,nsstio_realkind=4,nsstio_dblekind=8
  real(nsstio_realkind),parameter,public:: nsstio_realfill=-9999.
  real(nsstio_dblekind),parameter,public:: nsstio_dblefill=nsstio_realfill
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Types
  type,public:: nsstio_head
    character(nsstio_lhead1):: clabnsst='                                '
    real(nsstio_realkind):: fhour=0.
    integer(nsstio_intkind):: idate(4)=(/0,0,0,0/),latb=0,lonb=0,lsea=0,ivo=0
    integer(nsstio_intkind):: irealf=1
    integer(nsstio_intkind),allocatable:: lpl(:)
    real(nsstio_realkind),allocatable:: zsea(:)
  end type
  type,public:: nsstio_data
    real(nsstio_realkind),pointer:: tref     (:,:)=>null()
    real(nsstio_realkind),pointer:: dt_cool  (:,:)=>null()
    real(nsstio_realkind),pointer:: z_c      (:,:)=>null()
    real(nsstio_realkind),pointer:: dt_warm  (:,:)=>null()
    real(nsstio_realkind),pointer:: z_w      (:,:)=>null()
    real(nsstio_realkind),pointer:: C_0      (:,:)=>null()
    real(nsstio_realkind),pointer:: C_d      (:,:)=>null()
    real(nsstio_realkind),pointer:: W_0      (:,:)=>null()
    real(nsstio_realkind),pointer:: W_d      (:,:)=>null()
    real(nsstio_realkind),pointer:: slmsk    (:,:)=>null()
    real(nsstio_realkind),pointer:: ifd      (:,:)=>null()
    real(nsstio_realkind),pointer:: time_old (:,:)=>null()
    real(nsstio_realkind),pointer:: time_ins (:,:)=>null()
    real(nsstio_realkind),pointer:: I_Sw     (:,:)=>null()
    real(nsstio_realkind),pointer:: I_Q      (:,:)=>null()
    real(nsstio_realkind),pointer:: I_Qrain  (:,:)=>null()
    real(nsstio_realkind),pointer:: I_M      (:,:)=>null()
    real(nsstio_realkind),pointer:: I_Tau    (:,:)=>null()
    real(nsstio_realkind),pointer:: I_Sw_Zw  (:,:)=>null()
    real(nsstio_realkind),pointer:: I_Q_Ts   (:,:)=>null()
    real(nsstio_realkind),pointer:: I_M_Ts   (:,:)=>null()
  end type
  type,public:: nsstio_dbta
    real(nsstio_dblekind),pointer:: tref     (:,:)=>null()
    real(nsstio_dblekind),pointer:: dt_cool  (:,:)=>null()
    real(nsstio_dblekind),pointer:: z_c      (:,:)=>null()
    real(nsstio_dblekind),pointer:: dt_warm  (:,:)=>null()
    real(nsstio_dblekind),pointer:: z_w      (:,:)=>null()
    real(nsstio_dblekind),pointer:: C_0      (:,:)=>null()
    real(nsstio_dblekind),pointer:: C_d      (:,:)=>null()
    real(nsstio_dblekind),pointer:: W_0      (:,:)=>null()
    real(nsstio_dblekind),pointer:: W_d      (:,:)=>null()
    real(nsstio_dblekind),pointer:: slmsk    (:,:)=>null()
    real(nsstio_dblekind),pointer:: ifd      (:,:)=>null()
    real(nsstio_dblekind),pointer:: time_old (:,:)=>null()
    real(nsstio_dblekind),pointer:: time_ins (:,:)=>null()
    real(nsstio_dblekind),pointer:: I_Sw     (:,:)=>null()
    real(nsstio_dblekind),pointer:: I_Q      (:,:)=>null()
    real(nsstio_dblekind),pointer:: I_Qrain  (:,:)=>null()
    real(nsstio_dblekind),pointer:: I_M      (:,:)=>null()
    real(nsstio_dblekind),pointer:: I_Tau    (:,:)=>null()
    real(nsstio_dblekind),pointer:: I_Sw_Zw  (:,:)=>null()
    real(nsstio_dblekind),pointer:: I_Q_Ts   (:,:)=>null()
    real(nsstio_dblekind),pointer:: I_M_Ts   (:,:)=>null()
  end type
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Subprograms
  public nsstio_sropen,nsstio_swopen,nsstio_sclose,nsstio_srhead,nsstio_swhead
  public nsstio_alhead,nsstio_aldata,nsstio_axdata,nsstio_srdata,nsstio_swdata
  public nsstio_aldbta,nsstio_axdbta,nsstio_srdbta,nsstio_swdbta
  public nsstio_srohdc,nsstio_swohdc
  interface nsstio_srohdc
  module procedure nsstio_srohdca,nsstio_srohdcb
  end interface
  interface nsstio_swohdc
  module procedure nsstio_swohdca,nsstio_swohdcb
  end interface
contains
!-------------------------------------------------------------------------------
  subroutine nsstio_sropen(lu,cfname,iret)
    implicit none
    integer(nsstio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    integer(nsstio_intkind),intent(out):: iret
    integer ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    open(lu,file=cfname,form='unformatted',&
         status='old',action='read',iostat=ios)
    write(*,*) ' successfully opened : ',cfname, ios
    iret=ios
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_swopen(lu,cfname,iret)
    implicit none
    integer(nsstio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    integer(nsstio_intkind),intent(out):: iret
    integer ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    open(lu,file=cfname,form='unformatted',&
         status='unknown',action='readwrite',iostat=ios)
    iret=ios
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_sclose(lu,iret)
    implicit none
    integer(nsstio_intkind),intent(in):: lu
    integer(nsstio_intkind),intent(out):: iret
    integer ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    close(lu,iostat=ios)
    iret=ios
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_srhead(lu,head,iret)
    implicit none
    integer(nsstio_intkind),intent(in):: lu
    type(nsstio_head),intent(out):: head
    integer(nsstio_intkind),intent(out):: iret
    integer:: ios
    character(4):: cgfs,cnsst
    integer(nsstio_intkind):: nhead,ndata,nresv(3)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-2
    rewind lu
    read(lu,iostat=ios) head%clabnsst
    write(*,*) ' head%clabnsst done, ios : ',head%clabnsst, ios
    if(ios.ne.0) return
    if(head%clabnsst(1:8).eq.'GFS NSST') then  ! modern ocean file
      rewind lu
      read(lu,iostat=ios) cgfs,cnsst,head%ivo,nhead,ndata,nresv
      write(*,*) ' cgfs,cnsst done, ios : ',cgfs,cnsst, ios,' ivo=',head%ivo
      if(ios.ne.0) return
      if(head%ivo.eq.200710) then
        read(lu,iostat=ios)
        if(ios.ne.0) return
        read(lu,iostat=ios) head%fhour,head%idate,head%lonb,head%latb,&
                            head%lsea,head%irealf
        write(*,*) ' head%fhour, ios : ',head%fhour, ios
        if(ios.ne.0) return
        call nsstio_alhead(head,ios)
        if(ios.ne.0) return
        read(lu,iostat=ios) head%lpl
        if(ios.ne.0) return
        read(lu,iostat=ios) head%zsea
        if(ios.ne.0) return
      else
        return
      endif
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_swhead(lu,head,iret)
    implicit none
    integer(nsstio_intkind),intent(in):: lu
    type(nsstio_head),intent(in):: head
    integer(nsstio_intkind),intent(out):: iret
    integer:: ios
    integer i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-2
    print *, ' nsst ivo=',head%ivo,' ivo=',head%ivo,' idate=',head%idate
    if(head%ivo.eq.200710) then
      rewind lu
      write(lu,iostat=ios) 'GFS NSST',head%ivo,9,12+4*head%lsea,0,0,0
      if(ios.ne.0) return
      write(lu,iostat=ios) 4*(/8,9+12+4*head%lsea,25,head%latb/2,head%lsea/),&
                           4*head%irealf*(/(head%lonb*head%latb,&
                                            i=1,9+12+4*head%lsea)/)
      if(ios.ne.0) return
      write(lu,iostat=ios) head%fhour,head%idate,head%lonb,head%latb,&
                           head%lsea,head%irealf,(0,i=1,16)
      if(ios.ne.0) return
      write(lu,iostat=ios) head%lpl
      if(ios.ne.0) return
      write(lu,iostat=ios) head%zsea
      if(ios.ne.0) return
      iret=0
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_alhead(head,iret,latb,lsea)
    implicit none
    type(nsstio_head),intent(inout):: head
    integer(nsstio_intkind),intent(out):: iret
    integer(nsstio_intkind),optional,intent(in):: latb,lsea
    integer dim1l,dim1z
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(present(latb)) then
      dim1l=latb/2
    else
      dim1l=head%latb/2
    endif
    if(present(lsea)) then
      dim1z=lsea
    else
      dim1z=head%lsea
    endif
    if(allocated(head%lpl)) deallocate(head%lpl)
    if(allocated(head%zsea)) deallocate(head%zsea)
    allocate(head%lpl(dim1l),head%zsea(dim1z),stat=iret)
    if(iret.eq.0) then
      head%lpl=0
      head%zsea=nsstio_realfill
    endif
    if(iret.ne.0) then
      iret=-3
      write(*,*) ' fail to allocate nsstio%head, iret = ',iret
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_aldata(head,data,iret)
    implicit none
    type(nsstio_head),intent(in):: head
    type(nsstio_data),intent(inout):: data
    integer(nsstio_intkind),intent(out):: iret
    integer dim1,dim2,dim3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_axdata(data,iret)
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsea
    write(*,*) 'in nsstio_aldata, dim1, dim2, dim3 : ', dim1, dim2, dim3
    allocate(&
      data%tref(dim1,dim2),&
      data%dt_cool(dim1,dim2),&
      data%z_c(dim1,dim2),&
      data%dt_warm(dim1,dim2),&
      data%z_w(dim1,dim2),&
      data%C_0(dim1,dim2),&
      data%C_d(dim1,dim2),&
      data%W_0(dim1,dim2),&
      data%W_d(dim1,dim2),&
      data%slmsk(dim1,dim2),&
      data%ifd(dim1,dim2),&
      data%time_old(dim1,dim2),&
      data%time_ins(dim1,dim2),&
      data%I_Sw(dim1,dim2),&
      data%I_Q(dim1,dim2),&
      data%I_Qrain(dim1,dim2),&
      data%I_M(dim1,dim2),&
      data%I_Tau(dim1,dim2),&
      data%I_Sw_Zw(dim1,dim2),&
      data%I_Q_Ts(dim1,dim2),&
      data%I_M_Ts(dim1,dim2),&
      stat=iret)
    if(iret.ne.0) then
      iret=-3
      write(*,*) ' fail to allocate nsstio%data, iret = ',iret
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_axdata(data,iret)
    implicit none
    type(nsstio_data),intent(inout):: data
    integer(nsstio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    deallocate(&
      data%tref,&
      data%dt_cool,&
      data%z_c,&
      data%dt_warm,&
      data%z_w,&
      data%C_0,&
      data%C_d,&
      data%W_0,&
      data%W_d,&
      data%slmsk,&
      data%ifd,&
      data%time_old,&
      data%time_ins,&
      data%I_Sw,&
      data%I_Q,&
      data%I_Qrain,&
      data%I_M,&
      data%I_Tau,&
      data%I_Sw_Zw,&
      data%I_Q_Ts,&
      data%I_M_Ts,&
      stat=iret)
    nullify(&
      data%tref,&
      data%dt_cool,&
      data%z_c,&
      data%dt_warm,&
      data%z_w,&
      data%C_0,&
      data%C_d,&
      data%W_0,&
      data%W_d,&
      data%slmsk,&
      data%ifd,&
      data%time_old,&
      data%time_ins,&
      data%I_Sw,&
      data%I_Q,&
      data%I_Qrain,&
      data%I_M,&
      data%I_Tau,&
      data%I_Sw_Zw,&
      data%I_Q_Ts,&
      data%I_M_Ts)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_srdata(lu,head,data,iret)
    implicit none
    integer(nsstio_intkind),intent(in):: lu
    type(nsstio_head),intent(in):: head
    type(nsstio_data),intent(inout):: data
    integer(nsstio_intkind),intent(out):: iret
    integer:: dim1,dim2,dim3,mdim1,mdim2,mdim3
    integer:: ios
    integer i
    type(nsstio_dbta) dbta
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsea

    mdim1=min(&
      size(data%tref,1),&
      size(data%dt_cool,1),&
      size(data%z_c,1),&
      size(data%dt_warm,1),&
      size(data%z_w,1),&
      size(data%C_0,1),&
      size(data%C_d,1),&
      size(data%W_0,1),&
      size(data%W_d,1),&
      size(data%slmsk,1),&
      size(data%ifd,1),&
      size(data%time_old,1),&
      size(data%time_ins,1),&
      size(data%I_Sw,1),&
      size(data%I_Q,1),&
      size(data%I_Qrain,1),&
      size(data%I_M,1),&
      size(data%I_Tau,1),&
      size(data%I_Sw_Zw,1),&
      size(data%I_Q_Ts,1),&
      size(data%I_M_Ts,1))
    mdim2=min(&
      size(data%tref,2),&
      size(data%dt_cool,2),&
      size(data%z_c,2),&
      size(data%dt_warm,2),&
      size(data%z_w,2),&
      size(data%C_0,2),&
      size(data%C_d,2),&
      size(data%W_0,2),&
      size(data%W_d,2),&
      size(data%slmsk,2),&
      size(data%ifd,2),&
      size(data%time_old,2),&
      size(data%time_ins,2),&
      size(data%I_Sw,2),&
      size(data%I_Q,2),&
      size(data%I_Qrain,2),&
      size(data%I_M,2),&
      size(data%I_Tau,2),&
      size(data%I_Sw_Zw,2),&
      size(data%I_Q_Ts,2),&
      size(data%I_M_Ts,2))
    mdim3=0
    iret=-5
    if(mdim1.lt.dim1.or.&
       mdim2.lt.dim2.or.&
       mdim3.lt.dim3) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%ivo.eq.200710) then
      if(head%irealf.ne.2) then
        read(lu,iostat=ios)     data%tref(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)  data%dt_cool(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)      data%z_c(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)  data%dt_warm(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)      data%z_w(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)      data%C_0(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)      data%C_d(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)      data%W_0(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)      data%W_d(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)    data%slmsk(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)      data%ifd(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%time_old(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%time_ins(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)     data%I_Sw(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)      data%I_Q(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)  data%I_Qrain(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)      data%I_M(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)    data%I_Tau(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)  data%I_Sw_Zw(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)   data%I_Q_Ts(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)   data%I_M_Ts(:dim1,:dim2)
        if(ios.ne.0) return
      else
        call nsstio_aldbta(head,dbta,iret)
        if(iret.ne.0) return
        call nsstio_srdbta(lu,head,dbta,iret)
        if(iret.ne.0) return
        data%tref(:dim1,:dim2)     = dbta%tref(:dim1,:dim2)
        data%dt_cool(:dim1,:dim2)  = dbta%dt_cool(:dim1,:dim2)
        data%z_c(:dim1,:dim2)      = dbta%z_c(:dim1,:dim2)
        data%dt_warm(:dim1,:dim2)  = dbta%dt_warm(:dim1,:dim2)
        data%z_w(:dim1,:dim2)      = dbta%z_w(:dim1,:dim2)
        data%C_0(:dim1,:dim2)      = dbta%C_0(:dim1,:dim2)
        data%C_d(:dim1,:dim2)      = dbta%C_d(:dim1,:dim2)
        data%W_0(:dim1,:dim2)      = dbta%W_0(:dim1,:dim2)
        data%W_d(:dim1,:dim2)      = dbta%W_d(:dim1,:dim2)
        data%slmsk(:dim1,:dim2)    = dbta%slmsk(:dim1,:dim2)
        data%ifd(:dim1,:dim2)      = dbta%ifd(:dim1,:dim2)
        data%time_old(:dim1,:dim2) = dbta%time_old(:dim1,:dim2)
        data%time_ins(:dim1,:dim2) = dbta%time_ins(:dim1,:dim2)
        data%I_Sw(:dim1,:dim2)     = dbta%I_Sw(:dim1,:dim2)
        data%I_Q(:dim1,:dim2)      = dbta%I_Q(:dim1,:dim2)
        data%I_Qrain(:dim1,:dim2)  = dbta%I_Qrain(:dim1,:dim2)
        data%I_M(:dim1,:dim2)      = dbta%I_M(:dim1,:dim2)
        data%I_Tau(:dim1,:dim2)    = dbta%I_Tau(:dim1,:dim2)
        data%I_Sw_Zw(:dim1,:dim2)  = dbta%I_Sw_Zw(:dim1,:dim2)
        data%I_Q_Ts(:dim1,:dim2)   = dbta%I_Q_Ts(:dim1,:dim2)
        data%I_M_Ts(:dim1,:dim2)   = dbta%I_M_Ts(:dim1,:dim2)
        call nsstio_axdbta(dbta,iret)
      endif
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_swdata(lu,head,data,iret)
    implicit none
    integer(nsstio_intkind),intent(in):: lu
    type(nsstio_head),intent(in):: head
    type(nsstio_data),intent(in):: data
    integer(nsstio_intkind),intent(out):: iret
    integer:: dim1,dim2,dim3,mdim1,mdim2,mdim3
    integer:: ios
    integer i
    type(nsstio_dbta) dbta
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsea
    mdim1=min(&
      size(data%tref,1),&
      size(data%dt_cool,1),&
      size(data%z_c,1),&
      size(data%dt_warm,1),&
      size(data%z_w,1),&
      size(data%C_0,1),&
      size(data%C_d,1),&
      size(data%W_0,1),&
      size(data%W_d,1),&
      size(data%slmsk,1),&
      size(data%ifd,1),&
      size(data%time_old,1),&
      size(data%time_ins,1),&
      size(data%I_Sw,1),&
      size(data%I_Q,1),&
      size(data%I_Qrain,1),&
      size(data%I_M,1),&
      size(data%I_Tau,1),&
      size(data%I_Sw_Zw,1),&
      size(data%I_Q_Ts,1),&
      size(data%I_M_Ts,1))
    mdim2=min(&
      size(data%tref,2),&
      size(data%dt_cool,2),&
      size(data%z_c,2),&
      size(data%dt_warm,2),&
      size(data%z_w,2),&
      size(data%C_0,2),&
      size(data%C_d,2),&
      size(data%W_0,2),&
      size(data%W_d,2),&
      size(data%slmsk,2),&
      size(data%ifd,2),&
      size(data%time_old,2),&
      size(data%time_ins,2),&
      size(data%I_Sw,2),&
      size(data%I_Q,2),&
      size(data%I_Qrain,2),&
      size(data%I_M,2),&
      size(data%I_Tau,2),&
      size(data%I_Sw_Zw,2),&
      size(data%I_Q_Ts,2),&
      size(data%I_M_Ts,2))
    mdim3=0
    iret=-5
    if(mdim1.lt.dim1.or.&
       mdim2.lt.dim2.or.&
       mdim3.lt.dim3) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%ivo.eq.200710) then
      if(head%irealf.ne.2) then
        write(lu,iostat=ios) data%tref(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%dt_cool(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%z_c(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%dt_warm(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%z_w(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%C_0(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%C_d(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%W_0(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%W_d(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%slmsk(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%ifd(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%time_old(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%time_ins(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%I_Sw(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%I_Q(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%I_Qrain(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%I_M(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%I_Tau(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%I_Sw_Zw(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%I_Q_Ts(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%I_M_Ts(:dim1,:dim2)
        if(ios.ne.0) return
      else
        call nsstio_aldbta(head,dbta,iret)
        if(iret.ne.0) return
        dbta%tref(:dim1,:dim2)     = data%tref(:dim1,:dim2)
        dbta%dt_cool(:dim1,:dim2)  = data%dt_cool(:dim1,:dim2)
        dbta%z_c(:dim1,:dim2)      = data%z_c(:dim1,:dim2)
        dbta%dt_warm(:dim1,:dim2)  = data%dt_warm(:dim1,:dim2)
        dbta%z_w(:dim1,:dim2)      = data%z_w(:dim1,:dim2)
        dbta%C_0(:dim1,:dim2)      = data%C_0(:dim1,:dim2)
        dbta%C_d(:dim1,:dim2)      = data%C_d(:dim1,:dim2)
        dbta%W_0(:dim1,:dim2)      = data%W_0(:dim1,:dim2)
        dbta%W_d(:dim1,:dim2)      = data%W_d(:dim1,:dim2)
        dbta%slmsk(:dim1,:dim2)    = data%slmsk(:dim1,:dim2)
        dbta%ifd(:dim1,:dim2)      = data%ifd(:dim1,:dim2)
        dbta%time_old(:dim1,:dim2) = data%time_old(:dim1,:dim2)
        dbta%time_ins(:dim1,:dim2) = data%time_ins(:dim1,:dim2)
        dbta%I_Sw(:dim1,:dim2)     = data%I_Sw(:dim1,:dim2)
        dbta%I_Q(:dim1,:dim2)      = data%I_Q(:dim1,:dim2)
        dbta%I_Qrain(:dim1,:dim2)  = data%I_Qrain(:dim1,:dim2)
        dbta%I_M(:dim1,:dim2)      = data%I_M(:dim1,:dim2)
        dbta%I_Tau(:dim1,:dim2)    = data%I_Tau(:dim1,:dim2)
        dbta%I_Sw_Zw(:dim1,:dim2)  = data%I_Sw_Zw(:dim1,:dim2)
        dbta%I_Q_Ts(:dim1,:dim2)   = data%I_Q_Ts(:dim1,:dim2)
        dbta%I_M_Ts(:dim1,:dim2)   = data%I_M_Ts(:dim1,:dim2)
        call nsstio_swdbta(lu,head,dbta,iret)
        if(iret.ne.0) return
        call nsstio_axdbta(dbta,iret)
      endif
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_srohdca(lu,cfname,head,data,iret)
    implicit none
    integer(nsstio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(nsstio_head),intent(inout):: head
    type(nsstio_data),intent(inout):: data
    integer(nsstio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_sropen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_srhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_aldata(head,data,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_srdata(lu,head,data,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_sclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_swohdca(lu,cfname,head,data,iret)
    implicit none
    integer(nsstio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(nsstio_head),intent(in):: head
    type(nsstio_data),intent(in):: data
    integer(nsstio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_swopen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_swhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_swdata(lu,head,data,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_sclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_aldbta(head,dbta,iret)
    implicit none
    type(nsstio_head),intent(in):: head
    type(nsstio_dbta),intent(inout):: dbta
    integer(nsstio_intkind),intent(out):: iret
    integer dim1,dim2,dim3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_axdbta(dbta,iret)
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsea
    allocate(&
      dbta%tref    (dim1,dim2),&
      dbta%dt_cool (dim1,dim2),&
      dbta%z_c     (dim1,dim2),&
      dbta%dt_warm (dim1,dim2),&
      dbta%z_w     (dim1,dim2),&
      dbta%C_0     (dim1,dim2),&
      dbta%C_d     (dim1,dim2),&
      dbta%W_0     (dim1,dim2),&
      dbta%W_d     (dim1,dim2),&
      dbta%slmsk   (dim1,dim2),&
      dbta%ifd     (dim1,dim2),&
      dbta%time_old(dim1,dim2),&
      dbta%time_ins(dim1,dim2),&
      dbta%I_Sw    (dim1,dim2),&
      dbta%I_Q     (dim1,dim2),&
      dbta%I_Qrain (dim1,dim2),&
      dbta%I_M     (dim1,dim2),&
      dbta%I_Tau   (dim1,dim2),&
      dbta%I_Sw_Zw (dim1,dim2),&
      dbta%I_Q_Ts  (dim1,dim2),&
      dbta%I_M_Ts  (dim1,dim2),&
      stat=iret)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_axdbta(dbta,iret)
    implicit none
    type(nsstio_dbta),intent(inout):: dbta
    integer(nsstio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    deallocate(&
      dbta%tref,&
      dbta%dt_cool,&
      dbta%z_c,&
      dbta%dt_warm,&
      dbta%z_w,&
      dbta%C_0,&
      dbta%C_d,&
      dbta%W_0,&
      dbta%W_d,&
      dbta%slmsk,&
      dbta%ifd,&
      dbta%time_old,&
      dbta%time_ins,&
      dbta%I_Sw,&
      dbta%I_Q,&
      dbta%I_Qrain,&
      dbta%I_M,&
      dbta%I_Tau,&
      dbta%I_Sw_Zw,&
      dbta%I_Q_Ts,&
      dbta%I_M_Ts,&
      stat=iret)
    nullify(&
      dbta%tref,&
      dbta%dt_cool,&
      dbta%z_c,&
      dbta%dt_warm,&
      dbta%z_w,&
      dbta%C_0,&
      dbta%C_d,&
      dbta%W_0,&
      dbta%W_d,&
      dbta%slmsk,&
      dbta%ifd,&
      dbta%time_old,&
      dbta%time_ins,&
      dbta%I_Sw,&
      dbta%I_Q,&
      dbta%I_Qrain,&
      dbta%I_M,&
      dbta%I_Tau,&
      dbta%I_Sw_Zw,&
      dbta%I_Q_Ts,&
      dbta%I_M_Ts)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_srdbta(lu,head,dbta,iret)
    implicit none
    integer(nsstio_intkind),intent(in):: lu
    type(nsstio_head),intent(in):: head
    type(nsstio_dbta),intent(inout):: dbta
    integer(nsstio_intkind),intent(out):: iret
    integer:: dim1,dim2,dim3,mdim1,mdim2,mdim3
    integer:: ios
    integer i
    type(nsstio_data):: data
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsea
    mdim1=min(&
      size(dbta%tref,1),&
      size(dbta%dt_cool,1),&
      size(dbta%z_c,1),&
      size(dbta%dt_warm,1),&
      size(dbta%z_w,1),&
      size(dbta%C_0,1),&
      size(dbta%C_d,1),&
      size(dbta%W_0,1),&
      size(dbta%W_d,1),&
      size(dbta%slmsk,1),&
      size(dbta%ifd,1),&
      size(dbta%time_old,1),&
      size(dbta%time_ins,1),&
      size(dbta%I_Sw_Zw,1),&
      size(dbta%I_Q,1),&
      size(dbta%I_Qrain,1),&
      size(dbta%I_M,1),&
      size(dbta%I_Tau,1),&
      size(dbta%I_Sw_Zw,1),&
      size(dbta%I_Q_Ts,1),&
      size(dbta%I_M_Ts,1))
    mdim2=min(&
      size(dbta%tref,2),&
      size(dbta%dt_cool,2),&
      size(dbta%z_c,2),&
      size(dbta%dt_warm,2),&
      size(dbta%z_w,2),&
      size(dbta%C_0,2),&
      size(dbta%C_d,2),&
      size(dbta%W_0,2),&
      size(dbta%W_d,2),&
      size(dbta%slmsk,2),&
      size(dbta%ifd,2),&
      size(dbta%time_old,2),&
      size(dbta%time_ins,2),&
      size(dbta%I_Sw_Zw,2),&
      size(dbta%I_Q,2),&
      size(dbta%I_Qrain,2),&
      size(dbta%I_M,2),&
      size(dbta%I_Tau,2),&
      size(dbta%I_Sw_Zw,2),&
      size(dbta%I_Q_Ts,2),&
      size(dbta%I_M_Ts,2))
    mdim3=0
    iret=-5
    if(mdim1.lt.dim1.or.&
       mdim2.lt.dim2.or.&
       mdim3.lt.dim3) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      call nsstio_aldata(head,data,iret)
      if(iret.ne.0) return
      call nsstio_srdata(lu,head,data,iret)
      if(iret.ne.0) return
        dbta%tref(:dim1,:dim2)     = data%tref(:dim1,:dim2)
        dbta%dt_cool(:dim1,:dim2)  = data%dt_cool(:dim1,:dim2)
        dbta%z_c(:dim1,:dim2)      = data%z_c(:dim1,:dim2)
        dbta%dt_warm(:dim1,:dim2)  = data%dt_warm(:dim1,:dim2)
        dbta%z_w(:dim1,:dim2)      = data%z_w(:dim1,:dim2)
        dbta%C_0(:dim1,:dim2)      = data%C_0(:dim1,:dim2)
        dbta%C_d(:dim1,:dim2)      = data%C_d(:dim1,:dim2)
        dbta%W_0(:dim1,:dim2)      = data%W_0(:dim1,:dim2)
        dbta%W_d(:dim1,:dim2)      = data%W_d(:dim1,:dim2)
        dbta%slmsk(:dim1,:dim2)    = data%slmsk(:dim1,:dim2)
        dbta%ifd(:dim1,:dim2)      = data%ifd(:dim1,:dim2)
        dbta%time_old(:dim1,:dim2) = data%time_old(:dim1,:dim2)
        dbta%time_ins(:dim1,:dim2) = data%time_ins(:dim1,:dim2)
        dbta%I_Sw(:dim1,:dim2)     = data%I_Sw(:dim1,:dim2)
        dbta%I_Q(:dim1,:dim2)      = data%I_Q(:dim1,:dim2)
        dbta%I_Qrain(:dim1,:dim2)  = data%I_Qrain(:dim1,:dim2)
        dbta%I_M(:dim1,:dim2)      = data%I_M(:dim1,:dim2)
        dbta%I_Tau(:dim1,:dim2)    = data%I_Tau(:dim1,:dim2)
        dbta%I_Sw_Zw(:dim1,:dim2)  = data%I_Sw_Zw(:dim1,:dim2)
        dbta%I_Q_Ts(:dim1,:dim2)   = data%I_Q_Ts(:dim1,:dim2)
        dbta%I_M_Ts(:dim1,:dim2)   = data%I_M_Ts(:dim1,:dim2)
      call nsstio_axdata(data,iret)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    elseif(head%ivo == 200710) then
      read(lu,iostat=ios) dbta%tref(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%dt_cool(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%z_c(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%dt_warm(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%z_w(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%C_0(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%C_d(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%W_0(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%W_d(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%slmsk(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%ifd(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%time_old(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%time_ins(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%I_Sw(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%I_Q(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%I_Qrain(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%I_M(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%I_Tau(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%I_Sw_Zw(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%I_Q_Ts(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%I_M_Ts(:dim1,:dim2)
      if(ios.ne.0) return
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_swdbta(lu,head,dbta,iret)
    implicit none
    integer(nsstio_intkind),intent(in):: lu
    type(nsstio_head),intent(in):: head
    type(nsstio_dbta),intent(in):: dbta
    integer(nsstio_intkind),intent(out):: iret
    integer:: dim1,dim2,dim3,mdim1,mdim2,mdim3
    integer:: ios
    integer i
    type(nsstio_data):: data
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsea
    mdim1=min(&
      size(dbta%tref,1),&
      size(dbta%dt_cool,1),&
      size(dbta%z_c,1),&
      size(dbta%dt_warm,1),&
      size(dbta%z_w,1),&
      size(dbta%C_0,1),&
      size(dbta%C_d,1),&
      size(dbta%W_0,1),&
      size(dbta%W_d,1),&
      size(dbta%slmsk,1),&
      size(dbta%ifd,1),&
      size(dbta%time_old,1),&
      size(dbta%time_ins,1),&
      size(dbta%I_Sw,1),&
      size(dbta%I_Q,1),&
      size(dbta%I_Qrain,1),&
      size(dbta%I_M,1),&
      size(dbta%I_Tau,1),&
      size(dbta%I_Sw_Zw,1),&
      size(dbta%I_Q_Ts,1),&
      size(dbta%I_M_Ts,1))
    mdim2=min(&
      size(dbta%tref,2),&
      size(dbta%dt_cool,2),&
      size(dbta%z_c,2),&
      size(dbta%dt_warm,2),&
      size(dbta%z_w,2),&
      size(dbta%C_0,2),&
      size(dbta%C_d,2),&
      size(dbta%W_0,2),&
      size(dbta%W_d,2),&
      size(dbta%slmsk,2),&
      size(dbta%ifd,2),&
      size(dbta%time_old,2),&
      size(dbta%time_ins,2),&
      size(dbta%I_Sw_Zw,2),&
      size(dbta%I_Q,2),&
      size(dbta%I_Qrain,2),&
      size(dbta%I_M,2),&
      size(dbta%I_Tau,2),&
      size(dbta%I_Sw_Zw,2),&
      size(dbta%I_Q_Ts,2),&
      size(dbta%I_M_Ts,2))
    mdim3=0
    iret=-5
    if(mdim1.lt.dim1.or.&
       mdim2.lt.dim2.or.&
       mdim3.lt.dim3) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      call nsstio_aldata(head,data,iret)
      if(iret.ne.0) return
      data%tref(:dim1,:dim2)     = dbta%tref(:dim1,:dim2)
      data%dt_cool(:dim1,:dim2)  = dbta%dt_cool(:dim1,:dim2)
      data%z_c(:dim1,:dim2)      = dbta%z_c(:dim1,:dim2)
      data%dt_warm(:dim1,:dim2)  = dbta%dt_warm(:dim1,:dim2)
      data%z_w(:dim1,:dim2)      = dbta%z_w(:dim1,:dim2)
      data%C_0(:dim1,:dim2)      = dbta%C_0(:dim1,:dim2)
      data%C_d(:dim1,:dim2)      = dbta%C_d(:dim1,:dim2)
      data%W_0(:dim1,:dim2)      = dbta%W_0(:dim1,:dim2)
      data%W_d(:dim1,:dim2)      = dbta%W_d(:dim1,:dim2)
      data%slmsk(:dim1,:dim2)    = dbta%slmsk(:dim1,:dim2)
      data%ifd(:dim1,:dim2)      = dbta%ifd(:dim1,:dim2)
      data%time_old(:dim1,:dim2) = dbta%time_old(:dim1,:dim2)
      data%time_ins(:dim1,:dim2) = dbta%time_ins(:dim1,:dim2)
      data%I_Sw(:dim1,:dim2)     = dbta%I_Sw(:dim1,:dim2)
      data%I_Q(:dim1,:dim2)      = dbta%I_Q(:dim1,:dim2)
      data%I_Qrain(:dim1,:dim2)  = dbta%I_Qrain(:dim1,:dim2)
      data%I_M(:dim1,:dim2)      = dbta%I_M(:dim1,:dim2)
      data%I_Tau(:dim1,:dim2)    = dbta%I_Tau(:dim1,:dim2)
      data%I_Sw_Zw(:dim1,:dim2)  = dbta%I_Sw_Zw(:dim1,:dim2)
      data%I_Q_Ts(:dim1,:dim2)   = dbta%I_Q_Ts(:dim1,:dim2)
      data%I_M_Ts(:dim1,:dim2)   = dbta%I_M_Ts(:dim1,:dim2)
      call nsstio_swdata(lu,head,data,iret)
      if(iret.ne.0) return
      call nsstio_axdata(data,iret)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    elseif(head%ivo == 200710) then
      write(lu,iostat=ios) dbta%tref(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%dt_cool(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%z_c(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%dt_warm(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%z_w(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%C_0(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%C_d(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%W_0(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%W_d(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%slmsk(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%ifd(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%time_old(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%time_ins(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%I_Sw(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%I_Q(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%I_Qrain(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%I_M(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%I_Tau(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%I_Sw_Zw(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%I_Q_Ts(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%I_M_Ts(:dim1,:dim2)
      if(ios.ne.0) return
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_srohdcb(lu,cfname,head,dbta,iret)
    implicit none
    integer(nsstio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(nsstio_head),intent(inout):: head
    type(nsstio_dbta),intent(inout):: dbta
    integer(nsstio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_sropen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_srhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_aldbta(head,dbta,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_srdbta(lu,head,dbta,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_sclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nsstio_swohdcb(lu,cfname,head,dbta,iret)
    implicit none
    integer(nsstio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(nsstio_head),intent(in):: head
    type(nsstio_dbta),intent(in):: dbta
    integer(nsstio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_swopen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_swhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_swdbta(lu,head,dbta,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nsstio_sclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
end module

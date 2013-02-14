      SUBROUTINE AM_BM_hyb_gc
!
! hmhj : this is modified hybrid by finite difference from henry juang
!        work for temperature and enthalpy
!
      USE MACHINE , ONLY : kind_phys
 
      use resol_def
      use namelist_def , only : ref_temp
      use coordinate_def
      use physcons, rd => con_rd, cp => con_cp, rearth => con_rerth
      IMPLICIT NONE 

      REAL(KIND=KIND_EVOD)                                              
     &     PK5REF(LEVP1),DPKREF(LEVS),RPKREF(LEVS),rp2ref(levs),
     &     DBKREF(LEVS), BBKREF(LEVS),rdlref(levs),
     &     HV0(LEVS),HV(LEVS),HVK(LEVS),
     &     C0KREF(LEVP1),dprp,tcprp2,tcpcprp2,
     &     TREF,PSREF,RKAA,KAPPA,RKAPPA
      real alpha(levs),betta(levs),gamma(levs),delta(levs)
      real zm(levs-1,levs-1),tm(levs-1,levs)
      real pm(levs,levs-1),wtm(levs-1,levs)
      real dup(levs),dum(levs)
      real det,wmkm1,wmkp1
      integer lll(levs-1),mmm(levs-1)
      INTEGER K,I,J,N
                                                                        
!     print *,' enter  get_am_bm_hyb_gc_fd'

!     PSREF=80.
      PSREF=101.316                                                         
      KAPPA=RD/CP                                                       
      RKAPPA=1./KAPPA

      if( thermodyn_id.eq.3 ) then
        TREF=ref_temp*cp
        RKAA=KAPPA/(REARTH*REARTH)
      else
        TREF=ref_temp                                                         
        RKAA=RD/(REARTH*REARTH)
      endif
                                                                        
      DO K=1,LEVP1                                                      
       PK5REF(K)=AK5(K)+BK5(K)*PSREF+CK5(K)                                  
      ENDDO                                                             
                                                                        
      DO K=1,LEVS                                                       
       THREF (K)=TREF
       DPKREF(K)=PK5REF(K)-PK5REF(K+1)                                  
       rdlref(k)=0.5/dpkref(k)
       RPKREF(K)=1.0/(PK5REF(K)+PK5REF(K+1)) 
       RP2REF(K)=rpkref(k)*rpkref(k)
       DBKREF(K)=BK5(K)-BK5(K+1)                                  
       BBKREF(K)=BK5(K)+BK5(K+1)                                  
      ENDDO                                                             
 
      c0kref=0
      do k=2,levs
       c0kref(k)=ck5(k)*rkappa/(thref(k-1)+thref(k))
      enddo
                                                                        
c sv
      DO K=1,LEVS                                                       
       SVHYB(K)=DPKREF(K)
      ENDDO                                                             
                                                                        
c hv
      DO K=1,LEVS
        HV0(k)=thref(k)*RPKREF(k)*BBKREF(k)
      ENDDO
      do k=1,LEVS
        hvk(k)=RPKREF(k)*thref(k)*
     &        (DBKREF(k)-RPKREF(k)*DPKREF(k)*BBKREF(k))
      enddo
      hv(1)=0.0
      do k=1,LEVS-1
        hv(k)  = hv(k) + hvk(k)
        hv(k+1)= hv(k) + hvk(k)
      enddo
      hv(LEVS)  = hv(LEVS) + hvk(LEVS)
      do k=1,LEVS
        TOR_HYB(k)=RKAA*(hv0(k)+hv(k))
      enddo

c am
      AMHYB = 0.0
! am1+
      do i=1,levs
        AMHYB(i,  i)=thref(i)*rpkref(i)*(c0kref(i)+c0kref(i+1))
      enddo
      do i=2,levs
        AMHYB(i,i-1)=thref(i)*rpkref(i)*c0kref(i)
      enddo
      do i=1,levs-1
        AMHYB(i,i+1)=thref(i)*rpkref(i)*c0kref(i+1)
      enddo
! am2+
      do i=2,levs-1
        tcprp2=thref(i)*c0kref(i)*pk5ref(i+1)*rp2ref(i)
        amhyb(i,i-1)=amhyb(i,i-1)+2.*tcprp2
        do k=i+1,levs
          amhyb(k,i-1)=amhyb(k,i-1)+4.*tcprp2
        enddo
      enddo
! am3+
      do i=1,levs-1
        tcpcprp2=thref(i)*rp2ref(i)*
     &          (c0kref(i)*pk5ref(i+1)-c0kref(i+1)*pk5ref(i))
        amhyb(i,i)=amhyb(i,i)+2.*tcpcprp2
        do k=i+1,levs
          amhyb(k,i)=amhyb(k,i)+4.*tcpcprp2
        enddo
      enddo
! am4+
      do i=1,levs-1
        tcprp2=thref(i)*c0kref(i+1)*pk5ref(i)*rp2ref(i)
        amhyb(i,i+1)=amhyb(i,i+1)-2.*tcprp2
        do k=i+1,levs
          amhyb(k,i+1)=amhyb(k,i+1)-4.*tcprp2
        enddo
      enddo
! am5+
      do i=1,levs
        dprp=dpkref(i)*rpkref(i)
        amhyb(i,i)=amhyb(i,i)+dprp
        do k=i+1,levs
          amhyb(k,i)=amhyb(k,i)+2.*dprp
        enddo
      enddo
! apply RKAA to the sum
      do j=1,levs
        do i=1,levs
          AMHYB(i,j)=RKAA*amhyb(i,j)
        enddo
      enddo
c bm
      BMHYB = 0.0
      do i=1,levs
        BMHYB(i,i)=kappa*thref(i)*rpkref(i)*dpkref(i)
      enddo
      do j=2,levs
        do i=1,j-1
          BMHYB(i,j)=2.*kappa*thref(i)*rpkref(i)*dpkref(j)
        enddo
      enddo

c need zm, tm and pm for bm+
! alpha, betta, gamma
      alpha(levs)=0.0
      betta(   1)=0.0
      do k=2,levs
        alpha(k-1)=(pk5ref(k)+pk5ref(k+1))/(pk5ref(k-1)+pk5ref(k))
        alpha(k-1)=alpha(k-1)**kappa
      enddo
      do k=1,levs
        gamma(k)=1.0 - kappa*DPKREF(k)*RPKREF(k)*2.0
        delta(k)=1.0 + kappa*DPKREF(k)*RPKREF(k)*2.0
      enddo
      do k=1,levs-1
        betta(k+1)=(pk5ref(k)+pk5ref(k+1))/(pk5ref(k+1)+pk5ref(k+2))
        betta(k+1)=betta(k+1)**kappa
      enddo
! zm
      dup(levs)=0.0
      dum(1 )=0.0
      do k=1,levs-1
        dup(k  )=delta(k)*thref(k)-betta(k+1)*thref(k+1)
        dum(k+1)=alpha(k)*thref(k)-gamma(k+1)*thref(k+1)
      enddo
!
      zm=0.0		! (levs-1,levs-1)
      k=2
        wmkm1=c0kref(k)*rdlref(k-1)
        wmkp1=c0kref(k)*rdlref(  k)
        zm(k-1,k-1)=wmkm1*dup(k-1)+wmkp1*dum(k)-1.0
        zm(k-1,k  )=wmkp1*dup(k)
      do k=3,levs-1
        wmkm1=c0kref(k)*rdlref(k-1)
        wmkp1=c0kref(k)*rdlref(  k)
        zm(k-1,k-2)=wmkm1*dum(k-1)
        zm(k-1,k-1)=wmkm1*dup(k-1)+wmkp1*dum(k)-1.0
        zm(k-1,k  )=wmkp1*dup(k)
      enddo
      k=levs
        wmkm1=c0kref(k)*rdlref(k-1)
        wmkp1=c0kref(k)*rdlref(  k)
        zm(k-1,k-2)=wmkm1*dum(k-1)
        zm(k-1,k-1)=wmkm1*dup(k-1)+wmkp1*dum(k)-1.0
      call iminv(zm,levs-1,det,lll,mmm)
!
! tm
      tm=0.0
      do k=2,levs
        tm(k-1,k-1)=-c0kref(k)*kappa*thref(k-1)*dpkref(k-1)*rpkref(k-1)
        tm(k-1,k  )= c0kref(k)*kappa*thref(k  )*dpkref(k  )*rpkref(k  )
      enddo
      do k=2,levs
        do n=1,levs
          tm(k-1,n)=tm(k-1,n)-bk5(k)*dpkref(n)
        enddo
      enddo
      do k=2,levs
        do n=k,levs
          tm(k-1,n)=tm(k-1,n)+(1.-2.*c0kref(k)*kappa*
     &          (thref(k-1)*rpkref(k-1)+thref(k)*rpkref(k)))*dpkref(n)
        enddo
      enddo
! zm * tm
      wtm=0.0
      do i=1,levs
        do n=1,levs-1
          do j=1,levs-1
            wtm(j,i)=wtm(j,i)+zm(j,n)*tm(n,i)
          enddo
        enddo
      enddo

! pm
      pm=0.0
      k=1
        pm(k,k  )=(delta(k)*thref(k)-betta(k+1)*thref(k+1))*
     &            rdlref(k)
      do k=2,levs-1
        pm(k,k-1)=(alpha(k-1)*thref(k-1)-gamma(k)*thref(k))*
     &            rdlref(k)
        pm(k,k  )=(delta(k)*thref(k)-betta(k+1)*thref(k+1))*
     &            rdlref(k)
      enddo
      k=levs
        pm(k,k-1)=(alpha(k-1)*thref(k-1)-gamma(k)*thref(k))*
     &            rdlref(k)

!
! bm+ = pm * wtm
!
      do i=1,levs
        do k=1,levs
          do j=1,levs-1
            bmhyb(k,i)=bmhyb(k,i)+pm(k,j)*wtm(j,i)
          enddo
        enddo
      enddo
c
    
!     print *,' end of get_am_bm_hyb_gc_fd. '
!!
      RETURN                                                            
      END                                                               

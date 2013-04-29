! Program to get weighted smoothed mask
! original by Juhui.Ma
! Modified by Xiaqiong.Zhou Apr. 2013
      real,dimension(:,:,:),allocatable:: te,teda,tesm
      real ,dimension(:),allocatable::teda_vert,stra,str
       real ,dimension(:,:),allocatable::tek,tesmk
       namelist /namemsk/jcap,nlons,nlats,nlevs,smx,wx
       read(*,namemsk)
!!!!!!!!!!!!!!!!!decaying average!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        allocate(te(nlons,nlats,nlevs),teda(nlons,nlats,nlevs))
        allocate(tesm(nlons,nlats,nlevs))
      
        open(11,file='TE_SPRD_ave.grd',FORM="UNFORMATTED")
        do k=1,nlevs
         read(11) ((teda(i,j,k),i=1,nlons),j=1,nlats)
        enddo
        close(11)
        open(12,file='TE_SPRD.grd',FORM="UNFORMATTED")
        do k=1,nlevs
        read(12) ((te(i,j,k),i=1,nlons),j=1,nlats)
        enddo
        close(12)
        teda(1:nlons,1:nlats,1:nlevs)=(1-wx)*teda(1:nlons,1:nlats,1:nlevs)+wx*te(1:nlons,1:nlats,1:nlevs)
!!!!!!!!!!!!!!!!!!vertical profile!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
        deallocate(te)
        allocate(tek(nlons,nlats),tesmk(nlons,nlats))
        lrec=(jcap+1)*(jcap+2)
        allocate (stra(lrec),str(lrec))
       
           do k=1,nlevs
            tek(:,:)=teda(:,:,k)
            call sptez(0,jcap,4,nlons,nlats,stra,tek,-1)
            str=0.0
            call smooth(stra,str,1,smx,jcap)
            call sptez(0,jcap,4,nlons,nlats,str,tesmk,1)
            tesm(:,:,k)=tesmk(:,:)
           enddo
           open(26,file='TE_SPRD_ave_updated.grd',FORM="UNFORMATTED")
           do k=1,nlevs
            write(26) ((tesm(i,j,k),i=1,nlons),j=1,nlats)
           enddo
           close(26)

       end

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
!      print'('' xsize='',e12.6,i5)',xsize,iord
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
!     funct(ii)=funct(ii)*((2.72)**(-xkappa(ii)))
!      write(6,77) ii,funct(ii)
   77   format(1x,'ii,funct(ii)',i5,f12.2)
   78   format(1x,'ii,xkappa(ii)',i5,f12.2)
      enddo

      indo=0
      do m=0,jcap
       do n=m,jcap
        indo=indo+1
!         write(6,67) m,n,indo
   67      format(1x,'m,n,indo=',3i10)
!         cofo(indo)=funct(m+1)*cofi(indo)
        cofo(indo)=funct(n+1)*cofi(indo)
       enddo
      enddo
!       write(6,66)
   66 format(1x,'smoothing finished')
      deallocate(funct)
      return
      END subroutine smooth




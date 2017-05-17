!
!                                                   * Jim Purser    *
!                                                   * 31st Aug 2012 *
!
!=============================================================================
subroutine qtpint(x,y,n,nint,t,s,m)
!=============================================================================
! Quick-fix substitute for old STPINT routine, when nint=2 only.
! Linearly interpolate from n source pairs (x,y) to m targets (t,s)
!=============================================================================
implicit none
integer,          intent(IN ):: n,m,nint
real,dimension(n),intent(IN ):: x,y
real,dimension(m),intent(IN ):: t
real,dimension(m),intent(OUT):: s
!-----------------------------------------------------------------------------
integer:: i,ip,j
real   :: tj
!=============================================================================
if(nint/=2)stop 'In qtpint; nint must be equal to 2'
i=1
do j=1,m
   tj=t(j)
   do ip=i+1,n-1
      if(x(ip)>=tj)exit
   enddo
   i=ip-1
   s(j)=( (x(ip)-tj)*y(i)+(tj-x(i))*y(ip) )/(x(ip)-x(i))
enddo
end subroutine qtpint














C
C                                                   * Jim Purser    *
C                                                   * 31st Aug 2012 *
C
C=============================================================================
          subroutine qtpint(x,y,n,nint,t,s,m)
C=============================================================================
C Quick-fix substitute for old STPINT routine, when nint=2 only.
C Linearly interpolate from n source pairs (x,y) to m targets (t,s)
C=============================================================================
        integer n,m,nint
        dimension x(n),y(n)
        dimension t(m),s(m)
C-----------------------------------------------------------------------------
        integer i,ip,j
        real    tj
C=============================================================================
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
        return       
        end 

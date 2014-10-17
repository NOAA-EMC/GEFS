      SUBROUTINE LONLAT_PARA(global_lats_r,XLON,XLAT,lonsperlar)
!
c***********************************************************************
!
      USE MACHINE , ONLY : kind_rad,kind_phys

      use resol_def
      use layout1
      use gg_def
      use physcons, pi => con_pi
      implicit none
      integer i,j,lat
      integer                 lonsperlar(latr)
      real (kind=kind_rad) tpi,hpi,bphi
      PARAMETER (TPI=2.E0*PI,HPI=0.5E0*PI)
      integer              global_lats_r(latr)
      real (kind=kind_rad) XLON(lonr,lats_node_r)
      real (kind=kind_rad) XLAT(lonr,lats_node_r)
!
      xlon=0.
      xlat=0.
 
      DO j=1,lats_node_r
        lat = global_lats_r(ipt_lats_node_r-1+j)
        BPHI = TPI/lonsperlar(lat)
        if (lat.le.latr2) then
          DO i=1,lonsperlar(lat)
            XLON(I,J) = (i-1) * BPHI
            XLAT(I,J) = HPI - colrad_r(lat)
          ENDDO
        else
          DO i=1,lonsperlar(lat)
            XLON(I,J) =  (i-1) * BPHI
            XLAT(I,J) = colrad_r(lat)-HPI
          ENDDO
        endif
      ENDDO
 
      RETURN
      END
 

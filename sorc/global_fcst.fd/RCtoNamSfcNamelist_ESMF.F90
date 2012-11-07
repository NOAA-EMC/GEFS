! !SUBROUTINE RCtoNamSfcNamelist
!
! !DESCRIPTION: Put the parameters of the config file NAMSFC.rc to the 
!               original NAMSFC namelist.
!---------------------------------------------------------------------
! !REVISION HISTORY:
!
!  December 2004      Weiyu Yang Initial code.
!
! !INTERFACE:
!
 SUBROUTINE RCtoNamSfcNamelist(NAMSFC,  FNGLAC, FNMXIC,                       &
                      FNTSFC, FNWETC, FNSNOC, FNZORC, FNALBC, FNAISC,         &
                      FNPLRC, FNTG3C, FNSCVC, FNSMCC, FNSTCC, FNACNC,         &
                      FNVEGC, fnvetc, fnsotc,                                 &
                      FNTSFA, FNWETA, FNSNOA, FNZORA, FNALBA, FNAISA,         &
                      FNPLRA, FNTG3A, FNSCVA, FNSMCA, FNSTCA, FNACNA,         &
                      FNVMNC, FNVMXC, FNSLPC, FNABSC, FSICL,  FSICS,          &
                      FNVEGA, fnveta, fnsota,                                 &
                      FNMSKH,                                                 &
                      LDEBUG, LGCHEK, LQCBGS, CRITP1, CRITP2, CRITP3,         &
                      FNDCLM, FNDANL,                                         &
                      LANOM,                                                  &
                      FTSFL, FTSFS, FALBL, FALBS, FAISL, FAISS, FSNOL, FSNOS, &
                      FZORL, FZORS, FPLRL, FPLRS, FSMCL, FSMCS,               &
                      FSTCL, FSTCS, fvegl, fvegs, fvetl, fvets, fsotl, fsots, &
                      FCTSFL, FCTSFS, FCALBL, FCALBS, FCSNOL, FCSNOS,         &
                      FCZORL, FCZORS, FCPLRL, FCPLRS, FCSMCL, FCSMCS,         &
                      FCSTCL, FCSTCS, fsalfl, fsalfs, fcalfl, fcalfs,         &
                      ICTSFL, ICTSFS, ICALBL, ICALBS, ICSNOL, ICSNOS,         &
                      ICZORL, ICZORS, ICPLRL, ICPLRS, ICSMCL, ICSMCS,         &
                      ICSTCL, ICSTCS, icalfl, icalfs,                         &
                      GAUSM,   DEADS,  QCMSK,  ZNLST,                         &
                      MONCLM,  MONANL,  MONFCS,  MONMER,  MONDIF,  IGRDBG,    &
                      BLNMSK,  BLTMSK)

 USE nam_gfs_NAMSFC_NameList_ESMFMod

 IMPLICIT none

 TYPE(NAMSFC_NameList) :: NAMSFC

 CHARACTER(80) :: FNGLAC, FNMXIC
 CHARACTER(80) :: FNVMNC, FNVMXC, FNSLPC, FNABSC

 CHARACTER(80) :: FNTSFC, FNWETC, FNSNOC, FNZORC, FNALBC, FNAISC,  &
                  FNPLRC, FNTG3C, FNSCVC, FNSMCC, FNSTCC, FNACNC,  &
                  FNVEGC, fnvetc, fnsotc
 CHARACTER(80) :: FNTSFA, FNWETA, FNSNOA, FNZORA, FNALBA, FNAISA,  &
                  FNPLRA, FNTG3A, FNSCVA, FNSMCA, FNSTCA, FNACNA,  &
                  FNVEGA, fnveta, fnsota
 CHARACTER(80) :: FNMSKH
 CHARACTER(80) :: FNDCLM, FNDANL

 LOGICAL       :: LDEBUG, LGCHEK, LQCBGS
 LOGICAL       :: LANOM
 LOGICAL       :: GAUSM,   DEADS,  QCMSK,  ZNLST,                  &
                  MONCLM,  MONANL,  MONFCS,  MONMER,  MONDIF

 REAL(KIND = kind_io8) :: CRITP1, CRITP2, CRITP3,                                 &
                          FTSFL, FTSFS, FALBL, FALBS, FAISL, FAISS, FSNOL, FSNOS, &
                          FZORL, FZORS, FPLRL, FPLRS, FSMCL(25), FSMCS,           &
                          FSTCL(25), FSTCS(25),                                   &
                          FCSMCL(25), FCSMCS(25), FCSTCL(25), FCSTCS(25),         &
                          fvegl, fvegs, fvetl, fvets, fsotl, fsots,               &
                          FCTSFL, FCTSFS, FCALBL, FCALBS, FCSNOL, FCSNOS,         &
                          FCZORL, FCZORS, FCPLRL, FCPLRS,                         &
                          fsalfl, fsalfs, fcalfl, fcalfs,                         &
                          BLNMSK, BLTMSK,                                         &
                          FSICL,  FSICS
 INTEGER               :: ICTSFL, ICTSFS, ICALBL, ICALBS, ICSNOL, ICSNOS,         &
                          ICZORL, ICZORS, ICPLRL, ICPLRS,                         &
                          ICSMCL(25), ICSMCS(25), ICSTCL(25), ICSTCS(25),         &
                          icalfl, icalfs, IGRDBG

 FNGLAC = NAMSFC%FNGLAC 
 FNMXIC = NAMSFC%FNMXIC 
 FNTSFC = NAMSFC%FNTSFC 
 FNSNOC = NAMSFC%FNSNOC
 FNZORC = NAMSFC%FNZORC 
 FNALBC = NAMSFC%FNALBC 
 FNAISC = NAMSFC%FNAISC 
 FNTG3C = NAMSFC%FNTG3C 
 FNVEGC = NAMSFC%FNVEGC 
 fnvetc = NAMSFC%FNVETC 
 fnsotc = NAMSFC%FNSOTC 
 FNSMCC = NAMSFC%FNSMCC 
 FNMSKH = NAMSFC%FNMSKH 
 FNTSFA = NAMSFC%FNTSFA 
 FNACNA = NAMSFC%FNACNA 
 FNSNOA = NAMSFC%FNSNOA 

 FNVMNC = NAMSFC%FNVMNC 
 FNVMXC = NAMSFC%FNVMXC 
 FNSLPC = NAMSFC%FNSLPC 
 FNABSC = NAMSFC%FNABSC 

 FSICL  = NAMSFC%FSICL
 FSICS  = NAMSFC%FSICS

 IF(NAMSFC%LDEBUG == 0) THEN
     LDEBUG = .false.
 ELSE
     LDEBUG = .true.
 END IF

 FSMCL(2) = NAMSFC%FSMCL(2)
 FSMCL(3) = NAMSFC%FSMCL(3)
 FSMCL(4) = NAMSFC%FSMCL(4)
 FTSFS    = NAMSFC%FTSFS
 FAISS    = NAMSFC%FAISS
 FSNOL    = NAMSFC%FSNOL
 FTSFL    = NAMSFC%FTSFL
 FAISL    = NAMSFC%FAISL
 FSNOS    = NAMSFC%FSNOS

 END SUBROUTINE RCtoNamSfcNamelist

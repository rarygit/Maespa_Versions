        !COMPILER-GENERATED INTERFACE MODULE: Fri Oct 07 14:37:29 2016
        MODULE READROOTPARS__genmod
          INTERFACE 
            SUBROUTINE READROOTPARS(UFILE,ROOTRESFRACI,ROOTRADTABLEI,   &
     &ROOTSRLTABLEI,ROOTMASSTOTTABLEI,NROOTLAYERI,FRACROOTI,LAYTHICK,   &
     &ROOTBETA,DATESROOTI,NOROOTDATES,RFAGEBEGINI,RFPAR1I,RFPAR2I,      &
     &RFPAR3I,ROOTFRONTLIMITI)
              INTEGER(KIND=4) :: UFILE
              REAL(KIND=4) :: ROOTRESFRACI
              REAL(KIND=4) :: ROOTRADTABLEI(30)
              REAL(KIND=4) :: ROOTSRLTABLEI(30)
              REAL(KIND=4) :: ROOTMASSTOTTABLEI(30)
              INTEGER(KIND=4) :: NROOTLAYERI
              REAL(KIND=4) :: FRACROOTI(75,30)
              REAL(KIND=4) :: LAYTHICK(75)
              REAL(KIND=4) :: ROOTBETA
              INTEGER(KIND=4) :: DATESROOTI(30)
              INTEGER(KIND=4) :: NOROOTDATES
              INTEGER(KIND=4) :: RFAGEBEGINI
              REAL(KIND=4) :: RFPAR1I
              REAL(KIND=4) :: RFPAR2I
              REAL(KIND=4) :: RFPAR3I
              INTEGER(KIND=4) :: ROOTFRONTLIMITI
            END SUBROUTINE READROOTPARS
          END INTERFACE 
        END MODULE READROOTPARS__genmod

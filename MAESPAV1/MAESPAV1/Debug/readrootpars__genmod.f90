        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:46 2016
        MODULE READROOTPARS__genmod
          INTERFACE 
            SUBROUTINE READROOTPARS(UFILE,ROOTRESFRACI,ROOTRADTABLEI,   &
     &ROOTDENSTABLEI,ROOTMASSTOTTABLEI,NROOTLAYERI,FRACROOTI,LAYTHICK,  &
     &ROOTBETA,DATESROOTI,NOROOTDATES,NOROOTSPEC,NOSPEC)
              INTEGER(KIND=4) :: UFILE
              REAL(KIND=4) :: ROOTRESFRACI
              REAL(KIND=4) :: ROOTRADTABLEI(30)
              REAL(KIND=4) :: ROOTDENSTABLEI(30)
              REAL(KIND=4) :: ROOTMASSTOTTABLEI(30)
              INTEGER(KIND=4) :: NROOTLAYERI
              REAL(KIND=4) :: FRACROOTI(75,30,4)
              REAL(KIND=4) :: LAYTHICK(75)
              REAL(KIND=4) :: ROOTBETA
              INTEGER(KIND=4) :: DATESROOTI(30)
              INTEGER(KIND=4) :: NOROOTDATES
              INTEGER(KIND=4) :: NOROOTSPEC
              INTEGER(KIND=4) :: NOSPEC
            END SUBROUTINE READROOTPARS
          END INTERFACE 
        END MODULE READROOTPARS__genmod

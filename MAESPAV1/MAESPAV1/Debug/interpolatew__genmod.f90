        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:46 2016
        MODULE INTERPOLATEW__genmod
          INTERFACE 
            SUBROUTINE INTERPOLATEW(IDAY,ISTART,NOSPEC,NOKPDATES,DATESKP&
     &,PLANTKTABLE,PLANTK,NOROOTDATES,DATESROOT,NOROOTSPEC,ROOTRADTABLE,&
     &ROOTDENSTABLE,ROOTMASSTOTTABLE,FRACROOTSPEC,LAYTHICK,ROOTRESFRAC, &
     &ROOTXSECAREA,ROOTLEN,ROOTRESIST,ROOTMASS,NROOTLAYER,ROOTRAD)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: ISTART
              INTEGER(KIND=4) :: NOSPEC
              INTEGER(KIND=4) :: NOKPDATES
              INTEGER(KIND=4) :: DATESKP(30)
              REAL(KIND=4) :: PLANTKTABLE(30)
              REAL(KIND=4) :: PLANTK
              INTEGER(KIND=4) :: NOROOTDATES
              INTEGER(KIND=4) :: DATESROOT(30)
              INTEGER(KIND=4) :: NOROOTSPEC
              REAL(KIND=4) :: ROOTRADTABLE(30)
              REAL(KIND=4) :: ROOTDENSTABLE(30)
              REAL(KIND=4) :: ROOTMASSTOTTABLE(30)
              REAL(KIND=4) :: FRACROOTSPEC(75,4)
              REAL(KIND=4) :: LAYTHICK(75)
              REAL(KIND=4) :: ROOTRESFRAC
              REAL(KIND=4) :: ROOTXSECAREA
              REAL(KIND=4) :: ROOTLEN(75,4)
              REAL(KIND=4) :: ROOTRESIST
              REAL(KIND=4) :: ROOTMASS(75,4)
              INTEGER(KIND=4) :: NROOTLAYER
              REAL(KIND=4) :: ROOTRAD
            END SUBROUTINE INTERPOLATEW
          END INTERFACE 
        END MODULE INTERPOLATEW__genmod

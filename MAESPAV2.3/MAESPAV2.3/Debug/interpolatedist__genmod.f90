        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 28 16:12:48 2016
        MODULE INTERPOLATEDIST__genmod
          INTERFACE 
            SUBROUTINE INTERPOLATEDIST(IDAY,ISTART,FRACROOTTABLE,       &
     &NOROOTDATES,NOROOTSPEC,DATESROOT,FRACROOTSPEC,NROOTLAYER,         &
     &NALPHASPEC,FALPHATABLESPEC,DATESLIA2,NOLIADATES,FALPHASPEC,       &
     &NSPECIES,ISMAESPA,LAYTHICK,RFAGEBEGIN,RFPAR1,RFPAR2,RFPAR3,       &
     &ROOTFRONTLIMIT,ROOTFRONT)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: ISTART
              REAL(KIND=4) :: FRACROOTTABLE(51,45,4)
              INTEGER(KIND=4) :: NOROOTDATES
              INTEGER(KIND=4) :: NOROOTSPEC
              INTEGER(KIND=4) :: DATESROOT(45)
              REAL(KIND=4) :: FRACROOTSPEC(51,4)
              INTEGER(KIND=4) :: NROOTLAYER
              INTEGER(KIND=4) :: NALPHASPEC(4)
              REAL(KIND=4) :: FALPHATABLESPEC(20,45,4)
              INTEGER(KIND=4) :: DATESLIA2(45,4)
              INTEGER(KIND=4) :: NOLIADATES(4)
              REAL(KIND=4) :: FALPHASPEC(20,4)
              INTEGER(KIND=4) :: NSPECIES
              LOGICAL(KIND=4) :: ISMAESPA
              REAL(KIND=4) :: LAYTHICK(51)
              INTEGER(KIND=4) :: RFAGEBEGIN(4)
              REAL(KIND=4) :: RFPAR1(4)
              REAL(KIND=4) :: RFPAR2(4)
              REAL(KIND=4) :: RFPAR3(4)
              INTEGER(KIND=4) :: ROOTFRONTLIMIT
              REAL(KIND=4) :: ROOTFRONT(4)
            END SUBROUTINE INTERPOLATEDIST
          END INTERFACE 
        END MODULE INTERPOLATEDIST__genmod

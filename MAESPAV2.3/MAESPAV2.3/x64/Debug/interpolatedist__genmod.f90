        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 23 15:25:55 2016
        MODULE INTERPOLATEDIST__genmod
          INTERFACE 
            SUBROUTINE INTERPOLATEDIST(IDAY,ISTART,FRACROOTTABLE,       &
     &NOROOTDATES,NOROOTSPEC,DATESROOT,FRACROOTSPEC,NROOTLAYER,         &
     &NALPHASPEC,FALPHATABLESPEC,DATESLIA2,NOLIADATES,FALPHASPEC,       &
     &NSPECIES,ISMAESPA,LAYTHICK,RFAGEBEGIN,RFPAR1,RFPAR2,RFPAR3,       &
     &ROOTFRONTLIMIT,ROOTFRONT)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: ISTART
              REAL(KIND=4) :: FRACROOTTABLE(51,45,5)
              INTEGER(KIND=4) :: NOROOTDATES
              INTEGER(KIND=4) :: NOROOTSPEC
              INTEGER(KIND=4) :: DATESROOT(45)
              REAL(KIND=4) :: FRACROOTSPEC(51,5)
              INTEGER(KIND=4) :: NROOTLAYER
              INTEGER(KIND=4) :: NALPHASPEC(5)
              REAL(KIND=4) :: FALPHATABLESPEC(20,45,5)
              INTEGER(KIND=4) :: DATESLIA2(45,5)
              INTEGER(KIND=4) :: NOLIADATES(5)
              REAL(KIND=4) :: FALPHASPEC(20,5)
              INTEGER(KIND=4) :: NSPECIES
              LOGICAL(KIND=4) :: ISMAESPA
              REAL(KIND=4) :: LAYTHICK(51)
              INTEGER(KIND=4) :: RFAGEBEGIN(5)
              REAL(KIND=4) :: RFPAR1(5)
              REAL(KIND=4) :: RFPAR2(5)
              REAL(KIND=4) :: RFPAR3(5)
              INTEGER(KIND=4) :: ROOTFRONTLIMIT
              REAL(KIND=4) :: ROOTFRONT(5)
            END SUBROUTINE INTERPOLATEDIST
          END INTERFACE 
        END MODULE INTERPOLATEDIST__genmod

        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 28 18:03:55 2016
        MODULE SOILRESCALC__genmod
          INTERFACE 
            SUBROUTINE SOILRESCALC(USEMEASSW,SOILCOND,ROOTRESIST,       &
     &ROOTMASS,ROOTLEN,LAYTHICK,ROOTRAD,NROOTLAYER,SOILR1,SOILR2)
              INTEGER(KIND=4) :: USEMEASSW
              REAL(KIND=4) :: SOILCOND(51)
              REAL(KIND=4) :: ROOTRESIST
              REAL(KIND=4) :: ROOTMASS(51)
              REAL(KIND=4) :: ROOTLEN(51)
              REAL(KIND=4) :: LAYTHICK(51)
              REAL(KIND=4) :: ROOTRAD
              INTEGER(KIND=4) :: NROOTLAYER
              REAL(KIND=4) :: SOILR1(51)
              REAL(KIND=4) :: SOILR2(51)
            END SUBROUTINE SOILRESCALC
          END INTERFACE 
        END MODULE SOILRESCALC__genmod

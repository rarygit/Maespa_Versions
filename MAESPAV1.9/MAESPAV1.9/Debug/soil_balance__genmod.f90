        !COMPILER-GENERATED INTERFACE MODULE: Fri Oct 07 14:37:31 2016
        MODULE SOIL_BALANCE__genmod
          INTERFACE 
            SUBROUTINE SOIL_BALANCE(J,POREFRAC,ICEPROP,FRACWATER,       &
     &LAYTHICK,DRAINLIMIT,WATERLOSS,WATERGAIN,KSAT,BPAR,WS,WR,ALPHARET, &
     &NRET,RETFUNCTION,LDRAIN,PLATDRAIN,WATERGAINCAPIL)
              INTEGER(KIND=4) :: J
              REAL(KIND=4) :: POREFRAC(75)
              REAL(KIND=4) :: ICEPROP(75)
              REAL(KIND=4) :: FRACWATER(75)
              REAL(KIND=4) :: LAYTHICK(75)
              REAL(KIND=4) :: DRAINLIMIT
              REAL(KIND=4) :: WATERLOSS(75)
              REAL(KIND=4) :: WATERGAIN(75)
              REAL(KIND=4) :: KSAT
              REAL(KIND=4) :: BPAR
              REAL(KIND=4) :: WS
              REAL(KIND=4) :: WR
              REAL(KIND=4) :: ALPHARET
              REAL(KIND=4) :: NRET
              INTEGER(KIND=4) :: RETFUNCTION
              REAL(KIND=4) :: LDRAIN(75)
              REAL(KIND=4) :: PLATDRAIN
              REAL(KIND=4) :: WATERGAINCAPIL(75)
            END SUBROUTINE SOIL_BALANCE
          END INTERFACE 
        END MODULE SOIL_BALANCE__genmod

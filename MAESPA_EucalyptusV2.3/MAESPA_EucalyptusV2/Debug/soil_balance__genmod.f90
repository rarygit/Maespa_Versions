        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:25 2016
        MODULE SOIL_BALANCE__genmod
          INTERFACE 
            SUBROUTINE SOIL_BALANCE(J,POREFRAC,ICEPROP,FRACWATER,       &
     &LAYTHICK,DRAINLIMIT,WATERLOSS,WATERGAIN,KSAT,BPAR,WS,WR,ALPHARET, &
     &NRET,RETFUNCTION,LDRAIN,PLATDRAIN,WATERGAINCAPIL)
              INTEGER(KIND=4) :: J
              REAL(KIND=4) :: POREFRAC(51)
              REAL(KIND=4) :: ICEPROP(51)
              REAL(KIND=4) :: FRACWATER(51)
              REAL(KIND=4) :: LAYTHICK(51)
              REAL(KIND=4) :: DRAINLIMIT
              REAL(KIND=4) :: WATERLOSS(51)
              REAL(KIND=4) :: WATERGAIN(51)
              REAL(KIND=4) :: KSAT
              REAL(KIND=4) :: BPAR
              REAL(KIND=4) :: WS
              REAL(KIND=4) :: WR
              REAL(KIND=4) :: ALPHARET
              REAL(KIND=4) :: NRET
              INTEGER(KIND=4) :: RETFUNCTION
              REAL(KIND=4) :: LDRAIN(51)
              REAL(KIND=4) :: PLATDRAIN
              REAL(KIND=4) :: WATERGAINCAPIL(51)
            END SUBROUTINE SOIL_BALANCE
          END INTERFACE 
        END MODULE SOIL_BALANCE__genmod

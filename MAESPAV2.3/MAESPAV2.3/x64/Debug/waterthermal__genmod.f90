        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 06 12:06:59 2016
        MODULE WATERTHERMAL__genmod
          INTERFACE 
            SUBROUTINE WATERTHERMAL(NLAYER,FRACWATER,POREFRAC,SOILTEMP, &
     &LAYTHICK,WATERGAIN,TAIRK,WATERLOSS,PPTGAIN,VOLHC)
              INTEGER(KIND=4) :: NLAYER
              REAL(KIND=4) :: FRACWATER(51)
              REAL(KIND=4) :: POREFRAC(51)
              REAL(KIND=4) :: SOILTEMP(51)
              REAL(KIND=4) :: LAYTHICK(51)
              REAL(KIND=4) :: WATERGAIN(51)
              REAL(KIND=4) :: TAIRK
              REAL(KIND=4) :: WATERLOSS(51)
              REAL(KIND=4) :: PPTGAIN(51)
              REAL(KIND=4) :: VOLHC(51)
            END SUBROUTINE WATERTHERMAL
          END INTERFACE 
        END MODULE WATERTHERMAL__genmod

        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 06 12:06:59 2016
        MODULE HEATBALANCE__genmod
          INTERFACE 
            SUBROUTINE HEATBALANCE(NLAYER,FRACWATER,POREFRAC,TAIRK,     &
     &SOILTK,SOILTEMP,LAYTHICK,WATERGAIN,WATERLOSS,PPTGAIN,THERMCOND)
              INTEGER(KIND=4) :: NLAYER
              REAL(KIND=4) :: FRACWATER(51)
              REAL(KIND=4) :: POREFRAC(51)
              REAL(KIND=4) :: TAIRK
              REAL(KIND=4) :: SOILTK
              REAL(KIND=4) :: SOILTEMP(51)
              REAL(KIND=4) :: LAYTHICK(51)
              REAL(KIND=4) :: WATERGAIN(51)
              REAL(KIND=4) :: WATERLOSS(51)
              REAL(KIND=4) :: PPTGAIN(51)
              REAL(KIND=4) :: THERMCOND(51)
            END SUBROUTINE HEATBALANCE
          END INTERFACE 
        END MODULE HEATBALANCE__genmod
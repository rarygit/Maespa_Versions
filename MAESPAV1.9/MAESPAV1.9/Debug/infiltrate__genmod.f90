        !COMPILER-GENERATED INTERFACE MODULE: Fri Oct 07 14:37:31 2016
        MODULE INFILTRATE__genmod
          INTERFACE 
            SUBROUTINE INFILTRATE(SURFACE_WATERMM,NLAYER,POREFRAC,      &
     &FRACWATER,LAYTHICK,WATERGAIN,WATERLOSS,EXPINF,PPTGAIN,OVERFLOW)
              REAL(KIND=4) :: SURFACE_WATERMM
              INTEGER(KIND=4) :: NLAYER
              REAL(KIND=4) :: POREFRAC(75)
              REAL(KIND=4) :: FRACWATER(75)
              REAL(KIND=4) :: LAYTHICK(75)
              REAL(KIND=4) :: WATERGAIN(75)
              REAL(KIND=4) :: WATERLOSS(75)
              REAL(KIND=4) :: EXPINF
              REAL(KIND=4) :: PPTGAIN(75)
              REAL(KIND=4) :: OVERFLOW
            END SUBROUTINE INFILTRATE
          END INTERFACE 
        END MODULE INFILTRATE__genmod
        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 23 15:25:57 2016
        MODULE WETTINGLAYERS__genmod
          INTERFACE 
            SUBROUTINE WETTINGLAYERS(POREFRAC,WETTINGBOT,WETTINGTOP,    &
     &SURFACE_WATERMM,SNOW,SOILTK,QE,NLAYER,LAYTHICK,DRYTHICKMIN,       &
     &DRYTHICK)
              INTEGER(KIND=4) :: NLAYER
              REAL(KIND=4) :: POREFRAC(NLAYER)
              REAL(KIND=4) :: WETTINGBOT(10)
              REAL(KIND=4) :: WETTINGTOP(10)
              REAL(KIND=4) :: SURFACE_WATERMM
              REAL(KIND=4) :: SNOW
              REAL(KIND=4) :: SOILTK
              REAL(KIND=4) :: QE
              REAL(KIND=4) :: LAYTHICK(NLAYER)
              REAL(KIND=4) :: DRYTHICKMIN
              REAL(KIND=4) :: DRYTHICK
            END SUBROUTINE WETTINGLAYERS
          END INTERFACE 
        END MODULE WETTINGLAYERS__genmod

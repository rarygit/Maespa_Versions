        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 28 18:03:55 2016
        MODULE INITWATBAL__genmod
          INTERFACE 
            SUBROUTINE INITWATBAL(LAYTHICK,WETTINGBOT,WETTINGTOP,       &
     &POREFRAC,WATERGAIN,WATERLOSS,PPTGAIN,INITWATER,DRYTHICKMIN,       &
     &DRYTHICK,CANOPY_STORE,SURFACE_WATERMM,FRACWATER,WSOIL,WSOILROOT,  &
     &NLAYER,NROOTLAYER,ICEPROP,QE,RUNOFF,OUTFLOW,SOILDEPTH,SOILDATA,   &
     &USEMEASSW)
              REAL(KIND=4) :: LAYTHICK(51)
              REAL(KIND=4) :: WETTINGBOT(10)
              REAL(KIND=4) :: WETTINGTOP(10)
              REAL(KIND=4) :: POREFRAC(51)
              REAL(KIND=4) :: WATERGAIN(51)
              REAL(KIND=4) :: WATERLOSS(51)
              REAL(KIND=4) :: PPTGAIN(51)
              REAL(KIND=4) :: INITWATER(51)
              REAL(KIND=4) :: DRYTHICKMIN
              REAL(KIND=4) :: DRYTHICK
              REAL(KIND=4) :: CANOPY_STORE
              REAL(KIND=4) :: SURFACE_WATERMM
              REAL(KIND=4) :: FRACWATER(51)
              REAL(KIND=4) :: WSOIL
              REAL(KIND=4) :: WSOILROOT
              INTEGER(KIND=4) :: NLAYER
              INTEGER(KIND=4) :: NROOTLAYER
              REAL(KIND=4) :: ICEPROP(51)
              REAL(KIND=4) :: QE
              REAL(KIND=4) :: RUNOFF
              REAL(KIND=4) :: OUTFLOW
              REAL(KIND=4) :: SOILDEPTH
              INTEGER(KIND=4) :: SOILDATA
              INTEGER(KIND=4) :: USEMEASSW
            END SUBROUTINE INITWATBAL
          END INTERFACE 
        END MODULE INITWATBAL__genmod
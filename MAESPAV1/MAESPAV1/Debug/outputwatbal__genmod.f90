        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:45 2016
        MODULE OUTPUTWATBAL__genmod
          INTERFACE 
            SUBROUTINE OUTPUTWATBAL(IDAY,IHOUR,NROOTLAYER,NLAYER,WSOIL, &
     &WSOILROOT,PPT,CANOPY_STORE,EVAPSTORE,DRAINSTORE,SURFACE_WATERMM,  &
     &ETMM,ETMM2,USEMEASET,ETMEAS,DISCHARGE,FRACWATER,WEIGHTEDSWP,KTOT, &
     &DRYTHICK,SOILEVAP,OVERFLOW,THERMCOND,FRACUPTAKE,SOILMOISTURE,     &
     &FSOIL1,NSUMMED,TOTTMP,SOILTEMP,TAIR,QH,QE,QN,QC,RGLOBUND,RGLOBABV,&
     &RGLOBABV12,RADINTERC,ESOIL,TOTLAI,WTITLE,RADINTERC1,RADINTERC2,   &
     &RADINTERC3,SCLOSTTOT,SOILWP,FRACAPAR)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: IHOUR
              INTEGER(KIND=4) :: NROOTLAYER
              INTEGER(KIND=4) :: NLAYER
              REAL(KIND=4) :: WSOIL
              REAL(KIND=4) :: WSOILROOT
              REAL(KIND=4) :: PPT
              REAL(KIND=4) :: CANOPY_STORE
              REAL(KIND=4) :: EVAPSTORE
              REAL(KIND=4) :: DRAINSTORE
              REAL(KIND=4) :: SURFACE_WATERMM
              REAL(KIND=4) :: ETMM
              REAL(KIND=4) :: ETMM2
              INTEGER(KIND=4) :: USEMEASET
              REAL(KIND=4) :: ETMEAS
              REAL(KIND=4) :: DISCHARGE
              REAL(KIND=4) :: FRACWATER(75)
              REAL(KIND=4) :: WEIGHTEDSWP
              REAL(KIND=4) :: KTOT
              REAL(KIND=4) :: DRYTHICK
              REAL(KIND=4) :: SOILEVAP
              REAL(KIND=4) :: OVERFLOW
              REAL(KIND=4) :: THERMCOND(75)
              REAL(KIND=4) :: FRACUPTAKE(75)
              REAL(KIND=4) :: SOILMOISTURE
              REAL(KIND=4) :: FSOIL1
              INTEGER(KIND=4) :: NSUMMED
              REAL(KIND=4) :: TOTTMP
              REAL(KIND=4) :: SOILTEMP(75)
              REAL(KIND=4) :: TAIR
              REAL(KIND=4) :: QH
              REAL(KIND=4) :: QE
              REAL(KIND=4) :: QN
              REAL(KIND=4) :: QC
              REAL(KIND=4) :: RGLOBUND
              REAL(KIND=4) :: RGLOBABV
              REAL(KIND=4) :: RGLOBABV12
              REAL(KIND=4) :: RADINTERC
              REAL(KIND=4) :: ESOIL
              REAL(KIND=4) :: TOTLAI
              CHARACTER(LEN=80) :: WTITLE
              REAL(KIND=4) :: RADINTERC1
              REAL(KIND=4) :: RADINTERC2
              REAL(KIND=4) :: RADINTERC3
              REAL(KIND=4) :: SCLOSTTOT
              REAL(KIND=4) :: SOILWP(75)
              REAL(KIND=4) :: FRACAPAR
            END SUBROUTINE OUTPUTWATBAL
          END INTERFACE 
        END MODULE OUTPUTWATBAL__genmod
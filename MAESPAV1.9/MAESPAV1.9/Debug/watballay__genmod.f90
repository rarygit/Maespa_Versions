        !COMPILER-GENERATED INTERFACE MODULE: Fri Oct 07 14:37:31 2016
        MODULE WATBALLAY__genmod
          INTERFACE 
            SUBROUTINE WATBALLAY(IDAY,IHOUR,PPT,RUTTERB,RUTTERD,        &
     &MAXSTORAGE,THROUGHFALL,RADINTERC,CANOPY_STORE,EVAPSTORE,DRAINSTORE&
     &,SURFACE_WATERMM,POREFRAC,WETTINGBOT,WETTINGTOP,NLAYER,NROOTLAYER,&
     &LAYTHICK,SOILTK,QE,TAIRK,VPDPA,WIND,ZHT,Z0HT,ZPD,PRESS,ETMM,      &
     &USEMEASET,ETMEAS,FRACUPTAKE,ICEPROP,FRACWATER,DRAINLIMIT,KSAT,BPAR&
     &,WSOIL,WSOILROOT,DISCHARGE,DRYTHICKMIN,DRYTHICK,QEMM,OVERFLOW,    &
     &WATERGAIN,WATERLOSS,PPTGAIN,KEEPWET,EXPINF,WS,WR,PSIE,ALPHARET,   &
     &NRET,RETFUNCTION,SOILWP,IWATTABLAYER,ISIMWATTAB,PLATDRAIN,WATCAPIL&
     &,TOTLAI)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: IHOUR
              REAL(KIND=4) :: PPT
              REAL(KIND=4) :: RUTTERB
              REAL(KIND=4) :: RUTTERD
              REAL(KIND=4) :: MAXSTORAGE
              REAL(KIND=4) :: THROUGHFALL
              REAL(KIND=4) :: RADINTERC
              REAL(KIND=4) :: CANOPY_STORE
              REAL(KIND=4) :: EVAPSTORE
              REAL(KIND=4) :: DRAINSTORE
              REAL(KIND=4) :: SURFACE_WATERMM
              REAL(KIND=4) :: POREFRAC(75)
              REAL(KIND=4) :: WETTINGBOT(10)
              REAL(KIND=4) :: WETTINGTOP(10)
              INTEGER(KIND=4) :: NLAYER
              INTEGER(KIND=4) :: NROOTLAYER
              REAL(KIND=4) :: LAYTHICK(75)
              REAL(KIND=4) :: SOILTK
              REAL(KIND=4) :: QE
              REAL(KIND=4) :: TAIRK
              REAL(KIND=4) :: VPDPA
              REAL(KIND=4) :: WIND
              REAL(KIND=4) :: ZHT
              REAL(KIND=4) :: Z0HT
              REAL(KIND=4) :: ZPD
              REAL(KIND=4) :: PRESS
              REAL(KIND=4) :: ETMM
              INTEGER(KIND=4) :: USEMEASET
              REAL(KIND=4) :: ETMEAS
              REAL(KIND=4) :: FRACUPTAKE(75)
              REAL(KIND=4) :: ICEPROP(75)
              REAL(KIND=4) :: FRACWATER(75)
              REAL(KIND=4) :: DRAINLIMIT(75)
              REAL(KIND=4) :: KSAT(75)
              REAL(KIND=4) :: BPAR(75)
              REAL(KIND=4) :: WSOIL
              REAL(KIND=4) :: WSOILROOT
              REAL(KIND=4) :: DISCHARGE
              REAL(KIND=4) :: DRYTHICKMIN
              REAL(KIND=4) :: DRYTHICK
              REAL(KIND=4) :: QEMM
              REAL(KIND=4) :: OVERFLOW
              REAL(KIND=4) :: WATERGAIN(75)
              REAL(KIND=4) :: WATERLOSS(75)
              REAL(KIND=4) :: PPTGAIN(75)
              INTEGER(KIND=4) :: KEEPWET
              REAL(KIND=4) :: EXPINF
              REAL(KIND=4) :: WS(75)
              REAL(KIND=4) :: WR(75)
              REAL(KIND=4) :: PSIE(75)
              REAL(KIND=4) :: ALPHARET(75)
              REAL(KIND=4) :: NRET(75)
              INTEGER(KIND=4) :: RETFUNCTION
              REAL(KIND=4) :: SOILWP(75)
              INTEGER(KIND=4) :: IWATTABLAYER
              INTEGER(KIND=4) :: ISIMWATTAB
              REAL(KIND=4) :: PLATDRAIN
              REAL(KIND=4) :: WATCAPIL
              REAL(KIND=4) :: TOTLAI
            END SUBROUTINE WATBALLAY
          END INTERFACE 
        END MODULE WATBALLAY__genmod

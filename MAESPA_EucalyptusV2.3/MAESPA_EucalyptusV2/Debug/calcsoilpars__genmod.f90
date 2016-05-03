        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:25 2016
        MODULE CALCSOILPARS__genmod
          INTERFACE 
            SUBROUTINE CALCSOILPARS(NLAYER,NROOTLAYER,ISPEC,SOILWP,     &
     &FRACWATER,FRACORGANIC,POREFRAC,SOILCOND,THERMCOND,ROOTMASS,ROOTLEN&
     &,LAYTHICK,ICEPROP,EQUALUPTAKE,RETFUNCTION,USEMEASSW,SOILDATA,     &
     &SOILMOISTURE,PSIE,BPAR,KSAT,ROOTRESIST,ROOTRESFRAC,ROOTRAD,       &
     &MINROOTWP,TOTLAI,WIND,ZHT,Z0HT,GAMSOIL,WEIGHTEDSWP,TOTESTEVAP,    &
     &FRACUPTAKE,TOTSOILRES,ALPHARET,WS,WR,NRET,ZBC,RZ,ZPD,NOTREES,     &
     &EXTWIND,IWATTABLAYER,ISIMWATTAB)
              INTEGER(KIND=4) :: NLAYER
              INTEGER(KIND=4) :: NROOTLAYER
              INTEGER(KIND=4) :: ISPEC
              REAL(KIND=4) :: SOILWP(51)
              REAL(KIND=4) :: FRACWATER(51)
              REAL(KIND=4) :: FRACORGANIC(51)
              REAL(KIND=4) :: POREFRAC(51)
              REAL(KIND=4) :: SOILCOND(51)
              REAL(KIND=4) :: THERMCOND(51)
              REAL(KIND=4) :: ROOTMASS(51)
              REAL(KIND=4) :: ROOTLEN(51)
              REAL(KIND=4) :: LAYTHICK(51)
              REAL(KIND=4) :: ICEPROP(51)
              INTEGER(KIND=4) :: EQUALUPTAKE
              INTEGER(KIND=4) :: RETFUNCTION
              INTEGER(KIND=4) :: USEMEASSW
              INTEGER(KIND=4) :: SOILDATA
              REAL(KIND=4) :: SOILMOISTURE
              REAL(KIND=4) :: PSIE(51)
              REAL(KIND=4) :: BPAR(51)
              REAL(KIND=4) :: KSAT(51)
              REAL(KIND=4) :: ROOTRESIST
              REAL(KIND=4) :: ROOTRESFRAC
              REAL(KIND=4) :: ROOTRAD
              REAL(KIND=4) :: MINROOTWP
              REAL(KIND=4) :: TOTLAI
              REAL(KIND=4) :: WIND
              REAL(KIND=4) :: ZHT
              REAL(KIND=4) :: Z0HT
              REAL(KIND=4) :: GAMSOIL
              REAL(KIND=4) :: WEIGHTEDSWP
              REAL(KIND=4) :: TOTESTEVAP
              REAL(KIND=4) :: FRACUPTAKE(51)
              REAL(KIND=4) :: TOTSOILRES
              REAL(KIND=4) :: ALPHARET(51)
              REAL(KIND=4) :: WS(51)
              REAL(KIND=4) :: WR(51)
              REAL(KIND=4) :: NRET(51)
              REAL(KIND=4) :: ZBC(5000)
              REAL(KIND=4) :: RZ(5000)
              REAL(KIND=4) :: ZPD
              INTEGER(KIND=4) :: NOTREES
              REAL(KIND=4) :: EXTWIND
              INTEGER(KIND=4) :: IWATTABLAYER
              INTEGER(KIND=4) :: ISIMWATTAB
            END SUBROUTINE CALCSOILPARS
          END INTERFACE 
        END MODULE CALCSOILPARS__genmod

        !COMPILER-GENERATED INTERFACE MODULE: Tue Dec 06 10:14:09 2016
        MODULE WATERUPTAKELAYER__genmod
          INTERFACE 
            SUBROUTINE WATERUPTAKELAYER(SOILWP,SOILRRES1,SOILRRES2,     &
     &ROOTRESFRAC,MINROOTWP,TOTLAI,ICEPROP,EQUALUPTAKE,USEMEASSW,       &
     &SOILDATA,SOILMOISTURE,ROOTLEN,NROOTLAYER,WEIGHTEDSWP,FRACUPTAKE,  &
     &TOTSOILRES,LAYTHICK,TOTESTEVAP,ZBC,RZ)
              REAL(KIND=4) :: SOILWP(51)
              REAL(KIND=4) :: SOILRRES1(51)
              REAL(KIND=4) :: SOILRRES2(51)
              REAL(KIND=4) :: ROOTRESFRAC
              REAL(KIND=4) :: MINROOTWP
              REAL(KIND=4) :: TOTLAI
              REAL(KIND=4) :: ICEPROP(51)
              INTEGER(KIND=4) :: EQUALUPTAKE
              INTEGER(KIND=4) :: USEMEASSW
              INTEGER(KIND=4) :: SOILDATA
              REAL(KIND=4) :: SOILMOISTURE
              REAL(KIND=4) :: ROOTLEN(51)
              INTEGER(KIND=4) :: NROOTLAYER
              REAL(KIND=4) :: WEIGHTEDSWP
              REAL(KIND=4) :: FRACUPTAKE(51)
              REAL(KIND=4) :: TOTSOILRES
              REAL(KIND=4) :: LAYTHICK(51)
              REAL(KIND=4) :: TOTESTEVAP
              REAL(KIND=4) :: ZBC(5000)
              REAL(KIND=4) :: RZ(5000)
            END SUBROUTINE WATERUPTAKELAYER
          END INTERFACE 
        END MODULE WATERUPTAKELAYER__genmod

        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 30 18:40:56 2016
        MODULE SUMIPT__genmod
          INTERFACE 
            SUBROUTINE SUMIPT(TLEAF,APAR,ANIR,ATHR,ET,HFX,GSC,PSIL,     &
     &TLEAFTABLE,APARTABLE,ANIRTABLE,ATHRTABLE,ETTABLE,HTABLE,GSCTABLE, &
     &PSILTABLE,AREA,AREATOT,ITAR,IPT,TAIR)
              REAL(KIND=4) :: TLEAF
              REAL(KIND=4) :: APAR
              REAL(KIND=4) :: ANIR
              REAL(KIND=4) :: ATHR
              REAL(KIND=4) :: ET
              REAL(KIND=4) :: HFX
              REAL(KIND=4) :: GSC
              REAL(KIND=4) :: PSIL
              REAL(KIND=4) :: TLEAFTABLE(5000,5000)
              REAL(KIND=4) :: APARTABLE(5000,5000)
              REAL(KIND=4) :: ANIRTABLE(5000,5000)
              REAL(KIND=4) :: ATHRTABLE(5000,5000)
              REAL(KIND=4) :: ETTABLE(5000,5000)
              REAL(KIND=4) :: HTABLE(5000,5000)
              REAL(KIND=4) :: GSCTABLE(5000,5000)
              REAL(KIND=4) :: PSILTABLE(5000,5000)
              REAL(KIND=4) :: AREA
              REAL(KIND=4) :: AREATOT
              INTEGER(KIND=4) :: ITAR
              INTEGER(KIND=4) :: IPT
              REAL(KIND=4) :: TAIR
            END SUBROUTINE SUMIPT
          END INTERFACE 
        END MODULE SUMIPT__genmod

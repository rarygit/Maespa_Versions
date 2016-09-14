        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:42 2016
        MODULE SUMHR__genmod
          INTERFACE 
            SUBROUTINE SUMHR(APAR,ANIR,ATHR,ALEAF,RD,GSC,GBH,ET,HFX,    &
     &TLEAF,FSOIL,PSIL,CI,AREA,IHOUR,ILAY,ITAR,NOTARGETS,NUMPNT,NSUMMED,&
     &TOTTMP,PPAR,PPS,PTRANSP,THRAB,FCO2,FRESPF,GSCAN,GBHCAN,FH2O,FHEAT,&
     &TCAN,FSOIL1,PSILCAN,PSILCANMIN,CICAN,ECANMAX,ACANMAX)
              REAL(KIND=4) :: APAR
              REAL(KIND=4) :: ANIR
              REAL(KIND=4) :: ATHR
              REAL(KIND=4) :: ALEAF
              REAL(KIND=4) :: RD
              REAL(KIND=4) :: GSC
              REAL(KIND=4) :: GBH
              REAL(KIND=4) :: ET
              REAL(KIND=4) :: HFX
              REAL(KIND=4) :: TLEAF
              REAL(KIND=4) :: FSOIL
              REAL(KIND=4) :: PSIL
              REAL(KIND=4) :: CI
              REAL(KIND=4) :: AREA
              INTEGER(KIND=4) :: IHOUR
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: ITAR
              INTEGER(KIND=4) :: NOTARGETS
              INTEGER(KIND=4) :: NUMPNT
              INTEGER(KIND=4) :: NSUMMED
              REAL(KIND=4) :: TOTTMP
              REAL(KIND=4) :: PPAR(4500,15,96)
              REAL(KIND=4) :: PPS(4500,15,96)
              REAL(KIND=4) :: PTRANSP(4500,15,96)
              REAL(KIND=4) :: THRAB(4500,96,3)
              REAL(KIND=4) :: FCO2(4500,96)
              REAL(KIND=4) :: FRESPF(4500,96)
              REAL(KIND=4) :: GSCAN(4500,96)
              REAL(KIND=4) :: GBHCAN(4500,96)
              REAL(KIND=4) :: FH2O(4500,96)
              REAL(KIND=4) :: FHEAT(4500,96)
              REAL(KIND=4) :: TCAN(4500,96)
              REAL(KIND=4) :: FSOIL1
              REAL(KIND=4) :: PSILCAN(4500,96)
              REAL(KIND=4) :: PSILCANMIN(4500,96)
              REAL(KIND=4) :: CICAN(4500,96)
              REAL(KIND=4) :: ECANMAX(4500,96)
              REAL(KIND=4) :: ACANMAX(4500,96)
            END SUBROUTINE SUMHR
          END INTERFACE 
        END MODULE SUMHR__genmod

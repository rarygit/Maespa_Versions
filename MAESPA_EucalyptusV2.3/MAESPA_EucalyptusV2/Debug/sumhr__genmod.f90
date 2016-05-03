        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:20 2016
        MODULE SUMHR__genmod
          INTERFACE 
            SUBROUTINE SUMHR(APAR,ANIR,ATHR,ALEAF,RD,GSC,GBH,ET,        &
     &ETDEFICIT,HFX,TLEAF,FSOIL,PSIL,CI,AREA,IHOUR,ILAY,ITAR,NOTARGETS, &
     &NUMPNT,NSUMMED,TOTTMP,PPAR,PPS,PTRANSP,THRAB,FCO2,FRESPF,GSCAN,   &
     &GBHCAN,FH2O,ETCANDEFICIT,FHEAT,TCAN,FSOIL1,PSILCAN,PSILCANMIN,    &
     &CICAN,ECANMAX,ACANMAX,FOLT)
              REAL(KIND=4) :: APAR
              REAL(KIND=4) :: ANIR
              REAL(KIND=4) :: ATHR
              REAL(KIND=4) :: ALEAF
              REAL(KIND=4) :: RD
              REAL(KIND=4) :: GSC
              REAL(KIND=4) :: GBH
              REAL(KIND=4) :: ET
              REAL(KIND=4) :: ETDEFICIT
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
              REAL(KIND=4) :: PPAR(5000,15,96)
              REAL(KIND=4) :: PPS(5000,15,96)
              REAL(KIND=4) :: PTRANSP(5000,15,96)
              REAL(KIND=4) :: THRAB(5000,96,3)
              REAL(KIND=4) :: FCO2(5000,96)
              REAL(KIND=4) :: FRESPF(5000,96)
              REAL(KIND=4) :: GSCAN(5000,96)
              REAL(KIND=4) :: GBHCAN(5000,96)
              REAL(KIND=4) :: FH2O(5000,96)
              REAL(KIND=4) :: ETCANDEFICIT(5000,96)
              REAL(KIND=4) :: FHEAT(5000,96)
              REAL(KIND=4) :: TCAN(5000,96)
              REAL(KIND=4) :: FSOIL1
              REAL(KIND=4) :: PSILCAN(5000,96)
              REAL(KIND=4) :: PSILCANMIN(5000,96)
              REAL(KIND=4) :: CICAN(5000,96)
              REAL(KIND=4) :: ECANMAX(5000,96)
              REAL(KIND=4) :: ACANMAX(5000,96)
              REAL(KIND=4) :: FOLT
            END SUBROUTINE SUMHR
          END INTERFACE 
        END MODULE SUMHR__genmod

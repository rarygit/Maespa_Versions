        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:42 2016
        MODULE ZEROHR__genmod
          INTERFACE 
            SUBROUTINE ZEROHR(THRAB,FCO2,FRESPF,FRESPW,FRESPB,FRESPFR,  &
     &FRESPCR,FH2O,GSCAN,GBHCAN,FHEAT,PPAR,PPS,PTRANSP,TCAN,FSOIL1,     &
     &PSILCAN,PSILCANMIN,CICAN,NSUMMED,TOTTMP,ECANMAX,ACANMAX)
              REAL(KIND=4) :: THRAB(4500,96,3)
              REAL(KIND=4) :: FCO2(4500,96)
              REAL(KIND=4) :: FRESPF(4500,96)
              REAL(KIND=4) :: FRESPW(4500,96)
              REAL(KIND=4) :: FRESPB(4500,96)
              REAL(KIND=4) :: FRESPFR(4500,96)
              REAL(KIND=4) :: FRESPCR(4500,96)
              REAL(KIND=4) :: FH2O(4500,96)
              REAL(KIND=4) :: GSCAN(4500,96)
              REAL(KIND=4) :: GBHCAN(4500,96)
              REAL(KIND=4) :: FHEAT(4500,96)
              REAL(KIND=4) :: PPAR(4500,15,96)
              REAL(KIND=4) :: PPS(4500,15,96)
              REAL(KIND=4) :: PTRANSP(4500,15,96)
              REAL(KIND=4) :: TCAN(4500,96)
              REAL(KIND=4) :: FSOIL1
              REAL(KIND=4) :: PSILCAN(4500,96)
              REAL(KIND=4) :: PSILCANMIN(4500,96)
              REAL(KIND=4) :: CICAN(4500,96)
              INTEGER(KIND=4) :: NSUMMED
              REAL(KIND=4) :: TOTTMP
              REAL(KIND=4) :: ECANMAX(4500,96)
              REAL(KIND=4) :: ACANMAX(4500,96)
            END SUBROUTINE ZEROHR
          END INTERFACE 
        END MODULE ZEROHR__genmod

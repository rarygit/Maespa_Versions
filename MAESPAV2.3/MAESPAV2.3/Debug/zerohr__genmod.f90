        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 28 16:53:48 2016
        MODULE ZEROHR__genmod
          INTERFACE 
            SUBROUTINE ZEROHR(THRAB,FCO2,FRESPF,FRESPW,FRESPB,FRESPFR,  &
     &FRESPCR,FH2O,GSCAN,GBHCAN,FHEAT,PPAR,PPS,PTRANSP,TCAN,FSOIL1,     &
     &PSILCAN,PSILCANMIN,CICAN,NSUMMED,TOTTMP,ECANMAX,ACANMAX,          &
     &ETCANDEFICIT)
              REAL(KIND=4) :: THRAB(5000,96,3)
              REAL(KIND=4) :: FCO2(5000,96)
              REAL(KIND=4) :: FRESPF(5000,96)
              REAL(KIND=4) :: FRESPW(5000,96)
              REAL(KIND=4) :: FRESPB(5000,96)
              REAL(KIND=4) :: FRESPFR(5000,96)
              REAL(KIND=4) :: FRESPCR(5000,96)
              REAL(KIND=4) :: FH2O(5000,96)
              REAL(KIND=4) :: GSCAN(5000,96)
              REAL(KIND=4) :: GBHCAN(5000,96)
              REAL(KIND=4) :: FHEAT(5000,96)
              REAL(KIND=4) :: PPAR(5000,15,96)
              REAL(KIND=4) :: PPS(5000,15,96)
              REAL(KIND=4) :: PTRANSP(5000,15,96)
              REAL(KIND=4) :: TCAN(5000,96)
              REAL(KIND=4) :: FSOIL1
              REAL(KIND=4) :: PSILCAN(5000,96)
              REAL(KIND=4) :: PSILCANMIN(5000,96)
              REAL(KIND=4) :: CICAN(5000,96)
              INTEGER(KIND=4) :: NSUMMED
              REAL(KIND=4) :: TOTTMP
              REAL(KIND=4) :: ECANMAX(5000,96)
              REAL(KIND=4) :: ACANMAX(5000,96)
              REAL(KIND=4) :: ETCANDEFICIT(5000,96)
            END SUBROUTINE ZEROHR
          END INTERFACE 
        END MODULE ZEROHR__genmod

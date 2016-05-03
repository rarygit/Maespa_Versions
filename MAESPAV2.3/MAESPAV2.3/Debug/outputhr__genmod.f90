        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 28 16:12:47 2016
        MODULE OUTPUTHR__genmod
          INTERFACE 
            SUBROUTINE OUTPUTHR(IDAY,IHOUR,NOTARGETS,ITARGETS,ISPECIES, &
     &TCAN,NOLAY,PPAR,PPS,PTRANSP,FOLLAY,THRAB,FCO2,FRESPF,FRESPW,FRESPB&
     &,FH2OT,GSCAN,GBHCAN,FH2OCAN,FHEAT,VPD,TAIRABOVE,PAR,PSILCAN,      &
     &PSILCANMIN,CICAN,ECANMAX,ACANMAX,ZEN,AZ,ETCANDEFICIT)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: IHOUR
              INTEGER(KIND=4) :: NOTARGETS
              INTEGER(KIND=4) :: ITARGETS(5000)
              INTEGER(KIND=4) :: ISPECIES(5000)
              REAL(KIND=4) :: TCAN(5000,96)
              INTEGER(KIND=4) :: NOLAY
              REAL(KIND=4) :: PPAR(5000,15,96)
              REAL(KIND=4) :: PPS(5000,15,96)
              REAL(KIND=4) :: PTRANSP(5000,15,96)
              REAL(KIND=4) :: FOLLAY(15)
              REAL(KIND=4) :: THRAB(5000,96,3)
              REAL(KIND=4) :: FCO2(5000,96)
              REAL(KIND=4) :: FRESPF(5000,96)
              REAL(KIND=4) :: FRESPW(5000,96)
              REAL(KIND=4) :: FRESPB(5000,96)
              REAL(KIND=4) :: FH2OT(5000,96)
              REAL(KIND=4) :: GSCAN(5000,96)
              REAL(KIND=4) :: GBHCAN(5000,96)
              REAL(KIND=4) :: FH2OCAN(5000,96)
              REAL(KIND=4) :: FHEAT(5000,96)
              REAL(KIND=4) :: VPD(96)
              REAL(KIND=4) :: TAIRABOVE
              REAL(KIND=4) :: PAR(96)
              REAL(KIND=4) :: PSILCAN(5000,96)
              REAL(KIND=4) :: PSILCANMIN(5000,96)
              REAL(KIND=4) :: CICAN(5000,96)
              REAL(KIND=4) :: ECANMAX(5000,96)
              REAL(KIND=4) :: ACANMAX(5000,96)
              REAL(KIND=4) :: ZEN(96)
              REAL(KIND=4) :: AZ(96)
              REAL(KIND=4) :: ETCANDEFICIT(5000,96)
            END SUBROUTINE OUTPUTHR
          END INTERFACE 
        END MODULE OUTPUTHR__genmod

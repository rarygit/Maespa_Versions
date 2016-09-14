        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:45 2016
        MODULE OUTPUTHR__genmod
          INTERFACE 
            SUBROUTINE OUTPUTHR(IDAY,IHOUR,NOTARGETS,ITARGETS,ISPECIES, &
     &TCAN,NOLAY,PPAR,PPS,PTRANSP,FOLLAY,THRAB,FCO2,FRESPF,FRESPW,FRESPB&
     &,FH2OT,GSCAN,GBHCAN,FH2OCAN,FHEAT,VPD,TAIR,PAR,PSILCAN,PSILCANMIN,&
     &CICAN,ECANMAX,ACANMAX,ZEN,AZ)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: IHOUR
              INTEGER(KIND=4) :: NOTARGETS
              INTEGER(KIND=4) :: ITARGETS(4500)
              INTEGER(KIND=4) :: ISPECIES(4500)
              REAL(KIND=4) :: TCAN(4500,96)
              INTEGER(KIND=4) :: NOLAY
              REAL(KIND=4) :: PPAR(4500,15,96)
              REAL(KIND=4) :: PPS(4500,15,96)
              REAL(KIND=4) :: PTRANSP(4500,15,96)
              REAL(KIND=4) :: FOLLAY(15)
              REAL(KIND=4) :: THRAB(4500,96,3)
              REAL(KIND=4) :: FCO2(4500,96)
              REAL(KIND=4) :: FRESPF(4500,96)
              REAL(KIND=4) :: FRESPW(4500,96)
              REAL(KIND=4) :: FRESPB(4500,96)
              REAL(KIND=4) :: FH2OT(4500,96)
              REAL(KIND=4) :: GSCAN(4500,96)
              REAL(KIND=4) :: GBHCAN(4500,96)
              REAL(KIND=4) :: FH2OCAN(4500,96)
              REAL(KIND=4) :: FHEAT(4500,96)
              REAL(KIND=4) :: VPD(96)
              REAL(KIND=4) :: TAIR(96)
              REAL(KIND=4) :: PAR(96)
              REAL(KIND=4) :: PSILCAN(4500,96)
              REAL(KIND=4) :: PSILCANMIN(4500,96)
              REAL(KIND=4) :: CICAN(4500,96)
              REAL(KIND=4) :: ECANMAX(4500,96)
              REAL(KIND=4) :: ACANMAX(4500,96)
              REAL(KIND=4) :: ZEN(96)
              REAL(KIND=4) :: AZ(96)
            END SUBROUTINE OUTPUTHR
          END INTERFACE 
        END MODULE OUTPUTHR__genmod

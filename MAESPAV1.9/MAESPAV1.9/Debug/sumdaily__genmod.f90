        !COMPILER-GENERATED INTERFACE MODULE: Fri Oct 07 14:37:27 2016
        MODULE SUMDAILY__genmod
          INTERFACE 
            SUBROUTINE SUMDAILY(NOTARGETS,THRAB,FCO2,FRESPF,FRESPW,     &
     &FRESPB,FRESPCR,FRESPFR,FH2O,FH2OCAN,FHEAT,TDYAB,TOTCO2,TOTRESPF,  &
     &TOTRESPWM,TOTRESPB,TOTRESPCR,TOTRESPFR,TOTH2O,TOTH2OCAN,TOTHFX)
              INTEGER(KIND=4) :: NOTARGETS
              REAL(KIND=4) :: THRAB(5000,300,3)
              REAL(KIND=4) :: FCO2(5000,300)
              REAL(KIND=4) :: FRESPF(5000,300)
              REAL(KIND=4) :: FRESPW(5000,300)
              REAL(KIND=4) :: FRESPB(5000,300)
              REAL(KIND=4) :: FRESPCR(5000,300)
              REAL(KIND=4) :: FRESPFR(5000,300)
              REAL(KIND=4) :: FH2O(5000,300)
              REAL(KIND=4) :: FH2OCAN(5000,300)
              REAL(KIND=4) :: FHEAT(5000,300)
              REAL(KIND=4) :: TDYAB(5000,3)
              REAL(KIND=4) :: TOTCO2(5000)
              REAL(KIND=4) :: TOTRESPF(5000)
              REAL(KIND=4) :: TOTRESPWM(5000)
              REAL(KIND=4) :: TOTRESPB(5000)
              REAL(KIND=4) :: TOTRESPCR(5000)
              REAL(KIND=4) :: TOTRESPFR(5000)
              REAL(KIND=4) :: TOTH2O(5000)
              REAL(KIND=4) :: TOTH2OCAN(5000)
              REAL(KIND=4) :: TOTHFX(5000)
            END SUBROUTINE SUMDAILY
          END INTERFACE 
        END MODULE SUMDAILY__genmod

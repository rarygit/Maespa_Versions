        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:45 2016
        MODULE OUTPUTDY__genmod
          INTERFACE 
            SUBROUTINE OUTPUTDY(IDAY,NOTARGETS,ITARGETS,ISPECIES,TDYAB, &
     &TOTCO2,TOTRESPF,TOTRESPW,TOTRESPWG,TOTH2O,TOTH2OCAN,TOTHFX,       &
     &TOTRESPCR,TOTRESPFR,TOTRESPFRG,TOTRESPCRG,TOTRESPFG,TOTRESPB,     &
     &TOTRESPBG)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: NOTARGETS
              INTEGER(KIND=4) :: ITARGETS(4500)
              INTEGER(KIND=4) :: ISPECIES(4500)
              REAL(KIND=4) :: TDYAB(4500,3)
              REAL(KIND=4) :: TOTCO2(4500)
              REAL(KIND=4) :: TOTRESPF(4500)
              REAL(KIND=4) :: TOTRESPW(4500)
              REAL(KIND=4) :: TOTRESPWG
              REAL(KIND=4) :: TOTH2O(4500)
              REAL(KIND=4) :: TOTH2OCAN(4500)
              REAL(KIND=4) :: TOTHFX(4500)
              REAL(KIND=4) :: TOTRESPCR(4500)
              REAL(KIND=4) :: TOTRESPFR(4500)
              REAL(KIND=4) :: TOTRESPFRG
              REAL(KIND=4) :: TOTRESPCRG
              REAL(KIND=4) :: TOTRESPFG
              REAL(KIND=4) :: TOTRESPB(4500)
              REAL(KIND=4) :: TOTRESPBG
            END SUBROUTINE OUTPUTDY
          END INTERFACE 
        END MODULE OUTPUTDY__genmod

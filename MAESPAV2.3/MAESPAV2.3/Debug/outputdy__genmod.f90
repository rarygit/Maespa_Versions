        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 28 16:12:47 2016
        MODULE OUTPUTDY__genmod
          INTERFACE 
            SUBROUTINE OUTPUTDY(IDAY,NOTARGETS,ITARGETS,ISPECIES,TDYAB, &
     &TOTCO2,TOTRESPF,TOTRESPW,TOTRESPWG,TOTH2O,TOTH2OCAN,TOTHFX,       &
     &TOTRESPCR,TOTRESPFR,TOTRESPFRG,TOTRESPCRG,TOTRESPFG,TOTRESPB,     &
     &TOTRESPBG)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: NOTARGETS
              INTEGER(KIND=4) :: ITARGETS(5000)
              INTEGER(KIND=4) :: ISPECIES(5000)
              REAL(KIND=4) :: TDYAB(5000,3)
              REAL(KIND=4) :: TOTCO2(5000)
              REAL(KIND=4) :: TOTRESPF(5000)
              REAL(KIND=4) :: TOTRESPW(5000)
              REAL(KIND=4) :: TOTRESPWG
              REAL(KIND=4) :: TOTH2O(5000)
              REAL(KIND=4) :: TOTH2OCAN(5000)
              REAL(KIND=4) :: TOTHFX(5000)
              REAL(KIND=4) :: TOTRESPCR(5000)
              REAL(KIND=4) :: TOTRESPFR(5000)
              REAL(KIND=4) :: TOTRESPFRG
              REAL(KIND=4) :: TOTRESPCRG
              REAL(KIND=4) :: TOTRESPFG
              REAL(KIND=4) :: TOTRESPB(5000)
              REAL(KIND=4) :: TOTRESPBG
            END SUBROUTINE OUTPUTDY
          END INTERFACE 
        END MODULE OUTPUTDY__genmod
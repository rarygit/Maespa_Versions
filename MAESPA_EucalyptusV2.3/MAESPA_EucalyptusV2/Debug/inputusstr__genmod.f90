        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:21 2016
        MODULE INPUTUSSTR__genmod
          INTERFACE 
            SUBROUTINE INPUTUSSTR(NOUSPOINTS,X0,Y0,GRDAREAI,XLU,YLU,ZLU,&
     &USLAI,NOFUDATES,DATESFU,HTUS,NOHUDATES,DATESHU,FOLNUS,NONUDATES,  &
     &DATESNU,EXTKUS)
              INTEGER(KIND=4) :: NOUSPOINTS
              REAL(KIND=4) :: X0
              REAL(KIND=4) :: Y0
              REAL(KIND=4) :: GRDAREAI
              REAL(KIND=4) :: XLU(4000)
              REAL(KIND=4) :: YLU(4000)
              REAL(KIND=4) :: ZLU(4000)
              REAL(KIND=4) :: USLAI(1000,5000)
              INTEGER(KIND=4) :: NOFUDATES
              INTEGER(KIND=4) :: DATESFU(1000)
              REAL(KIND=4) :: HTUS(1000,5000)
              INTEGER(KIND=4) :: NOHUDATES
              INTEGER(KIND=4) :: DATESHU(1000)
              REAL(KIND=4) :: FOLNUS(1000,5000)
              INTEGER(KIND=4) :: NONUDATES
              INTEGER(KIND=4) :: DATESNU(1000)
              REAL(KIND=4) :: EXTKUS
            END SUBROUTINE INPUTUSSTR
          END INTERFACE 
        END MODULE INPUTUSSTR__genmod

        !COMPILER-GENERATED INTERFACE MODULE: Tue Dec 06 10:14:12 2016
        MODULE INPUTUSSTR__genmod
          INTERFACE 
            SUBROUTINE INPUTUSSTR(NOUSPOINTS,X0,Y0,GRDAREAI,XLU,YLU,ZLU,&
     &USLAI,NOFUDATES,DATESFU,HTUS,NOHUDATES,DATESHU,FOLNUS,NONUDATES,  &
     &DATESNU,EXTKUS)
              INTEGER(KIND=4) :: NOUSPOINTS
              REAL(KIND=4) :: X0
              REAL(KIND=4) :: Y0
              REAL(KIND=4) :: GRDAREAI
              REAL(KIND=4) :: XLU(5000)
              REAL(KIND=4) :: YLU(5000)
              REAL(KIND=4) :: ZLU(5000)
              REAL(KIND=4) :: USLAI(45,5000)
              INTEGER(KIND=4) :: NOFUDATES
              INTEGER(KIND=4) :: DATESFU(45)
              REAL(KIND=4) :: HTUS(45,5000)
              INTEGER(KIND=4) :: NOHUDATES
              INTEGER(KIND=4) :: DATESHU(45)
              REAL(KIND=4) :: FOLNUS(45,5000)
              INTEGER(KIND=4) :: NONUDATES
              INTEGER(KIND=4) :: DATESNU(45)
              REAL(KIND=4) :: EXTKUS
            END SUBROUTINE INPUTUSSTR
          END INTERFACE 
        END MODULE INPUTUSSTR__genmod

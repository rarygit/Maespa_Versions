        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 30 18:21:12 2016
        MODULE OUTPUTUS__genmod
          INTERFACE 
            SUBROUTINE OUTPUTUS(IDAY,NOUSPOINTS,XLU,YLU,ZLU,UIBEAM,     &
     &UIDIFF,PARUS,APARUS,PSUS,ETUS)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: NOUSPOINTS
              REAL(KIND=4) :: XLU(5000)
              REAL(KIND=4) :: YLU(5000)
              REAL(KIND=4) :: ZLU(5000)
              REAL(KIND=4) :: UIBEAM(96,5000)
              REAL(KIND=4) :: UIDIFF(96,5000)
              REAL(KIND=4) :: PARUS(96,5000)
              REAL(KIND=4) :: APARUS(96,5000)
              REAL(KIND=4) :: PSUS(96,5000)
              REAL(KIND=4) :: ETUS(96,5000)
            END SUBROUTINE OUTPUTUS
          END INTERFACE 
        END MODULE OUTPUTUS__genmod

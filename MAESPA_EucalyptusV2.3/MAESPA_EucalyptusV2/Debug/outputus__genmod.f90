        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:21 2016
        MODULE OUTPUTUS__genmod
          INTERFACE 
            SUBROUTINE OUTPUTUS(IDAY,NOUSPOINTS,XLU,YLU,ZLU,UIBEAM,     &
     &UIDIFF,PARUS,APARUS,PSUS,ETUS)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: NOUSPOINTS
              REAL(KIND=4) :: XLU(4000)
              REAL(KIND=4) :: YLU(4000)
              REAL(KIND=4) :: ZLU(4000)
              REAL(KIND=4) :: UIBEAM(96,4000)
              REAL(KIND=4) :: UIDIFF(96,4000)
              REAL(KIND=4) :: PARUS(96,4000)
              REAL(KIND=4) :: APARUS(96,4000)
              REAL(KIND=4) :: PSUS(96,4000)
              REAL(KIND=4) :: ETUS(96,4000)
            END SUBROUTINE OUTPUTUS
          END INTERFACE 
        END MODULE OUTPUTUS__genmod

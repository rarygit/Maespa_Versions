        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 23 16:30:01 2016
        MODULE READRW__genmod
          INTERFACE 
            SUBROUTINE READRW(UFILE,MODELRW,EFFYRWI,RMWI,RTEMPWI,       &
     &NOWQDATES,DATESRWQ,Q10WTABLE,COLLA,COLLK,STEMSDWI,RMAI,STEMFORMI)
              INTEGER(KIND=4) :: UFILE
              INTEGER(KIND=4) :: MODELRW
              REAL(KIND=4) :: EFFYRWI
              REAL(KIND=4) :: RMWI
              REAL(KIND=4) :: RTEMPWI
              INTEGER(KIND=4) :: NOWQDATES
              INTEGER(KIND=4) :: DATESRWQ(45)
              REAL(KIND=4) :: Q10WTABLE(45)
              REAL(KIND=4) :: COLLA
              REAL(KIND=4) :: COLLK
              REAL(KIND=4) :: STEMSDWI
              REAL(KIND=4) :: RMAI
              REAL(KIND=4) :: STEMFORMI
            END SUBROUTINE READRW
          END INTERFACE 
        END MODULE READRW__genmod

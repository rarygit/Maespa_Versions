        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 30 18:41:00 2016
        MODULE READTREEARRAY__genmod
          INTERFACE 
            SUBROUTINE READTREEARRAY(UFILE,NARRAY,NOALLTREES,NDATE,     &
     &IDATES,VALUESI)
              INTEGER(KIND=4) :: UFILE
              INTEGER(KIND=4) :: NARRAY
              INTEGER(KIND=4) :: NOALLTREES
              INTEGER(KIND=4) :: NDATE
              INTEGER(KIND=4) :: IDATES(45)
              REAL(KIND=4) :: VALUESI(45,5000)
            END SUBROUTINE READTREEARRAY
          END INTERFACE 
        END MODULE READTREEARRAY__genmod

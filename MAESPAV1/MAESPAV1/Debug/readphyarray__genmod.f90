        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:46 2016
        MODULE READPHYARRAY__genmod
          INTERFACE 
            SUBROUTINE READPHYARRAY(UFILE,NARRAY,NOLAY,NOAGEP,NDATE,    &
     &IDATES,VALUESI)
              INTEGER(KIND=4) :: UFILE
              INTEGER(KIND=4) :: NARRAY
              INTEGER(KIND=4) :: NOLAY
              INTEGER(KIND=4) :: NOAGEP
              INTEGER(KIND=4) :: NDATE
              INTEGER(KIND=4) :: IDATES(30)
              REAL(KIND=4) :: VALUESI(30,15,3)
            END SUBROUTINE READPHYARRAY
          END INTERFACE 
        END MODULE READPHYARRAY__genmod
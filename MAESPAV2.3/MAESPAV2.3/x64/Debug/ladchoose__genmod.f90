        !COMPILER-GENERATED INTERFACE MODULE: Tue Dec 06 10:14:11 2016
        MODULE LADCHOOSE__genmod
          INTERFACE 
            SUBROUTINE LADCHOOSE(IDAY,ISTART,NSPECIES,NOLADDATES,       &
     &DATESLAD,BPTTABLESPEC,BPTSPEC)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: ISTART
              INTEGER(KIND=4) :: NSPECIES
              INTEGER(KIND=4) :: NOLADDATES(5)
              INTEGER(KIND=4) :: DATESLAD(45,5)
              REAL(KIND=4) :: BPTTABLESPEC(8,3,5,45)
              REAL(KIND=4) :: BPTSPEC(8,3,5)
            END SUBROUTINE LADCHOOSE
          END INTERFACE 
        END MODULE LADCHOOSE__genmod

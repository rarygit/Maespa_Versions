        !COMPILER-GENERATED INTERFACE MODULE: Tue Dec 06 10:14:11 2016
        MODULE READBETA__genmod
          INTERFACE 
            SUBROUTINE READBETA(UFILE,NOAGECI,JLEAFI,BPTI,RANDOMI,      &
     &DATESLADOUT,NOLADDATES)
              INTEGER(KIND=4) :: UFILE
              INTEGER(KIND=4) :: NOAGECI
              INTEGER(KIND=4) :: JLEAFI
              REAL(KIND=4) :: BPTI(8,3,45)
              REAL(KIND=4) :: RANDOMI
              INTEGER(KIND=4) :: DATESLADOUT(45)
              INTEGER(KIND=4) :: NOLADDATES
            END SUBROUTINE READBETA
          END INTERFACE 
        END MODULE READBETA__genmod

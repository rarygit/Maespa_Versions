        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 23 16:30:02 2016
        MODULE CALCFBIOM__genmod
          INTERFACE 
            SUBROUTINE CALCFBIOM(IDAY,NOSLADATES,FOLLAY,SLA,PROP,NOLAY, &
     &NOAGEP,FBIOM,FBINC)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: NOSLADATES
              REAL(KIND=4) :: FOLLAY(15)
              REAL(KIND=4) :: SLA(15,3)
              REAL(KIND=4) :: PROP(3)
              INTEGER(KIND=4) :: NOLAY
              INTEGER(KIND=4) :: NOAGEP
              REAL(KIND=4) :: FBIOM
              REAL(KIND=4) :: FBINC
            END SUBROUTINE CALCFBIOM
          END INTERFACE 
        END MODULE CALCFBIOM__genmod

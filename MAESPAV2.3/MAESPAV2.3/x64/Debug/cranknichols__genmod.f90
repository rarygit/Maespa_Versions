        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 06 12:06:59 2016
        MODULE CRANKNICHOLS__genmod
          INTERFACE 
            SUBROUTINE CRANKNICHOLS(NLAYER,LAYTHICK,SOILTK,SOILTEMP,    &
     &VOLHC,THERMCOND)
              INTEGER(KIND=4) :: NLAYER
              REAL(KIND=4) :: LAYTHICK(51)
              REAL(KIND=4) :: SOILTK
              REAL(KIND=4) :: SOILTEMP(51)
              REAL(KIND=4) :: VOLHC(51)
              REAL(KIND=4) :: THERMCOND(51)
            END SUBROUTINE CRANKNICHOLS
          END INTERFACE 
        END MODULE CRANKNICHOLS__genmod
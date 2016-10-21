        !COMPILER-GENERATED INTERFACE MODULE: Fri Oct 07 14:37:31 2016
        MODULE THERMAL__genmod
          INTERFACE 
            SUBROUTINE THERMAL(TAIR,VPD,FSUN,RADABV)
              REAL(KIND=4) :: TAIR(300)
              REAL(KIND=4) :: VPD(300)
              REAL(KIND=4) :: FSUN(300)
              REAL(KIND=4) :: RADABV(300,3)
            END SUBROUTINE THERMAL
          END INTERFACE 
        END MODULE THERMAL__genmod

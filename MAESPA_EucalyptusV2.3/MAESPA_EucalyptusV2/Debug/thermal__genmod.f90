        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:23 2016
        MODULE THERMAL__genmod
          INTERFACE 
            SUBROUTINE THERMAL(TAIR,VPD,FSUN,RADABV,EMSKY)
              REAL(KIND=4) :: TAIR(96)
              REAL(KIND=4) :: VPD(96)
              REAL(KIND=4) :: FSUN(96)
              REAL(KIND=4) :: RADABV(96,3)
              REAL(KIND=4) :: EMSKY(96)
            END SUBROUTINE THERMAL
          END INTERFACE 
        END MODULE THERMAL__genmod

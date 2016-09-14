        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 06 12:06:48 2016
        MODULE ABSTHERM__genmod
          INTERFACE 
            SUBROUTINE ABSTHERM(IPT,MLAYERI,LAYERI,EXPDIF,RADABV,TAIR,  &
     &TSOIL,RHOSOL,DIFUP,DIFDN,SCLOST,ESOIL,DOWNTH,TCAN2,TLEAF,         &
     &TLEAFLAYER)
              INTEGER(KIND=4) :: MLAYERI
              INTEGER(KIND=4) :: IPT
              INTEGER(KIND=4) :: LAYERI
              REAL(KIND=4) :: EXPDIF
              REAL(KIND=4) :: RADABV
              REAL(KIND=4) :: TAIR
              REAL(KIND=4) :: TSOIL
              REAL(KIND=4) :: RHOSOL
              REAL(KIND=4) :: DIFUP(5000,3)
              REAL(KIND=4) :: DIFDN(5000,3)
              REAL(KIND=4) :: SCLOST(5000,3)
              REAL(KIND=4) :: ESOIL
              REAL(KIND=4) :: DOWNTH(5000)
              REAL(KIND=4) :: TCAN2
              REAL(KIND=4) :: TLEAF
              REAL(KIND=4) :: TLEAFLAYER(MLAYERI)
            END SUBROUTINE ABSTHERM
          END INTERFACE 
        END MODULE ABSTHERM__genmod

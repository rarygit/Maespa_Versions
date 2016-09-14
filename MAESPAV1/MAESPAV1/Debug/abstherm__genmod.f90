        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:41 2016
        MODULE ABSTHERM__genmod
          INTERFACE 
            SUBROUTINE ABSTHERM(IPT,MLAYERI,LAYERI,EXPDIF,RADABV,TAIR,  &
     &TSOIL,RHOSOL,DIFUP,DIFDN,SCLOST,ESOIL,DOWNTH)
              INTEGER(KIND=4) :: IPT
              INTEGER(KIND=4) :: MLAYERI
              INTEGER(KIND=4) :: LAYERI
              REAL(KIND=4) :: EXPDIF
              REAL(KIND=4) :: RADABV
              REAL(KIND=4) :: TAIR
              REAL(KIND=4) :: TSOIL
              REAL(KIND=4) :: RHOSOL
              REAL(KIND=4) :: DIFUP(4500,3)
              REAL(KIND=4) :: DIFDN(4500,3)
              REAL(KIND=4) :: SCLOST(4500,3)
              REAL(KIND=4) :: ESOIL
              REAL(KIND=4) :: DOWNTH(4500)
            END SUBROUTINE ABSTHERM
          END INTERFACE 
        END MODULE ABSTHERM__genmod

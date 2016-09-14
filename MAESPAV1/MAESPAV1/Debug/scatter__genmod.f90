        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:41 2016
        MODULE SCATTER__genmod
          INTERFACE 
            SUBROUTINE SCATTER(IPT,IWAVE,MLAYERI,LAYERI,DLAI,EXPDIF,ZEN,&
     &BEXT,DMULT2,SOMULT,BMULT,RADABV,FBEAM,TAIR,TSOIL,ARHO,ATAU,RHOSOL,&
     &DIFUP,DIFDN,SCLOST,THDOWN)
              INTEGER(KIND=4) :: IPT
              INTEGER(KIND=4) :: IWAVE
              INTEGER(KIND=4) :: MLAYERI
              INTEGER(KIND=4) :: LAYERI
              REAL(KIND=4) :: DLAI
              REAL(KIND=4) :: EXPDIF
              REAL(KIND=4) :: ZEN
              REAL(KIND=4) :: BEXT
              REAL(KIND=4) :: DMULT2
              REAL(KIND=4) :: SOMULT
              REAL(KIND=4) :: BMULT
              REAL(KIND=4) :: RADABV
              REAL(KIND=4) :: FBEAM
              REAL(KIND=4) :: TAIR
              REAL(KIND=4) :: TSOIL
              REAL(KIND=4) :: ARHO
              REAL(KIND=4) :: ATAU
              REAL(KIND=4) :: RHOSOL
              REAL(KIND=4) :: DIFUP(4500,3)
              REAL(KIND=4) :: DIFDN(4500,3)
              REAL(KIND=4) :: SCLOST(4500,3)
              REAL(KIND=4) :: THDOWN(4500)
            END SUBROUTINE SCATTER
          END INTERFACE 
        END MODULE SCATTER__genmod

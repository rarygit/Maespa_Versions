        !COMPILER-GENERATED INTERFACE MODULE: Tue Dec 06 10:14:13 2016
        MODULE SCATTER__genmod
          INTERFACE 
            SUBROUTINE SCATTER(IPT,ITAR,IWAVE,MLAYERI,LAYERI,DLAI,EXPDIF&
     &,ZEN,BEXT,DMULT2,SOMULT,BMULT,RADABV,FBEAM,TAIR,TSOIL,ARHO,ATAU,  &
     &RHOSOL,DIFUP,DIFDN,SCLOST,THDOWN,TCAN2,TLEAFTABLE,EMSKY,NUMPNT,   &
     &TOTLAI,FOLLAY,FOLNTR,LGP)
              INTEGER(KIND=4) :: MLAYERI
              INTEGER(KIND=4) :: IPT
              INTEGER(KIND=4) :: ITAR
              INTEGER(KIND=4) :: IWAVE
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
              REAL(KIND=4) :: DIFUP(5000,3)
              REAL(KIND=4) :: DIFDN(5000,3)
              REAL(KIND=4) :: SCLOST(5000,3)
              REAL(KIND=4) :: THDOWN(5000)
              REAL(KIND=4) :: TCAN2
              REAL(KIND=4) :: TLEAFTABLE(5000,5000)
              REAL(KIND=4) :: EMSKY
              INTEGER(KIND=4) :: NUMPNT
              REAL(KIND=4) :: TOTLAI
              REAL(KIND=4) :: FOLLAY(15)
              REAL(KIND=4) :: FOLNTR
              INTEGER(KIND=4) :: LGP(5000)
            END SUBROUTINE SCATTER
          END INTERFACE 
        END MODULE SCATTER__genmod

        !COMPILER-GENERATED INTERFACE MODULE: Fri Oct 07 14:37:28 2016
        MODULE ABSRAD__genmod
          INTERFACE 
            SUBROUTINE ABSRAD(IPT,IWAVE,NZEN,DEXT,BEXT,BMULT,RELDF,     &
     &RADABV,FBEAM,ZEN,ABSRP,DIFDN,DIFUP,DFLUX,BFLUX,SCATFX,DEXTT)
              INTEGER(KIND=4) :: IPT
              INTEGER(KIND=4) :: IWAVE
              INTEGER(KIND=4) :: NZEN
              REAL(KIND=4) :: DEXT(20)
              REAL(KIND=4) :: BEXT
              REAL(KIND=4) :: BMULT
              REAL(KIND=4) :: RELDF
              REAL(KIND=4) :: RADABV
              REAL(KIND=4) :: FBEAM
              REAL(KIND=4) :: ZEN
              REAL(KIND=4) :: ABSRP
              REAL(KIND=4) :: DIFDN
              REAL(KIND=4) :: DIFUP
              REAL(KIND=4) :: DFLUX(5000,3)
              REAL(KIND=4) :: BFLUX(5000,3)
              REAL(KIND=4) :: SCATFX(5000,3)
              REAL(KIND=4) :: DEXTT(5000,20)
            END SUBROUTINE ABSRAD
          END INTERFACE 
        END MODULE ABSRAD__genmod

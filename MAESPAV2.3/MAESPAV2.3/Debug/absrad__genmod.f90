        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 30 18:40:58 2016
        MODULE ABSRAD__genmod
          INTERFACE 
            SUBROUTINE ABSRAD(ITAR,IPT,IWAVE,NZEN,DEXT,BEXT,BMULT,RELDF,&
     &RADABV,FBEAM,ZEN,ABSRP,DIFDN,DIFUP,DFLUX,BFLUX,SCATFX,DEXTT,      &
     &TLEAFTABLE)
              INTEGER(KIND=4) :: ITAR
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
              REAL(KIND=4) :: TLEAFTABLE(5000,5000)
            END SUBROUTINE ABSRAD
          END INTERFACE 
        END MODULE ABSRAD__genmod

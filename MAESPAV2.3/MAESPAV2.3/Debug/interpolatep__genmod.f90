        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 28 16:12:48 2016
        MODULE INTERPOLATEP__genmod
          INTERFACE 
            SUBROUTINE INTERPOLATEP(IDAY,ISTART,NOJDATES,DATESJ,        &
     &JMAXTABLE,NOVDATES,DATESV,VCMAXTABLE,NORDATES,DATESRD,RDTABLE,    &
     &NOSLADATES,DATESSLA,SLATABLE,NOADATES,DATESA,AJQTABLE,NOFQDATES,  &
     &DATESFQ,Q10FTABLE,NOWQDATES,DATESWQ,Q10WTABLE,NOLAY,NOAGEP,JMAX25,&
     &VCMAX25,RD0,SLA,AJQ,Q10F,Q10W,NOGSDATES,DATESGS,G0TABLE,G1TABLE,G0&
     &,G1,NOWLEAFDATES,DATESWLEAF,WLEAFTABLE,WLEAF,G02TABLE,G12TABLE,G02&
     &,G12)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: ISTART
              INTEGER(KIND=4) :: NOJDATES
              INTEGER(KIND=4) :: DATESJ(45)
              REAL(KIND=4) :: JMAXTABLE(45,15,3)
              INTEGER(KIND=4) :: NOVDATES
              INTEGER(KIND=4) :: DATESV(45)
              REAL(KIND=4) :: VCMAXTABLE(45,15,3)
              INTEGER(KIND=4) :: NORDATES
              INTEGER(KIND=4) :: DATESRD(45)
              REAL(KIND=4) :: RDTABLE(45,15,3)
              INTEGER(KIND=4) :: NOSLADATES
              INTEGER(KIND=4) :: DATESSLA(45)
              REAL(KIND=4) :: SLATABLE(45,15,3)
              INTEGER(KIND=4) :: NOADATES
              INTEGER(KIND=4) :: DATESA(45)
              REAL(KIND=4) :: AJQTABLE(45,15,3)
              INTEGER(KIND=4) :: NOFQDATES
              INTEGER(KIND=4) :: DATESFQ(45)
              REAL(KIND=4) :: Q10FTABLE(45)
              INTEGER(KIND=4) :: NOWQDATES
              INTEGER(KIND=4) :: DATESWQ(45)
              REAL(KIND=4) :: Q10WTABLE(45)
              INTEGER(KIND=4) :: NOLAY
              INTEGER(KIND=4) :: NOAGEP
              REAL(KIND=4) :: JMAX25(15,3)
              REAL(KIND=4) :: VCMAX25(15,3)
              REAL(KIND=4) :: RD0(15,3)
              REAL(KIND=4) :: SLA(15,3)
              REAL(KIND=4) :: AJQ(15,3)
              REAL(KIND=4) :: Q10F
              REAL(KIND=4) :: Q10W
              INTEGER(KIND=4) :: NOGSDATES
              INTEGER(KIND=4) :: DATESGS(45)
              REAL(KIND=4) :: G0TABLE(45)
              REAL(KIND=4) :: G1TABLE(45)
              REAL(KIND=4) :: G0
              REAL(KIND=4) :: G1
              INTEGER(KIND=4) :: NOWLEAFDATES
              INTEGER(KIND=4) :: DATESWLEAF(45)
              REAL(KIND=4) :: WLEAFTABLE(45)
              REAL(KIND=4) :: WLEAF
              REAL(KIND=4) :: G02TABLE(45)
              REAL(KIND=4) :: G12TABLE(45)
              REAL(KIND=4) :: G02
              REAL(KIND=4) :: G12
            END SUBROUTINE INTERPOLATEP
          END INTERFACE 
        END MODULE INTERPOLATEP__genmod

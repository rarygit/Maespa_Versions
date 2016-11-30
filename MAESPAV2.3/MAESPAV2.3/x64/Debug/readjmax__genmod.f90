        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 30 18:21:15 2016
        MODULE READJMAX__genmod
          INTERFACE 
            SUBROUTINE READJMAX(UFILE,MODELJM,NOLAY,NOAGEP,NONDATES,    &
     &DATESN,LEAFN,NOJDATES,DATESJ,JMAXTABLE,NOVDATES,DATESV,VCMAXTABLE,&
     &NOADATES,DATESA,AJQTABLE,IECOI,EAVJI,EDVJI,DELSJI,EAVCI,EDVCI,    &
     &DELSCI,TVJUPI,TVJDNI,THETAI)
              INTEGER(KIND=4) :: UFILE
              INTEGER(KIND=4) :: MODELJM
              INTEGER(KIND=4) :: NOLAY
              INTEGER(KIND=4) :: NOAGEP
              INTEGER(KIND=4) :: NONDATES
              INTEGER(KIND=4) :: DATESN(45)
              REAL(KIND=4) :: LEAFN(45,15,3)
              INTEGER(KIND=4) :: NOJDATES
              INTEGER(KIND=4) :: DATESJ(45)
              REAL(KIND=4) :: JMAXTABLE(45,15,3)
              INTEGER(KIND=4) :: NOVDATES
              INTEGER(KIND=4) :: DATESV(45)
              REAL(KIND=4) :: VCMAXTABLE(45,15,3)
              INTEGER(KIND=4) :: NOADATES
              INTEGER(KIND=4) :: DATESA(45)
              REAL(KIND=4) :: AJQTABLE(45,15,3)
              INTEGER(KIND=4) :: IECOI
              REAL(KIND=4) :: EAVJI
              REAL(KIND=4) :: EDVJI
              REAL(KIND=4) :: DELSJI
              REAL(KIND=4) :: EAVCI
              REAL(KIND=4) :: EDVCI
              REAL(KIND=4) :: DELSCI
              REAL(KIND=4) :: TVJUPI
              REAL(KIND=4) :: TVJDNI
              REAL(KIND=4) :: THETAI
            END SUBROUTINE READJMAX
          END INTERFACE 
        END MODULE READJMAX__genmod

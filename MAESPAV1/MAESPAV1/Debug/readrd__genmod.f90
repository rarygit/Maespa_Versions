        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:46 2016
        MODULE READRD__genmod
          INTERFACE 
            SUBROUTINE READRD(UFILE,MODELRD,NOLAY,NOAGEP,NONDATES,DATESN&
     &,LEAFN,NORDATES,DATESRD,RDTABLE,NOFQDATES,DATESRFQ,Q10FTABLE,K10F,&
     &RTEMPI,DAYRESPI,EFFYRFI,TBELOWI)
              INTEGER(KIND=4) :: UFILE
              INTEGER(KIND=4) :: MODELRD
              INTEGER(KIND=4) :: NOLAY
              INTEGER(KIND=4) :: NOAGEP
              INTEGER(KIND=4) :: NONDATES
              INTEGER(KIND=4) :: DATESN(30)
              REAL(KIND=4) :: LEAFN(30,15,3)
              INTEGER(KIND=4) :: NORDATES
              INTEGER(KIND=4) :: DATESRD(30)
              REAL(KIND=4) :: RDTABLE(30,15,3)
              INTEGER(KIND=4) :: NOFQDATES
              INTEGER(KIND=4) :: DATESRFQ(30)
              REAL(KIND=4) :: Q10FTABLE(30)
              REAL(KIND=4) :: K10F
              REAL(KIND=4) :: RTEMPI
              REAL(KIND=4) :: DAYRESPI
              REAL(KIND=4) :: EFFYRFI
              REAL(KIND=4) :: TBELOWI
            END SUBROUTINE READRD
          END INTERFACE 
        END MODULE READRD__genmod

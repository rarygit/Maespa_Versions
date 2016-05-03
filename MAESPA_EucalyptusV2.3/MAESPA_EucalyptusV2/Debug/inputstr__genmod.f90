        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:23 2016
        MODULE INPUTSTR__genmod
          INTERFACE 
            SUBROUTINE INPUTSTR(NSPECIES,STRFILES,JLEAF,BPT,RANDOM,     &
     &NOAGEC,JSHAPE,SHAPEC,EXTWIND,NALPHA,ALPHA,FALPHA,COEFFT,EXPONT,   &
     &WINTERC,BCOEFFT,BEXPONT,BINTERC,RCOEFFT,REXPONT,RINTERC,FRFRAC,   &
     &IN_PATH,DATESLIA2,NOLIADATES,DATESLAD,NOLADDATES)
              INTEGER(KIND=4) :: NSPECIES
              CHARACTER(LEN=30) :: STRFILES(4)
              INTEGER(KIND=4) :: JLEAF(4)
              REAL(KIND=4) :: BPT(8,3,4,1000)
              REAL(KIND=4) :: RANDOM(4)
              INTEGER(KIND=4) :: NOAGEC(4)
              INTEGER(KIND=4) :: JSHAPE(4)
              REAL(KIND=4) :: SHAPEC(4)
              REAL(KIND=4) :: EXTWIND(4)
              INTEGER(KIND=4) :: NALPHA(4)
              REAL(KIND=4) :: ALPHA(20,4)
              REAL(KIND=4) :: FALPHA(20,1000,4)
              REAL(KIND=4) :: COEFFT(4)
              REAL(KIND=4) :: EXPONT(4)
              REAL(KIND=4) :: WINTERC(4)
              REAL(KIND=4) :: BCOEFFT(4)
              REAL(KIND=4) :: BEXPONT(4)
              REAL(KIND=4) :: BINTERC(4)
              REAL(KIND=4) :: RCOEFFT(4)
              REAL(KIND=4) :: REXPONT(4)
              REAL(KIND=4) :: RINTERC(4)
              REAL(KIND=4) :: FRFRAC(4)
              CHARACTER(*) :: IN_PATH
              INTEGER(KIND=4) :: DATESLIA2(1000,4)
              INTEGER(KIND=4) :: NOLIADATES(4)
              INTEGER(KIND=4) :: DATESLAD(1000,4)
              INTEGER(KIND=4) :: NOLADDATES(4)
            END SUBROUTINE INPUTSTR
          END INTERFACE 
        END MODULE INPUTSTR__genmod

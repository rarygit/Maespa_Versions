        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 30 18:21:16 2016
        MODULE INPUTSTR__genmod
          INTERFACE 
            SUBROUTINE INPUTSTR(NSPECIES,STRFILES,JLEAF,BPT,RANDOM,     &
     &NOAGEC,JSHAPE,SHAPEC,EXTWIND,NALPHA,ALPHA,FALPHA,COEFFT,EXPONT,   &
     &WINTERC,BCOEFFT,BEXPONT,BINTERC,RCOEFFT,REXPONT,RINTERC,FRFRAC,   &
     &IN_PATH,DATESLIA2,NOLIADATES,DATESLAD,NOLADDATES)
              INTEGER(KIND=4) :: NSPECIES
              CHARACTER(LEN=30) :: STRFILES(5)
              INTEGER(KIND=4) :: JLEAF(5)
              REAL(KIND=4) :: BPT(8,3,5,45)
              REAL(KIND=4) :: RANDOM(5)
              INTEGER(KIND=4) :: NOAGEC(5)
              INTEGER(KIND=4) :: JSHAPE(5)
              REAL(KIND=4) :: SHAPEC(5)
              REAL(KIND=4) :: EXTWIND(5)
              INTEGER(KIND=4) :: NALPHA(5)
              REAL(KIND=4) :: ALPHA(20,5)
              REAL(KIND=4) :: FALPHA(20,45,5)
              REAL(KIND=4) :: COEFFT(5)
              REAL(KIND=4) :: EXPONT(5)
              REAL(KIND=4) :: WINTERC(5)
              REAL(KIND=4) :: BCOEFFT(5)
              REAL(KIND=4) :: BEXPONT(5)
              REAL(KIND=4) :: BINTERC(5)
              REAL(KIND=4) :: RCOEFFT(5)
              REAL(KIND=4) :: REXPONT(5)
              REAL(KIND=4) :: RINTERC(5)
              REAL(KIND=4) :: FRFRAC(5)
              CHARACTER(*) :: IN_PATH
              INTEGER(KIND=4) :: DATESLIA2(45,5)
              INTEGER(KIND=4) :: NOLIADATES(5)
              INTEGER(KIND=4) :: DATESLAD(45,5)
              INTEGER(KIND=4) :: NOLADDATES(5)
            END SUBROUTINE INPUTSTR
          END INTERFACE 
        END MODULE INPUTSTR__genmod

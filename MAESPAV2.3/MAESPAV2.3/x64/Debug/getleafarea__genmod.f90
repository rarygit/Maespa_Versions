        !COMPILER-GENERATED INTERFACE MODULE: Tue Dec 06 10:14:11 2016
        MODULE GETLEAFAREA__genmod
          INTERFACE 
            SUBROUTINE GETLEAFAREA(UFILE,IFLUSH,DT1I,DT2I,DT3I,DT4I,    &
     &EXPTIMEI,APP,EXPAN,NOALLTREES,NOLADATES,DATESLA,FLT)
              INTEGER(KIND=4) :: UFILE
              INTEGER(KIND=4) :: IFLUSH
              REAL(KIND=4) :: DT1I
              REAL(KIND=4) :: DT2I
              REAL(KIND=4) :: DT3I
              REAL(KIND=4) :: DT4I
              REAL(KIND=4) :: EXPTIMEI
              REAL(KIND=4) :: APP
              REAL(KIND=4) :: EXPAN
              INTEGER(KIND=4) :: NOALLTREES
              INTEGER(KIND=4) :: NOLADATES
              INTEGER(KIND=4) :: DATESLA(45)
              REAL(KIND=4) :: FLT(45,5000)
            END SUBROUTINE GETLEAFAREA
          END INTERFACE 
        END MODULE GETLEAFAREA__genmod
